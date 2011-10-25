/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2005, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"

/* Enables rerequest thread.  Set to 0 and rebuild to disable it. */
#define	USE_REREQ_THREAD 1

static int kick_blockers __P((ENV *, REPMGR_CONNECTION *, void *));
static int mismatch_err __P((const ENV *));
static int __repmgr_await_threads __P((ENV *));
static int __repmgr_restart __P((ENV *, int, u_int32_t));
static int __repmgr_start_msg_threads __P((ENV *, u_int));

/*
 * PUBLIC: int __repmgr_start __P((DB_ENV *, int, u_int32_t));
 */
int
__repmgr_start(dbenv, nthreads, flags)
	DB_ENV *dbenv;
	int nthreads;
	u_int32_t flags;
{
	DB_REP *db_rep;
	REP *rep;
	DB_THREAD_INFO *ip;
	ENV *env;
	int first, is_listener, locked, min, need_masterseek, ret;

	env = dbenv->env;
	db_rep = env->rep_handle;
	rep = db_rep->region;

	switch (flags) {
	case 0:
	case DB_REP_CLIENT:
	case DB_REP_ELECTION:
	case DB_REP_MASTER:
		break;
	default:
		__db_errx(env,
		    "repmgr_start: unrecognized flags parameter value");
		return (EINVAL);
	}

	ENV_REQUIRES_CONFIG_XX(
	    env, rep_handle, "DB_ENV->repmgr_start", DB_INIT_REP);
	if (!F_ISSET(env, ENV_THREAD)) {
		__db_errx(env,
		    "Replication Manager needs an environment with DB_THREAD");
		return (EINVAL);
	}

	if (APP_IS_BASEAPI(env)) {
		__db_errx(env,
"DB_ENV->repmgr_start: cannot call from base replication application");
		return (EINVAL);
	}

	/* Check that the required initialization has been done. */
	if (db_rep->my_addr.host == NULL) {
		__db_errx(env,
		    "repmgr_set_local_site must be called before repmgr_start");
		return (EINVAL);
	}

	if (db_rep->finished) {
		__db_errx(env, "repmgr is shutting down");
		return (EINVAL);
	}

	/*
	 * Figure out the current situation.  The current invocation of
	 * repmgr_start() is either the first one (on the given env handle), or
	 * a subsequent one.  If we've already got a select thread running, then
	 * this must be a subsequent one.
	 *
	 * Then, in case there could be multiple processes, we're either the
	 * main listener process or a subordinate process.  On a "subsequent"
	 * repmgr_start() call we already have enough information to know which
	 * it is.  Otherwise, negotiate with information in the shared region to
	 * claim the listener role if possible.
	 *
	 * To avoid a race, once we decide we're in the first call, start the
	 * select thread immediately, so that no other thread thinks the same
	 * thing.
	 */
	LOCK_MUTEX(db_rep->mutex);
	locked = TRUE;
	if (db_rep->selector == NULL) {
		first = TRUE;

		ENV_ENTER(env, ip);
		MUTEX_LOCK(env, rep->mtx_repmgr);
		if (rep->listener == 0) {
			is_listener = TRUE;
			__os_id(dbenv, &rep->listener, NULL);
		} else {
			is_listener = FALSE;
			nthreads = 0;
		}
		MUTEX_UNLOCK(env, rep->mtx_repmgr);
		ENV_LEAVE(env, ip);

		/*
		 * No select thread is running, so this must be the first call.
		 * Therefore, we require a flags value (to tell us how we are to
		 * be started).
		 */
		if (flags == 0) {
			__db_errx(env,
	  "a non-zero flags value is required for initial repmgr_start() call");
			ret = EINVAL;
			goto err;
		}

		if ((ret = __repmgr_init(env)) != 0)
			goto err;
		if (is_listener && (ret = __repmgr_listen(env)) != 0)
			goto err;
		if ((ret = __repmgr_start_selector(env)) != 0)
			goto err;
	} else {
		first = FALSE;
		is_listener = !IS_SUBORDINATE(db_rep);
	}
	UNLOCK_MUTEX(db_rep->mutex);
	locked = FALSE;

	if (!first) {
		/*
		 * Subsequent call is allowed when ELECTIONS are turned off, so
		 * that the application can make its own dynamic role changes.
		 * It's also allowed in any case, if not trying to change roles
		 * (flags == 0), in order to change number of message processing
		 * threads.  The __repmgr_restart() function will take care of
		 * these cases entirely.
		 */
		if (!is_listener || (flags != 0 &&
		    FLD_ISSET(db_rep->region->config, REP_C_ELECTIONS))) {
			__db_errx(env, "repmgr is already started");
			ret = EINVAL;
		} else
			ret = __repmgr_restart(env, nthreads, flags);
		return (ret);
	}

	/*
	 * The minimum legal number of threads is either 1 or 0, depending upon
	 * whether we're the main process or a subordinate.
	 */
	min = is_listener ? 1 : 0;
	if (nthreads < min) {
		__db_errx(env,
		    "repmgr_start: nthreads parameter must be >= %d", min);
		ret = EINVAL;
		goto err;
	}

	/*
	 * When using leases there are times when a thread processing a message
	 * must block, waiting for leases to be refreshed.  But refreshing the
	 * leases requires another thread to accept the lease grant messages.
	 *
	 * Note that it's OK to silently fudge the number here, because the
	 * documentation says that "[i]n addition to these message processing
	 * threads, the Replication Manager creates and manages a few of its own
	 * threads of control."
	 */
	if (nthreads < 2 && is_listener && IS_USING_LEASES(env))
		nthreads = 2;

	if ((ret = __rep_set_transport_int(env, SELF_EID, __repmgr_send)) != 0)
		goto err;
	if (is_listener) {
		/*
		 * Make some sort of call to rep_start before starting message
		 * processing threads, to ensure that incoming messages being
		 * processed always have a rep context properly configured.
		 * Note that even if we're starting without recovery, we need a
		 * rep_start call in case we're using leases.  Leases keep track
		 * of rep_start calls even within an env region lifetime.
		 */
		need_masterseek = FALSE;
		if ((db_rep->init_policy = flags) == DB_REP_MASTER) {
			if ((ret = __repmgr_repstart(env, DB_REP_MASTER)) != 0)
				goto err;
		} else {
			if ((ret = __repmgr_repstart(env, DB_REP_CLIENT)) != 0)
				goto err;
			if (rep->master_id == DB_EID_INVALID ||
			    rep->master_id == SELF_EID)
				need_masterseek = TRUE;
		}

		LOCK_MUTEX(db_rep->mutex);
		locked = TRUE;
#if USE_REREQ_THREAD
		if ((ret = __repmgr_start_rereq_thread(env)) != 0)
			goto err;
#endif

		/*
		 * Since these allocated memory blocks are used by other
		 * threads, we have to be a bit careful about freeing them in
		 * case of any errors.  __repmgr_await_threads (which we call in
		 * the err: coda below) takes care of that.
		 *
		 * Start by allocating enough space for 2 election threads.  We
		 * occasionally need that many; more are possible, but would be
		 * extremely rare.
		 */
#define	ELECT_THREADS_ALLOC	2

		if ((ret = __os_calloc(env, ELECT_THREADS_ALLOC,
		    sizeof(REPMGR_RUNNABLE *), &db_rep->elect_threads)) != 0)
			goto err;
		db_rep->aelect_threads = ELECT_THREADS_ALLOC;
		STAT(rep->mstat.st_max_elect_threads = ELECT_THREADS_ALLOC);

		if ((ret = __os_calloc(env, (u_int)nthreads,
		    sizeof(REPMGR_RUNNABLE *), &db_rep->messengers)) != 0)
			goto err;
		db_rep->athreads = (u_int)nthreads;

		db_rep->nthreads = 0;
		if ((ret =
		    __repmgr_start_msg_threads(env, (u_int)nthreads)) != 0)
			goto err;

		if (need_masterseek) {
			/*
			 * The repstart_time field records that time when we
			 * last issued a rep_start(CLIENT) that sent out a
			 * NEWCLIENT message.  We use it to avoid doing so
			 * twice in quick succession (to give the master a
			 * reasonable chance to respond).  The rep_start()
			 * that we just issued above doesn't count, because we
			 * haven't established any connections yet, and so no
			 * message could have been sent out.  The instant we
			 * get our first connection set up we want to send out
			 * our first real NEWCLIENT.
			 */
			timespecclear(&db_rep->repstart_time);

			if ((ret = __repmgr_init_election(env,
			    ELECT_F_STARTUP)) != 0)
				goto err;
		}
		UNLOCK_MUTEX(db_rep->mutex);
		locked = FALSE;
	}

	return (is_listener ? 0 : DB_REP_IGNORE);

err:
	/* If we couldn't succeed at everything, undo the parts we did do. */
	if (locked)
		UNLOCK_MUTEX(db_rep->mutex);
	if (db_rep->selector != NULL) {
		(void)__repmgr_stop_threads(env);
		(void)__repmgr_await_threads(env);
	}
	LOCK_MUTEX(db_rep->mutex);
	(void)__repmgr_net_close(env);
	if (REPMGR_INITED(db_rep))
		(void)__repmgr_deinit(env);
	UNLOCK_MUTEX(db_rep->mutex);
	return (ret);
}

/*
 * PUBLIC: int __repmgr_valid_config __P((ENV *, u_int32_t));
 */
int
__repmgr_valid_config(env, flags)
	ENV *env;
	u_int32_t flags;
{
	DB_REP *db_rep;
	int ret;

	db_rep = env->rep_handle;
	ret = 0;

	DB_ASSERT(env, REP_ON(env));
	LOCK_MUTEX(db_rep->mutex);

	/* (Can't check IS_SUBORDINATE if select thread isn't running yet.) */
	if (LF_ISSET(REP_C_ELECTIONS) &&
	    db_rep->selector != NULL && IS_SUBORDINATE(db_rep)) {
		__db_errx(env,
		   "can't configure repmgr elections from subordinate process");
		ret = EINVAL;
	}
	UNLOCK_MUTEX(db_rep->mutex);
	return (ret);
}

/*
 * Starts message processing threads.  On entry, the actual number of threads
 * already active is db_rep->nthreads; the desired number of threads is passed
 * as "n".
 *
 * Caller must hold mutex.
 */
static int
__repmgr_start_msg_threads(env, n)
	ENV *env;
	u_int n;
{
	DB_REP *db_rep;
	REPMGR_RUNNABLE *messenger;
	int ret;

	db_rep = env->rep_handle;
	DB_ASSERT(env, db_rep->athreads >= n);
	while (db_rep->nthreads < n) {
		if ((ret = __os_calloc(env,
		    1, sizeof(REPMGR_RUNNABLE), &messenger)) != 0)
			return (ret);

		messenger->run = __repmgr_msg_thread;
		if ((ret = __repmgr_thread_start(env, messenger)) != 0) {
			__os_free(env, messenger);
			return (ret);
		}
		db_rep->messengers[db_rep->nthreads++] = messenger;
	}
	return (0);
}

/*
 * Handles a repmgr_start() call that occurs when repmgr is already running.
 * This is allowed (when elections are not in use), to dynamically change
 * master/client role.  It is also allowed (regardless of the ELECTIONS setting)
 * to change the number of msg processing threads.
 */
static int
__repmgr_restart(env, nthreads, flags)
	ENV *env;
	int nthreads;
	u_int32_t flags;
{
	DB_REP *db_rep;
	REP *rep;
	REPMGR_RUNNABLE **th;
	u_int32_t cur_repflags;
	int locked, ret, t_ret;
	u_int delta, i, nth;

	th = NULL;
	ret = 0;
	locked = FALSE;

	if (flags == DB_REP_ELECTION) {
		__db_errx(env,
	      "subsequent repmgr_start() call may not specify DB_REP_ELECTION");
		return (EINVAL);
	}
	if (nthreads < 0) {
		__db_errx(env,
		    "repmgr_start: nthreads parameter must be >= 0");
		ret = EINVAL;
	}

	db_rep = env->rep_handle;
	DB_ASSERT(env, REP_ON(env));
	rep = db_rep->region;

	cur_repflags = F_ISSET(rep, REP_F_MASTER | REP_F_CLIENT);
	DB_ASSERT(env, cur_repflags);
	if (FLD_ISSET(cur_repflags, REP_F_MASTER) &&
	    flags == DB_REP_CLIENT)
		ret = __repmgr_repstart(env, DB_REP_CLIENT);
	else if (FLD_ISSET(cur_repflags, REP_F_CLIENT) &&
	    flags == DB_REP_MASTER)
		ret = __repmgr_repstart(env, DB_REP_MASTER);
	if (ret != 0)
		return (ret);

	if (nthreads == 0)
		return (0);
	if (nthreads == 1 && IS_USING_LEASES(env))
		nthreads = 2;
	nth = (u_int)nthreads;

	ret = 0;
	LOCK_MUTEX(db_rep->mutex);
	locked = TRUE;
	if (nth > db_rep->nthreads) {
		/*
		 * To increase the number of threads, first allocate more space,
		 * unless we already have enough unused space available.
		 */
		if (db_rep->athreads < nth) {
			if ((ret = __os_realloc(env,
			    sizeof(REPMGR_RUNNABLE *) * nth,
			    &db_rep->messengers)) != 0)
				goto out;
			db_rep->athreads = nth;
		}
		ret = __repmgr_start_msg_threads(env, nth);
	} else if (nth < db_rep->nthreads) {
		/*
		 * Remove losers from array, and then wait for each of them.  We
		 * have to make an array copy, because we have to drop the mutex
		 * to wait for the threads to complete, and if we left the real
		 * array in the handle in the pending state while waiting,
		 * another thread could come along wanting to make another
		 * change, and would make a mess.
		 *     The alternative is about as inelegant: we could do these
		 * one at a time here if we added another field to the handle,
		 * to keep track of both the actual number of threads and the
		 * user's desired number of threads.
		 */
		/*
		 * Make sure signalling the condition variable works, before
		 * making a mess of the data structures.  Although it may seem a
		 * little backwards, it doesn't really matter since we're
		 * holding the mutex.  Once we allocate the temp array and grab
		 * ownership of the loser thread structs, we must continue
		 * trying (even if errors) so that we definitely free the
		 * memory.
		 */
		if ((ret = __repmgr_wake_msngers(env, nth)) != 0)
			goto out;
		delta = db_rep->nthreads - nth;
		if ((ret = __os_calloc(env, (size_t)delta,
		    sizeof(REPMGR_RUNNABLE *), &th)) != 0)
			goto out;
		for (i = 0; i < delta; i++) {
			th[i] = db_rep->messengers[nth + i];
			th[i]->quit_requested = TRUE;
			db_rep->messengers[nth + i] = NULL;
		}
		db_rep->nthreads = nth;
		UNLOCK_MUTEX(db_rep->mutex);
		locked = FALSE;

		DB_ASSERT(env, ret == 0);
		for (i = 0; i < delta; i++) {
			if ((t_ret = __repmgr_thread_join(th[i])) != 0 &&
			    ret == 0)
				ret = t_ret;
			__os_free(env, th[i]);
		}
		__os_free(env, th);
	}

out:	if (locked)
		UNLOCK_MUTEX(db_rep->mutex);
	return (ret);
}

/*
 * PUBLIC: int __repmgr_autostart __P((ENV *));
 *
 * Preconditions: rep_start() has been called; we're within an ENV_ENTER.
 *     Because of this, we mustn't call __rep_set_transport(), but rather we
 *     poke in send() function address manually.
 */
int
__repmgr_autostart(env)
	ENV *env;
{
	DB_REP *db_rep;
	int ret;

	db_rep = env->rep_handle;

	DB_ASSERT(env, REP_ON(env));
	LOCK_MUTEX(db_rep->mutex);

	if (REPMGR_INITED(db_rep))
		ret = 0;
	else
		ret = __repmgr_init(env);
	if (ret != 0)
		goto out;

	RPRINT(env, (env, DB_VERB_REPMGR_MISC,
	    "Automatically joining existing repmgr env"));

	db_rep->send = __repmgr_send;

	if (db_rep->selector == NULL && !db_rep->finished)
		ret = __repmgr_start_selector(env);

out:
	UNLOCK_MUTEX(db_rep->mutex);
	return (ret);
}

/*
 * PUBLIC: int __repmgr_start_selector __P((ENV *));
 */
int
__repmgr_start_selector(env)
	ENV *env;
{
	DB_REP *db_rep;
	REPMGR_RUNNABLE *selector;
	int ret;

	db_rep = env->rep_handle;
	if ((ret = __os_calloc(env, 1, sizeof(REPMGR_RUNNABLE), &selector))
	    != 0)
		return (ret);
	selector->run = __repmgr_select_thread;

	/*
	 * In case the select thread ever examines db_rep->selector, set it
	 * before starting the thread (since once we create it we could be
	 * racing with it).
	 */
	db_rep->selector = selector;
	if ((ret = __repmgr_thread_start(env, selector)) != 0) {
		__db_err(env, ret, "can't start selector thread");
		__os_free(env, selector);
		db_rep->selector = NULL;
		return (ret);
	}

	return (0);
}

/*
 * PUBLIC: int __repmgr_close __P((ENV *));
 */
int
__repmgr_close(env)
	ENV *env;
{
	DB_REP *db_rep;
	int ret, t_ret;

	ret = 0;
	db_rep = env->rep_handle;
	if (db_rep->selector != NULL) {
		RPRINT(env, (env, DB_VERB_REPMGR_MISC,
		    "Stopping repmgr threads"));
		ret = __repmgr_stop_threads(env);
		if ((t_ret = __repmgr_await_threads(env)) != 0 && ret == 0)
			ret = t_ret;
		RPRINT(env, (env, DB_VERB_REPMGR_MISC,
		    "Repmgr threads are finished"));
	}

	if ((t_ret = __repmgr_net_close(env)) != 0 && ret == 0)
		ret = t_ret;

	if ((t_ret = __repmgr_deinit(env)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * PUBLIC: int __repmgr_set_ack_policy __P((DB_ENV *, int));
 */
int
__repmgr_set_ack_policy(dbenv, policy)
	DB_ENV *dbenv;
	int policy;
{
	DB_REP *db_rep;
	DB_THREAD_INFO *ip;
	ENV *env;
	REP *rep;

	env = dbenv->env;
	db_rep = env->rep_handle;
	rep = db_rep->region;

	ENV_NOT_CONFIGURED(
	    env, db_rep->region, "DB_ENV->repmgr_set_ack_policy", DB_INIT_REP);

	if (APP_IS_BASEAPI(env)) {
		__db_errx(env, "%s %s", "DB_ENV->repmgr_set_ack_policy:",
		    "cannot call from base replication application");
		return (EINVAL);
	}

	switch (policy) {
	case DB_REPMGR_ACKS_ALL: /* FALLTHROUGH */
	case DB_REPMGR_ACKS_ALL_AVAILABLE: /* FALLTHROUGH */
	case DB_REPMGR_ACKS_ALL_PEERS: /* FALLTHROUGH */
	case DB_REPMGR_ACKS_NONE: /* FALLTHROUGH */
	case DB_REPMGR_ACKS_ONE: /* FALLTHROUGH */
	case DB_REPMGR_ACKS_ONE_PEER: /* FALLTHROUGH */
	case DB_REPMGR_ACKS_QUORUM:
		if (REP_ON(env))
			rep->perm_policy = policy;
		else
			db_rep->perm_policy = policy;
		/*
		 * Setting an ack policy makes this a replication manager
		 * application.
		 */
		APP_SET_REPMGR(env);
		return (0);
	default:
		__db_errx(env,
		    "unknown ack_policy in DB_ENV->repmgr_set_ack_policy");
		return (EINVAL);
	}
}

/*
 * PUBLIC: int __repmgr_get_ack_policy __P((DB_ENV *, int *));
 */
int
__repmgr_get_ack_policy(dbenv, policy)
	DB_ENV *dbenv;
	int *policy;
{
	ENV *env;
	DB_REP *db_rep;
	REP *rep;

	env = dbenv->env;
	db_rep = env->rep_handle;
	rep = db_rep->region;

	*policy = REP_ON(env) ? rep->perm_policy : db_rep->perm_policy;
	return (0);
}

/*
 * PUBLIC: int __repmgr_env_create __P((ENV *, DB_REP *));
 */
int
__repmgr_env_create(env, db_rep)
	ENV *env;
	DB_REP *db_rep;
{
	COMPQUIET(env, NULL);

	/* Set some default values. */
	db_rep->ack_timeout = DB_REPMGR_DEFAULT_ACK_TIMEOUT;
	db_rep->connection_retry_wait = DB_REPMGR_DEFAULT_CONNECTION_RETRY;
	db_rep->election_retry_wait = DB_REPMGR_DEFAULT_ELECTION_RETRY;
	db_rep->config_nsites = 0;
	db_rep->perm_policy = DB_REPMGR_ACKS_QUORUM;
	FLD_SET(db_rep->config, REP_C_ELECTIONS);

	db_rep->listen_fd = INVALID_SOCKET;
	TAILQ_INIT(&db_rep->connections);
	TAILQ_INIT(&db_rep->retries);

	db_rep->input_queue.size = 0;
	STAILQ_INIT(&db_rep->input_queue.header);

	__repmgr_env_create_pf(db_rep);

	return (0);
}

/*
 * PUBLIC: void __repmgr_env_destroy __P((ENV *, DB_REP *));
 */
void
__repmgr_env_destroy(env, db_rep)
	ENV *env;
	DB_REP *db_rep;
{
	__repmgr_queue_destroy(env);
	__repmgr_net_destroy(env, db_rep);
	if (db_rep->messengers != NULL) {
		__os_free(env, db_rep->messengers);
		db_rep->messengers = NULL;
	}
}

/*
 * PUBLIC: int __repmgr_stop_threads __P((ENV *));
 */
int
__repmgr_stop_threads(env)
	ENV *env;
{
	DB_REP *db_rep;
	int ret;

	db_rep = env->rep_handle;

	/*
	 * Hold mutex for the purpose of waking up threads, but then get out of
	 * the way to let them clean up and exit.
	 */
	LOCK_MUTEX(db_rep->mutex);
	db_rep->finished = TRUE;
	if ((ret = __repmgr_signal(&db_rep->check_election)) != 0)
		goto unlock;

#if USE_REREQ_THREAD
	if ((ret = __repmgr_signal(&db_rep->check_rereq)) != 0)
		goto unlock;
#endif

	/*
	 * Because we've set "finished", it's enough to wake msg_avail, even on
	 * Windows.  (We don't need to wake per-thread Event Objects here, as we
	 * did in the case of only wanting to stop a subset of msg threads.)
	 */
	if ((ret = __repmgr_signal(&db_rep->msg_avail)) != 0)
		goto unlock;

	if ((ret = __repmgr_each_connection(env,
	    kick_blockers, NULL, TRUE)) != 0)
		goto unlock;
	UNLOCK_MUTEX(db_rep->mutex);

	return (__repmgr_wake_main_thread(env));

unlock:
	UNLOCK_MUTEX(db_rep->mutex);
	return (ret);
}

static int
kick_blockers(env, conn, unused)
	ENV *env;
	REPMGR_CONNECTION *conn;
	void *unused;
{
	COMPQUIET(env, NULL);
	COMPQUIET(unused, NULL);

	return (conn->blockers > 0 ? __repmgr_signal(&conn->drained) : 0);
}

static int
__repmgr_await_threads(env)
	ENV *env;
{
	DB_REP *db_rep;
	REPMGR_RUNNABLE *th;
	int ret, t_ret;
	u_int i;

	db_rep = env->rep_handle;
	ret = 0;
	for (i = 0; i < db_rep->aelect_threads; i++) {
		th = db_rep->elect_threads[i];
		if (th != NULL) {
			if ((t_ret = __repmgr_thread_join(th)) != 0 && ret == 0)
				ret = t_ret;
			__os_free(env, th);
		}
	}
	__os_free(env, db_rep->elect_threads);
	db_rep->aelect_threads = 0;

	for (i = 0;
	    i < db_rep->nthreads && db_rep->messengers[i] != NULL; i++) {
		th = db_rep->messengers[i];
		if ((t_ret = __repmgr_thread_join(th)) != 0 && ret == 0)
			ret = t_ret;
		__os_free(env, th);
	}
	__os_free(env, db_rep->messengers);
	db_rep->messengers = NULL;

	if (db_rep->selector != NULL) {
		if ((t_ret = __repmgr_thread_join(db_rep->selector)) != 0 &&
		    ret == 0)
			ret = t_ret;
		__os_free(env, db_rep->selector);
		db_rep->selector = NULL;
	}

#if USE_REREQ_THREAD
	if (db_rep->rereq_thread != NULL) {
		if ((t_ret =
		    __repmgr_thread_join(db_rep->rereq_thread)) != 0 &&
		    ret == 0)
			ret = t_ret;
		__os_free(env, db_rep->rereq_thread);
		db_rep->rereq_thread = NULL;
	}
#endif

	return (ret);
}

/*
 * PUBLIC: int __repmgr_set_local_site __P((DB_ENV *, const char *, u_int,
 * PUBLIC:     u_int32_t));
 */
int
__repmgr_set_local_site(dbenv, host, port, flags)
	DB_ENV *dbenv;
	const char *host;
	u_int port;
	u_int32_t flags;
{
	DB_REP *db_rep;
	DB_THREAD_INFO *ip;
	ENV *env;
	REGENV *renv;
	REGINFO *infop;
	REP *rep;
	repmgr_netaddr_t addr;
	char *myhost;
	int locked, ret;

	env = dbenv->env;
	db_rep = env->rep_handle;

	ENV_NOT_CONFIGURED(
	    env, db_rep->region, "DB_ENV->repmgr_set_local_site", DB_INIT_REP);

	if (APP_IS_BASEAPI(env)) {
		__db_errx(env, "%s %s", "DB_ENV->repmgr_set_local_site:",
		    "cannot call from base replication application");
		return (EINVAL);
	}

	if (db_rep->selector != NULL) {
		__db_errx(env,
"DB_ENV->repmgr_set_local_site: must be called before DB_ENV->repmgr_start");
		return (EINVAL);
	}

	if (flags != 0)
		return (__db_ferr(env, "DB_ENV->repmgr_set_local_site", 0));

	if (host == NULL || port == 0) {
		__db_errx(env,
		    "repmgr_set_local_site: host name and port (>0) required");
		return (EINVAL);
	}

	/*
	 * If the local site address hasn't already been set, just set it from
	 * the given inputs.  If it has, all we do is verify that it matches
	 * what had already been set previously.
	 *
	 * Do this in the shared region if we have one, or else just in the
	 * local handle.
	 *
	 * In either case, don't perturb global structures until we're sure
	 * everything will succeed.
	 */
	COMPQUIET(rep, NULL);
	COMPQUIET(ip, NULL);
	COMPQUIET(renv, NULL);
	locked = FALSE;
	ret = 0;
	if (REP_ON(env)) {
		rep = db_rep->region;
		ENV_ENTER(env, ip);
		MUTEX_LOCK(env, rep->mtx_repmgr);

		infop = env->reginfo;
		renv = infop->primary;
		MUTEX_LOCK(env, renv->mtx_regenv);
		locked = TRUE;
		if (rep->my_addr.host == INVALID_ROFF) {
			if ((ret = __repmgr_pack_netaddr(env,
			    host, port, NULL, &addr)) != 0)
				goto unlock;

			if ((ret = __env_alloc(infop,
			    strlen(host)+1, &myhost)) == 0) {
				(void)strcpy(myhost, host);
				rep->my_addr.host = R_OFFSET(infop, myhost);
				rep->my_addr.port = port;
			} else {
				__repmgr_cleanup_netaddr(env, &addr);
				goto unlock;
			}
			memcpy(&db_rep->my_addr, &addr, sizeof(addr));
			rep->siteinfo_seq++;
		} else {
			myhost = R_ADDR(infop, rep->my_addr.host);
			if (strcmp(myhost, host) != 0 ||
			    port != rep->my_addr.port) {
				ret = mismatch_err(env);
				goto unlock;
			}
		}
	} else {
		if (db_rep->my_addr.host == NULL) {
			if ((ret = __repmgr_pack_netaddr(env,
			    host, port, NULL, &db_rep->my_addr)) != 0)
				goto unlock;
		} else if (strcmp(host, db_rep->my_addr.host) != 0 ||
		    port != db_rep->my_addr.port) {
			ret = mismatch_err(env);
			goto unlock;
		}
	}

unlock:
	if (locked) {
		MUTEX_UNLOCK(env, renv->mtx_regenv);
		MUTEX_UNLOCK(env, rep->mtx_repmgr);
		ENV_LEAVE(env, ip);
	}
	/*
	 * Setting a local site makes this a replication manager application.
	 */
	if (ret == 0)
		APP_SET_REPMGR(env);
	return (ret);
}

/*
 * PUBLIC: int __repmgr_get_local_site __P((DB_ENV *, const char **, u_int *));
 */
int
__repmgr_get_local_site(dbenv, hostp, portp)
	DB_ENV *dbenv;
	const char **hostp;
	u_int *portp;
{
	DB_REP *db_rep;
	ENV *env;

	env = dbenv->env;
	db_rep = env->rep_handle;

	if (db_rep->my_addr.host == NULL) {
		__db_errx(env,
		    "local site address has not yet been set");
		return (EINVAL);
	}
	*hostp = db_rep->my_addr.host;
	*portp = db_rep->my_addr.port;
	return (0);
}

static int
mismatch_err(env)
	const ENV *env;
{
	__db_errx(env, "A (different) local site address has already been set");
	return (EINVAL);
}

/*
 * If the application only calls this method from a single thread (e.g., during
 * its initialization), it will avoid the problems with the non-thread-safe host
 * name lookup.  In any case, if we relegate the blocking lookup to here it
 * won't affect our select() loop.
 *
 * PUBLIC: int __repmgr_add_remote_site __P((DB_ENV *, const char *, u_int,
 * PUBLIC: int *, u_int32_t));
 */
int
__repmgr_add_remote_site(dbenv, host, port, eidp, flags)
	DB_ENV *dbenv;
	const char *host;
	u_int port;
	int *eidp;
	u_int32_t flags;
{
	DB_REP *db_rep;
	DB_THREAD_INFO *ip;
	ENV *env;
	REPMGR_SITE *site;
	int eid, locked, ret;

	env = dbenv->env;
	db_rep = env->rep_handle;
	locked = FALSE;
	ret = 0;

	ENV_NOT_CONFIGURED(
	    env, db_rep->region, "DB_ENV->repmgr_add_remote_site", DB_INIT_REP);

	if (APP_IS_BASEAPI(env)) {
		__db_errx(env, "%s %s", "DB_ENV->repmgr_add_remote_site:",
		    "cannot call from base replication application");
		return (EINVAL);
	}

	if ((ret = __db_fchk(env,
	    "DB_ENV->repmgr_add_remote_site", flags, DB_REPMGR_PEER)) != 0)
		return (ret);

	if (host == NULL) {
		__db_errx(env,
		    "repmgr_add_remote_site: host name is required");
		return (EINVAL);
	}

	if (REP_ON(env)) {
		LOCK_MUTEX(db_rep->mutex);
		locked = TRUE;

		ret = __repmgr_add_site(env, host, port, &site, flags, TRUE);
		if (ret == EEXIST) {
			/*
			 * With NEWSITE messages arriving at any time, it would
			 * be impractical for applications to avoid this.  Also
			 * this provides a way they can still set peer.
			 */
			ret = 0;
		}
		if (ret != 0)
			goto out;
		eid = EID_FROM_SITE(site);
		if (eidp != NULL)
			*eidp = eid;
	} else {
		if ((site = __repmgr_find_site(env, host, port)) == NULL) {
			if ((ret = __repmgr_new_site(env, &site, host, port,
			    SITE_IDLE, LF_ISSET(DB_REPMGR_PEER))) != 0)
				goto out;
		/*
		 * For an existing site, always update the remote peer flag
		 * according to the flags passed in.
		 */
		} else
			if (LF_ISSET(DB_REPMGR_PEER))
				F_SET(site, SITE_IS_PEER);
			else
				F_CLR(site, SITE_IS_PEER);
	}

out:
	if (locked)
		UNLOCK_MUTEX(db_rep->mutex);
	/*
	 * Adding a remote site makes this a replication manager application.
	 */
	if (ret == 0)
		APP_SET_REPMGR(env);
	return (ret);
}
