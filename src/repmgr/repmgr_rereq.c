/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2009, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"

static int rereq_loop __P((ENV *));

/*
 * PUBLIC: int __repmgr_start_rereq_thread __P((ENV *));
 *
 * The rerequest thread runs on repmgr clients to periodically check
 * for and request missing information.  The missing information is
 * most often log records to fill in gaps, but can also be database
 * pages.  The time to wait is taken from the user-configured maximum
 * time for a client to wait before requesting transmission of a missing
 * message (max_gap).
 */
int
__repmgr_start_rereq_thread(env)
	ENV *env;
{
	DB_REP *db_rep;
	REPMGR_RUNNABLE *rereq_thread;
	int ret;

	db_rep = env->rep_handle;
	if ((ret = __os_calloc(env, 1, sizeof(REPMGR_RUNNABLE), &rereq_thread))
	    != 0)
		return (ret);
	rereq_thread->run = __repmgr_rereq_thread;

	/*
	 * In case the rereq thread ever examines db_rep->rereq_thread, set it
	 * before starting the thread (since once we create it we could be
	 * racing with it).
	 */
	db_rep->rereq_thread = rereq_thread;
	if ((ret = __repmgr_thread_start(env, rereq_thread)) != 0) {
		__db_err(env, ret, "can't start rereq thread");
		__os_free(env, rereq_thread);
		db_rep->rereq_thread = NULL;
		return (ret);
	}

	return (0);
}

/*
 * PUBLIC: void *__repmgr_rereq_thread __P((void *));
 */
void *
__repmgr_rereq_thread(argsp)
	void *argsp;
{
	REPMGR_RUNNABLE *args;
	ENV *env;
	int ret;

	args = argsp;
	env = args->env;

	if ((ret = rereq_loop(env)) != 0) {
		__db_err(env, ret, "rereq thread failed");
		__repmgr_thread_failure(env, ret);
	}
	return (NULL);
}

static int
rereq_loop(env)
	ENV *env;
{
	DB_REP *db_rep;
	REP *rep;
#ifdef DB_WIN32
	DWORD duration;
	db_timeout_t t;
#else
	struct timespec deadline;
	db_timespec wait_til;
#endif
	int known, ret;

	db_rep = env->rep_handle;
	rep = db_rep->region;
	ret = 0;

	for (;;) {
		LOCK_MUTEX(db_rep->mutex);
		/* Terminate loop if repmgr finished or if master. */
		if (db_rep->finished || rep->master_id == SELF_EID)
			goto unlock;
#ifdef DB_WIN32
		DB_TIMESPEC_TO_TIMEOUT(t, &rep->max_gap, TRUE);
		duration = t / US_PER_MS;
		/* SignalObjectAndWait leaves mutex unlocked. */
		if ((ret = SignalObjectAndWait(*db_rep->mutex,
		    db_rep->check_rereq, duration, FALSE)) !=
		    WAIT_OBJECT_0 && ret != WAIT_TIMEOUT)
			goto out;
		LOCK_MUTEX(db_rep->mutex);
#else
		__os_gettime(env, &wait_til, 1);
		timespecadd(&wait_til, &rep->max_gap);
		deadline.tv_sec = wait_til.tv_sec;
		deadline.tv_nsec = wait_til.tv_nsec;
		/* pthread_cond_timedwait leaves mutex locked. */
		if ((ret = pthread_cond_timedwait(
		    &db_rep->check_rereq, db_rep->mutex, &deadline))
		    != ETIMEDOUT && ret != 0)
			goto unlock;
#endif
		/*
		 * Clear out any expected error status from the wait
		 * and recheck loop termination conditions.
		 */
		ret = 0;
		if (db_rep->finished || rep->master_id == SELF_EID)
			goto unlock;
		/*
		 * Call into core replication to check for and request
		 * any missing information.
		 */
		known = __repmgr_master_is_known(env);
		UNLOCK_MUTEX(db_rep->mutex);
		if (known && (ret = __rep_check_missing(env)) != 0)
			goto out;
	}
unlock:
	UNLOCK_MUTEX(db_rep->mutex);
out:
	return (ret);
}
