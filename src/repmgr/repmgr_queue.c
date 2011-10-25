/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2006, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"

/*
 * Frees not only the queue header, but also any messages that may be on it,
 * along with their data buffers.
 *
 * PUBLIC: void __repmgr_queue_destroy __P((ENV *));
 */
void
__repmgr_queue_destroy(env)
	ENV *env;
{
	DB_REP *db_rep;
	REPMGR_MESSAGE *m;

	db_rep = env->rep_handle;

	while (!STAILQ_EMPTY(&db_rep->input_queue.header)) {
		m = STAILQ_FIRST(&db_rep->input_queue.header);
		STAILQ_REMOVE_HEAD(&db_rep->input_queue.header, entries);
		__os_free(env, m);
	}
}

/*
 * PUBLIC: int __repmgr_queue_get __P((ENV *,
 * PUBLIC:     REPMGR_MESSAGE **, REPMGR_RUNNABLE *));
 *
 * Get the first input message from the queue and return it to the caller.  The
 * caller hereby takes responsibility for the entire message buffer, and should
 * free it when done.
 *
 * Note that caller is NOT expected to hold the mutex.  This is asymmetric with
 * put(), because put() is expected to be called in a loop after select, where
 * it's already necessary to be holding the mutex.
 */
int
__repmgr_queue_get(env, msgp, th)
	ENV *env;
	REPMGR_MESSAGE **msgp;
	REPMGR_RUNNABLE *th;
{
	DB_REP *db_rep;
	REPMGR_MESSAGE *m;
#ifdef DB_WIN32
	HANDLE wait_events[2];
#endif
	int ret;

	ret = 0;
	db_rep = env->rep_handle;

	LOCK_MUTEX(db_rep->mutex);
	while (STAILQ_EMPTY(&db_rep->input_queue.header) &&
	    !db_rep->finished && !th->quit_requested) {
#ifdef DB_WIN32
		/*
		 * On Windows, msg_avail means either there's something in the
		 * queue, or we're all finished.  So, reset the event if that is
		 * not true.
		 */
		if (STAILQ_EMPTY(&db_rep->input_queue.header) &&
		    !db_rep->finished &&
		    !ResetEvent(db_rep->msg_avail)) {
			ret = GetLastError();
			goto err;
		}
		wait_events[0] = db_rep->msg_avail;
		wait_events[1] = th->quit_event;
		UNLOCK_MUTEX(db_rep->mutex);

		if ((ret = WaitForMultipleObjects(2,
		    wait_events, FALSE, INFINITE)) == WAIT_FAILED) {
			ret = GetLastError();
			goto out;
		}

		LOCK_MUTEX(db_rep->mutex);
#else
		if ((ret = pthread_cond_wait(&db_rep->msg_avail,
		    db_rep->mutex)) != 0)
			goto err;
#endif
	}
	if (db_rep->finished || th->quit_requested)
		ret = DB_REP_UNAVAIL;
	else {
		m = STAILQ_FIRST(&db_rep->input_queue.header);
		STAILQ_REMOVE_HEAD(&db_rep->input_queue.header, entries);
		db_rep->input_queue.size--;
		*msgp = m;
	}

err:
	UNLOCK_MUTEX(db_rep->mutex);
#ifdef DB_WIN32
out:
#endif
	return (ret);
}

/*
 * PUBLIC: int __repmgr_queue_put __P((ENV *, REPMGR_MESSAGE *));
 *
 * !!!
 * Caller must hold repmgr->mutex.
 */
int
__repmgr_queue_put(env, msg)
	ENV *env;
	REPMGR_MESSAGE *msg;
{
	DB_REP *db_rep;

	db_rep = env->rep_handle;

	STAILQ_INSERT_TAIL(&db_rep->input_queue.header, msg, entries);
	db_rep->input_queue.size++;

	return (__repmgr_signal(&db_rep->msg_avail));
}

/*
 * PUBLIC: int __repmgr_queue_size __P((ENV *));
 *
 * !!!
 * Caller must hold repmgr->mutex.
 */
int
__repmgr_queue_size(env)
	ENV *env;
{
	return (env->rep_handle->input_queue.size);
}
