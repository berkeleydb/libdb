/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2005, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/lock.h"
#include "dbinc/txn.h"

/*
 * __lock_failchk --
 *	Check for locks held by dead threads of control and release
 *	read locks.  If any write locks were held by dead non-trasnactional
 *	lockers then we must abort and run recovery.  Otherwise we release
 *	read locks for lockers owned by dead threads.  Write locks for
 *	dead transactional lockers will be freed when we abort the transaction.
 *
 * PUBLIC: int __lock_failchk __P((ENV *));
 */
int
__lock_failchk(env)
	ENV *env;
{
	DB_ENV *dbenv;
	DB_LOCKER *lip;
	DB_LOCKREGION *lrp;
	DB_LOCKREQ request;
	DB_LOCKTAB *lt;
	struct __db_lock *lp;
	u_int32_t i;
	int released, ret;
	char buf[DB_THREADID_STRLEN];

	dbenv = env->dbenv;
	lt = env->lk_handle;
	lrp = lt->reginfo.primary;

retry:	LOCK_LOCKERS(env, lrp);

	ret = 0;
	for (i = 0; i < lrp->locker_t_size; i++)
		SH_TAILQ_FOREACH(lip, &lt->locker_tab[i], links, __db_locker) {
			/*
			 * If the locker is transactional, we can ignore it if
			 * it has no read locks or has no locks at all.  Check
			 * the heldby list rather then nlocks since a lock may
			 * be PENDING.  __txn_failchk aborts any transactional
			 * lockers.  Non-transactional lockers progress to
			 * is_alive test.
			 */
			if ((lip->id >= TXN_MINIMUM) &&
			     (SH_LIST_EMPTY(&lip->heldby) ||
			     lip->nlocks == lip->nwrites))
				continue;

			/* If the locker is still alive, it's not a problem. */
			if (dbenv->is_alive(dbenv, lip->pid, lip->tid,
			    F_ISSET(lip, DB_LOCKER_HANDLE_LOCKER) ?
			    DB_MUTEX_PROCESS_ONLY : 0))
				continue;

			/*
			 * Does this locker hold anything THIS function can
			 * actually release?  Only two things happen below: the
			 * DB_LOCK_PUT_READ request, which releases just
			 * DB_LOCK_READ and DB_LOCK_READ_UNCOMMITTED (the
			 * writes==0 arm of __lock_vec), and __lock_freelocker,
			 * which is reached only for a NON-transactional locker
			 * (id < TXN_MINIMUM).
			 *
			 * So for a dead TRANSACTIONAL locker holding neither of
			 * those modes, every statement below is a no-op and the
			 * `goto retry' at the end of the body re-walks an
			 * unchanged table -- forever.  That is not theoretical:
			 * a read-only DB_TXN_SERIALIZABLE transaction whose
			 * process is killed leaves exactly this shape, one
			 * DB_LOCK_SIREAD marker on heldby with nlocks=1 and
			 * nwrites=0, so the skip test above does not fire
			 * either (a SIREAD marker counts in nlocks but is not a
			 * write lock).  Measured before this guard: 45,284,819
			 * identical BDB2053 lines -- a 3.1 GB log -- for a
			 * single locker id, and DB_ENV->failchk never returned.
			 * SIREAD is RETAINED by PUT_READ deliberately (see the
			 * mode enumeration in __lock_vec and issue #140), so
			 * this can never make progress here.
			 *
			 * Such a locker is not ours to clean: __txn_failchk
			 * aborts the transaction, which releases the marker.
			 * But __env_failchk_int calls __lock_failchk BEFORE
			 * __txn_failchk (see env_failchk.c), so spinning here
			 * waits for a state only the later pass can produce.
			 * Leave it alone and carry on with the walk.
			 *
			 * NOTE this is NOT specific to lk_partitions=1; it
			 * reproduces identically with the default partitioning,
			 * so the tracker's original framing of S5 as a
			 * one-partition problem was wrong and the v2026.09.6
			 * latch-alias fix was never going to address it.
			 */
			if (lip->id >= TXN_MINIMUM) {
				released = 0;
				SH_LIST_FOREACH(lp, &lip->heldby,
				    locker_links, __db_lock)
					if (lp->mode == DB_LOCK_READ ||
					    lp->mode ==
					    DB_LOCK_READ_UNCOMMITTED) {
						released = 1;
						break;
					}
				if (released == 0)
					continue;
			}

			/*
			 * We can only deal with read locks.  If a
			 * non-transactional locker holds write locks we
			 * have to assume a Berkeley DB operation was
			 * interrupted with only 1-of-N pages modified.
			 */
			if (lip->id < TXN_MINIMUM && lip->nwrites != 0) {
				ret = __db_failed(env, DB_STR_A("2052",
				    "locker has write locks", ""),
				     lip->pid, lip->tid);
				break;
			}

			/*
			 * Discard the locker and its read locks.
			 */
			if (!SH_LIST_EMPTY(&lip->heldby)) {
				__db_msg(env, DB_STR_A("2053",
				    "Freeing read locks for locker %#lx: %s",
				    "%#lx %s"), (u_long)lip->id,
				    dbenv->thread_id_string(
				    dbenv, lip->pid, lip->tid, buf));
				UNLOCK_LOCKERS(env, lrp);
				memset(&request, 0, sizeof(request));
				request.op = DB_LOCK_PUT_READ;
				if ((ret = __lock_vec(env,
				    lip, 0, &request, 1, NULL)) != 0)
					return (ret);
			}
			else
				UNLOCK_LOCKERS(env, lrp);

			/*
			 * This locker is most likely referenced by a cursor
			 * which is owned by a dead thread.  Normally the
			 * cursor would be available for other threads
			 * but we assume the dead thread will never release
			 * it.
			 */
			if (lip->id < TXN_MINIMUM &&
			    (ret = __lock_freelocker(lt, lip)) != 0)
				return (ret);
			goto retry;
		}

	UNLOCK_LOCKERS(env, lrp);

	return (ret);
}
