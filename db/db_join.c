/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)db_join.c	11.6 (Sleepycat) 10/19/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "db_join.h"
#include "db_am.h"
#include "btree.h"

static int __db_join_close __P((DBC *));
static int __db_join_del __P((DBC *, u_int32_t));
static int __db_join_get __P((DBC *, DBT *, DBT *, u_int32_t));
static int __db_join_getnext __P((DBC *, DBT *, DBT *, DBT *, u_int32_t));
static int __db_join_put __P((DBC *, DBT *, DBT *, u_int32_t));

/*
 * This is the duplicate-assisted join functionality.  Right now we're
 * going to write it such that we return one item at a time, although
 * I think we may need to optimize it to return them all at once.
 * It should be easier to get it working this way, and I believe that
 * changing it should be fairly straightforward.
 *
 * XXX
 * Right now we do not maintain the number of duplicates so we do
 * not optimize the join.  If the caller does, then best performance
 * will be achieved by putting the cursor with the smallest cardinality
 * first.
 *
 * The first cursor moves sequentially through the duplicate set while
 * the others search explicitly for the duplicate in question.
 *
 */

/*
 * __db_join --
 *	This is the interface to the duplicate-assisted join functionality.
 * In the same way that cursors mark a position in a database, a cursor
 * can mark a position in a join.  While most cursors are created by the
 * cursor method of a DB, join cursors are created through an explicit
 * call to DB->join.
 *
 * The curslist is an array of existing, intialized cursors and primary
 * is the DB of the primary file.  The data item that joins all the
 * cursors in the curslist is used as the key into the primary and that
 * key and data are returned.  When no more items are left in the join
 * set, the  c_next operation off the join cursor will return DB_NOTFOUND.
 *
 * PUBLIC: int __db_join __P((DB *, DBC **, DBC **, u_int32_t));
 */
int
__db_join(primary, curslist, dbcp, flags)
	DB *primary;
	DBC **curslist, **dbcp;
	u_int32_t flags;
{
	DBC *dbc;
	JOIN_CURSOR *jc;
	int i, ret, nslots;

	COMPQUIET(nslots, 0);

	PANIC_CHECK(primary->dbenv);

	if ((ret = __db_joinchk(primary, flags)) != 0)
		return (ret);

	if (curslist == NULL || curslist[0] == NULL)
		return (EINVAL);

	dbc = NULL;
	jc = NULL;

	if ((ret = __os_calloc(1, sizeof(DBC), &dbc)) != 0)
		goto err;

	if ((ret = __os_calloc(1, sizeof(JOIN_CURSOR), &jc)) != 0)
		goto err;

	if ((ret = __os_malloc(256, NULL, &jc->j_key.data)) != 0)
		goto err;
	jc->j_key.ulen = 256;
	F_SET(&jc->j_key, DB_DBT_USERMEM);

	for (jc->j_curslist = curslist;
	    *jc->j_curslist != NULL; jc->j_curslist++)
		;

	/*
	 * The number of cursor slots we allocate is one greater than
	 * the number of cursors involved in the join, because the
	 * list is NULL-terminated.
	 */
	nslots = jc->j_curslist - curslist + 1;

	/*
	 * !!! -- A note on the various lists hanging off jc.
	 *
	 * j_curslist is the initial NULL-terminated list of cursors passed
	 * into __db_join.  The original cursors are not modified; pristine
	 * copies are required because, in databases with unsorted dups, we
	 * must reset all of the secondary cursors after the first each
	 * time the first one is incremented, or else we will lose data
	 * which happen to be sorted differently in two different cursors.
	 *
	 * j_workcurs is where we put those copies that we're planning to
	 * work with.  They're lazily c_dup'ed from j_curslist as we need
	 * them, and closed when the join cursor is closed or when we need
	 * to reset them to their original values (in which case we just
	 * c_dup afresh).
	 *
	 * j_fdupcurs is an array of cursors which point to the first
	 * duplicate in the duplicate set that contains the data value
	 * we're currently interested in.  We need this to make
	 * __db_join_get correctly return duplicate duplicates;  i.e., if a
	 * given data value occurs twice in the set belonging to cursor #2,
	 * and thrice in the set belonging to cursor #3, and once in all
	 * the other cursors, successive calls to __db_join_get need to
	 * return that data item six times.  To make this happen, each time
	 * cursor N is allowed to advance to a new datum, all cursors M
	 * such that M > N have to be reset to the first duplicate with
	 * that datum, so __db_join_get will return all the dup-dups again.
	 * We could just reset them to the original cursor from j_curslist,
	 * but that would be a bit slower in the unsorted case and a LOT
	 * slower in the sorted one.
	 *
	 * j_exhausted is a list of boolean values which represent
	 * whether or not their corresponding cursors are "exhausted",
	 * i.e. whether the datum under the corresponding cursor has
	 * been found not to exist in any unreturned combinations of
	 * later secondary cursors, in which case they are ready to be
	 * incremented.
	 */

	/* We don't want to free regions whose callocs have failed. */
	jc->j_curslist = NULL;
	jc->j_workcurs = NULL;
	jc->j_fdupcurs = NULL;
	jc->j_exhausted = NULL;

	if ((ret = __os_calloc(nslots, sizeof(DBC *),
	    &jc->j_curslist)) != 0)
		goto err;
	if ((ret = __os_calloc(nslots, sizeof(DBC *),
	    &jc->j_workcurs)) != 0)
		goto err;
	if ((ret = __os_calloc(nslots, sizeof(DBC *),
	    &jc->j_fdupcurs)) != 0)
		goto err;
	if ((ret = __os_calloc(nslots, sizeof(u_int8_t),
	    &jc->j_exhausted)) != 0)
		goto err;
	for (i = 0; curslist[i] != NULL; i++) {
		jc->j_curslist[i] = curslist[i];
		jc->j_workcurs[i] = NULL;
		jc->j_fdupcurs[i] = NULL;
		jc->j_exhausted[i] = 0;
	}

	/*
	 * We never need to reset the 0th cursor, so there's no
	 * solid reason to use workcurs[0] rather than curslist[0] in
	 * join_get.  Nonetheless, it feels cleaner to do it for symmetry,
	 * and this is the most logical place to copy it.
	 *
	 * !!!
	 * There's no need to close the new cursor if we goto err only
	 * because this is the last thing that can fail.  Modifier of this
	 * function beware!
	 */
	if ((ret = __os_malloc(sizeof(DBC), NULL, jc->j_workcurs)) != 0)
		goto err;
	if ((ret = jc->j_curslist[0]->c_dup(jc->j_curslist[0], jc->j_workcurs,
	    DB_POSITIONI)) != 0)
		goto err;

	dbc->c_close = __db_join_close;
	dbc->c_del = __db_join_del;
	dbc->c_get = __db_join_get;
	dbc->c_put = __db_join_put;
	dbc->internal = jc;
	dbc->dbp = primary;
	jc->j_primary = primary;

	*dbcp = dbc;

	return (0);

err:	if (jc != NULL) {
		if (jc->j_curslist != NULL)
			__os_free(jc->j_curslist, nslots * sizeof(DBC *));
		if (jc->j_workcurs != NULL) {
			if (jc->j_workcurs[0] != NULL)
				__os_free(jc->j_workcurs[0], sizeof(DBC));
			__os_free(jc->j_workcurs, nslots * sizeof(DBC *));
		}
		if (jc->j_fdupcurs != NULL)
			__os_free(jc->j_fdupcurs, nslots * sizeof(DBC *));
		if (jc->j_exhausted != NULL)
			__os_free(jc->j_exhausted, nslots * sizeof(u_int8_t));
		__os_free(jc, sizeof(JOIN_CURSOR));
	}
	if (dbc != NULL)
		__os_free(dbc, sizeof(DBC));
	return (ret);
}

static int
__db_join_put(dbc, key, data, flags)
	DBC *dbc;
	DBT *key;
	DBT *data;
	u_int32_t flags;
{
	PANIC_CHECK(dbc->dbp->dbenv);

	COMPQUIET(key, NULL);
	COMPQUIET(data, NULL);
	COMPQUIET(flags, 0);
	return (EINVAL);
}

static int
__db_join_del(dbc, flags)
	DBC *dbc;
	u_int32_t flags;
{
	PANIC_CHECK(dbc->dbp->dbenv);

	COMPQUIET(flags, 0);
	return (EINVAL);
}

static int
__db_join_get(dbc, key, data, flags)
	DBC *dbc;
	DBT *key, *data;
	u_int32_t flags;
{
	DBT currkey;
	DB *dbp;
	DBC *cp;
	JOIN_CURSOR *jc;
	int ret, i, j;
	u_int32_t operation;

	dbp = dbc->dbp;
	memset(&currkey, 0, sizeof(currkey));

	PANIC_CHECK(dbp->dbenv);

	operation = LF_ISSET(DB_OPFLAGS_MASK);
	if (operation != 0 && operation != DB_JOIN_ITEM)
		return (__db_ferr(dbp->dbenv, "DBcursor->c_get", 0));

	LF_CLR(DB_OPFLAGS_MASK);
	if ((ret =
	    __db_fchk(dbp->dbenv, "DBcursor->c_get", flags, DB_RMW)) != 0)
		return (ret);

	/*
	 * Partial gets on join cursors don't make much sense, and the
	 * DBT_PARTIAL flag is liable to produce some rather strange
	 * results given the weird way the DBTs are used ("key" is used as
	 * the datum in all the secondary cursors), so we simply
	 * disallow it.
	 */
	if (F_ISSET(key, DB_DBT_PARTIAL) || F_ISSET(data, DB_DBT_PARTIAL))
		return (EINVAL);

	jc = (JOIN_CURSOR *)dbc->internal;

retry:
	ret = jc->j_workcurs[0]->c_get(jc->j_workcurs[0],
	    &jc->j_key, key, jc->j_exhausted[0] ? DB_NEXT_DUP : DB_CURRENT);

	if (ret == ENOMEM) {
		jc->j_key.ulen <<= 1;
		if ((ret =
		    __os_realloc(jc->j_key.ulen, NULL, &jc->j_key.data)) != 0)
			goto err;
		goto retry;
	}

	/*
	 * If ret == DB_NOTFOUND, we're out of elements of the first
	 * secondary cursor.  This is how we finally finish the join
	 * if all goes well.
	 */
	if (ret != 0)
		goto err;

	/*
	 * Copy key into currkey;  this is the current duplicate data
	 * value that we're interested in, which we will use for comparison
	 * purposes with c_gets on all the other secondary cursors.
	 */

	if ((ret = __os_realloc(key->size, NULL, &currkey.data)) != 0)
		goto err;
	memcpy(currkey.data, key->data, key->size);
	currkey.size = key->size;

	/*
	 * If jc->j_curslist[1] == NULL, we have only one cursor in the join.
	 * Thus, we can safely increment that one cursor on each call
	 * to __db_join_get, and we signal this by setting jc->j_exhausted[0]
	 * right away.
	 *
	 * Otherwise, reset jc->j_exhausted[0] to 0, so that we don't
	 * increment it until we know we're ready to.
	 */
	if (jc->j_curslist[1] == NULL)
		jc->j_exhausted[0] = 1;
	else
		jc->j_exhausted[0] = 0;

	/* We have the first element; now look for it in the other cursors. */
	for (i = 1; jc->j_curslist[i] != NULL; i++) {
		if (jc->j_workcurs[i] == NULL)
			/* If this is NULL, we need to dup curslist into it. */
			if ((ret = jc->j_curslist[i]->c_dup(
			    jc->j_curslist[i], jc->j_workcurs + i,
			    DB_POSITIONI)) != 0)
				goto err;
retry2:
		cp = jc->j_workcurs[i];

		if ((ret = __db_join_getnext(cp, &jc->j_key, key, &currkey,
			    jc->j_exhausted[i])) == DB_NOTFOUND) {
			/*
			 * jc->j_workcurs[i] has no more of the datum we're
			 * interested in.  Go back one cursor and get
			 * a new dup.  We can't just move to a new
			 * element of the outer relation, because that way
			 * we might miss duplicate duplicates in cursor i-1.
			 *
			 * If this takes us back to the first cursor,
			 * -then- we can move to a new element of the outer
			 * relation.
			 */
			--i;
			jc->j_exhausted[i] = 1;

			if (i == 0) {
				for (j = 1; jc->j_workcurs[j] != NULL; j++) {
					/*
					 * We're moving to a new element of
					 * the first secondary cursor.  If
					 * that cursor is sorted, then any
					 * other sorted cursors can be safely
					 * reset to the first duplicate
					 * duplicate in the current set if we
					 * have a pointer to it (we can't just
					 * leave them be, or we'll miss
					 * duplicate duplicates in the outer
					 * relation).
					 *
					 * If the first cursor is unsorted, or
					 * if cursor j is unsorted, we can
					 * make no assumptions about what
					 * we're looking for next or where it
					 * will be, so we reset to the very
					 * beginning (setting workcurs NULL
					 * will achieve this next go-round).
					 *
					 * XXX: This is likely to break
					 * horribly if any two cursors are
					 * both sorted, but have different
					 * specified sort functions.  For,
					 * now, we dismiss this as pathology
					 * and let strange things happen--we
					 * can't make rope childproof.
					 */
					if ((ret = jc->j_workcurs[j]->c_close(
					    jc->j_workcurs[j])) != 0)
						goto err;
					if ((jc->j_workcurs[0]->dbp->dup_compare
					    == NULL) ||
					    (jc->j_workcurs[j]->dbp->dup_compare
					    == NULL) ||
					    jc->j_fdupcurs[j] == NULL)
						/*
						 * Unsafe conditions;
						 * reset fully.
						 */
						jc->j_workcurs[j] = NULL;
					else
						/* Partial reset suffices. */
						if ((jc->j_fdupcurs[j]->c_dup(
						    jc->j_fdupcurs[j],
						    &jc->j_workcurs[j],
						    DB_POSITIONI)) != 0)
							goto err;
					jc->j_exhausted[j] = 0;
				}
				goto retry;
				/* NOTREACHED */
			}

			/*
			 * We're about to advance the cursor and need to
			 * reset all of the workcurs[j] where j>i, so that
			 * we don't miss any duplicate duplicates.
			 */
			for (j = i + 1;
			    jc->j_workcurs[j] != NULL;
			    j++) {
				if ((ret = jc->j_workcurs[j]->c_close(
				    jc->j_workcurs[j])) != 0)
					goto err;
				if (jc->j_fdupcurs[j] != NULL) {
					if ((ret = jc->j_fdupcurs[j]->c_dup(
					    jc->j_fdupcurs[j],
					    &jc->j_workcurs[j],
					    DB_POSITIONI)) != 0)
						goto err;
					jc->j_exhausted[j] = 0;
				} else
					jc->j_workcurs[j] = NULL;
			}
			goto retry2;
		        /* NOTREACHED */
		}

		if (ret == ENOMEM) {
			jc->j_key.ulen <<= 1;
			if ((ret = __os_realloc(jc->j_key.ulen,
			    NULL, &jc->j_key.data)) != 0)
				goto err;
			goto retry2;
		}

		if (ret != 0)
			goto err;

		/*
		 * If we made it this far, we've found a matching
		 * datum in cursor i.  Mark the current cursor
		 * unexhausted, so we don't miss any duplicate
		 * duplicates the next go-round--unless this is the
		 * very last cursor, in which case there are none to
		 * miss, and we'll need that exhausted flag to finally
		 * get a DB_NOTFOUND and move on to the next datum in
		 * the outermost cursor.
		 */
		if (jc->j_curslist[i + 1] != NULL)
			jc->j_exhausted[i] = 0;
		else
			jc->j_exhausted[i] = 1;


		/*
		 * If jc->j_fdupcurs[i] is NULL, this is the first
		 * time we've gotten this far since the original
		 * __db_join.  If jc->j_exhausted[0] == 1, it's the
		 * first time we're here since advancing cursor 0.  In
		 * either case, we have a new datum of interest, and
		 * we set jc->j_fdupcurs[i], which stores the first
		 * duplicate duplicate of the current datum.
		 */
		if (jc->j_exhausted[0] == 1 || jc->j_fdupcurs[i] == NULL) {
			if (jc->j_fdupcurs[i] != NULL)
				if ((ret = jc->j_fdupcurs[i]->c_close(
				    jc->j_fdupcurs[i])) != 0)
					goto err;
			if ((ret = cp->c_dup(cp, &jc->j_fdupcurs[i],
			    DB_POSITIONI)) != 0)
				goto err;
		}

	}

err:
	/*
	 * We're done with this;  free it now, before
	 * both error and regular returns.
	 */
	if (currkey.data != NULL)
		__os_free(currkey.data, 0);

	if (ret != 0)
		return (ret);
	/*
	 * ret == 0;  we have a key to return.  If DB_JOIN_ITEM is
	 * set, we return it;  otherwise we do the lookup in the
	 * primary and then return.
	 */

	if (operation == DB_JOIN_ITEM)
		return (0);
	else
		return ((jc->j_primary->get)(jc->j_primary,
		    jc->j_curslist[0]->txn, key, data, 0));
}

static int
__db_join_close(dbc)
	DBC *dbc;
{
	JOIN_CURSOR *jc;
	int i, ret, t_ret;

	PANIC_CHECK(dbc->dbp->dbenv);

	jc = (JOIN_CURSOR *)dbc->internal;
	ret = t_ret = 0;


	/*
	 * Close any open scratch cursors.  In each case, there may
	 * not be as many outstanding as there are cursors in
	 * curslist, but the first NULL we hit will be after the last
	 * of whatever's there.  If one of them fails, there's no
	 * reason not to close everything else; we'll just return the
	 * error code of the last one to fail.  There's not much the
	 * caller can do anyway, since this cursor only exists hanging
	 * off a db-internal data structure that they shouldn't be
	 * mucking with.
	 */
	for (i = 0; jc->j_workcurs[i] != NULL; i++)
		if((t_ret = jc->j_workcurs[i]->c_close(jc->j_workcurs[i])) != 0)
			ret = t_ret;
	for (i = 0; jc->j_fdupcurs[i] != NULL; i++)
		if((t_ret = jc->j_fdupcurs[i]->c_close(jc->j_fdupcurs[i])) != 0)
			ret = t_ret;

	__os_free(jc->j_exhausted, 0);
	__os_free(jc->j_curslist, 0);
	__os_free(jc->j_key.data, jc->j_key.ulen);
	__os_free(jc, sizeof(JOIN_CURSOR));
	__os_free(dbc, sizeof(DBC));

	return (ret);
}


/*
 * __db_join_getnext--
 * 	This function replaces the DBC_CONTINUE and DBC_KEYSET
 * 	functionality inside the various cursor get routines.
 *
 * 	If exhausted == 0, we're not done with the current datum;
 * 	return it if it matches "matching", otherwise search
 * 	using DBC_CONTINUE (which is faster than iteratively doing
 * 	DB_NEXT_DUP) forward until we find one that does.
 *
 * 	If exhausted == 1, we are done with the current datum, so just
 * 	leap forward to searching NEXT_DUPs.
 *
 * 	If no matching datum exists, returns DB_NOTFOUND, else 0.
 */
static int
__db_join_getnext(dbc, key, data, matching, exhausted)
	DBC *dbc;
	DBT *key, *data, *matching;
	u_int32_t exhausted;
{
	int ret, cmp;
	DB *dbp;
	int (*func) __P((const DBT *, const DBT *));

	dbp = dbc->dbp;

	func = (dbp->dup_compare == NULL) ? __bam_defcmp : dbp->dup_compare;

	switch (exhausted) {
	case 0:
		if ((ret = dbc->c_get(dbc, key, data, DB_CURRENT)) != 0)
			break;
		cmp = func(matching, data);
		if (cmp == 0)
			return (0);

		/*
		 * Didn't match--we want to fall through and search future
		 * dups.  But we've just stepped on the value of data,
		 * so we copy matching back into it.
		 *
		 * We don't have to copy the data itself, because
		 * the ensuing c_get call will take care of things for us.
		 */
		data->data = matching->data;
		data->size = matching->size;

		/* FALLTHROUGH */
	case 1:
		F_SET(dbc, DBC_CONTINUE);
		ret = dbc->c_get(dbc, key, data, DB_GET_BOTH);
		F_CLR(dbc, DBC_CONTINUE);
		break;
	default:
		ret = EINVAL;
		break;
	}

	return (ret);
}
