/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * foreign_reassoc.c --
 *	Regression test for RFC 0014 §8.1: __db_associate_foreign linked its
 *	DB_FOREIGN_INFO into fdbp->f_primaries BEFORE refusing a secondary
 *	that already had a foreign database, so the EINVAL left a stale entry
 *	pointing at the secondary in the second foreign database's list.
 *
 * Primary P, secondary S (S key = P data), foreign F1 and F2, one env.
 * F1->associate_foreign(S, CASCADE) succeeds; F2->associate_foreign(S,
 * CASCADE) must be EINVAL.  Each hazard of the stale entry is one arm, run
 * as its own process by the runner (argv[2]) so a crash cannot hide another:
 *
 *   i    wrong cascade: delete "s" from F2 -> P must still hold k.
 *        Control: deleting "s" from F1 DOES cascade k away, so "still
 *        holds" is an observation, not a cascade that never works.
 *   ii   silently disabled FK check: a P put whose S key is absent from F1
 *        is DB_FOREIGN_CONFLICT (control), then close F2, then the same put
 *        must still be DB_FOREIGN_CONFLICT.
 *   iii  use-after-free: close S, then delete "s" from F2.  Under ASan,
 *        ASan reports it.  Without ASan a freed DB is usually still intact,
 *        so the read would go unseen; the free hook therefore poisons S's
 *        handle on free (and keeps it, so nothing else lands there), which
 *        turns the use into a deterministic crash in every build.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

#if defined(__SANITIZE_ADDRESS__)
#define	TEST_ASAN	1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
#define	TEST_ASAN	1
#endif
#endif

static const char *arm;
static void *poison_target;	/* S's handle, once arm iii is about to close it */

static void
die(int ret, const char *what)
{
	printf("VERDICT foreign_reassoc FAIL arm %s: %s: %s\n",
	    arm, what, db_strerror(ret));
	exit(1);
}

static void
fail(const char *why)
{
	printf("VERDICT foreign_reassoc FAIL arm %s: %s\n", arm, why);
	exit(1);
}

/*
 * Poison-and-keep free hook for arm iii in a non-ASan build.  __db_close
 * already fills the handle with CLEAR_BYTE before freeing it; keeping the
 * block (never handing it back to malloc) keeps that fill in place, so a
 * stale f_info->dbp reads garbage pointers and faults.  In a DIAGNOSTIC
 * build __os_free passes the block's real start, one db_allocinfo_t (a
 * size_t/uintmax_t union) before the DB pointer, hence the two offsets.
 */
static void
poison_free(void *p)
{
	char *t;

	t = poison_target;
	if (t != NULL && ((char *)p == t ||
	    (char *)p + sizeof(union { size_t s; uintmax_t a; }) == t)) {
		memset(t, 0xa5, sizeof(DB));
		poison_target = NULL;
		return;
	}
	free(p);
}

static int
skey_cb(DB *sdbp, const DBT *pkey, const DBT *pdata, DBT *skey)
{
	(void)sdbp;
	(void)pkey;
	memset(skey, 0, sizeof(*skey));
	skey->data = pdata->data;
	skey->size = pdata->size;
	return (0);
}

static DB *
open_db(DB_ENV *env, const char *name)
{
	DB *dbp;
	int ret;

	if ((ret = db_create(&dbp, env, 0)) != 0)
		die(ret, "db_create");
	if ((ret = dbp->open(dbp,
	    NULL, name, NULL, DB_BTREE, DB_CREATE, 0644)) != 0)
		die(ret, name);
	return (dbp);
}

static int
put(DB *dbp, const char *k, const char *v)
{
	DBT key, data;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)k;
	key.size = (u_int32_t)strlen(k);
	data.data = (void *)v;
	data.size = (u_int32_t)strlen(v);
	return (dbp->put(dbp, NULL, &key, &data, 0));
}

static int
del(DB *dbp, const char *k)
{
	DBT key;

	memset(&key, 0, sizeof(key));
	key.data = (void *)k;
	key.size = (u_int32_t)strlen(k);
	return (dbp->del(dbp, NULL, &key, 0));
}

static int
get(DB *dbp, const char *k)
{
	DBT key, data;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)k;
	key.size = (u_int32_t)strlen(k);
	return (dbp->get(dbp, NULL, &key, &data, 0));
}

int
main(int argc, char **argv)
{
	DB_ENV *env;
	DB *f1, *f2, *p, *s;
	int ret;

	if (argc != 3)
		fail("usage: foreign_reassoc <home> i|ii|iii");
	arm = argv[2];
	if (strcmp(arm, "i") && strcmp(arm, "ii") && strcmp(arm, "iii"))
		fail("unknown arm");
#ifndef TEST_ASAN
	if (strcmp(arm, "iii") == 0)
		(void)db_env_set_func_free(poison_free);
#endif

	if ((ret = db_env_create(&env, 0)) != 0)
		die(ret, "db_env_create");
	env->set_errfile(env, stderr);
	env->set_errpfx(env, "foreign_reassoc");
	if ((ret = env->open(env, argv[1], DB_CREATE | DB_INIT_MPOOL, 0644)) != 0)
		die(ret, "env->open");

	p = open_db(env, "p.db");
	s = open_db(env, "s.db");
	f1 = open_db(env, "f1.db");
	f2 = open_db(env, "f2.db");
	if ((ret = p->associate(p, NULL, s, skey_cb, 0)) != 0)
		die(ret, "associate");
	if ((ret = f1->associate_foreign(f1, s, NULL, DB_FOREIGN_CASCADE)) != 0)
		die(ret, "F1->associate_foreign");
	if ((ret = f2->associate_foreign(f2,
	    s, NULL, DB_FOREIGN_CASCADE)) != EINVAL) {
		printf("F2->associate_foreign returned %d, want EINVAL\n", ret);
		fail("a second foreign database was not refused");
	}

	if ((ret = put(f1, "s", "")) != 0 || (ret = put(f2, "s", "")) != 0)
		die(ret, "put F1/F2");
	if ((ret = put(p, "k", "s")) != 0)
		die(ret, "put P k->s");

	if (strcmp(arm, "i") == 0) {
		if ((ret = del(f2, "s")) != 0)
			die(ret, "delete s from F2");
		if ((ret = get(p, "k")) == DB_NOTFOUND)
			fail("deleting from F2 cascaded into P: k is gone "
			    "(stale f_primaries entry, hazard 1)");
		if (ret != 0)
			die(ret, "get P k");
		/* Control: the real foreign database does cascade. */
		if ((ret = del(f1, "s")) != 0)
			die(ret, "delete s from F1");
		if ((ret = get(p, "k")) != DB_NOTFOUND)
			fail("control: deleting from F1 did not cascade, so "
			    "this arm cannot observe a cascade");
		printf("arm i: F2 delete left P intact; F1 delete cascaded\n");
	} else if (strcmp(arm, "ii") == 0) {
		if ((ret = put(p, "k2", "zz")) != DB_FOREIGN_CONFLICT) {
			printf("put P k2->zz before F2 close: %d\n", ret);
			fail("control: the F1 foreign check is not live");
		}
		if ((ret = f2->close(f2, 0)) != 0)
			die(ret, "close F2");
		f2 = NULL;
		if ((ret = put(p, "k2", "zz")) != DB_FOREIGN_CONFLICT) {
			printf("put P k2->zz after F2 close: %d (%s)\n",
			    ret, db_strerror(ret));
			fail("closing F2 disabled S's foreign check against F1 "
			    "(stale f_primaries entry, hazard 2)");
		}
		printf("arm ii: FK check against F1 survived F2's close\n");
	} else {
		poison_target = s;
		if ((ret = s->close(s, 0)) != 0)
			die(ret, "close S");
		s = NULL;
		/* Against the defect this dereferences the freed S. */
		if ((ret = del(f2, "s")) != 0)
			die(ret, "delete s from F2 after S closed");
		if ((ret = get(p, "k")) != 0)
			die(ret, "get P k after F2 delete");
		printf("arm iii: F2 delete after S close touched nothing "
		    "(%s)\n",
#ifdef TEST_ASAN
		    "ASan build"
#else
		    "poisoned-handle build"
#endif
		    );
	}

	if ((s != NULL && (ret = s->close(s, 0)) != 0) ||
	    (ret = p->close(p, 0)) != 0 ||
	    (ret = f1->close(f1, 0)) != 0 ||
	    (f2 != NULL && (ret = f2->close(f2, 0)) != 0) ||
	    (ret = env->close(env, 0)) != 0)
		die(ret, "close");
	printf("VERDICT foreign_reassoc PASS arm %s\n", arm);
	return (0);
}
