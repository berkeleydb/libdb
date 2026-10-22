/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_unique_mp.c --
 *	RFC 0014 commit 7 test (spec §7.2 typed_unique_mp): the UNIQUE race
 *	from two real PROCESSES (fork + exec, not threads).  The parent builds
 *	the typed primary and its unique index, then forks two children that
 *	exec this same binary with a role argument; each child opens the shared
 *	environment, waits on a barrier file, then inserts the SAME unique data
 *	value under its own transaction, retrying on DB_LOCK_DEADLOCK.  Exactly
 *	one child must commit; the other must end with DB_KEYEXIST (D6).  After
 *	both, the primary and the unique index each hold one row.
 *
 *	Usage: typed_unique_mp <homedir>            (parent/orchestrator)
 *	       typed_unique_mp <homedir> child <id> (exec'd child)
 */
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "db.h"

static int
field0(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
    const u_int8_t **pp, u_int32_t *lenp, int *isnull)
{
	(void)rc;
	if (i != 0) return (DB_CODEC_MALFORMED);
	*pp = rec->data; *lenp = rec->size; *isnull = 0;
	return (0);
}
static DB_CODEC mc = {
	"mc", 1, DB_CODEC_MEMCMP | DB_CODEC_EQUALIMAGE | DB_CODEC_TOTAL,
	0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0
};
static const DB_CODEC *dt[1] = { &mc };
static DB_RECORD_CODEC drc = {
	"drc", 1, DB_CODEC_TOTAL, 1, field0, NULL, dt, NULL
};

static DB_ENV *
openenv(const char *home, u_int32_t extra)
{
	DB_ENV *e;
	if (db_env_create(&e, 0) != 0) return (NULL);
	(void)e->set_lk_detect(e, DB_LOCK_YOUNGEST);
	if (e->open(e, home, DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_TXN | extra, 0) != 0)
		return (NULL);
	return (e);
}

static int
openprimary(DB_ENV *e, DB_TXN *txn, DB **udbpp, DB **pdbpp, int create)
{
	DB *u, *p;
	DB_SCHEMA s;
	static DB_CONSTRAINT con;
	u_int32_t cf = create ? DB_CREATE : 0;

	if (db_create(&u, e, 0) != 0) return (-1);
	if (u->open(u, txn, "uq.db", "uidx", DB_BTREE, cf, 0600) != 0)
		return (-1);
	if (db_create(&p, e, 0) != 0) return (-1);
	memset(&s, 0, sizeof(s)); s.type = DB_BTREE; s.data = &drc;
	memset(&con, 0, sizeof(con));
	con.kind = DB_CONSTRAINT_UNIQUE; con.side = DB_TYPED_DATA;
	con.field = 0; con.name = "u_val"; con.unique_db = u;
	s.constraints = &con; s.nconstraints = 1;
	if (db_set_schema(p, &s, 0) != 0) return (-1);
	if (p->open(p, txn, "uq.db", "prim", DB_BTREE, cf, 0600) != 0)
		return (-1);
	*udbpp = u; *pdbpp = p;
	return (0);
}

/* Child: open env+db, wait on barrier, insert the shared value, report. */
static int
child(const char *home, int id)
{
	DB_ENV *e;
	DB *u, *p;
	DB_TXN *txn;
	DBT k, d;
	unsigned char kb[1];
	char bf[1024];
	int ret, tries;

	if ((e = openenv(home, 0)) == NULL) return (3);
	{
		DB_TXN *ot;
		if (e->txn_begin(e, NULL, &ot, 0) != 0) return (3);
		if (openprimary(e, ot, &u, &p, 0) != 0) {
			(void)ot->abort(ot); return (3);
		}
		(void)ot->commit(ot, 0);
	}

	/* Barrier: signal ready, then spin until the go file appears. */
	snprintf(bf, sizeof(bf), "%s/ready.%d", home, id);
	(void)close(open(bf, O_CREAT | O_WRONLY, 0600));
	snprintf(bf, sizeof(bf), "%s/go", home);
	for (tries = 0; tries < 20000; tries++) {
		if (access(bf, F_OK) == 0) break;
		usleep(1000);
	}

	tries = 0;
retry:
	if (e->txn_begin(e, NULL, &txn, 0) != 0) return (3);
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	kb[0] = (unsigned char)id; k.data = kb; k.size = 1;
	d.data = (void *)"UNIQ"; d.size = 4;
	ret = p->put(p, txn, &k, &d, 0);
	if (ret == 0) {
		if ((ret = txn->commit(txn, 0)) == 0) {
			(void)p->close(p, 0); (void)u->close(u, 0);
			(void)e->close(e, 0);
			return (0);		/* WINNER */
		}
	} else {
		(void)txn->abort(txn);
		if (ret == DB_LOCK_DEADLOCK && ++tries < 50) {
			usleep(2000);
			goto retry;
		}
	}
	(void)p->close(p, 0); (void)u->close(u, 0);
	(void)e->close(e, 0);
	/* DB_KEYEXIST means UNIQUE rejected us (the loser): exit 1. */
	return (ret == DB_KEYEXIST ? 1 : 3);
}

static int
pcount(DB *db)
{
	DBC *c;
	DBT k, d;
	int n = 0;
	if (db->cursor(db, NULL, &c, 0) != 0) return (-1);
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	while (c->get(c, &k, &d, DB_NEXT) == 0) n++;
	(void)c->close(c);
	return (n);
}

int
main(int argc, char *argv[])
{
	DB_ENV *e;
	DB_TXN *ot;
	DB *u, *p;
	const char *home, *self;
	char idbuf[16], bf[1024];
	pid_t pid[2];
	int i, st, winners, losers, errs, tries, nfail;

	if (argc < 2) { printf("VERDICT typed_unique_mp FAIL usage\n"); return (1); }
	home = argv[1];
	self = argv[0];

	if (argc >= 4 && strcmp(argv[2], "child") == 0)
		return (child(home, atoi(argv[3])));

	/* Parent: build the typed primary + unique index. */
	if ((e = openenv(home, 0)) == NULL) {
		printf("VERDICT typed_unique_mp FAIL env setup\n"); return (1);
	}
	if (e->txn_begin(e, NULL, &ot, 0) != 0) goto bad;
	if (openprimary(e, ot, &u, &p, 1) != 0) goto bad;
	(void)ot->commit(ot, 0);
	(void)p->close(p, 0); (void)u->close(u, 0);
	(void)e->close(e, 0);

	/* Remove any stale barrier files. */
	for (i = 1; i <= 2; i++) {
		snprintf(bf, sizeof(bf), "%s/ready.%d", home, i);
		(void)unlink(bf);
	}
	snprintf(bf, sizeof(bf), "%s/go", home);
	(void)unlink(bf);

	/* Fork + exec two children. */
	for (i = 0; i < 2; i++) {
		snprintf(idbuf, sizeof(idbuf), "%d", i + 1);
		if ((pid[i] = fork()) == 0) {
			execl(self, self, home, "child", idbuf, (char *)NULL);
			_exit(127);
		}
	}

	/* Wait until both children are ready, then release the barrier. */
	for (tries = 0; tries < 20000; tries++) {
		int ready = 0;
		for (i = 1; i <= 2; i++) {
			snprintf(bf, sizeof(bf), "%s/ready.%d", home, i);
			if (access(bf, F_OK) == 0) ready++;
		}
		if (ready == 2) break;
		usleep(1000);
	}
	snprintf(bf, sizeof(bf), "%s/go", home);
	(void)close(open(bf, O_CREAT | O_WRONLY, 0600));

	winners = losers = errs = 0;
	for (i = 0; i < 2; i++) {
		(void)waitpid(pid[i], &st, 0);
		if (WIFEXITED(st)) {
			switch (WEXITSTATUS(st)) {
			case 0: winners++; break;
			case 1: losers++; break;
			default: errs++; break;
			}
		} else
			errs++;
	}

	nfail = 0;
	if (winners != 1 || losers != 1 || errs != 0) {
		printf("  FAIL: winners=%d losers=%d errs=%d (want 1/1/0)\n",
		    winners, losers, errs);
		nfail++;
	} else
		printf("  ok: exactly one winner, one DB_KEYEXIST loser\n");

	/* Verify one row in each. */
	if ((e = openenv(home, 0)) != NULL) {
		DB_TXN *vt;
		if (e->txn_begin(e, NULL, &vt, 0) == 0 &&
		    openprimary(e, vt, &u, &p, 0) == 0) {
			(void)vt->commit(vt, 0);
			if (pcount(p) == 1) printf("  ok: primary has one row\n");
			else { printf("  FAIL: primary count=%d\n", pcount(p)); nfail++; }
			if (pcount(u) == 1) printf("  ok: unique index has one row\n");
			else { printf("  FAIL: index count=%d\n", pcount(u)); nfail++; }
			(void)p->close(p, 0); (void)u->close(u, 0);
		} else { printf("  FAIL: reopen for verify\n"); nfail++; }
		(void)e->close(e, 0);
	} else { printf("  FAIL: reopen env for verify\n"); nfail++; }

	if (nfail == 0)
		printf("VERDICT typed_unique_mp PASS one winner, one DB_KEYEXIST\n");
	else
		printf("VERDICT typed_unique_mp FAIL %d checks\n", nfail);
	return (nfail == 0 ? 0 : 1);
bad:
	printf("VERDICT typed_unique_mp FAIL setup\n");
	return (1);
}
