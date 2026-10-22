/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_unique_iso.c --
 *	RFC 0014 commit 11 test (review nit N1): the UNIQUE check-then-insert
 *	race from two real PROCESSES under each of three isolation levels.  The
 *	shipped typed_unique_mp exercises only 2PL; this one adds the SNAPSHOT
 *	(MVCC) check-then-insert hazard, which is the dangerous one -- two
 *	snapshot transactions each read "no such unique key" from their own
 *	snapshot and both try to insert.  The UNIQUE secondary must still admit
 *	exactly one.
 *
 *	Levels (each in its own home subdir, fresh race):
 *	  iso 0 = 2PL          (DB_INIT_TXN, plain txns)
 *	  iso 1 = SNAPSHOT     (env DB_MULTIVERSION, txn DB_TXN_SNAPSHOT)
 *	  iso 2 = SERIALIZABLE (plain 2PL txn; writes are already serializable)
 *	Exactly one child commits; the other gets DB_KEYEXIST, every level.
 *	After the race the primary and the unique index each hold one row.
 *	Real fork+exec processes (not threads), a ready/go barrier via files.
 *	One VERDICT line.
 *
 *	Usage: typed_unique_iso <home>        (parent, runs all three levels)
 *	       typed_unique_iso <home> child <id> <iso>   (a race participant)
 */
#include <sys/types.h>
#include <sys/stat.h>
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
openenv(const char *home, int iso)
{
	DB_ENV *e;
	u_int32_t extra = 0;
	if (iso == 1) extra = DB_MULTIVERSION;	/* SNAPSHOT needs MVCC */
	if (db_env_create(&e, 0) != 0) return (NULL);
	(void)e->set_lk_detect(e, DB_LOCK_YOUNGEST);
	if (e->open(e, home, DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_TXN | extra, 0) != 0)
		return (NULL);
	return (e);
}

static u_int32_t
txnflags(int iso)
{
	if (iso == 1) return (DB_TXN_SNAPSHOT);
	return (0);	/* 2PL and SERIALIZABLE: plain txn */
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

static int
child(const char *home, int id, int iso)
{
	DB_ENV *e;
	DB *u, *p;
	DB_TXN *txn;
	DBT k, d;
	unsigned char kb[1];
	char bf[1024];
	int ret, tries;

	if ((e = openenv(home, iso)) == NULL) return (3);
	{
		DB_TXN *ot;
		if (e->txn_begin(e, NULL, &ot, 0) != 0) return (3);
		if (openprimary(e, ot, &u, &p, 0) != 0) {
			(void)ot->abort(ot); return (3);
		}
		(void)ot->commit(ot, 0);
	}

	snprintf(bf, sizeof(bf), "%s/ready.%d", home, id);
	(void)close(open(bf, O_CREAT | O_WRONLY, 0600));
	snprintf(bf, sizeof(bf), "%s/go", home);
	for (tries = 0; tries < 20000; tries++) {
		if (access(bf, F_OK) == 0) break;
		usleep(1000);
	}

	tries = 0;
retry:
	if (e->txn_begin(e, NULL, &txn, txnflags(iso)) != 0) return (3);
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	/*
	 * Both children insert the SAME unique value "UNIQ" (data field 0),
	 * under distinct primary keys, so the collision is in the unique index,
	 * not the primary.  That is the check-then-insert hazard.
	 */
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
		if ((ret == DB_LOCK_DEADLOCK || ret == DB_LOCK_NOTGRANTED) &&
		    ++tries < 100) {
			usleep(2000);
			goto retry;
		}
	}
	(void)p->close(p, 0); (void)u->close(u, 0);
	(void)e->close(e, 0);
	return (ret == DB_KEYEXIST ? 1 : (ret == 0 ? 0 : 3));
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

static int
run_level(const char *home, const char *self, int iso, const char *label)
{
	DB_ENV *e;
	DB_TXN *ot;
	DB *u, *p;
	char idbuf[16], isobuf[16], bf[1024];
	pid_t pid[2];
	int i, st, winners, losers, errs, tries, nfail;

	if ((e = openenv(home, iso)) == NULL) {
		printf("  FAIL[%s]: env setup\n", label); return (1);
	}
	if (e->txn_begin(e, NULL, &ot, 0) != 0) goto bad;
	if (openprimary(e, ot, &u, &p, 1) != 0) goto bad;
	(void)ot->commit(ot, 0);
	(void)p->close(p, 0); (void)u->close(u, 0);
	(void)e->close(e, 0);

	for (i = 1; i <= 2; i++) {
		snprintf(bf, sizeof(bf), "%s/ready.%d", home, i);
		(void)unlink(bf);
	}
	snprintf(bf, sizeof(bf), "%s/go", home);
	(void)unlink(bf);

	snprintf(isobuf, sizeof(isobuf), "%d", iso);
	for (i = 0; i < 2; i++) {
		snprintf(idbuf, sizeof(idbuf), "%d", i + 1);
		if ((pid[i] = fork()) == 0) {
			execl(self, self, home, "child", idbuf, isobuf,
			    (char *)NULL);
			_exit(127);
		}
	}
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
		printf("  FAIL[%s]: winners=%d losers=%d errs=%d (want 1/1/0)\n",
		    label, winners, losers, errs);
		nfail++;
	} else
		printf("  ok[%s]: one winner, one DB_KEYEXIST loser\n", label);

	if ((e = openenv(home, iso)) != NULL) {
		DB_TXN *vt;
		if (e->txn_begin(e, NULL, &vt, 0) == 0 &&
		    openprimary(e, vt, &u, &p, 0) == 0) {
			(void)vt->commit(vt, 0);
			if (pcount(p) != 1) {
				printf("  FAIL[%s]: primary count=%d\n", label,
				    pcount(p)); nfail++;
			}
			if (pcount(u) != 1) {
				printf("  FAIL[%s]: index count=%d\n", label,
				    pcount(u)); nfail++;
			}
			(void)p->close(p, 0); (void)u->close(u, 0);
		} else { printf("  FAIL[%s]: reopen verify\n", label); nfail++; }
		(void)e->close(e, 0);
	} else { printf("  FAIL[%s]: reopen env\n", label); nfail++; }
	return (nfail);
bad:
	printf("  FAIL[%s]: setup\n", label); return (1);
}

int
main(int argc, char *argv[])
{
	const char *home, *self;
	char sub[1024];
	int iso, nfail;

	if (argc < 2) { printf("VERDICT typed_unique_iso FAIL usage\n"); return (1); }
	home = argv[1];
	self = argv[0];

	if (argc >= 5 && strcmp(argv[2], "child") == 0)
		return (child(home, atoi(argv[3]), atoi(argv[4])));

	nfail = 0;
	for (iso = 0; iso < 3; iso++) {
		const char *label = iso == 0 ? "2PL" :
		    iso == 1 ? "SNAPSHOT" : "SERIALIZABLE";
		snprintf(sub, sizeof(sub), "%s/iso%d", home, iso);
		(void)mkdir(sub, 0700);
		nfail += run_level(sub, self, iso, label);
	}
	if (nfail == 0)
		printf("VERDICT typed_unique_iso PASS one winner per level, all 3 levels\n");
	else
		printf("VERDICT typed_unique_iso FAIL %d checks\n", nfail);
	return (nfail == 0 ? 0 : 1);
}
