/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_unique_closed.c --
 *	RFC 0014 commit 7 test (spec §7.2 typed_unique_closed): closing a
 *	UNIQUE index's unique_db handle before its primary must make a later
 *	put on the primary fail EINVAL (§3.7 step 2) -- otherwise UNIQUE would
 *	be silently unenforced.  One VERDICT line.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

static int nfail, ncase;
#define	CK(c, m) do { ncase++; if (!(c)) { printf("  FAIL [%d]: %s\n", ncase, m); nfail++; } else printf("  ok   [%d]: %s\n", ncase, m); } while (0)

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
static DB_ENV *dbenv;

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
put(DB *p, int keyn, const char *val)
{
	DBT k, d;
	unsigned char kb[1];
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	kb[0] = (unsigned char)keyn; k.data = kb; k.size = 1;
	d.data = (void *)val; d.size = (u_int32_t)strlen(val);
	return (p->put(p, NULL, &k, &d, 0));
}

int
main(int argc, char *argv[])
{
	DB *u, *p;
	DB_SCHEMA s;
	DB_CONSTRAINT con;
	const char *home = (argc > 1) ? argv[1] : ".";
	int ret;

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_unique_closed FAIL env setup\n");
		return (1);
	}
	if (db_create(&u, dbenv, 0) != 0) goto bad;
	if (u->open(u, NULL, "uc.db", "uidx", DB_BTREE, DB_CREATE, 0600) != 0)
		goto bad;
	if (db_create(&p, dbenv, 0) != 0) goto bad;
	memset(&s, 0, sizeof(s)); s.type = DB_BTREE; s.data = &drc;
	memset(&con, 0, sizeof(con));
	con.kind = DB_CONSTRAINT_UNIQUE; con.side = DB_TYPED_DATA;
	con.field = 0; con.name = "u_val"; con.unique_db = u;
	s.constraints = &con; s.nconstraints = 1;
	if (db_set_schema(p, &s, 0) != 0) goto bad;
	if (p->open(p, NULL, "uc.db", "prim", DB_BTREE, DB_CREATE, 0600) != 0)
		goto bad;

	/* A put while the unique index is open works. */
	CK(put(p, 1, "AAA") == 0, "put with unique index open succeeds");

	/* Close the unique_db out from under the primary. */
	CK(u->close(u, 0) == 0, "close the unique index");

	/* A put now must be refused EINVAL (step 2), not silently unenforced. */
	ret = put(p, 2, "BBB");
	CK(ret == EINVAL, "put after unique index closed -> EINVAL");
	CK(pcount(p) == 1, "no row stored after the refused put");

	(void)p->close(p, 0);
	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_unique_closed PASS all %d checks\n", ncase);
	else
		printf("VERDICT typed_unique_closed FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
bad:
	printf("VERDICT typed_unique_closed FAIL setup\n");
	return (1);
}
