/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 * SPDX-License-Identifier: Sleepycat
 *
 * repmgr_tls_bench.c -- a relative throughput probe for repmgr TLS vs
 * plaintext.  A master and one client (two processes on one host).  The master
 * commits NOPS single-key transactions as fast as the ack policy allows and
 * reports committed ops/second; the client acknowledges per policy.  Run once
 * with TLS env vars set and once without; the ratio is the TLS overhead.
 *
 * This is an A/B RATIO on one host, not a metal-grade absolute number: both
 * arms share the same loopback and CPU, so the plaintext/TLS ratio is the
 * meaningful output.  Absolute ops/s depend on the (shared) host.
 *
 * Usage: repmgr_tls_bench <mhome> <chome> <mport> <cport> <nops>
 */
#include <sys/types.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

static int tls_on(void) {
	return (getenv("REP_TLS_CERT") && getenv("REP_TLS_KEY") &&
	    getenv("REP_TLS_CA"));
}

static int
cfg_tls(DB_ENV *dbenv)
{
	u_int32_t flags;

	if (!tls_on())
		return (0);
	flags = DB_REPMGR_TLS_VERIFY_PEER;
	return (dbenv->repmgr_set_tls(dbenv, getenv("REP_TLS_CERT"),
	    getenv("REP_TLS_KEY"), getenv("REP_TLS_CA"), flags));
}

static int
site(const char *home, u_int16_t self, u_int16_t peer, int master, long nops)
{
	DB_ENV *dbenv;
	DB_SITE *s;
	int ret, tries;

	if (db_env_create(&dbenv, 0) != 0)
		return (1);
	dbenv->set_errpfx(dbenv, home);
	dbenv->set_errfile(dbenv, stderr);
	(void)dbenv->rep_set_priority(dbenv, 100);
	if ((ret = cfg_tls(dbenv)) != 0) {
		dbenv->err(dbenv, ret, "set_tls");
		return (1);
	}
	(void)dbenv->repmgr_set_ack_policy(dbenv, DB_REPMGR_ACKS_ALL);
	if (dbenv->repmgr_site(dbenv, "127.0.0.1", self, &s, 0) != 0)
		return (1);
	(void)s->set_config(s, DB_LOCAL_SITE, 1);
	if (master)
		(void)s->set_config(s, DB_GROUP_CREATOR, 1);
	(void)s->close(s);
	if (dbenv->repmgr_site(dbenv, "127.0.0.1", peer, &s, 0) != 0)
		return (1);
	(void)s->set_config(s, DB_BOOTSTRAP_HELPER, 1);
	(void)s->close(s);
	(void)dbenv->rep_set_timeout(dbenv, DB_REP_HEARTBEAT_SEND, 500000);
	(void)dbenv->rep_set_timeout(dbenv, DB_REP_HEARTBEAT_MONITOR, 2000000);

	if ((ret = dbenv->open(dbenv, home, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_REP | DB_INIT_TXN |
	    DB_RECOVER | DB_THREAD, 0)) != 0) {
		fprintf(stderr, "%s: open: %s\n", home, db_strerror(ret));
		return (1);
	}
	if (!master)
		sleep(1);
	ret = dbenv->repmgr_start(dbenv, 3, master ? DB_REP_MASTER :
	    DB_REP_CLIENT);
	for (tries = 0; !master && ret == DB_REP_UNAVAIL && tries < 10;
	    tries++) {
		sleep(1);
		ret = dbenv->repmgr_start(dbenv, 3, DB_REP_CLIENT);
	}
	if (ret != 0) {
		fprintf(stderr, "%s: repmgr_start: %s\n", home,
		    db_strerror(ret));
		return (1);
	}

	if (master) {
		DB *db;
		DBT key, data;
		struct timeval t0, t1;
		long i;
		char kbuf[32], vbuf[256];
		double secs;

		sleep(4);		/* let the client join + sync */
		if (db_create(&db, dbenv, 0) != 0)
			return (1);
		if ((ret = db->open(db, NULL, "bench.db", NULL, DB_BTREE,
		    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0)) != 0) {
			dbenv->err(dbenv, ret, "db open");
			return (1);
		}
		memset(vbuf, 'x', sizeof(vbuf));
		gettimeofday(&t0, NULL);
		for (i = 0; i < nops; i++) {
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			(void)snprintf(kbuf, sizeof(kbuf), "k%ld", i % 1000);
			key.data = kbuf;
			key.size = (u_int32_t)strlen(kbuf);
			data.data = vbuf;
			data.size = (u_int32_t)sizeof(vbuf);
			if ((ret = db->put(db, NULL, &key, &data, 0)) != 0) {
				dbenv->err(dbenv, ret, "put");
				break;
			}
		}
		gettimeofday(&t1, NULL);
		secs = (t1.tv_sec - t0.tv_sec) +
		    (t1.tv_usec - t0.tv_usec) / 1e6;
		printf("TLS=%s nops=%ld secs=%.3f ops_per_sec=%.1f\n",
		    tls_on() ? "on" : "off", nops, secs, nops / secs);
		(void)db->close(db, 0);
		sleep(2);
		(void)dbenv->close(dbenv, 0);
		return (ret);
	} else {
		sleep(20);		/* stay up while the master drives */
		(void)dbenv->close(dbenv, 0);
		return (0);
	}
}

int
main(int argc, char *argv[])
{
	pid_t c;
	int st;
	long nops;

	if (argc != 6) {
		fprintf(stderr, "usage: %s mhome chome mport cport nops\n",
		    argv[0]);
		return (2);
	}
	nops = atol(argv[5]);
	if ((c = fork()) == 0)
		_exit(site(argv[2], (u_int16_t)atoi(argv[4]),
		    (u_int16_t)atoi(argv[3]), 0, nops));
	(void)site(argv[1], (u_int16_t)atoi(argv[3]),
	    (u_int16_t)atoi(argv[4]), 1, nops);
	(void)waitpid(c, &st, 0);
	return (0);
}
