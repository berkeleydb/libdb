/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 * SPDX-License-Identifier: Sleepycat
 *
 * repmgr_tls.c -- a minimal two-site Replication Manager group used to prove
 * that (a) replication works over TLS, and (b) the wire is encrypted.  The
 * master forks a client, both start repmgr on localhost (distinct ports and
 * env homes), the master writes a unique MARKER key/value, and the client
 * polls until it sees it replicated (or times out).
 *
 * TLS is turned on when all of REP_TLS_CERT/REP_TLS_KEY/REP_TLS_CA are set in
 * the environment; otherwise the group runs in cleartext.  When REP_TLS_REQUIRE
 * is set, DB_REPMGR_TLS_REQUIRE is passed so plaintext peers are refused.
 *
 * Usage: repmgr_tls <master_home> <client_home> <master_port> <client_port>
 * Exit:  0 if the client observed the marker; non-zero otherwise.  The
 *        LD_PRELOAD wirecap shim (run_repmgr_tls_wirecap.sh) records the bytes;
 *        the harness greps the capture for the marker.
 */
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

#define	MARKER_KEY	"repmgr_tls_marker_key"
#define	MARKER_VAL	"REPMGR_TLS_MARKER_abcdef0123456789_VALUE"
#define	DBNAME		"rep.db"
#define	POLL_SECS	30

static int tls_configured(void);
static int setup_tls(DB_ENV *);
static int make_env(const char *, DB_ENV **);
static int
start_site(const char *home, u_int16_t self_port, u_int16_t peer_port,
    int is_master, int creator);

static int
tls_configured()
{
	return (getenv("REP_TLS_CERT") != NULL &&
	    getenv("REP_TLS_KEY") != NULL && getenv("REP_TLS_CA") != NULL);
}

static int
setup_tls(dbenv)
	DB_ENV *dbenv;
{
	u_int32_t flags;
	int ret;

	if (!tls_configured())
		return (0);
	flags = DB_REPMGR_TLS_VERIFY_PEER;
	if (getenv("REP_TLS_REQUIRE") != NULL)
		flags |= DB_REPMGR_TLS_REQUIRE;
	if (getenv("REP_TLS_NO_VERIFY") != NULL)
		flags = DB_REPMGR_TLS_NO_VERIFY;
	if ((ret = dbenv->repmgr_set_tls(dbenv, getenv("REP_TLS_CERT"),
	    getenv("REP_TLS_KEY"), getenv("REP_TLS_CA"), flags)) != 0) {
		dbenv->err(dbenv, ret, "repmgr_set_tls");
		return (ret);
	}
	return (0);
}

static int
make_env(home, dbenvp)
	const char *home;
	DB_ENV **dbenvp;
{
	DB_ENV *dbenv;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (ret);
	}
	dbenv->set_errpfx(dbenv, home);
	dbenv->set_errfile(dbenv, stderr);
	if (getenv("REP_VERBOSE") != NULL) {
		(void)dbenv->set_verbose(dbenv, DB_VERB_REPLICATION, 1);
		(void)dbenv->set_verbose(dbenv, DB_VERB_REPMGR_MISC, 1);
		(void)dbenv->set_verbose(dbenv, DB_VERB_REPMGR_CONNFAIL, 1);
		dbenv->set_msgfile(dbenv, stderr);
	}
	(void)dbenv->rep_set_priority(dbenv, 100);
	*dbenvp = dbenv;
	return (0);
}

static int
start_site(home, self_port, peer_port, is_master, creator)
	const char *home;
	u_int16_t self_port, peer_port;
	int is_master, creator;
{
	DB_ENV *dbenv;
	DB_SITE *dbsite;
	int ret;

	if ((ret = make_env(home, &dbenv)) != 0)
		return (ret);
	if ((ret = setup_tls(dbenv)) != 0)
		goto err;
	if ((ret = dbenv->repmgr_set_ack_policy(dbenv,
	    DB_REPMGR_ACKS_ALL)) != 0)
		goto err;

	if ((ret = dbenv->repmgr_site(dbenv, "127.0.0.1", self_port,
	    &dbsite, 0)) != 0)
		goto err;
	(void)dbsite->set_config(dbsite, DB_LOCAL_SITE, 1);
	if (creator)
		(void)dbsite->set_config(dbsite, DB_GROUP_CREATOR, 1);
	(void)dbsite->close(dbsite);

	if ((ret = dbenv->repmgr_site(dbenv, "127.0.0.1", peer_port,
	    &dbsite, 0)) != 0)
		goto err;
	(void)dbsite->set_config(dbsite, DB_BOOTSTRAP_HELPER, 1);
	(void)dbsite->close(dbsite);

	(void)dbenv->rep_set_timeout(dbenv, DB_REP_HEARTBEAT_SEND, 500000);
	(void)dbenv->rep_set_timeout(dbenv, DB_REP_HEARTBEAT_MONITOR, 2000000);

	if ((ret = dbenv->open(dbenv, home, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_REP | DB_INIT_TXN |
	    DB_RECOVER | DB_THREAD, 0)) != 0) {
		fprintf(stderr, "%s: env open: %s\n", home, db_strerror(ret));
		goto err;
	}

	if ((ret = dbenv->repmgr_start(dbenv, 3,
	    is_master ? DB_REP_MASTER : DB_REP_CLIENT)) != 0) {
		/*
		 * A client may start before the master's listener is up; retry
		 * a few times on DB_REP_UNAVAIL rather than failing the test.
		 */
		if (!is_master && ret == DB_REP_UNAVAIL) {
			int tries;
			for (tries = 0; tries < 10 && ret != 0; tries++) {
				sleep(1);
				ret = dbenv->repmgr_start(dbenv, 3,
				    DB_REP_CLIENT);
			}
		}
		if (ret != 0) {
			fprintf(stderr, "%s: repmgr_start: %s\n",
			    home, db_strerror(ret));
			goto err;
		}
	}

	if (is_master) {
		DB *db;
		DBT key, data;
		int i;

		/* Wait for the client to connect, then write the marker. */
		sleep(6);
		if ((ret = db_create(&db, dbenv, 0)) != 0)
			goto err;
		if ((ret = db->open(db, NULL, DBNAME, NULL, DB_BTREE,
		    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0)) != 0) {
			dbenv->err(dbenv, ret, "master db open");
			goto err;
		}
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = (void *)MARKER_KEY;
		key.size = (u_int32_t)strlen(MARKER_KEY);
		data.data = (void *)MARKER_VAL;
		data.size = (u_int32_t)strlen(MARKER_VAL);
		/* Write a few times so a dropped early message is tolerated. */
		for (i = 0; i < 5; i++) {
			if ((ret = db->put(db, NULL, &key, &data,
			    0)) != 0) {
				dbenv->err(dbenv, ret, "master put");
				break;
			}
			sleep(1);
		}
		(void)db->close(db, 0);
		/* Give replication time to flush, then stop. */
		sleep(3);
		(void)dbenv->close(dbenv, 0);
		return (ret);
	} else {
		DB *db;
		DBT key, data;
		int i, found;

		found = 0;
		for (i = 0; i < POLL_SECS; i++) {
			int oret, gret;
			sleep(1);
			if (db_create(&db, dbenv, 0) != 0)
				continue;
			if ((oret = db->open(db, NULL, DBNAME, NULL, DB_BTREE,
			    DB_RDONLY | DB_THREAD, 0)) != 0) {
				if (getenv("REP_VERBOSE") != NULL)
					fprintf(stderr,
					    "CLIENT poll %d: open=%d %s\n",
					    i, oret, db_strerror(oret));
				(void)db->close(db, 0);
				continue;
			}
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = (void *)MARKER_KEY;
			key.size = (u_int32_t)strlen(MARKER_KEY);
			data.flags = DB_DBT_MALLOC;
			gret = db->get(db, NULL, &key, &data, 0);
			if (getenv("REP_VERBOSE") != NULL)
				fprintf(stderr, "CLIENT poll %d: get=%d\n",
				    i, gret);
			if (gret == 0 &&
			    data.size == (u_int32_t)strlen(MARKER_VAL) &&
			    memcmp(data.data, MARKER_VAL, data.size) == 0)
				found = 1;
			if (data.data != NULL)
				free(data.data);
			(void)db->close(db, 0);
			if (found)
				break;
		}
		(void)dbenv->close(dbenv, 0);
		if (found) {
			printf("CLIENT: marker replicated\n");
			return (0);
		}
		fprintf(stderr, "CLIENT: marker NOT replicated in %d s\n",
		    POLL_SECS);
		return (1);
	}

err:
	(void)dbenv->close(dbenv, 0);
	return (ret == 0 ? 1 : ret);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	pid_t client;
	int cstat;
	u_int16_t mport, cport;

	if (argc != 5) {
		fprintf(stderr, "usage: %s mhome chome mport cport\n", argv[0]);
		return (2);
	}
	mport = (u_int16_t)atoi(argv[3]);
	cport = (u_int16_t)atoi(argv[4]);

	printf("TLS %s\n", tls_configured() ? "ON" : "OFF");

	if ((client = fork()) < 0) {
		perror("fork");
		return (2);
	}
	if (client == 0) {
		/*
		 * Child: the client.  Not the group creator.  Wait briefly so
		 * the master's listener is up before we try to join, avoiding a
		 * "too few remote sites" at startup.
		 */
		sleep(2);
		_exit(start_site(argv[2], cport, mport, 0, 0));
	}

	/* Parent: the master and group creator. */
	(void)start_site(argv[1], mport, cport, 1, 1);

	if (waitpid(client, &cstat, 0) != client) {
		perror("waitpid");
		return (2);
	}
	if (WIFEXITED(cstat) && WEXITSTATUS(cstat) == 0)
		return (0);
	fprintf(stderr, "client exit status %d\n", cstat);
	return (1);
}
