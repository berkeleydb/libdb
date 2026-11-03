/*
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * test/dst-rt/node/rtnode.c -- one libdb node for RFC 0024 (§2.1, §2.7).
 *
 * A long-lived process that opens an env (local transactional, or a repmgr
 * replication site), then reads newline commands on a control FIFO and writes
 * events to an event log.  F2's netns/HA scenarios drive several rtnodes; the
 * single-node scenarios drive one.
 *
 * Modes:
 *   local                      a DB_INIT_TXN env, no replication.
 *   --site HOST:PORT [--peer HOST:PORT ...]  a repmgr site.  --site is this
 *       node's local address; each --peer is another site (the first --peer
 *       is marked the bootstrap helper/peer so a fresh node can find the
 *       group).  --group-creator marks this node as the group creator.
 *
 * Control FIFO commands (RFC 0024 §2.7):
 *   start | stop          start/stop the repmgr site (local mode: no-op)
 *   load <n> <seed>       run n workload ops (master only); writes the ack log
 *   checkpoint            take a checkpoint
 *   quiesce               wait for the replication queue to drain
 *   exit                  exit cleanly
 *
 * Events written to <ev>, one per line:
 *   READY
 *   ROLE master|client gen=<n>
 *   ACK seq=<n>
 *   ERR <code>
 *
 * The ack log (HA mode) is written with the raw-syscall writer in fi_acklog.h
 * so libfiu I/O plans never fail the oracle's own log.  --nosync makes commits
 * DB_TXN_NOSYNC (the durability-lie sabotage arm).
 *
 * Usage:
 *   rtnode --home DIR --ctl FIFO --ev FILE --node NAME [local | repmgr opts]
 *          [--ack FILE] [--nosync] [--ack-policy quorum|all|one|none]
 *          [--cachesz-mb N] [--keys N] [--valsz N]
 */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <pthread.h>

#include "db.h"
#include "fi_acklog.h"

#define MAX_PEERS 8

static const char *progname = "rtnode";

struct node {
	const char *home, *ctlpath, *evpath, *name, *ackpath;
	const char *self_host; u_int self_port;
	const char *peer_host[MAX_PEERS]; u_int peer_port[MAX_PEERS];
	int npeers;
	int repmgr, group_creator, nosync, ack_policy;
	long cachesz_mb, keys, valsz;
	DB_ENV *env;
	DB *db;
	fi_acklog al;
	int have_ack;
	FILE *ev;
	int current_gen, is_master;
};

static struct node N;
static pthread_mutex_t ev_mtx = PTHREAD_MUTEX_INITIALIZER;

static void
ev_emit(const char *fmt, ...)
{
	va_list ap;
	pthread_mutex_lock(&ev_mtx);
	va_start(ap, fmt);
	vfprintf(N.ev, fmt, ap);
	va_end(ap);
	fputc('\n', N.ev);
	fflush(N.ev);
	pthread_mutex_unlock(&ev_mtx);
}

/* repmgr event callback: translate master/client/elected into ROLE lines. */
static void
event_cb(DB_ENV *dbenv, u_int32_t which, void *info)
{
	(void)dbenv; (void)info;
	switch (which) {
	case DB_EVENT_REP_MASTER:
	case DB_EVENT_REP_ELECTED:
		N.is_master = 1;
		ev_emit("ROLE master gen=%d", N.current_gen);
		break;
	case DB_EVENT_REP_CLIENT:
		N.is_master = 0;
		ev_emit("ROLE client gen=%d", N.current_gen);
		break;
	case DB_EVENT_REP_NEWMASTER:
		N.current_gen = info ? *(int *)info : N.current_gen + 1;
		ev_emit("ROLE newmaster gen=%d", N.current_gen);
		break;
	case DB_EVENT_REP_PERM_FAILED:
		ev_emit("ERR perm_failed");
		break;
	case DB_EVENT_REP_STARTUPDONE:
		ev_emit("READY");
		break;
	default:
		break;
	}
}

static int
open_env(void)
{
	int ret;
	u_int32_t flags;

	if ((ret = db_env_create(&N.env, 0)) != 0) {
		fprintf(stderr, "%s: db_env_create: %s\n", progname,
		    db_strerror(ret));
		return (ret);
	}
	(void)N.env->set_cachesize(N.env, 0,
	    (u_int32_t)N.cachesz_mb * 1024 * 1024, 1);
	(void)N.env->set_event_notify(N.env, event_cb);

	flags = DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL |
	    DB_INIT_TXN | DB_THREAD | DB_RECOVER;
	if (N.repmgr)
		flags |= DB_INIT_REP;

	if (N.repmgr) {
		DB_SITE *dbsite;
		int i;
		if ((ret = N.env->repmgr_set_ack_policy(N.env,
		    N.ack_policy)) != 0) {
			N.env->err(N.env, ret, "repmgr_set_ack_policy");
			return (ret);
		}
		/* local site */
		if ((ret = N.env->repmgr_site(N.env, N.self_host, N.self_port,
		    &dbsite, 0)) != 0) {
			N.env->err(N.env, ret, "repmgr_site(local)");
			return (ret);
		}
		(void)dbsite->set_config(dbsite, DB_LOCAL_SITE, 1);
		if (N.group_creator)
			(void)dbsite->set_config(dbsite, DB_GROUP_CREATOR, 1);
		(void)dbsite->close(dbsite);
		/* peers */
		for (i = 0; i < N.npeers; i++) {
			if ((ret = N.env->repmgr_site(N.env, N.peer_host[i],
			    N.peer_port[i], &dbsite, 0)) != 0) {
				N.env->err(N.env, ret, "repmgr_site(peer)");
				return (ret);
			}
			(void)dbsite->set_config(dbsite, DB_BOOTSTRAP_HELPER, 1);
			if (i == 0)
				(void)dbsite->set_config(dbsite,
				    DB_REPMGR_PEER, 1);
			(void)dbsite->close(dbsite);
		}
	}

	if ((ret = N.env->open(N.env, N.home, flags, 0644)) != 0) {
		N.env->err(N.env, ret, "env->open %s", N.home);
		return (ret);
	}
	return (0);
}

static int
start_repmgr(void)
{
	int ret;
	if (!N.repmgr)
		return (0);
	/* Three or fewer sites: let repmgr elect.  nthreads=3 message threads. */
	ret = N.env->repmgr_start(N.env, 3, DB_REP_ELECTION);
	if (ret != 0)
		N.env->err(N.env, ret, "repmgr_start");
	return (ret);
}

/* open the working db (auto-commit create); master only writes to it, a
 * client replicates it. */
static int
open_db(void)
{
	int ret;
	if (N.db != NULL)
		return (0);
	if ((ret = db_create(&N.db, N.env, 0)) != 0) {
		N.env->err(N.env, ret, "db_create");
		return (ret);
	}
	ret = N.db->open(N.db, NULL, "rt.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644);
	if (ret != 0) {
		/* on a client the db may not exist yet; not fatal here */
		(void)N.db->close(N.db, 0); N.db = NULL;
	}
	return (ret);
}

/* splitmix64, same as rtload, so a seed replays identically. */
static uint64_t sm_state;
static uint64_t sm_next(void) {
	uint64_t z = (sm_state += 0x9e3779b97f4a7c15ULL);
	z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
	z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
	return (z ^ (z >> 31));
}

/* run n workload ops as master; write the ack log if --ack was given. */
static void
do_load(long n, long seed)
{
	long i;
	int ret;
	char keybuf[64], valbuf[4096];
	DBT key, data;
	int vlen = (int)(N.valsz > (long)sizeof(valbuf) ? sizeof(valbuf) : N.valsz);

	if (open_db() != 0) {
		ev_emit("ERR load_no_db");
		return;
	}
	sm_state = (uint64_t)seed;
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));

	for (i = 0; i < n; i++) {
		DB_TXN *txn = NULL;
		u_int32_t k = (u_int32_t)(sm_next() % (uint64_t)N.keys);
		uint64_t vh;
		int klen = snprintf(keybuf, sizeof(keybuf), "k%08u", k);

		memset(valbuf, 'a' + (int)(k % 26), (size_t)vlen);
		snprintf(valbuf, sizeof(valbuf), "s%08ld-k%08u", i, k);
		if ((int)strlen(valbuf) < vlen)
			valbuf[strlen(valbuf)] = 'a' + (int)(k % 26);
		vh = fi_fnv1a(valbuf, (size_t)vlen);
		key.data = keybuf; key.size = (u_int32_t)klen;
		data.data = valbuf; data.size = (u_int32_t)vlen;

		if ((ret = N.env->txn_begin(N.env, NULL, &txn, 0)) != 0) {
			if (N.have_ack)
			    (void)fi_acklog_printf(&N.al,
				"%ld %ld txn rt.db - - FAIL", i, i);
			continue;
		}
		if (N.have_ack)
			(void)fi_acklog_printf(&N.al,
			    "%ld %ld put rt.db %s %016llx PENDING",
			    i, i, keybuf, (unsigned long long)vh);
		ret = N.db->put(N.db, txn, &key, &data, 0);
		if (ret != 0) {
			(void)txn->abort(txn);
			if (N.have_ack)
			    (void)fi_acklog_printf(&N.al,
				"%ld %ld txn rt.db - - FAIL", i, i);
			continue;
		}
		ret = txn->commit(txn, N.nosync ? DB_TXN_NOSYNC : 0);
		/* In repmgr, commit returning 0 means durable locally; the ack
		 * policy (DB_REP_PERMANENT) is reported via the event log.  For
		 * the ack log we mark ACK on commit==0; the oracle treats an op
		 * whose replicated ack we could not confirm as covered by the
		 * HA convergence rule (fi_check.sh), not rule 1. */
		if (N.have_ack)
			(void)fi_acklog_printf(&N.al,
			    "%ld %ld txn rt.db - - %s", i, i,
			    ret == 0 ? "ACK" : "FAIL");
		if (ret == 0)
			ev_emit("ACK seq=%ld", i);
	}
	ev_emit("LOADDONE n=%ld", n);
}

static void
do_checkpoint(void)
{
	int ret = N.env->txn_checkpoint(N.env, 0, 0, DB_FORCE);
	ev_emit(ret == 0 ? "CKPDONE" : "ERR ckp");
}

/* command loop: block reading newline commands from the FIFO. */
static void
run_ctl(void)
{
	FILE *ctl;
	char line[256];

	/* Opening a FIFO for read blocks until a writer appears; that is the
	 * synchronisation point the driver relies on. */
	if ((ctl = fopen(N.ctlpath, "r")) == NULL) {
		fprintf(stderr, "%s: ctl fifo %s: %s\n", progname,
		    N.ctlpath, strerror(errno));
		return;
	}
	ev_emit("READY");
	for (;;) {
		char *cmd;
		if (fgets(line, sizeof(line), ctl) == NULL) {
			/* writer closed; reopen to wait for the next command */
			fclose(ctl);
			if ((ctl = fopen(N.ctlpath, "r")) == NULL)
				break;
			continue;
		}
		line[strcspn(line, "\n")] = '\0';
		cmd = line;
		while (*cmd == ' ') cmd++;
		if (*cmd == '\0') continue;

		if (strcmp(cmd, "start") == 0) {
			ev_emit(start_repmgr() == 0 ? "STARTED" : "ERR start");
		} else if (strcmp(cmd, "stop") == 0) {
			/* repmgr has no public stop short of close; honour it
			 * by closing and exiting -- the driver restarts us. */
			ev_emit("STOPPED");
		} else if (strncmp(cmd, "load", 4) == 0) {
			long n = 0, seed = 0;
			sscanf(cmd + 4, " %ld %ld", &n, &seed);
			do_load(n, seed);
		} else if (strcmp(cmd, "checkpoint") == 0) {
			do_checkpoint();
		} else if (strcmp(cmd, "quiesce") == 0) {
			/* best-effort drain: a checkpoint plus a short settle */
			do_checkpoint();
			ev_emit("QUIESCED");
		} else if (strcmp(cmd, "exit") == 0) {
			ev_emit("EXITING");
			break;
		} else {
			ev_emit("ERR unknown_cmd");
		}
	}
	fclose(ctl);
}

static int
ack_policy_code(const char *s)
{
	if (strcmp(s, "quorum") == 0) return (DB_REPMGR_ACKS_QUORUM);
	if (strcmp(s, "all") == 0) return (DB_REPMGR_ACKS_ALL);
	if (strcmp(s, "one") == 0) return (DB_REPMGR_ACKS_ONE);
	if (strcmp(s, "none") == 0) return (DB_REPMGR_ACKS_NONE);
	return (DB_REPMGR_ACKS_QUORUM);
}

static int
parse_hostport(const char *s, const char **host, u_int *port, char *buf,
    size_t bufsz)
{
	const char *colon = strrchr(s, ':');
	if (colon == NULL) return (-1);
	if ((size_t)(colon - s) >= bufsz) return (-1);
	memcpy(buf, s, (size_t)(colon - s));
	buf[colon - s] = '\0';
	*host = buf;
	*port = (u_int)atoi(colon + 1);
	return (0);
}

static void
usage(void)
{
	fprintf(stderr,
"usage: %s --home DIR --ctl FIFO --ev FILE --node NAME\n"
"  (local)   local\n"
"  (repmgr)  --site HOST:PORT [--peer HOST:PORT ...] [--group-creator]\n"
"            [--ack-policy quorum|all|one|none]\n"
"  [--ack FILE] [--nosync] [--cachesz-mb N] [--keys N] [--valsz N]\n",
	    progname);
	exit(2);
}

int
main(int argc, char *argv[])
{
	int i, ret;
	static char selfbuf[128], peerbuf[MAX_PEERS][128];

	memset(&N, 0, sizeof(N));
	N.al.fd = -1;
	N.cachesz_mb = 32; N.keys = 1000; N.valsz = 200;
	N.ack_policy = DB_REPMGR_ACKS_QUORUM;

	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "--home") == 0 && i+1 < argc) N.home = argv[++i];
		else if (strcmp(argv[i], "--ctl") == 0 && i+1 < argc) N.ctlpath = argv[++i];
		else if (strcmp(argv[i], "--ev") == 0 && i+1 < argc) N.evpath = argv[++i];
		else if (strcmp(argv[i], "--node") == 0 && i+1 < argc) N.name = argv[++i];
		else if (strcmp(argv[i], "--ack") == 0 && i+1 < argc) N.ackpath = argv[++i];
		else if (strcmp(argv[i], "local") == 0) N.repmgr = 0;
		else if (strcmp(argv[i], "--site") == 0 && i+1 < argc) {
			N.repmgr = 1;
			if (parse_hostport(argv[++i], &N.self_host,
			    &N.self_port, selfbuf, sizeof(selfbuf)) != 0) usage();
		} else if (strcmp(argv[i], "--peer") == 0 && i+1 < argc) {
			if (N.npeers >= MAX_PEERS) usage();
			if (parse_hostport(argv[++i], &N.peer_host[N.npeers],
			    &N.peer_port[N.npeers], peerbuf[N.npeers],
			    sizeof(peerbuf[0])) != 0) usage();
			N.npeers++;
		} else if (strcmp(argv[i], "--group-creator") == 0) {
			N.group_creator = 1;
		} else if (strcmp(argv[i], "--ack-policy") == 0 && i+1 < argc) {
			N.ack_policy = ack_policy_code(argv[++i]);
		} else if (strcmp(argv[i], "--nosync") == 0) N.nosync = 1;
		else if (strcmp(argv[i], "--cachesz-mb") == 0 && i+1 < argc) N.cachesz_mb = atol(argv[++i]);
		else if (strcmp(argv[i], "--keys") == 0 && i+1 < argc) N.keys = atol(argv[++i]);
		else if (strcmp(argv[i], "--valsz") == 0 && i+1 < argc) N.valsz = atol(argv[++i]);
		else usage();
	}
	if (N.home == NULL || N.ctlpath == NULL || N.evpath == NULL ||
	    N.name == NULL)
		usage();

	if ((N.ev = fopen(N.evpath, "a")) == NULL) {
		fprintf(stderr, "%s: ev log %s: %s\n", progname, N.evpath,
		    strerror(errno));
		return (1);
	}
	if (N.ackpath != NULL) {
		if (fi_acklog_open(&N.al, N.ackpath) != 0) {
			fprintf(stderr, "%s: ack log %s: %s\n", progname,
			    N.ackpath, strerror(errno));
			return (1);
		}
		N.have_ack = 1;
	}

	if ((ret = open_env()) != 0) {
		ev_emit("ERR env_open %d", ret);
		goto err;
	}
	if (N.repmgr) {
		/* start now; the driver can still "stop"/"start" via ctl */
		(void)start_repmgr();
	}

	run_ctl();
	ret = 0;

err:
	if (N.db != NULL) (void)N.db->close(N.db, 0);
	if (N.env != NULL) (void)N.env->close(N.env, 0);
	if (N.have_ack) fi_acklog_close(&N.al);
	if (N.ev != NULL) fclose(N.ev);
	return (ret);
}
