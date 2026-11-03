/*
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 * SPDX-License-Identifier: Sleepycat
 *
 * hanode.c -- one libdb repmgr site for the RFC 0024 runtime-fault DST layer.
 *
 * A node is a real process with its own env home.  It implements the RFC 0024
 * section 2.7 node-control interface so the HA scenarios (S4-S8) can drive it
 * from a shell:
 *
 *   - reads newline commands on a FIFO  <run>/ctl.<node>:
 *       start | stop | load <n> <seed> | checkpoint | quiesce | exit
 *   - writes events to                  <run>/ev.<node>.log:
 *       READY | ROLE master|client gen=<n> | ACK seq=<n> | ERR <code>
 *   - in HA mode appends per-op lines to <run>/ack.<node>.log:
 *       <seq> <txnid> <op> <db> <key> <vhash> <outcome> [<lfile>/<loff>]
 *     outcome is ACK (commit 0 and ack policy met), FAIL (error, must not be
 *     visible) or UNKNOWN (process died / ack lost, may be visible).
 *
 * This is the F2 stand-in for F1's rtnode repmgr mode; it is wire- and
 * log-format compatible with the RFC so the oracle (rtcheck + fi_check.sh)
 * works against either.  Kills are external (kill -9 <pid>); the pid is
 * written to <run>/pid.<node>.
 *
 * Usage:
 *   hanode --run DIR --node NAME --home DIR --port P --self HOST \
 *          --site HOST:PORT [--site ...] [--creator] \
 *          [--ack quorum|none|all] [--priority N] [--db NAME]
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <db.h>

#define MAXSITES 16
#define MAXLINE  4096

typedef struct { char host[256]; int port; } site_t;

static DB_ENV *dbenv;
static DB	*db;
static char	 run_dir[1024], node_name[64], home_dir[1024], db_name[128] = "ha.db";
static char	 self_host[256];
static int	 self_port;
static site_t	 sites[MAXSITES];
static int	 nsites;
static int	 is_creator;
static int	 ack_policy = DB_REPMGR_ACKS_QUORUM;
static int	 priority = 100;

static FILE	*ev_fp;			/* event log */
static int	 ack_fd = -1;		/* ack log, O_APPEND write(2) */
static volatile int g_is_master;	/* set by event callback */
static volatile unsigned long g_perm_failed; /* bumped by PERM_FAILED event */
static volatile int g_started;

static void ev(const char *fmt, ...);
static u_int64_t fnv1a(const void *p, size_t n);
static u_int32_t current_gen(void);

/* ---- event + ack logging ------------------------------------------------- */
static void
ev(const char *fmt, ...)
{
	va_list ap;
	if (ev_fp == NULL)
		return;
	va_start(ap, fmt);
	vfprintf(ev_fp, fmt, ap);
	va_end(ap);
	fputc('\n', ev_fp);
	fflush(ev_fp);
}

static void
ack_write(const char *line)
{
	size_t n = strlen(line);
	if (ack_fd < 0)
		return;
	(void)write(ack_fd, line, n);	/* O_APPEND single write is atomic */
}

static u_int64_t
fnv1a(const void *p, size_t n)
{
	const unsigned char *s = p;
	u_int64_t h = 1469598103934665603ULL;
	size_t i;
	for (i = 0; i < n; i++) { h ^= s[i]; h *= 1099511628211ULL; }
	return (h);
}

/* ---- repmgr event callback ---------------------------------------------- */
static void
event_cb(DB_ENV *env, u_int32_t which, void *info)
{
	int err;
	switch (which) {
	case DB_EVENT_PANIC:
		err = *(int *)info;
		ev("ERR panic=%d", err);
		_exit(3);
	case DB_EVENT_REP_MASTER:
		g_is_master = 1;
		ev("ROLE master gen=%u", current_gen());
		break;
	case DB_EVENT_REP_CLIENT:
		g_is_master = 0;
		ev("ROLE client gen=%u", current_gen());
		break;
	case DB_EVENT_REP_NEWMASTER:
		ev("NEWMASTER gen=%u", current_gen());
		break;
	case DB_EVENT_REP_PERM_FAILED:
		g_perm_failed++;
		ev("PERMFAIL n=%lu", g_perm_failed);
		break;
	case DB_EVENT_REP_STARTUPDONE:
		ev("STARTUPDONE");
		break;
	default:
		break;
	}
	(void)env;
}

static u_int32_t
current_gen(void)
{
	DB_REP_STAT *rsp;
	u_int32_t gen = 0;
	if (dbenv != NULL &&
	    dbenv->rep_stat(dbenv, &rsp, 0) == 0) {
		gen = rsp->st_gen;
		free(rsp);
	}
	return (gen);
}

/* ---- env + repmgr setup -------------------------------------------------- */
static int
node_start(void)
{
	DB_SITE *dbsite;
	u_int32_t start_policy;
	int i, ret;

	if (g_started)
		return (0);

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "env_create: %s\n", db_strerror(ret));
		return (ret);
	}
	dbenv->set_errpfx(dbenv, node_name);
	dbenv->set_errfile(dbenv, stderr);
	(void)dbenv->set_event_notify(dbenv, event_cb);
	(void)dbenv->rep_set_priority(dbenv, priority);
	(void)dbenv->repmgr_set_ack_policy(dbenv, ack_policy);
	/* elections on, heartbeats configured (RFC S4) */
	(void)dbenv->rep_set_timeout(dbenv, DB_REP_HEARTBEAT_SEND,    500000);
	(void)dbenv->rep_set_timeout(dbenv, DB_REP_HEARTBEAT_MONITOR, 2000000);
	(void)dbenv->rep_set_timeout(dbenv, DB_REP_ACK_TIMEOUT,       1000000);
	(void)dbenv->rep_set_timeout(dbenv, DB_REP_ELECTION_TIMEOUT,  1000000);
	(void)dbenv->rep_set_timeout(dbenv, DB_REP_CONNECTION_RETRY,  1000000);

	/* local site */
	if ((ret = dbenv->repmgr_site(dbenv, self_host, (u_int)self_port,
	    &dbsite, 0)) != 0) {
		dbenv->err(dbenv, ret, "repmgr_site(local)");
		return (ret);
	}
	(void)dbsite->set_config(dbsite, DB_LOCAL_SITE, 1);
	if (is_creator)
		(void)dbsite->set_config(dbsite, DB_GROUP_CREATOR, 1);
	(void)dbsite->close(dbsite);

	/* remote sites */
	for (i = 0; i < nsites; i++) {
		if ((ret = dbenv->repmgr_site(dbenv, sites[i].host,
		    (u_int)sites[i].port, &dbsite, 0)) != 0) {
			dbenv->err(dbenv, ret, "repmgr_site(%s:%d)",
			    sites[i].host, sites[i].port);
			return (ret);
		}
		(void)dbsite->set_config(dbsite, DB_BOOTSTRAP_HELPER, 1);
		(void)dbsite->close(dbsite);
	}

	if ((ret = dbenv->open(dbenv, home_dir,
	    DB_CREATE | DB_RECOVER | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_INIT_REP | DB_THREAD, 0600)) != 0) {
		dbenv->err(dbenv, ret, "env->open");
		return (ret);
	}

	start_policy = DB_REP_ELECTION;
	if ((ret = dbenv->repmgr_start(dbenv, 3, start_policy)) != 0) {
		dbenv->err(dbenv, ret, "repmgr_start");
		return (ret);
	}
	g_started = 1;
	ev("READY pid=%d", (int)getpid());
	return (0);
}

static int
open_db(void)
{
	int ret;
	if (db != NULL)
		return (0);
	if ((ret = db_create(&db, dbenv, 0)) != 0)
		return (ret);
	if ((ret = db->open(db, NULL, db_name, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0600)) != 0) {
		dbenv->err(dbenv, ret, "db->open");
		(void)db->close(db, 0);
		db = NULL;
		return (ret);
	}
	return (0);
}

/* run <n> seeded put txns; master only.  Records ack-log outcomes. */
static void
do_load(int nops, unsigned seed)
{
	DB_TXN *txn;
	DBT key, data;
	char kbuf[64], vbuf[256], line[512];
	unsigned long pf_before;
	int i, ret;
	u_int64_t vh;

	if (!g_is_master) {
		ev("ERR load-not-master");
		return;
	}
	ev("LOAD begin n=%d seed=%u", nops, seed);
	if (open_db() != 0)
		return;
	srandom(seed);
	for (i = 0; i < nops; i++) {
		int klen = snprintf(kbuf, sizeof(kbuf), "k%08lx",
		    (unsigned long)random());
		int vlen = snprintf(vbuf, sizeof(vbuf), "v%010d-%08lx",
		    i, (unsigned long)random());
		vh = fnv1a(vbuf, (size_t)vlen);

		if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0) {
			ev("ERR txn_begin=%d", ret);
			return;
		}
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf;  key.size = (u_int32_t)klen;
		data.data = vbuf; data.size = (u_int32_t)vlen;
		pf_before = g_perm_failed;
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0) {
			(void)txn->abort(txn);
			snprintf(line, sizeof(line),
			    "%d %u put %s %s %016llx FAIL\n",
			    i, (unsigned)i, db_name, kbuf,
			    (unsigned long long)vh);
			ack_write(line);
			continue;
		}
		ret = txn->commit(txn, 0);
		if (ret != 0) {
			snprintf(line, sizeof(line),
			    "%d %u put %s %s %016llx FAIL\n",
			    i, (unsigned)i, db_name, kbuf,
			    (unsigned long long)vh);
			ack_write(line);
			continue;
		}
		/*
		 * commit returned 0.  Under QUORUM/ALL, a PERM_FAILED event
		 * raised during this commit means the ack policy was NOT met,
		 * so the write is not guaranteed durable -> UNKNOWN.  Otherwise
		 * ACK.  (Under ACKS_NONE every write is reported ACK even
		 * though none are replication-durable -- that is the S5
		 * sabotage arm, which the oracle must catch.)
		 */
		if (ack_policy != DB_REPMGR_ACKS_NONE &&
		    g_perm_failed != pf_before) {
			snprintf(line, sizeof(line),
			    "%d %u put %s %s %016llx UNKNOWN\n",
			    i, (unsigned)i, db_name, kbuf,
			    (unsigned long long)vh);
		} else {
			snprintf(line, sizeof(line),
			    "%d %u put %s %s %016llx ACK\n",
			    i, (unsigned)i, db_name, kbuf,
			    (unsigned long long)vh);
			ev("ACK seq=%d", i);
		}
		ack_write(line);
	}
}

static void
do_checkpoint(void)
{
	int ret;
	if (dbenv == NULL) return;
	if ((ret = dbenv->txn_checkpoint(dbenv, 0, 0, DB_FORCE)) != 0)
		ev("ERR checkpoint=%d", ret);
}

static void
do_quiesce(void)
{
	/* best-effort: flush the log and give replication time to drain */
	if (dbenv != NULL)
		(void)dbenv->log_flush(dbenv, NULL);
	sleep(2);
	ev("QUIESCED");
}

static void
node_stop(void)
{
	if (db != NULL)    { (void)db->close(db, 0); db = NULL; }
	if (dbenv != NULL) { (void)dbenv->close(dbenv, 0); dbenv = NULL; }
	g_started = 0;
}

/* ---- control loop -------------------------------------------------------- */
static void
usage(void)
{
	fprintf(stderr,
"usage: hanode --run DIR --node NAME --home DIR --port P --self HOST\n"
"              --site HOST:PORT [--site ...] [--creator]\n"
"              [--ack quorum|none|all] [--priority N] [--db NAME]\n");
	exit(2);
}

int
main(int argc, char **argv)
{
	char ctl_path[1100], ev_path[1100], ack_path[1100], pid_path[1100];
	char line[MAXLINE], *cmd, *a1, *a2, *sp;
	FILE *ctl;
	int i, ctl_fd;

	for (i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "--run") && i+1 < argc)
			snprintf(run_dir, sizeof(run_dir), "%s", argv[++i]);
		else if (!strcmp(argv[i], "--node") && i+1 < argc)
			snprintf(node_name, sizeof(node_name), "%s", argv[++i]);
		else if (!strcmp(argv[i], "--home") && i+1 < argc)
			snprintf(home_dir, sizeof(home_dir), "%s", argv[++i]);
		else if (!strcmp(argv[i], "--port") && i+1 < argc)
			self_port = atoi(argv[++i]);
		else if (!strcmp(argv[i], "--self") && i+1 < argc)
			snprintf(self_host, sizeof(self_host), "%s", argv[++i]);
		else if (!strcmp(argv[i], "--site") && i+1 < argc) {
			if (nsites >= MAXSITES) usage();
			sp = strrchr(argv[++i], ':');
			if (sp == NULL) usage();
			*sp = '\0';
			snprintf(sites[nsites].host, 256, "%s", argv[i]);
			sites[nsites].port = atoi(sp + 1);
			nsites++;
		} else if (!strcmp(argv[i], "--creator"))
			is_creator = 1;
		else if (!strcmp(argv[i], "--priority") && i+1 < argc)
			priority = atoi(argv[++i]);
		else if (!strcmp(argv[i], "--db") && i+1 < argc)
			snprintf(db_name, sizeof(db_name), "%s", argv[++i]);
		else if (!strcmp(argv[i], "--ack") && i+1 < argc) {
			++i;
			if (!strcmp(argv[i], "quorum"))
				ack_policy = DB_REPMGR_ACKS_QUORUM;
			else if (!strcmp(argv[i], "none"))
				ack_policy = DB_REPMGR_ACKS_NONE;
			else if (!strcmp(argv[i], "all"))
				ack_policy = DB_REPMGR_ACKS_ALL;
			else usage();
		} else
			usage();
	}
	if (!run_dir[0] || !node_name[0] || !home_dir[0] || !self_port ||
	    !self_host[0])
		usage();

	snprintf(ctl_path, sizeof(ctl_path), "%s/ctl.%s", run_dir, node_name);
	snprintf(ev_path,  sizeof(ev_path),  "%s/ev.%s.log", run_dir, node_name);
	snprintf(ack_path, sizeof(ack_path), "%s/ack.%s.log", run_dir, node_name);
	snprintf(pid_path, sizeof(pid_path), "%s/pid.%s", run_dir, node_name);

	(void)mkfifo(ctl_path, 0600);		/* ok if it already exists */
	ev_fp = fopen(ev_path, "a");
	ack_fd = open(ack_path, O_WRONLY | O_CREAT | O_APPEND, 0600);
	{ FILE *pf = fopen(pid_path, "w");
	  if (pf) { fprintf(pf, "%d\n", (int)getpid()); fclose(pf); } }

	signal(SIGPIPE, SIG_IGN);

	/*
	 * Open the FIFO read-write so our own read end never sees EOF when the
	 * last writer (a one-shot echo) closes.  Re-read line by line forever.
	 */
	if ((ctl_fd = open(ctl_path, O_RDWR)) < 0) {
		perror("open ctl");
		return (1);
	}
	ctl = fdopen(ctl_fd, "r");

	for (;;) {
		if (fgets(line, sizeof(line), ctl) == NULL) {
			if (feof(ctl)) { clearerr(ctl); usleep(50000); continue; }
			break;
		}
		line[strcspn(line, "\r\n")] = '\0';
		cmd = strtok(line, " \t");
		if (cmd == NULL)
			continue;
		a1 = strtok(NULL, " \t");
		a2 = strtok(NULL, " \t");

		if (!strcmp(cmd, "start"))
			(void)node_start();
		else if (!strcmp(cmd, "stop"))
			node_stop();
		else if (!strcmp(cmd, "load") && a1 && a2)
			do_load(atoi(a1), (unsigned)strtoul(a2, NULL, 0));
		else if (!strcmp(cmd, "checkpoint"))
			do_checkpoint();
		else if (!strcmp(cmd, "quiesce"))
			do_quiesce();
		else if (!strcmp(cmd, "exit")) {
			node_stop();
			break;
		} else
			ev("ERR badcmd=%s", cmd);
	}

	if (ev_fp) fclose(ev_fp);
	if (ack_fd >= 0) close(ack_fd);
	return (0);
}
