/*
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * test/dst-rt/node/rtload.c -- the runtime-fault-injection workload generator
 * (RFC 0024 §2.1, §2.7).
 *
 * Opens a local transactional env, runs a SEEDED sequence of operations, and
 * writes a client-side ACK log so the oracle (rtcheck) can decide, after a
 * fault, whether each op's durability promise held:
 *
 *   <seq> <txnid> <op> <db> <key> <vhash> <outcome>
 *
 * outcome is ACK (commit returned 0), FAIL (an error was returned, so the op
 * must NOT be visible) or UNKNOWN (the process died or the ack was lost, so it
 * may be visible either way).  A put's vhash is a 64-bit FNV-1a of the value;
 * a del's is '-'.
 *
 * Determinism: all op choices come from a seeded PRNG.  Combined with libfiu's
 * FIU_PRNG_SEED and kill-at-op-count (--kill-at), a single seed replays the
 * same schedule, which is the RFC's single-node replay property (§2.5).
 *
 * SABOTAGE: --nosync commits with DB_TXN_NOSYNC yet still logs the op ACK.
 * That is a durability LIE: after directio + kill, acked-but-unsynced writes
 * are lost and rtcheck must catch it.  This is the S1 sabotage arm (§F1 gate).
 *
 * Usage:
 *   rtload --home DIR --ack FILE --node NAME --seed N --ops N [--db NAME]
 *          [--keys N] [--valsz N] [--txn-ops N] [--nosync] [--kill-at N]
 *          [--del-pct P] [--cachesz-mb N]
 */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>

#include "db.h"
#include "fi_acklog.h"

static const char *progname = "rtload";

/* A small, explicit, deterministic PRNG (splitmix64) so the schedule does not
 * depend on the platform's rand().  Same seed, same ops, everywhere. */
static uint64_t prng_state;
static uint64_t
prng_next(void)
{
	uint64_t z = (prng_state += 0x9e3779b97f4a7c15ULL);
	z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
	z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
	return (z ^ (z >> 31));
}
static uint32_t
prng_below(uint32_t n)
{
	return (n == 0 ? 0 : (uint32_t)(prng_next() % n));
}

struct cfg {
	const char *home, *ackpath, *node, *dbname;
	long seed, ops, keys, valsz, txn_ops, kill_at, del_pct, cachesz_mb;
	int nosync;
};

static void
usage(void)
{
	fprintf(stderr,
"usage: %s --home DIR --ack FILE --node NAME --seed N --ops N\n"
"          [--db NAME] [--keys N] [--valsz N] [--txn-ops N] [--nosync]\n"
"          [--kill-at N] [--del-pct P] [--cachesz-mb N]\n", progname);
	exit(2);
}

int
main(int argc, char *argv[])
{
	struct cfg c;
	DB_ENV *env = NULL;
	DB *db = NULL;
	DB_TXN *txn = NULL;
	fi_acklog al;
	int ret, i, j, commit_flags, oret;
	long seq = 0;
	u_int32_t env_flags, txnid_counter = 0;
	char keybuf[64], valbuf[4096];
	DBT key, val;

	memset(&c, 0, sizeof(c));
	c.dbname = "rt.db";
	c.keys = 1000; c.valsz = 200; c.txn_ops = 4;
	c.del_pct = 10; c.cachesz_mb = 32; c.kill_at = -1;

	for (i = 1; i < argc; i++) {
#define ARG(name) (strcmp(argv[i], name) == 0)
#define VAL() (i + 1 < argc ? argv[++i] : (usage(), (char *)0))
		if (ARG("--home")) c.home = VAL();
		else if (ARG("--ack")) c.ackpath = VAL();
		else if (ARG("--node")) c.node = VAL();
		else if (ARG("--db")) c.dbname = VAL();
		else if (ARG("--seed")) c.seed = atol(VAL());
		else if (ARG("--ops")) c.ops = atol(VAL());
		else if (ARG("--keys")) c.keys = atol(VAL());
		else if (ARG("--valsz")) c.valsz = atol(VAL());
		else if (ARG("--txn-ops")) c.txn_ops = atol(VAL());
		else if (ARG("--del-pct")) c.del_pct = atol(VAL());
		else if (ARG("--kill-at")) c.kill_at = atol(VAL());
		else if (ARG("--cachesz-mb")) c.cachesz_mb = atol(VAL());
		else if (ARG("--nosync")) c.nosync = 1;
		else usage();
	}
	if (c.home == NULL || c.ackpath == NULL || c.node == NULL || c.ops <= 0)
		usage();
	if (c.valsz > (long)sizeof(valbuf)) c.valsz = sizeof(valbuf);
	if (c.txn_ops < 1) c.txn_ops = 1;

	prng_state = (uint64_t)c.seed;

	if ((ret = fi_acklog_open(&al, c.ackpath)) != 0) {
		fprintf(stderr, "%s: ack log %s: %s\n", progname, c.ackpath,
		    strerror(ret));
		return (1);
	}

	if ((ret = db_env_create(&env, 0)) != 0) {
		fprintf(stderr, "%s: db_env_create: %s\n", progname,
		    db_strerror(ret));
		return (1);
	}
	(void)env->set_cachesize(env, 0,
	    (u_int32_t)c.cachesz_mb * 1024 * 1024, 1);
	env_flags = DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL |
	    DB_INIT_TXN | DB_THREAD;
	if ((ret = env->open(env, c.home, env_flags, 0644)) != 0) {
		env->err(env, ret, "env->open %s", c.home);
		goto err;
	}

	if ((ret = db_create(&db, env, 0)) != 0) {
		env->err(env, ret, "db_create");
		goto err;
	}
	/* One auto-commit create so the db exists before the measured ops. */
	if ((ret = db->open(db, NULL, c.dbname, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644)) != 0) {
		env->err(env, ret, "db->open %s", c.dbname);
		goto err;
	}

	/* DB_TXN_NOSYNC on commit is the sabotage: durability is NOT guaranteed
	 * but we still log ACK.  The honest path syncs (flags 0). */
	commit_flags = c.nosync ? DB_TXN_NOSYNC : 0;

	memset(&key, 0, sizeof(key));
	memset(&val, 0, sizeof(val));

	while (seq < c.ops) {
		int nops = 1 + (int)prng_below((uint32_t)c.txn_ops);
		u_int32_t this_txn = ++txnid_counter;
		long txn_start_seq = seq;

		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0) {
			env->err(env, ret, "txn_begin");
			/* Begin failed: log the whole group FAIL and continue. */
			for (j = 0; j < nops && seq < c.ops; j++, seq++)
				(void)fi_acklog_printf(&al,
				    "%ld %u txn %s - - FAIL",
				    seq, this_txn, c.dbname);
			continue;
		}

		oret = 0;
		for (j = 0; j < nops && seq < c.ops; j++, seq++) {
			u_int32_t k = prng_below((uint32_t)c.keys);
			int is_del = ((long)prng_below(100) < c.del_pct);
			int klen = snprintf(keybuf, sizeof(keybuf), "k%08u", k);
			key.data = keybuf; key.size = (u_int32_t)klen;

			/* kill-at: exit hard at a seeded op count, leaving the
			 * in-flight txn uncommitted.  The op is logged UNKNOWN
			 * by the restart-side reconciliation (we do not log it
			 * here, because we die before the ack). */
			if (c.kill_at >= 0 && seq == c.kill_at) {
				fflush(NULL);
				/* SIGKILL self: no atexit, no flush, mimic a
				 * crash at this exact deterministic op. */
				(void)kill(getpid(), SIGKILL);
				_exit(137);
			}

			if (is_del) {
				ret = db->del(db, txn, &key, 0);
				if (ret != 0 && ret != DB_NOTFOUND)
					oret = ret;
				/* record the member op (del) under the txnid */
				(void)fi_acklog_printf(&al,
				    "%ld %u del %s %s - PENDING",
				    seq, this_txn, c.dbname, keybuf);
			} else {
				uint64_t vh;
				int vlen = (int)c.valsz;
				/* deterministic value: seed+seq so a replay
				 * reproduces the exact bytes and vhash. */
				memset(valbuf, 'a' + (int)(k % 26), (size_t)vlen);
				snprintf(valbuf, sizeof(valbuf),
				    "s%08ld-k%08u", seq, k);
				/* restore the pad the snprintf NUL-truncated */
				if ((int)strlen(valbuf) < vlen)
					valbuf[strlen(valbuf)] =
					    'a' + (int)(k % 26);
				val.data = valbuf; val.size = (u_int32_t)vlen;
				vh = fi_fnv1a(valbuf, (size_t)vlen);
				ret = db->put(db, txn, &key, &val, 0);
				if (ret != 0)
					oret = ret;
				(void)fi_acklog_printf(&al,
				    "%ld %u put %s %s %016llx PENDING",
				    seq, this_txn, c.dbname, keybuf,
				    (unsigned long long)vh);
			}
		}

		if (oret != 0) {
			(void)txn->abort(txn);
			(void)fi_acklog_printf(&al, "%ld %u txn %s - - FAIL",
			    txn_start_seq, this_txn, c.dbname);
		} else {
			ret = txn->commit(txn, (u_int32_t)commit_flags);
			if (ret == 0)
				(void)fi_acklog_printf(&al,
				    "%ld %u txn %s - - ACK",
				    txn_start_seq, this_txn, c.dbname);
			else
				(void)fi_acklog_printf(&al,
				    "%ld %u txn %s - - FAIL",
				    txn_start_seq, this_txn, c.dbname);
		}
		txn = NULL;
	}

	ret = 0;
err:
	if (txn != NULL) (void)txn->abort(txn);
	if (db != NULL) (void)db->close(db, 0);
	if (env != NULL) (void)env->close(env, 0);
	fi_acklog_close(&al);
	return (ret == 0 ? 0 : 1);
}
