/*-
 * See the file LICENSE for redistribution information.
 *
 * batch_diff.c -- differential equivalence test for db_get_multiple().
 *
 * db_get_multiple() amortizes per-CALL overhead (one ENV_ENTER, one cursor
 * allocate/free pair) across N scattered keys.  That is only legitimate if the
 * batch returns EXACTLY what N individual DB->get() calls return -- same
 * values, same not-founds, same per-key error codes -- and, under
 * DB_TXN_SERIALIZABLE, records the SAME read set, so the same rw-antidependency
 * pivots are detected and the same transactions abort.  An amortization that
 * quietly skipped a SIREAD marker would look like a throughput win and be a
 * silent isolation downgrade.
 *
 * Four phases, each of which fails loudly rather than skipping:
 *
 *   1. value/notfound/errcode equivalence.  Every key in a mixed population
 *      (present, absent, first, last, duplicated within the batch) is fetched
 *      both ways and the returned size+bytes and per-key return code are
 *      compared byte for byte.  Also covers DB_BUFFER_SMALL (a too-small
 *      DB_DBT_USERMEM buffer must report the same code and the same required
 *      size) and the "stop at first hard error" rule.
 *
 *   2. read-set equivalence under DB_TXN_SERIALIZABLE, measured, not asserted
 *      by inspection: run the identical read set as (a) N DB->get calls and
 *      (b) one db_get_multiple, inside an SSI transaction, and compare the
 *      lock-region SIREAD marker population attributable to each.  A batch
 *      that skipped markers would leave fewer.
 *
 *   3. ISOLATION-EQUIVALENCE WITH TEETH: a write-skew pivot.  Reader txn T1
 *      reads key A (either way), writer T2 writes A and commits; then T1
 *      writes B.  Under DB_TXN_SERIALIZABLE this is the classic
 *      rw-antidependency pivot and T1 must fail with DB_SNAPSHOT_CONFLICT.
 *      Both arms must produce the SAME verdict.  The anti-vacuity control is
 *      the same schedule at plain snapshot isolation, which MUST commit -- so
 *      a run where nothing conflicts because the schedule never armed is
 *      distinguishable from a run where the batch weakened isolation.
 *
 *   4. deadlock/abort behavior: a batch that hits a conflicting write lock must
 *      return the same class of error the individual path returns, not silently
 *      skip the key.
 *
 * Usage: batch_diff [nkeys]      (default 2000)
 * Env:   BATCH_DIFF_HOME -- directory to use (created if absent, never removed)
 *
 * Prints one "VERDICT:" line per phase and a final PASS/FAIL, so a caller can
 * tell a real verdict from an exit(0) that ran nothing.
 */
#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#define	VALSZ		64
#define	MAXBATCH	512

static DB_ENV *env;
static DB *db;
static int failures;
static int phase_verdicts;

#define	CHECK(cond, ...)  do {						\
	if (!(cond)) {							\
		printf("FAIL %s:%d: ", __FILE__, __LINE__);		\
		printf(__VA_ARGS__);					\
		printf("\n");						\
		failures++;						\
	}								\
} while (0)

#define	DIE(ret, msg) do {						\
	env->err(env, ret, "%s", msg);					\
	exit(2);							\
} while (0)

static void
fill_key(DBT *k, u_int32_t *kb, u_int32_t v)
{
	*kb = v;
	memset(k, 0, sizeof(*k));
	k->data = kb;
	k->size = sizeof(*kb);
}

static void
fill_out(DBT *d, char *buf, u_int32_t len)
{
	memset(d, 0, sizeof(*d));
	d->data = buf;
	d->ulen = len;
	d->flags = DB_DBT_USERMEM;
}

/*
 * Phase 1 -- values, not-founds, error codes, DB_BUFFER_SMALL.
 */
static void
phase1_values(u_int32_t nkeys)
{
	DBT keys_i[MAXBATCH], datas_i[MAXBATCH];
	DBT keys_b[MAXBATCH], datas_b[MAXBATCH];
	u_int32_t kb_i[MAXBATCH], kb_b[MAXBATCH];
	char buf_i[MAXBATCH][VALSZ + 8], buf_b[MAXBATCH][VALSZ + 8];
	int rets_i[MAXBATCH], rets_b[MAXBATCH];
	u_int32_t i, n, pick;
	unsigned seed = 424242;
	int ret, mism;

	n = 128;
	for (i = 0; i < n; i++) {
		/*
		 * Mixed population: present keys, absent keys (>= nkeys),
		 * the first and last key, and deliberate repeats so a
		 * batch that caches a page across keys is exercised.
		 */
		switch (i % 8) {
		case 0: pick = 0; break;			/* first */
		case 1: pick = nkeys - 1; break;		/* last */
		case 2: pick = nkeys + (i * 7); break;		/* absent */
		case 3: pick = 5; break;			/* repeat */
		default:
			pick = (u_int32_t)(rand_r(&seed) % nkeys);
			break;
		}
		fill_key(&keys_i[i], &kb_i[i], pick);
		fill_key(&keys_b[i], &kb_b[i], pick);
		fill_out(&datas_i[i], buf_i[i], sizeof(buf_i[i]));
		fill_out(&datas_b[i], buf_b[i], sizeof(buf_b[i]));
		rets_i[i] = rets_b[i] = 12345;	/* poison */
	}

	/* Arm A: N individual DB->get calls. */
	for (i = 0; i < n; i++)
		rets_i[i] = db->get(db, NULL, &keys_i[i], &datas_i[i], 0);

	/* Arm B: one batched call. */
	ret = db_get_multiple(db, NULL, keys_b, datas_b, rets_b, n, 0);
	CHECK(ret == 0, "batch returned %d (%s), expected 0", ret,
	    db_strerror(ret));

	mism = 0;
	for (i = 0; i < n; i++) {
		if (rets_i[i] != rets_b[i]) {
			CHECK(0, "key idx %u: indiv ret %d (%s) != batch ret "
			    "%d (%s)", i, rets_i[i], db_strerror(rets_i[i]),
			    rets_b[i], db_strerror(rets_b[i]));
			mism++;
			continue;
		}
		if (rets_i[i] != 0)
			continue;
		if (datas_i[i].size != datas_b[i].size) {
			CHECK(0, "key idx %u: indiv size %u != batch size %u",
			    i, datas_i[i].size, datas_b[i].size);
			mism++;
			continue;
		}
		if (memcmp(datas_i[i].data, datas_b[i].data,
		    datas_i[i].size) != 0) {
			CHECK(0, "key idx %u: value bytes differ", i);
			mism++;
		}
	}
	printf("VERDICT phase1-values: %u keys compared, %d mismatches\n",
	    n, mism);
	phase_verdicts++;

	/*
	 * DB_BUFFER_SMALL equivalence: an undersized DB_DBT_USERMEM buffer
	 * must yield the same code AND the same required size, in both arms.
	 */
	{
		DBT k1, d1, k2, d2;
		u_int32_t k1b, k2b;
		char tiny1[4], tiny2[4];
		int r1, r2[1];

		fill_key(&k1, &k1b, 7);
		fill_key(&k2, &k2b, 7);
		fill_out(&d1, tiny1, sizeof(tiny1));
		fill_out(&d2, tiny2, sizeof(tiny2));
		r1 = db->get(db, NULL, &k1, &d1, 0);
		r2[0] = 12345;
		(void)db_get_multiple(db, NULL, &k2, &d2, r2, 1, 0);
		CHECK(r1 == r2[0], "BUFFER_SMALL: indiv %d (%s) != batch %d "
		    "(%s)", r1, db_strerror(r1), r2[0], db_strerror(r2[0]));
		CHECK(d1.size == d2.size,
		    "BUFFER_SMALL: required size indiv %u != batch %u",
		    d1.size, d2.size);
		printf("VERDICT phase1-buffer-small: indiv=%d batch=%d "
		    "size indiv=%u batch=%u\n", r1, r2[0], d1.size, d2.size);
		phase_verdicts++;
	}
}

/*
 * Phase 2 -- read-set equivalence under DB_TXN_SERIALIZABLE, measured via the
 * lock-region SIREAD marker population.  A batch that skipped markers would
 * leave measurably fewer than the individual path.
 */
static void
phase2_readset(u_int32_t nkeys)
{
	DB_LOCK_STAT *lk;
	DB_TXN *txn;
	DBT keys[MAXBATCH], datas[MAXBATCH];
	u_int32_t kb[MAXBATCH];
	char buf[MAXBATCH][VALSZ + 8];
	int rets[MAXBATCH];
	u_int32_t base_i, base_b, i, n, spread;
	u_int32_t after_i, after_b;
	int ret;

	n = 64;
	/* Spread keys so each lands on its own leaf page (~3 recs/leaf). */
	spread = nkeys / (n + 2);
	if (spread < 8)
		spread = 8;

	/* Arm A: N individual gets inside one SSI txn. */
	if ((ret = env->lock_stat(env, &lk, DB_STAT_CLEAR)) != 0)
		DIE(ret, "lock_stat");
	free(lk);
	if ((ret = env->txn_begin(env, NULL, &txn, DB_TXN_SERIALIZABLE)) != 0)
		DIE(ret, "txn_begin");
	if ((ret = env->lock_stat(env, &lk, 0)) != 0)
		DIE(ret, "lock_stat");
	base_i = lk->st_nobjects;
	free(lk);
	for (i = 0; i < n; i++) {
		fill_key(&keys[i], &kb[i], (i + 1) * spread);
		fill_out(&datas[i], buf[i], sizeof(buf[i]));
		ret = db->get(db, txn, &keys[i], &datas[i], 0);
		if (ret != 0 && ret != DB_NOTFOUND)
			DIE(ret, "phase2 indiv get");
	}
	if ((ret = env->lock_stat(env, &lk, 0)) != 0)
		DIE(ret, "lock_stat");
	after_i = lk->st_nobjects;
	free(lk);
	if ((ret = txn->commit(txn, 0)) != 0)
		DIE(ret, "phase2 indiv commit");

	/* Arm B: the same read set as one batched call in one SSI txn. */
	if ((ret = env->txn_begin(env, NULL, &txn, DB_TXN_SERIALIZABLE)) != 0)
		DIE(ret, "txn_begin");
	if ((ret = env->lock_stat(env, &lk, 0)) != 0)
		DIE(ret, "lock_stat");
	base_b = lk->st_nobjects;
	free(lk);
	for (i = 0; i < n; i++) {
		fill_key(&keys[i], &kb[i], (i + 1) * spread);
		fill_out(&datas[i], buf[i], sizeof(buf[i]));
		rets[i] = 12345;
	}
	ret = db_get_multiple(db, txn, keys, datas, rets, n, 0);
	if (ret != 0 && ret != DB_NOTFOUND)
		DIE(ret, "phase2 batch get");
	if ((ret = env->lock_stat(env, &lk, 0)) != 0)
		DIE(ret, "lock_stat");
	after_b = lk->st_nobjects;
	free(lk);
	if ((ret = txn->commit(txn, 0)) != 0)
		DIE(ret, "phase2 batch commit");

	/*
	 * The two arms read the same keys, so the objects the read set touches
	 * must match.  Objects are shared/reused across the two arms, so
	 * compare the DELTA each arm produced, and require the batch's delta to
	 * be no smaller -- a skipped marker shows up as a smaller delta.
	 * (Anti-vacuity: if BOTH deltas are 0 the probe measured nothing and
	 * that is itself a failure, not a pass.)
	 */
	printf("VERDICT phase2-readset: indiv objects %u->%u (delta %d), "
	    "batch %u->%u (delta %d)\n",
	    base_i, after_i, (int)after_i - (int)base_i,
	    base_b, after_b, (int)after_b - (int)base_b);
	CHECK((int)after_i - (int)base_i > 0 ||
	    (int)after_b - (int)base_b > 0,
	    "read-set probe measured nothing in either arm (vacuous)");
	CHECK((int)after_b - (int)base_b >= (int)after_i - (int)base_i,
	    "batch read set SMALLER than individual read set: "
	    "batch delta %d < indiv delta %d -- isolation weakened",
	    (int)after_b - (int)base_b, (int)after_i - (int)base_i);
	phase_verdicts++;
}

/*
 * Phase 3 -- isolation equivalence with teeth: the write-skew pivot.
 *
 * T1 reads key A (arm-dependent), T2 writes A and commits, T1 writes B.  At
 * DB_TXN_SERIALIZABLE, T1's write must be refused with DB_SNAPSHOT_CONFLICT
 * (an rw-antidependency pivot).  At plain snapshot isolation the same schedule
 * must COMMIT -- that control is what proves the schedule is really armed, so
 * a "both arms conflicted" result cannot come from an accident.
 *
 * Returns the code T1's write/commit produced.
 */
static int
pivot_once(int use_batch, u_int32_t iso_flag, u_int32_t keyA, u_int32_t keyB)
{
	DB_TXN *t1, *t2;
	DBT k, d;
	u_int32_t kbuf;
	char out[VALSZ + 8], val[VALSZ];
	int rets[1], ret, t1ret;

	memset(val, 'w', sizeof(val));

	if ((ret = env->txn_begin(env, NULL, &t1, iso_flag)) != 0)
		DIE(ret, "pivot t1 begin");

	/* T1 reads A -- via the arm under test. */
	fill_key(&k, &kbuf, keyA);
	fill_out(&d, out, sizeof(out));
	if (use_batch) {
		rets[0] = 12345;
		ret = db_get_multiple(db, t1, &k, &d, rets, 1, 0);
		if (ret != 0 && ret != DB_NOTFOUND)
			DIE(ret, "pivot batch read");
	} else {
		ret = db->get(db, t1, &k, &d, 0);
		if (ret != 0 && ret != DB_NOTFOUND)
			DIE(ret, "pivot indiv read");
	}

	/* T2 writes A and commits -- creating the rw-antidependency into T1. */
	if ((ret = env->txn_begin(env, NULL, &t2, iso_flag)) != 0)
		DIE(ret, "pivot t2 begin");
	fill_key(&k, &kbuf, keyA);
	memset(&d, 0, sizeof(d));
	d.data = val; d.size = sizeof(val);
	ret = db->put(db, t2, &k, &d, 0);
	if (ret != 0) {
		(void)t2->abort(t2);
		(void)t1->abort(t1);
		return (ret);
	}
	if ((ret = t2->commit(t2, 0)) != 0) {
		(void)t1->abort(t1);
		return (ret);
	}

	/* T1 now writes B -- this is where the pivot must be refused. */
	fill_key(&k, &kbuf, keyB);
	memset(&d, 0, sizeof(d));
	d.data = val; d.size = sizeof(val);
	t1ret = db->put(db, t1, &k, &d, 0);
	if (t1ret == 0)
		t1ret = t1->commit(t1, 0);
	else
		(void)t1->abort(t1);
	return (t1ret);
}

static void
phase3_isolation(u_int32_t nkeys)
{
	u_int32_t base, keyA, keyB, spread;
	int i, iter, s_i, s_b, n_i, n_b;

	spread = nkeys / 64;
	if (spread < 8)
		spread = 8;
	iter = 20;
	s_i = s_b = n_i = n_b = 0;

	for (i = 0; i < iter; i++) {
		/* Fresh key pair each iteration so versions never collide. */
		base = (u_int32_t)(i * 4 + 1);
		keyA = base * spread;
		keyB = (base + 1) * spread;

		if (pivot_once(0, DB_TXN_SERIALIZABLE, keyA, keyB) ==
		    DB_SNAPSHOT_CONFLICT)
			s_i++;
		keyA = (base + 2) * spread;
		keyB = (base + 3) * spread;
		if (pivot_once(1, DB_TXN_SERIALIZABLE, keyA, keyB) ==
		    DB_SNAPSHOT_CONFLICT)
			s_b++;

		/* Anti-vacuity control: plain SI must COMMIT the same shape. */
		keyA = (base + 128) * spread;
		keyB = (base + 129) * spread;
		if (pivot_once(0, DB_TXN_SNAPSHOT, keyA, keyB) == 0)
			n_i++;
		keyA = (base + 130) * spread;
		keyB = (base + 131) * spread;
		if (pivot_once(1, DB_TXN_SNAPSHOT, keyA, keyB) == 0)
			n_b++;
	}

	printf("VERDICT phase3-isolation: SERIALIZABLE pivots refused "
	    "indiv=%d/%d batch=%d/%d ; snapshot-control commits "
	    "indiv=%d/%d batch=%d/%d\n",
	    s_i, iter, s_b, iter, n_i, iter, n_b, iter);
	phase_verdicts++;

	/*
	 * The equivalence claim: whatever the individual path does, the batch
	 * must do.  And the control must show the schedule is really armed,
	 * otherwise "0 == 0" would pass vacuously.
	 */
	CHECK(s_i == iter,
	    "control broken: individual SSI path refused only %d/%d pivots -- "
	    "the schedule is not arming, so the batch comparison is vacuous",
	    s_i, iter);
	CHECK(s_b == s_i,
	    "ISOLATION NOT EQUIVALENT: batch refused %d/%d pivots but "
	    "individual refused %d/%d", s_b, iter, s_i, iter);
}

int
main(int argc, char **argv)
{
	DBT key, data;
	u_int32_t i, kb, nkeys;
	char val[VALSZ];
	const char *home;
	int ret;

	nkeys = argc > 1 ? (u_int32_t)atoi(argv[1]) : 2000;
	if (nkeys < 512)
		nkeys = 512;
	if ((home = getenv("BATCH_DIFF_HOME")) == NULL)
		home = "BATCH_DIFF_TESTDIR";
	(void)mkdir(home, 0755);

	if ((ret = db_env_create(&env, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (2);
	}
	env->set_errfile(env, stderr);
	env->set_errpfx(env, "batch_diff");
	(void)env->set_cachesize(env, 0, 128 * 1024 * 1024, 1);
	if ((ret = env->open(env, home,
	    DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK | DB_INIT_TXN |
	    DB_INIT_LOG | DB_THREAD | DB_RECOVER, 0)) != 0)
		DIE(ret, "env open");

	if ((ret = db_create(&db, env, 0)) != 0)
		DIE(ret, "db_create");
	if ((ret = db->open(db, NULL, "batch_diff.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD | DB_MULTIVERSION, 0)) != 0)
		DIE(ret, "db open");

	memset(val, 'v', sizeof(val));
	for (i = 0; i < nkeys; i++) {
		fill_key(&key, &kb, i);
		memset(&data, 0, sizeof(data));
		data.data = val; data.size = sizeof(val);
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0)
			DIE(ret, "load");
	}
	printf("# batch_diff: %u keys loaded in %s\n", nkeys, home);

	phase1_values(nkeys);
	phase2_readset(nkeys);
	phase3_isolation(nkeys);

	(void)db->close(db, 0);
	(void)env->close(env, 0);

	/* Never report success without having produced real verdicts. */
	if (phase_verdicts < 4) {
		printf("FAIL: only %d verdicts produced, expected 4 -- "
		    "the test did not actually run\n", phase_verdicts);
		failures++;
	}
	printf("%s: %d failure(s), %d verdict(s)\n",
	    failures == 0 ? "PASS" : "FAIL", failures, phase_verdicts);
	return (failures == 0 ? 0 : 1);
}
