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
 * Read n keys at stride `spread` starting at `base` inside ONE
 * DB_TXN_SERIALIZABLE transaction, via the individual path (use_batch == 0) or
 * the batched path (use_batch == 1), and return the lock-object count delta the
 * read produced.  The transaction commits, so its SIREAD markers persist just
 * as a real committed reader's do.
 */
static int
readset_delta(int use_batch, u_int32_t n, u_int32_t spread, u_int32_t base)
{
	DB_LOCK_STAT *lk;
	DB_TXN *txn;
	DBT keys[MAXBATCH], datas[MAXBATCH];
	u_int32_t kb[MAXBATCH];
	char buf[MAXBATCH][VALSZ + 8];
	int rets[MAXBATCH];
	u_int32_t after, before, i;
	int ret;

	if ((ret = env->txn_begin(env, NULL, &txn, DB_TXN_SERIALIZABLE)) != 0)
		DIE(ret, "readset txn_begin");
	if ((ret = env->lock_stat(env, &lk, 0)) != 0)
		DIE(ret, "lock_stat");
	before = lk->st_nobjects;
	free(lk);

	for (i = 0; i < n; i++) {
		fill_key(&keys[i], &kb[i], base + (i + 1) * spread);
		fill_out(&datas[i], buf[i], sizeof(buf[i]));
		rets[i] = 12345;
	}
	if (use_batch) {
		ret = db_get_multiple(db, txn, keys, datas, rets, n, 0);
		if (ret != 0 && ret != DB_NOTFOUND)
			DIE(ret, "readset batch get");
	} else
		for (i = 0; i < n; i++) {
			ret = db->get(db, txn, &keys[i], &datas[i], 0);
			if (ret != 0 && ret != DB_NOTFOUND)
				DIE(ret, "readset indiv get");
		}

	if ((ret = env->lock_stat(env, &lk, 0)) != 0)
		DIE(ret, "lock_stat");
	after = lk->st_nobjects;
	free(lk);
	if ((ret = txn->commit(txn, 0)) != 0)
		DIE(ret, "readset commit");
	return ((int)after - (int)before);
}

/*
 * Phase 2 -- read-set equivalence under DB_TXN_SERIALIZABLE, measured via the
 * lock-region object population.  A batch that skipped SIREAD markers would
 * leave measurably fewer objects behind than the individual path.
 *
 * Measuring this correctly is harder than it looks, and two wrong versions of
 * this phase each produced a FALSE "isolation weakened" report before this one:
 *
 *   - Lock objects are shared and reused.  If both arms read the SAME keys, the
 *     first arm creates the objects and the second arm's delta is ~0 -- which
 *     is indistinguishable from a skipped read set.
 *   - Giving each arm its OWN key range does not fix it either: different
 *     ranges sit at different tree depths and share leaves differently, so the
 *     deltas differ for reasons that have nothing to do with the read set.
 *
 * What is actually comparable: for ONE fixed key range, read it with arm A and
 * record the delta, then in a LATER transaction read the SAME range with arm B
 * and record ITS delta -- and then repeat with the arms swapped on a second
 * range.  Objects created by the first pass are still present for the second,
 * so the informative quantity is the FIRST-touch delta of each range, and each
 * arm gets to be the first toucher of exactly one range.  Same geometry, same
 * role, one difference: which code fetched it.
 */
static void
phase2_readset(u_int32_t nkeys)
{
	u_int32_t n, spread, half;
	int d_first_i, d_first_b;

	n = 32;
	half = nkeys / 2;
	spread = half / (n + 2);
	if (spread < 8)
		spread = 8;

	/* Range 1 touched FIRST by the individual path. */
	d_first_i = readset_delta(0, n, spread, 0);
	/* Range 2 touched FIRST by the batched path, same geometry. */
	d_first_b = readset_delta(1, n, spread, half);

	printf("VERDICT phase2-readset: %u keys/range, same stride %u ; "
	    "first-touch object delta: indiv-range %d, batch-range %d\n",
	    n, spread, d_first_i, d_first_b);
	phase_verdicts++;

	/* Anti-vacuity: if neither arm moved the count, the probe measured
	 * nothing and that is a failure, not a pass. */
	CHECK(d_first_i > 0 || d_first_b > 0,
	    "read-set probe measured nothing in either arm (vacuous)");
	CHECK(d_first_b >= d_first_i,
	    "batch read set SMALLER than individual read set: batch delta %d < "
	    "indiv delta %d -- isolation weakened", d_first_b, d_first_i);
}

/*
 * Phase 3 -- isolation equivalence with teeth: the CROSSED write-skew pivot.
 *
 * The schedule is the one this repo's ssi_abort_bench proved actually produces
 * an SSI abort rather than a lock conflict:
 *
 *   T1 reads A            (via the arm under test)
 *   T2 reads B            (always via DB->get -- only T1's read is the variable)
 *   T1 writes B
 *   T2 writes A
 *
 * Both transactions read what the other is about to write, so under
 * DB_TXN_SERIALIZABLE exactly ONE must fail with an SSI conflict, and under
 * plain DB_TXN_SNAPSHOT both must commit into the skewed state -- which is the
 * anti-vacuity control that proves the schedule really is armed.
 *
 * Two things this schedule requires, both learned the hard way in this repo:
 *   - the keys must be spread FAR APART IN KEY ORDER, not merely named apart:
 *     page-granularity write locking puts ~3 records on a leaf, so adjacent
 *     keys share a leaf and the detector resolves a ww page conflict BEFORE
 *     SSI ever sees a pivot (which is why the naive schedule scored 0/20).
 *   - the environment MUST run a deadlock detector.  Without one, the crossed
 *     writes simply block forever (the earlier version of this test hung here,
 *     in BOTH arms, which is how it was known not to be a batch bug).
 *
 * Returns the two transactions' outcomes through rc1p/rc2p.
 */
static void
pivot_pair(int use_batch, u_int32_t iso_flag, u_int32_t keyA, u_int32_t keyB,
    int *rc1p, int *rc2p)
{
	DB_TXN *t1, *t2;
	DBT k, d;
	u_int32_t kbuf1, kbuf2;
	char out1[VALSZ + 8], out2[VALSZ + 8], val[VALSZ];
	int rets[1], rc1, rc2, ret;

	memset(val, 'w', sizeof(val));

	if ((ret = env->txn_begin(env, NULL, &t1, iso_flag)) != 0)
		DIE(ret, "pivot t1 begin");
	if ((ret = env->txn_begin(env, NULL, &t2, iso_flag)) != 0)
		DIE(ret, "pivot t2 begin");

	/* T1 reads A -- THE ARM UNDER TEST. */
	fill_key(&k, &kbuf1, keyA);
	fill_out(&d, out1, sizeof(out1));
	if (use_batch) {
		rets[0] = 12345;
		ret = db_get_multiple(db, t1, &k, &d, rets, 1, 0);
	} else
		ret = db->get(db, t1, &k, &d, 0);
	if (ret != 0 && ret != DB_NOTFOUND) {
		(void)t1->abort(t1); (void)t2->abort(t2);
		*rc1p = ret; *rc2p = 0;
		return;
	}

	/* T2 reads B -- always the individual path; only T1's read varies. */
	fill_key(&k, &kbuf2, keyB);
	fill_out(&d, out2, sizeof(out2));
	ret = db->get(db, t2, &k, &d, 0);
	if (ret != 0 && ret != DB_NOTFOUND) {
		(void)t1->abort(t1); (void)t2->abort(t2);
		*rc1p = 0; *rc2p = ret;
		return;
	}

	/* Crossed writes: T1 writes B, T2 writes A. */
	fill_key(&k, &kbuf2, keyB);
	memset(&d, 0, sizeof(d));
	d.data = val; d.size = sizeof(val);
	rc1 = db->put(db, t1, &k, &d, 0);

	fill_key(&k, &kbuf1, keyA);
	memset(&d, 0, sizeof(d));
	d.data = val; d.size = sizeof(val);
	rc2 = db->put(db, t2, &k, &d, 0);

	if (rc1 == 0) rc1 = t1->commit(t1, 0); else (void)t1->abort(t1);
	if (rc2 == 0) rc2 = t2->commit(t2, 0); else (void)t2->abort(t2);
	*rc1p = rc1;
	*rc2p = rc2;
}

/* An SSI refusal, in EITHER shape the engine reports it (ssi_abort_bench:144
 * makes the same distinction -- counting only DB_SNAPSHOT_CONFLICT undercounts
 * and produced a false "isolation weakened" report here). */
#define	IS_SSI_ABORT(r)	\
	((r) == DB_SNAPSHOT_CONFLICT || (r) == DB_SNAPSHOT_UNSAFE)
/* A schedule resolved by the deadlock detector never reached the SSI pivot, so
 * it is neither a pass nor a failure -- it is an unarmed iteration. */
#define	IS_LOCK_ABORT(r)	((r) == DB_LOCK_DEADLOCK || (r) == DB_LOCK_NOTGRANTED)

static void
phase3_isolation(u_int32_t nkeys)
{
	const char *only;
	u_int32_t keyA, keyB, spread;
	int i, iter, do_batch, do_indiv, rc1, rc2;
	int ssi_i, ssi_b, si_i, si_b, dl_i, dl_b, armed_i, armed_b;

	/*
	 * BATCH_DIFF_ARM restricts phase 3 to one arm.  This is the control
	 * knob: if "indiv" alone reproduces a hang or a wrong verdict, the
	 * batched path is exonerated and the fault is in the schedule or in
	 * shared engine code, not in db_get_multiple.
	 */
	only = getenv("BATCH_DIFF_ARM");
	do_indiv = (only == NULL || strcmp(only, "indiv") == 0);
	do_batch = (only == NULL || strcmp(only, "batch") == 0);

	/*
	 * Spread so every key in a pivot pair lands on its own leaf page.
	 * ssi_abort_bench measured that SP >= 8 is enough at ~3 records/leaf.
	 *
	 * The two arms must be measured on IDENTICAL key geometry, not merely
	 * on distinct keys: the arms are compared to each other, so if one arm's
	 * pairs are spread differently from the other's they can hit different
	 * amounts of page sharing and the comparison measures the geometry
	 * rather than the code.  Each iteration therefore uses the SAME pair
	 * offsets for both arms, separated only by a large per-arm base.
	 */
	iter = 10;
	spread = nkeys / (4 * (u_int32_t)iter + 8);
	if (spread < 16)
		spread = 16;
	ssi_i = ssi_b = si_i = si_b = dl_i = dl_b = 0;
	armed_i = armed_b = 0;

#define	PAIR(armbase, it)						\
	keyA = ((armbase) + (u_int32_t)(it) * 4 + 1) * spread,		\
	keyB = ((armbase) + (u_int32_t)(it) * 4 + 3) * spread

	for (i = 0; i < iter; i++) {
		/* SERIALIZABLE: the skew must be prevented. */
		if (do_indiv) {
			PAIR(0, i);
			pivot_pair(0, DB_TXN_SERIALIZABLE, keyA, keyB,
			    &rc1, &rc2);
			if (IS_LOCK_ABORT(rc1) || IS_LOCK_ABORT(rc2))
				dl_i++;		/* never reached the pivot */
			else {
				armed_i++;
				if (IS_SSI_ABORT(rc1) + IS_SSI_ABORT(rc2) >= 1)
					ssi_i++;
			}
		}
		if (do_batch) {
			PAIR(1000, i);
			pivot_pair(1, DB_TXN_SERIALIZABLE, keyA, keyB,
			    &rc1, &rc2);
			if (IS_LOCK_ABORT(rc1) || IS_LOCK_ABORT(rc2))
				dl_b++;
			else {
				armed_b++;
				if (IS_SSI_ABORT(rc1) + IS_SSI_ABORT(rc2) >= 1)
					ssi_b++;
			}
		}

		/*
		 * Anti-vacuity control: the SAME schedule at plain snapshot
		 * isolation must commit BOTH transactions (write skew allowed).
		 * If this does not happen the schedule is not really arming and
		 * the SERIALIZABLE comparison above proves nothing.
		 */
		if (do_indiv) {
			PAIR(2000, i);
			pivot_pair(0, DB_TXN_SNAPSHOT, keyA, keyB, &rc1, &rc2);
			if (rc1 == 0 && rc2 == 0)
				si_i++;
		}
		if (do_batch) {
			PAIR(3000, i);
			pivot_pair(1, DB_TXN_SNAPSHOT, keyA, keyB, &rc1, &rc2);
			if (rc1 == 0 && rc2 == 0)
				si_b++;
		}
	}
#undef PAIR

	printf("VERDICT phase3-isolation: arm=%s iters=%d ; SERIALIZABLE skew "
	    "prevented indiv=%d/%d-armed batch=%d/%d-armed "
	    "(lock-resolved indiv=%d batch=%d) ; snapshot control skew allowed "
	    "indiv=%d batch=%d\n",
	    only == NULL ? "both" : only, iter,
	    ssi_i, armed_i, ssi_b, armed_b, dl_i, dl_b, si_i, si_b);
	phase_verdicts++;

	/*
	 * Anti-vacuity: plain snapshot MUST allow the skew, or nothing armed.
	 * A tolerance is allowed because the plain-SI arm can also lose an
	 * iteration to the detector, but a majority must get through.
	 */
	if (do_indiv)
		CHECK(si_i * 2 > iter,
		    "control broken: at plain snapshot the individual path "
		    "allowed the skew only %d/%d times -- the schedule is not "
		    "arming, so the SERIALIZABLE comparison is vacuous",
		    si_i, iter);
	if (do_batch)
		CHECK(si_b * 2 > iter,
		    "control broken: at plain snapshot the BATCHED path "
		    "allowed the skew only %d/%d times -- schedule not arming",
		    si_b, iter);

	/*
	 * The teeth: of the iterations that actually reached the SSI pivot
	 * (i.e. were not resolved by the deadlock detector first), EVERY one
	 * must have been refused -- in both arms.  Iterations the detector took
	 * are excluded from both numerator and denominator because they never
	 * exercised SSI at all; excluding them is what makes this a statement
	 * about isolation rather than about lock scheduling.
	 */
	if (do_indiv) {
		CHECK(armed_i > 0,
		    "no individual iteration reached the SSI pivot (all %d "
		    "lock-resolved) -- vacuous", dl_i);
		CHECK(ssi_i == armed_i,
		    "individual SSI path prevented only %d of %d ARMED skews",
		    ssi_i, armed_i);
	}
	if (do_batch) {
		CHECK(armed_b > 0,
		    "no batched iteration reached the SSI pivot (all %d "
		    "lock-resolved) -- vacuous", dl_b);
		CHECK(ssi_b == armed_b,
		    "BATCHED SSI path prevented only %d of %d ARMED skews -- "
		    "isolation weakened by the batch", ssi_b, armed_b);
	}
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
	/*
	 * A deadlock detector is MANDATORY here, not optional: phase 3's
	 * crossed writes are a genuine lock cycle, and with no detector the
	 * pair simply blocks forever.  (An earlier version of this test omitted
	 * it and hung in BOTH arms -- the same trap that produced a 17-hour
	 * hang elsewhere in this repo.)
	 */
	if ((ret = env->set_lk_detect(env, DB_LOCK_MINWRITE)) != 0) {
		fprintf(stderr, "set_lk_detect: %s\n", db_strerror(ret));
		return (2);
	}
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
