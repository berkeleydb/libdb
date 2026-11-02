/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * crypto_iv_unpredictable --
 *	Encryption IVs must not be predictable from earlier IVs.
 *
 *	libdb stores the AES-CBC IV in clear next to each encrypted page and log
 *	record.  Until 2026-11 the IVs came from a Mersenne Twister (MT19937).
 *	Its 624-word state can be rebuilt from 624 consecutive outputs by
 *	inverting the output tempering, after which every later output is known.
 *	CBC requires unpredictable IVs, so that let an observer predict the IV of
 *	the next page written.
 *
 *	This test calls __db_generate_iv directly (it links the static library),
 *	collects 624 + 1000 IV words, and applies exactly that attack: untemper
 *	624 words into a state, regenerate the twister forward, and count how
 *	many of the next 1000 words it predicts.  A twister source is predicted
 *	1000/1000.  A CSPRNG source is predicted about 0 (chance is 1000 / 2^32).
 *	The test also checks no IV word is zero, and that 4096 full IVs are
 *	pairwise distinct.
 *
 *	The prediction includes the old generator's "redraw on zero" rule, so a
 *	twister IV source is fully modelled.
 *
 * usage: crypto_iv_unpredictable <home>
 * Exit 0 = PASS.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db_config.h"
#include "db_int.h"

#define	MT_N		624
#define	MT_M		397
#define	PREDICT		1000
#define	NIV		4096

static u_int32_t
untemper(u_int32_t y)
{
	u_int32_t x;
	int i;

	/* y ^= y >> 18 */
	y ^= y >> 18;
	/* y ^= (y << 15) & 0xefc60000 */
	y ^= (y << 15) & 0xefc60000UL;
	/* y ^= (y << 7) & 0x9d2c5680: invert in 7-bit steps */
	x = y;
	for (i = 0; i < 5; i++)
		x = y ^ ((x << 7) & 0x9d2c5680UL);
	y = x;
	/* y ^= y >> 11: invert in 11-bit steps */
	x = y;
	for (i = 0; i < 3; i++)
		x = y ^ (x >> 11);
	return (x);
}

static u_int32_t
temper(u_int32_t y)
{
	y ^= y >> 11;
	y ^= (y << 7) & 0x9d2c5680UL;
	y ^= (y << 15) & 0xefc60000UL;
	y ^= y >> 18;
	return (y);
}

/* Regenerate the 624-word state in place (one MT19937 "twist"). */
static void
twist(u_int32_t *mt)
{
	static const u_int32_t mag01[2] = { 0, 0x9908b0dfUL };
	u_int32_t y;
	int kk;

	for (kk = 0; kk < MT_N - MT_M; kk++) {
		y = (mt[kk] & 0x80000000UL) | (mt[kk + 1] & 0x7fffffffUL);
		mt[kk] = mt[kk + MT_M] ^ (y >> 1) ^ mag01[y & 1];
	}
	for (; kk < MT_N - 1; kk++) {
		y = (mt[kk] & 0x80000000UL) | (mt[kk + 1] & 0x7fffffffUL);
		mt[kk] = mt[kk + (MT_M - MT_N)] ^ (y >> 1) ^ mag01[y & 1];
	}
	y = (mt[MT_N - 1] & 0x80000000UL) | (mt[0] & 0x7fffffffUL);
	mt[MT_N - 1] = mt[MT_M - 1] ^ (y >> 1) ^ mag01[y & 1];
}

/*
 * predict --
 *	Given out[0 .. MT_N + PREDICT - 1], rebuild the state from the first
 *	MT_N words and count how many of the next PREDICT words it predicts,
 *	twisting again every MT_N outputs.
 */
static int
predict(const u_int32_t *out)
{
	u_int32_t st[MT_N];
	int i, idx, hits;

	for (i = 0; i < MT_N; i++)
		st[i] = untemper(out[i]);
	idx = MT_N;
	for (hits = 0, i = 0; i < PREDICT; i++) {
		if (idx >= MT_N) {
			twist(st);
			idx = 0;
		}
		if (temper(st[idx++]) == out[MT_N + i])
			hits++;
	}
	return (hits);
}

/*
 * self_check --
 *	Prove the attack works: generate a real MT19937 stream and predict it.
 *	If this fails, a "0 predicted" result below would mean nothing.
 */
static int
self_check(void)
{
	u_int32_t mt[MT_N], out[MT_N + PREDICT], seed;
	int i, idx;

	seed = 5489;
	mt[0] = seed;
	for (i = 1; i < MT_N; i++)
		mt[i] = 1812433253UL * (mt[i - 1] ^ (mt[i - 1] >> 30)) +
		    (u_int32_t)i;
	idx = MT_N;
	for (i = 0; i < MT_N + PREDICT; i++) {
		if (idx >= MT_N) {
			twist(mt);
			idx = 0;
		}
		out[i] = temper(mt[idx++]);
	}
	return (predict(out));
}

static int
cmp_iv(const void *a, const void *b)
{
	return (memcmp(a, b, DB_IV_BYTES));
}

int
main(int argc, char *argv[])
{
	DB_ENV *dbenv;
	ENV *env;
	u_int32_t *w, *ivs, iv[DB_IV_BYTES / 4];
	int hits, i, j, n, ret, words_per_iv, zero, dup;

	if (argc != 2) {
		fprintf(stderr, "usage: crypto_iv_unpredictable <home>\n");
		return (2);
	}
	if ((hits = self_check()) != PREDICT) {
		printf("VERDICT crypto_iv_unpredictable FAIL the attack itself "
		    "is broken (%d/%d on a real MT19937 stream)\n",
		    hits, PREDICT);
		return (1);
	}

	if ((ret = db_env_create(&dbenv, 0)) != 0 ||
	    (ret = dbenv->open(dbenv, argv[1], DB_CREATE | DB_INIT_MPOOL |
	    DB_PRIVATE, 0)) != 0) {
		fprintf(stderr, "env: %s\n", db_strerror(ret));
		return (2);
	}
	env = dbenv->env;
	words_per_iv = DB_IV_BYTES / 4;

	/* A stream of IV words, in the order they are generated. */
	n = MT_N + PREDICT;
	if ((w = malloc(sizeof(u_int32_t) *
	    (size_t)(n + words_per_iv))) == NULL)
		return (2);
	for (i = 0; i < n; i += words_per_iv) {
		if ((ret = __db_generate_iv(env, iv)) != 0) {
			fprintf(stderr, "__db_generate_iv: %s\n",
			    db_strerror(ret));
			return (2);
		}
		memcpy(&w[i], iv, sizeof(iv));
	}

	/*
	 * Predict.  The old generator skipped zero outputs; untempered zeros
	 * cannot be observed, so the state words line up as long as no zero
	 * was drawn (probability ~ 1624 / 2^32).
	 */
	hits = predict(w);

	/* No zero word; 4096 distinct IVs. */
	if ((ivs = malloc((size_t)NIV * DB_IV_BYTES)) == NULL)
		return (2);
	for (zero = 0, i = 0; i < NIV; i++) {
		if ((ret = __db_generate_iv(env,
		    (u_int32_t *)(ivs + (size_t)i * words_per_iv))) != 0)
			return (2);
		for (j = 0; j < words_per_iv; j++)
			if (ivs[(size_t)i * words_per_iv + j] == 0)
				zero++;
	}
	qsort(ivs, NIV, DB_IV_BYTES, cmp_iv);
	for (dup = 0, i = 1; i < NIV; i++)
		if (memcmp(ivs + (size_t)(i - 1) * words_per_iv,
		    ivs + (size_t)i * words_per_iv, DB_IV_BYTES) == 0)
			dup++;
	(void)dbenv->close(dbenv, 0);

	if (hits > 2 || zero != 0 || dup != 0) {
		printf("VERDICT crypto_iv_unpredictable FAIL %d of %d IV words "
		    "predicted from the previous %d (MT19937 state recovery), "
		    "%d zero words, %d duplicate IVs\n",
		    hits, PREDICT, MT_N, zero, dup);
		return (1);
	}
	printf("VERDICT crypto_iv_unpredictable PASS %d of %d IV words "
	    "predicted by MT19937 state recovery (the attack predicts a real "
	    "MT19937 stream %d/%d); %d IVs distinct, no zero words\n",
	    hits, PREDICT, PREDICT, PREDICT, NIV);
	return (0);
}
