/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 */

/*
 * Encryption IV generation.
 *
 * This file used to hold the MT19937 Mersenne Twister (Matsumoto and
 * Nishimura), and the IVs came from it.  The generator is gone; the file keeps
 * its name so the build lists (dist/srcfiles.in, meson, Windows, Android) do
 * not change.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/crypto.h"
#include "dbinc/hmac.h"

/*
 * __db_generate_iv --
 *	Generate an initialization vector (IV) for one AES-CBC encryption.
 *
 *	The IV comes straight from the operating system's CSPRNG
 *	(__os_csprng).  It used to come from a Mersenne Twister (MT19937), which
 *	is a statistical generator, not a cryptographic one: 624 consecutive
 *	32-bit outputs determine its whole state, and an attacker who reads the
 *	IVs stored in clear in encrypted pages and log records can predict every
 *	later IV.  CBC needs unpredictable IVs, so that is a real weakness, and
 *	seeding the twister from the CSPRNG (2026-09) did not remove it.
 *
 *	A zero 32-bit word is still rejected and redrawn, as before, so the IVs
 *	keep their old shape.  The twister state (env->mt, env->mti,
 *	env->mtx_mt) is left in the ENV for ABI compatibility within the LTS
 *	line, and is no longer used for IVs.
 *
 *	There is deliberately no fallback: if the OS has no entropy source,
 *	encryption fails rather than using a predictable IV.
 *
 * PUBLIC: int __db_generate_iv __P((ENV *, u_int32_t *));
 */
int
__db_generate_iv(env, iv)
	ENV *env;
	u_int32_t *iv;
{
	int i, n, ret;

	n = DB_IV_BYTES / sizeof(u_int32_t);
	for (i = 0; i < n; i++)
		do {
			if ((ret = __os_csprng(env,
			    &iv[i], sizeof(iv[i]))) != 0) {
				__db_err(env, ret, DB_STR("5391",
		    "no cryptographic random source for an encryption IV"));
				return (ret);
			}
		} while (iv[i] == 0);
	return (0);
}
