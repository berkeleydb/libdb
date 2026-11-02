/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * RFC 3394 AES Key Wrap over the engine's public-domain Rijndael core, plus
 * the RFC 0019 region verifier helpers.  The data-encryption key (DEK) is
 * wrapped by the provider's key-encryption key (KEK) so that a key rotation
 * re-wraps the DEK rather than re-encrypting every page.
 *
 * Validated against RFC 3394 section 4.6 vectors (cross-checked with openssl
 * id-aes256-wrap) by rfc/0019/kdf_selfcheck.c and test/db/kdf_vectors.c.
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/crypto.h"
#include "dbinc/hmac.h"

#ifdef HAVE_CRYPTO
#include "crypto/rijndael/rijndael-alg-fst.h"

static const u_int8_t kw_iv[8] =
    { 0xA6,0xA6,0xA6,0xA6,0xA6,0xA6,0xA6,0xA6 };

/*
 * __db_aes_keywrap --
 *	RFC 3394 AES key wrap.  kek is a 256-bit key-encryption key; dek is
 *	the plaintext key, a multiple of 8 bytes and at least 16; out is
 *	deklen+8 bytes.
 *
 * PUBLIC: int __db_aes_keywrap __P((ENV *, const u_int8_t *,
 * PUBLIC:     const u_int8_t *, size_t, u_int8_t *));
 */
int
__db_aes_keywrap(env, kek, dek, deklen, out)
	ENV *env;
	const u_int8_t *kek, *dek;
	size_t deklen;
	u_int8_t *out;
{
	u32 rk[4*(MAXNR+1)];
	u_int8_t A[8], B[16], R[DB_CRYPTO_DEK_LEN];
	size_t n, i, j, t;
	int Nr;

	if (deklen < 16 || deklen % 8 != 0 || deklen > sizeof(R))
		return (EINVAL);
	COMPQUIET(env, NULL);

	Nr = __db_rijndaelKeySetupEnc(rk, kek, 256);
	n = deklen / 8;
	memcpy(A, kw_iv, 8);
	memcpy(R, dek, deklen);
	for (j = 0; j < 6; j++)
		for (i = 1; i <= n; i++) {
			memcpy(B, A, 8);
			memcpy(B + 8, R + (i-1)*8, 8);
			__db_rijndaelEncrypt(rk, Nr, B, B);
			memcpy(A, B, 8);
			t = n*j + i;
			A[7] ^= (u_int8_t)(t & 0xff);
			A[6] ^= (u_int8_t)((t >> 8) & 0xff);
			A[5] ^= (u_int8_t)((t >> 16) & 0xff);
			memcpy(R + (i-1)*8, B + 8, 8);
		}
	memcpy(out, A, 8);
	memcpy(out + 8, R, deklen);
	__db_explicit_bzero(rk, sizeof(rk));
	__db_explicit_bzero(R, sizeof(R));
	return (0);
}

/*
 * __db_aes_keyunwrap --
 *	RFC 3394 AES key unwrap.  Returns 0 and fills out (inlen-8 bytes) on
 *	success, EPERM if the integrity check fails (wrong KEK or tampered
 *	wrapped key).
 *
 * PUBLIC: int __db_aes_keyunwrap __P((ENV *, const u_int8_t *,
 * PUBLIC:     const u_int8_t *, size_t, u_int8_t *));
 */
int
__db_aes_keyunwrap(env, kek, in, inlen, out)
	ENV *env;
	const u_int8_t *kek, *in;
	size_t inlen;
	u_int8_t *out;
{
	u32 rk[4*(MAXNR+1)];
	u_int8_t A[8], B[16], R[DB_CRYPTO_DEK_LEN];
	size_t n, i, j, t;
	int Nr, ok;

	if (inlen < 24 || inlen % 8 != 0 || inlen - 8 > sizeof(R))
		return (EINVAL);
	COMPQUIET(env, NULL);

	Nr = __db_rijndaelKeySetupDec(rk, kek, 256);
	n = inlen / 8 - 1;
	memcpy(A, in, 8);
	memcpy(R, in + 8, inlen - 8);
	for (j = 6; j-- > 0; )
		for (i = n; i >= 1; i--) {
			t = n*j + i;
			A[7] ^= (u_int8_t)(t & 0xff);
			A[6] ^= (u_int8_t)((t >> 8) & 0xff);
			A[5] ^= (u_int8_t)((t >> 16) & 0xff);
			memcpy(B, A, 8);
			memcpy(B + 8, R + (i-1)*8, 8);
			__db_rijndaelDecrypt(rk, Nr, B, B);
			memcpy(A, B, 8);
			memcpy(R + (i-1)*8, B + 8, 8);
		}
	ok = __db_ct_memeq(A, kw_iv, 8);
	if (ok)
		memcpy(out, R, inlen - 8);
	__db_explicit_bzero(rk, sizeof(rk));
	__db_explicit_bzero(R, sizeof(R));
	return (ok ? 0 : EPERM);
}

/*
 * __db_crypto_make_verifier --
 *	Derive the salted verifier for a secret: PBKDF2-HMAC-SHA256 of the
 *	secret over the salt at the given iteration count.
 *
 * PUBLIC: int __db_crypto_make_verifier __P((ENV *, const u_int8_t *, size_t,
 * PUBLIC:     const u_int8_t *, u_int32_t, u_int8_t *));
 */
int
__db_crypto_make_verifier(env, secret, slen, salt, iters, verifier)
	ENV *env;
	const u_int8_t *secret;
	size_t slen;
	const u_int8_t *salt;
	u_int32_t iters;
	u_int8_t *verifier;
{
	COMPQUIET(env, NULL);
	return (__db_pbkdf2_sha256(secret, slen, salt, DB_CRYPTO_SALT_LEN,
	    iters, verifier, DB_CRYPTO_VRFY_LEN));
}
#endif /* HAVE_CRYPTO */
