/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * The SHA-256 core is a public-domain implementation (after Brad Conte's
 * public-domain sha256.c, https://github.com/B-Con/crypto-algorithms).
 * HMAC-SHA256, PBKDF2-HMAC-SHA256 and the constant-time compare are small
 * original implementations.  Used only for the RFC 0019 key verifier, key
 * derivation and key wrapping -- NOT for the page/log MAC, which stays SHA1.
 *
 * Validated against known vectors (SHA-256("abc"), RFC 4231 HMAC case 1,
 * RFC 6070-style PBKDF2, cross-checked with openssl) by rfc/0019/kdf_selfcheck.c
 * and by the engine test test/db/kdf_vectors.c.
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/hmac.h"

#define	SHA256_BLOCK_SIZE	64

#define	ROTR(a, b)	(((a) >> (b)) | ((a) << (32 - (b))))
#define	CH(x, y, z)	(((x) & (y)) ^ (~(x) & (z)))
#define	MAJ(x, y, z)	(((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define	EP0(x)		(ROTR(x, 2) ^ ROTR(x, 13) ^ ROTR(x, 22))
#define	EP1(x)		(ROTR(x, 6) ^ ROTR(x, 11) ^ ROTR(x, 25))
#define	SIG0(x)		(ROTR(x, 7) ^ ROTR(x, 18) ^ ((x) >> 3))
#define	SIG1(x)		(ROTR(x, 17) ^ ROTR(x, 19) ^ ((x) >> 10))

static const u_int32_t sha256_k[64] = {
	0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,0x3956c25b,0x59f111f1,
	0x923f82a4,0xab1c5ed5,0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,
	0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,0xe49b69c1,0xefbe4786,
	0x0fc19dc6,0x240ca1cc,0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
	0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,0xc6e00bf3,0xd5a79147,
	0x06ca6351,0x14292967,0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,
	0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,0xa2bfe8a1,0xa81a664b,
	0xc24b8b70,0xc76c51a3,0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
	0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,0x391c0cb3,0x4ed8aa4a,
	0x5b9cca4f,0x682e6ff3,0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,
	0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2
};

static void
__db_sha256_transform(ctx, data)
	SHA256_CTX *ctx;
	const u_int8_t *data;
{
	u_int32_t a, b, c, d, e, f, g, h, i, t1, t2, m[64];

	for (i = 0; i < 16; ++i)
		m[i] = (u_int32_t)data[i*4] << 24 |
		    (u_int32_t)data[i*4+1] << 16 |
		    (u_int32_t)data[i*4+2] << 8 | (u_int32_t)data[i*4+3];
	for (; i < 64; ++i)
		m[i] = SIG1(m[i-2]) + m[i-7] + SIG0(m[i-15]) + m[i-16];

	a = ctx->state[0]; b = ctx->state[1]; c = ctx->state[2];
	d = ctx->state[3]; e = ctx->state[4]; f = ctx->state[5];
	g = ctx->state[6]; h = ctx->state[7];
	for (i = 0; i < 64; ++i) {
		t1 = h + EP1(e) + CH(e,f,g) + sha256_k[i] + m[i];
		t2 = EP0(a) + MAJ(a,b,c);
		h = g; g = f; f = e; e = d + t1;
		d = c; c = b; b = a; a = t1 + t2;
	}
	ctx->state[0] += a; ctx->state[1] += b; ctx->state[2] += c;
	ctx->state[3] += d; ctx->state[4] += e; ctx->state[5] += f;
	ctx->state[6] += g; ctx->state[7] += h;
}

/*
 * __db_SHA256Init --
 *
 * PUBLIC: void __db_SHA256Init __P((SHA256_CTX *));
 */
void
__db_SHA256Init(ctx)
	SHA256_CTX *ctx;
{
	ctx->datalen = 0;
	ctx->bitlen = 0;
	ctx->state[0] = 0x6a09e667; ctx->state[1] = 0xbb67ae85;
	ctx->state[2] = 0x3c6ef372; ctx->state[3] = 0xa54ff53a;
	ctx->state[4] = 0x510e527f; ctx->state[5] = 0x9b05688c;
	ctx->state[6] = 0x1f83d9ab; ctx->state[7] = 0x5be0cd19;
}

/*
 * __db_SHA256Update --
 *
 * PUBLIC: void __db_SHA256Update __P((SHA256_CTX *, const u_int8_t *, size_t));
 */
void
__db_SHA256Update(ctx, data, len)
	SHA256_CTX *ctx;
	const u_int8_t *data;
	size_t len;
{
	size_t i;

	for (i = 0; i < len; ++i) {
		ctx->buffer[ctx->datalen++] = data[i];
		if (ctx->datalen == SHA256_BLOCK_SIZE) {
			__db_sha256_transform(ctx, ctx->buffer);
			ctx->bitlen += 512;
			ctx->datalen = 0;
		}
	}
}

/*
 * __db_SHA256Final --
 *
 * PUBLIC: void __db_SHA256Final __P((u_int8_t *, SHA256_CTX *));
 */
void
__db_SHA256Final(hash, ctx)
	u_int8_t *hash;
	SHA256_CTX *ctx;
{
	u_int32_t i;

	i = ctx->datalen;
	ctx->buffer[i++] = 0x80;
	if (ctx->datalen < 56) {
		while (i < 56)
			ctx->buffer[i++] = 0x00;
	} else {
		while (i < 64)
			ctx->buffer[i++] = 0x00;
		__db_sha256_transform(ctx, ctx->buffer);
		memset(ctx->buffer, 0, 56);
	}
	ctx->bitlen += (u_int64_t)ctx->datalen * 8;
	for (i = 0; i < 8; i++)
		ctx->buffer[63-i] = (u_int8_t)(ctx->bitlen >> (i*8));
	__db_sha256_transform(ctx, ctx->buffer);
	for (i = 0; i < 8; ++i) {
		hash[i*4]   = (u_int8_t)(ctx->state[i] >> 24);
		hash[i*4+1] = (u_int8_t)(ctx->state[i] >> 16);
		hash[i*4+2] = (u_int8_t)(ctx->state[i] >> 8);
		hash[i*4+3] = (u_int8_t)(ctx->state[i]);
	}
}

/*
 * __db_hmac_sha256 --
 *	HMAC-SHA256 (RFC 2104/4231).
 *
 * PUBLIC: void __db_hmac_sha256 __P((const u_int8_t *, size_t,
 * PUBLIC:     const u_int8_t *, size_t, u_int8_t *));
 */
void
__db_hmac_sha256(key, keylen, msg, msglen, out)
	const u_int8_t *key;
	size_t keylen;
	const u_int8_t *msg;
	size_t msglen;
	u_int8_t *out;
{
	SHA256_CTX ctx;
	u_int8_t k[SHA256_BLOCK_SIZE], ki[SHA256_BLOCK_SIZE];
	u_int8_t ko[SHA256_BLOCK_SIZE], ihash[DB_SHA256_DIGEST];
	size_t i;

	memset(k, 0, sizeof(k));
	if (keylen > SHA256_BLOCK_SIZE) {
		__db_SHA256Init(&ctx);
		__db_SHA256Update(&ctx, key, keylen);
		__db_SHA256Final(k, &ctx);
	} else
		memcpy(k, key, keylen);

	for (i = 0; i < SHA256_BLOCK_SIZE; i++) {
		ki[i] = k[i] ^ 0x36;
		ko[i] = k[i] ^ 0x5c;
	}
	__db_SHA256Init(&ctx);
	__db_SHA256Update(&ctx, ki, SHA256_BLOCK_SIZE);
	__db_SHA256Update(&ctx, msg, msglen);
	__db_SHA256Final(ihash, &ctx);
	__db_SHA256Init(&ctx);
	__db_SHA256Update(&ctx, ko, SHA256_BLOCK_SIZE);
	__db_SHA256Update(&ctx, ihash, DB_SHA256_DIGEST);
	__db_SHA256Final(out, &ctx);

	/* Scrub the expanded key material from the stack. */
	__db_explicit_bzero(k, sizeof(k));
	__db_explicit_bzero(ki, sizeof(ki));
	__db_explicit_bzero(ko, sizeof(ko));
}

/*
 * __db_pbkdf2_sha256 --
 *	PBKDF2 with HMAC-SHA256 as the PRF (RFC 8018).  saltlen must leave
 *	room for a 4-byte block counter in a 256-byte scratch; our salts are
 *	DB_CRYPTO_SALT_LEN (16) so this always holds.
 *
 * PUBLIC: int __db_pbkdf2_sha256 __P((const u_int8_t *, size_t,
 * PUBLIC:     const u_int8_t *, size_t, u_int32_t, u_int8_t *, size_t));
 */
int
__db_pbkdf2_sha256(pw, pwlen, salt, saltlen, iters, out, outlen)
	const u_int8_t *pw;
	size_t pwlen;
	const u_int8_t *salt;
	size_t saltlen;
	u_int32_t iters;
	u_int8_t *out;
	size_t outlen;
{
	u_int8_t U[DB_SHA256_DIGEST], T[DB_SHA256_DIGEST];
	u_int8_t saltblk[256];
	u_int32_t blk, i, j;
	size_t off, n;

	if (saltlen + 4 > sizeof(saltblk) || iters == 0)
		return (EINVAL);

	for (off = 0, blk = 1; off < outlen; blk++) {
		memcpy(saltblk, salt, saltlen);
		saltblk[saltlen+0] = (u_int8_t)(blk >> 24);
		saltblk[saltlen+1] = (u_int8_t)(blk >> 16);
		saltblk[saltlen+2] = (u_int8_t)(blk >> 8);
		saltblk[saltlen+3] = (u_int8_t)(blk);
		__db_hmac_sha256(pw, pwlen, saltblk, saltlen+4, U);
		memcpy(T, U, DB_SHA256_DIGEST);
		for (i = 1; i < iters; i++) {
			__db_hmac_sha256(pw, pwlen, U, DB_SHA256_DIGEST, U);
			for (j = 0; j < DB_SHA256_DIGEST; j++)
				T[j] ^= U[j];
		}
		n = outlen - off;
		if (n > DB_SHA256_DIGEST)
			n = DB_SHA256_DIGEST;
		memcpy(out + off, T, n);
		off += n;
	}
	__db_explicit_bzero(U, sizeof(U));
	__db_explicit_bzero(T, sizeof(T));
	return (0);
}

/*
 * __db_ct_memeq --
 *	Constant-time equality.  Returns 1 if the two buffers are equal,
 *	0 otherwise.  The running time depends only on n, not on where (or
 *	whether) the buffers differ.
 *
 * PUBLIC: int __db_ct_memeq __P((const void *, const void *, size_t));
 */
int
__db_ct_memeq(a, b, n)
	const void *a, *b;
	size_t n;
{
	const volatile u_int8_t *pa, *pb;
	u_int8_t diff;
	size_t i;

	pa = a;
	pb = b;
	diff = 0;
	for (i = 0; i < n; i++)
		diff |= (u_int8_t)(pa[i] ^ pb[i]);
	return (diff == 0);
}

/*
 * __db_explicit_bzero --
 *	Zero a buffer in a way the compiler cannot elide.  Uses the platform
 *	explicit_bzero / memset_s where available; falls back to a volatile
 *	pointer memset otherwise.
 *
 * PUBLIC: void __db_explicit_bzero __P((void *, size_t));
 */
void
__db_explicit_bzero(b, n)
	void *b;
	size_t n;
{
#if defined(HAVE_EXPLICIT_BZERO)
	explicit_bzero(b, n);
#elif defined(HAVE_MEMSET_S)
	(void)memset_s(b, n, 0, n);
#else
	volatile u_int8_t *p = b;
	while (n-- > 0)
		*p++ = 0;
#endif
}
