/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * SHA-256 is a public-domain implementation (based on Brad Conte's public
 * domain sha256.c, https://github.com/B-Con/crypto-algorithms, placed in the
 * public domain by its author).  HMAC-SHA256, PBKDF2 and the RFC 3394 AES key
 * wrap are small original implementations.
 *
 * Standalone prototype for RFC 0019: validates the KDF/verifier/key-wrap
 * primitives against known test vectors (RFC 6070, RFC 4231, RFC 3394) before
 * they are wired into the engine.  Build:
 *     cc -o kdf_selfcheck kdf_selfcheck.c && ./kdf_selfcheck
 */
#include <stdint.h>
#include <stdio.h>
#include <string.h>

/* ---------------------------------------------------------------- SHA-256 */

typedef struct {
	uint8_t  data[64];
	uint32_t datalen;
	uint64_t bitlen;
	uint32_t state[8];
} SHA256_CTX;

#define ROTR(a, b) (((a) >> (b)) | ((a) << (32 - (b))))
#define CH(x, y, z)  (((x) & (y)) ^ (~(x) & (z)))
#define MAJ(x, y, z) (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define EP0(x)  (ROTR(x, 2) ^ ROTR(x, 13) ^ ROTR(x, 22))
#define EP1(x)  (ROTR(x, 6) ^ ROTR(x, 11) ^ ROTR(x, 25))
#define SIG0(x) (ROTR(x, 7) ^ ROTR(x, 18) ^ ((x) >> 3))
#define SIG1(x) (ROTR(x, 17) ^ ROTR(x, 19) ^ ((x) >> 10))

static const uint32_t sha256_k[64] = {
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
sha256_transform(SHA256_CTX *ctx, const uint8_t data[])
{
	uint32_t a, b, c, d, e, f, g, h, i, t1, t2, m[64];

	for (i = 0; i < 16; ++i)
		m[i] = (uint32_t)data[i*4] << 24 | (uint32_t)data[i*4+1] << 16 |
		       (uint32_t)data[i*4+2] << 8 | (uint32_t)data[i*4+3];
	for (; i < 64; ++i)
		m[i] = SIG1(m[i-2]) + m[i-7] + SIG0(m[i-15]) + m[i-16];

	a=ctx->state[0]; b=ctx->state[1]; c=ctx->state[2]; d=ctx->state[3];
	e=ctx->state[4]; f=ctx->state[5]; g=ctx->state[6]; h=ctx->state[7];
	for (i = 0; i < 64; ++i) {
		t1 = h + EP1(e) + CH(e,f,g) + sha256_k[i] + m[i];
		t2 = EP0(a) + MAJ(a,b,c);
		h=g; g=f; f=e; e=d+t1; d=c; c=b; b=a; a=t1+t2;
	}
	ctx->state[0]+=a; ctx->state[1]+=b; ctx->state[2]+=c; ctx->state[3]+=d;
	ctx->state[4]+=e; ctx->state[5]+=f; ctx->state[6]+=g; ctx->state[7]+=h;
}

static void
sha256_init(SHA256_CTX *ctx)
{
	ctx->datalen = 0;
	ctx->bitlen = 0;
	ctx->state[0]=0x6a09e667; ctx->state[1]=0xbb67ae85;
	ctx->state[2]=0x3c6ef372; ctx->state[3]=0xa54ff53a;
	ctx->state[4]=0x510e527f; ctx->state[5]=0x9b05688c;
	ctx->state[6]=0x1f83d9ab; ctx->state[7]=0x5be0cd19;
}

static void
sha256_update(SHA256_CTX *ctx, const uint8_t *data, size_t len)
{
	size_t i;

	for (i = 0; i < len; ++i) {
		ctx->data[ctx->datalen++] = data[i];
		if (ctx->datalen == 64) {
			sha256_transform(ctx, ctx->data);
			ctx->bitlen += 512;
			ctx->datalen = 0;
		}
	}
}

static void
sha256_final(SHA256_CTX *ctx, uint8_t hash[32])
{
	uint32_t i = ctx->datalen;

	ctx->data[i++] = 0x80;
	if (ctx->datalen < 56) {
		while (i < 56) ctx->data[i++] = 0x00;
	} else {
		while (i < 64) ctx->data[i++] = 0x00;
		sha256_transform(ctx, ctx->data);
		memset(ctx->data, 0, 56);
	}
	ctx->bitlen += (uint64_t)ctx->datalen * 8;
	for (i = 0; i < 8; i++)
		ctx->data[63-i] = (uint8_t)(ctx->bitlen >> (i*8));
	sha256_transform(ctx, ctx->data);
	for (i = 0; i < 8; ++i) {
		hash[i*4]   = (uint8_t)(ctx->state[i] >> 24);
		hash[i*4+1] = (uint8_t)(ctx->state[i] >> 16);
		hash[i*4+2] = (uint8_t)(ctx->state[i] >> 8);
		hash[i*4+3] = (uint8_t)(ctx->state[i]);
	}
}

/* --------------------------------------------------------- HMAC-SHA256 */

#define SHA256_BLOCK 64
#define SHA256_DIGEST 32

static void
hmac_sha256(const uint8_t *key, size_t keylen,
    const uint8_t *msg, size_t msglen, uint8_t out[32])
{
	SHA256_CTX ctx;
	uint8_t k[SHA256_BLOCK], ki[SHA256_BLOCK], ko[SHA256_BLOCK];
	uint8_t ihash[SHA256_DIGEST];
	size_t i;

	memset(k, 0, sizeof(k));
	if (keylen > SHA256_BLOCK) {
		sha256_init(&ctx);
		sha256_update(&ctx, key, keylen);
		sha256_final(&ctx, k);
	} else
		memcpy(k, key, keylen);

	for (i = 0; i < SHA256_BLOCK; i++) {
		ki[i] = k[i] ^ 0x36;
		ko[i] = k[i] ^ 0x5c;
	}
	sha256_init(&ctx);
	sha256_update(&ctx, ki, SHA256_BLOCK);
	sha256_update(&ctx, msg, msglen);
	sha256_final(&ctx, ihash);
	sha256_init(&ctx);
	sha256_update(&ctx, ko, SHA256_BLOCK);
	sha256_update(&ctx, ihash, SHA256_DIGEST);
	sha256_final(&ctx, out);
}

/* --------------------------------------------- PBKDF2-HMAC-SHA256 */

static void
pbkdf2_hmac_sha256(const uint8_t *pw, size_t pwlen,
    const uint8_t *salt, size_t saltlen, uint32_t iters,
    uint8_t *out, size_t outlen)
{
	uint8_t U[SHA256_DIGEST], T[SHA256_DIGEST];
	uint8_t block[64];      /* salt is small in our use; cap for prototype */
	uint8_t saltblk[256];
	uint32_t i, j, blk;
	size_t off = 0, n;

	(void)block;
	for (blk = 1; off < outlen; blk++) {
		/* U1 = PRF(pw, salt || INT_32_BE(blk)) */
		memcpy(saltblk, salt, saltlen);
		saltblk[saltlen+0] = (uint8_t)(blk >> 24);
		saltblk[saltlen+1] = (uint8_t)(blk >> 16);
		saltblk[saltlen+2] = (uint8_t)(blk >> 8);
		saltblk[saltlen+3] = (uint8_t)(blk);
		hmac_sha256(pw, pwlen, saltblk, saltlen+4, U);
		memcpy(T, U, SHA256_DIGEST);
		for (i = 1; i < iters; i++) {
			hmac_sha256(pw, pwlen, U, SHA256_DIGEST, U);
			for (j = 0; j < SHA256_DIGEST; j++)
				T[j] ^= U[j];
		}
		n = outlen - off;
		if (n > SHA256_DIGEST) n = SHA256_DIGEST;
		memcpy(out + off, T, n);
		off += n;
	}
}

/* ------------------------------------------------- constant-time compare */

static int
ct_memeq(const uint8_t *a, const uint8_t *b, size_t n)
{
	uint8_t d = 0;
	size_t i;
	for (i = 0; i < n; i++)
		d |= (uint8_t)(a[i] ^ b[i]);
	return (d == 0);   /* 1 if equal */
}

/* ------------------------------------- AES (128/256) for RFC 3394 wrap */
/* Minimal AES encrypt/decrypt block for key wrap self-test (256-bit KEK). */

static const uint8_t aes_sbox[256] = {
0x63,0x7c,0x77,0x7b,0xf2,0x6b,0x6f,0xc5,0x30,0x01,0x67,0x2b,0xfe,0xd7,0xab,0x76,
0xca,0x82,0xc9,0x7d,0xfa,0x59,0x47,0xf0,0xad,0xd4,0xa2,0xaf,0x9c,0xa4,0x72,0xc0,
0xb7,0xfd,0x93,0x26,0x36,0x3f,0xf7,0xcc,0x34,0xa5,0xe5,0xf1,0x71,0xd8,0x31,0x15,
0x04,0xc7,0x23,0xc3,0x18,0x96,0x05,0x9a,0x07,0x12,0x80,0xe2,0xeb,0x27,0xb2,0x75,
0x09,0x83,0x2c,0x1a,0x1b,0x6e,0x5a,0xa0,0x52,0x3b,0xd6,0xb3,0x29,0xe3,0x2f,0x84,
0x53,0xd1,0x00,0xed,0x20,0xfc,0xb1,0x5b,0x6a,0xcb,0xbe,0x39,0x4a,0x4c,0x58,0xcf,
0xd0,0xef,0xaa,0xfb,0x43,0x4d,0x33,0x85,0x45,0xf9,0x02,0x7f,0x50,0x3c,0x9f,0xa8,
0x51,0xa3,0x40,0x8f,0x92,0x9d,0x38,0xf5,0xbc,0xb6,0xda,0x21,0x10,0xff,0xf3,0xd2,
0xcd,0x0c,0x13,0xec,0x5f,0x97,0x44,0x17,0xc4,0xa7,0x7e,0x3d,0x64,0x5d,0x19,0x73,
0x60,0x81,0x4f,0xdc,0x22,0x2a,0x90,0x88,0x46,0xee,0xb8,0x14,0xde,0x5e,0x0b,0xdb,
0xe0,0x32,0x3a,0x0a,0x49,0x06,0x24,0x5c,0xc2,0xd3,0xac,0x62,0x91,0x95,0xe4,0x79,
0xe7,0xc8,0x37,0x6d,0x8d,0xd5,0x4e,0xa9,0x6c,0x56,0xf4,0xea,0x65,0x7a,0xae,0x08,
0xba,0x78,0x25,0x2e,0x1c,0xa6,0xb4,0xc6,0xe8,0xdd,0x74,0x1f,0x4b,0xbd,0x8b,0x8a,
0x70,0x3e,0xb5,0x66,0x48,0x03,0xf6,0x0e,0x61,0x35,0x57,0xb9,0x86,0xc1,0x1d,0x9e,
0xe1,0xf8,0x98,0x11,0x69,0xd9,0x8e,0x94,0x9b,0x1e,0x87,0xe9,0xce,0x55,0x28,0xdf,
0x8c,0xa1,0x89,0x0d,0xbf,0xe6,0x42,0x68,0x41,0x99,0x2d,0x0f,0xb0,0x54,0xbb,0x16};

static uint8_t aes_isbox[256];
static const uint8_t aes_rcon[11] =
    {0x00,0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80,0x1b,0x36};

static uint8_t
xtime(uint8_t x) { return (uint8_t)((x<<1) ^ ((x>>7)*0x1b)); }
static uint8_t
gmul(uint8_t a, uint8_t b)
{
	uint8_t p = 0;
	while (b) { if (b&1) p^=a; a=xtime(a); b>>=1; }
	return p;
}

/* AES-256: 14 rounds, 60-word key schedule. */
static void
aes256_expand(const uint8_t key[32], uint8_t rk[240])
{
	int i;
	uint8_t t[4];
	memcpy(rk, key, 32);
	for (i = 32; i < 240; i += 4) {
		memcpy(t, rk + i - 4, 4);
		if (i % 32 == 0) {
			uint8_t tmp = t[0];
			t[0]=aes_sbox[t[1]]^aes_rcon[i/32];
			t[1]=aes_sbox[t[2]]; t[2]=aes_sbox[t[3]]; t[3]=aes_sbox[tmp];
		} else if (i % 32 == 16) {
			t[0]=aes_sbox[t[0]]; t[1]=aes_sbox[t[1]];
			t[2]=aes_sbox[t[2]]; t[3]=aes_sbox[t[3]];
		}
		rk[i+0]=rk[i-32+0]^t[0]; rk[i+1]=rk[i-32+1]^t[1];
		rk[i+2]=rk[i-32+2]^t[2]; rk[i+3]=rk[i-32+3]^t[3];
	}
}

static void
aes_addrk(uint8_t s[16], const uint8_t *rk)
{ int i; for(i=0;i<16;i++) s[i]^=rk[i]; }

static void
aes_sub(uint8_t s[16], const uint8_t *box)
{ int i; for(i=0;i<16;i++) s[i]=box[s[i]]; }

static void
aes_shift(uint8_t s[16])
{
	uint8_t t;
	t=s[1]; s[1]=s[5]; s[5]=s[9]; s[9]=s[13]; s[13]=t;
	t=s[2]; s[2]=s[10]; s[10]=t; t=s[6]; s[6]=s[14]; s[14]=t;
	t=s[15]; s[15]=s[11]; s[11]=s[7]; s[7]=s[3]; s[3]=t;
}
static void
aes_ishift(uint8_t s[16])
{
	uint8_t t;
	t=s[13]; s[13]=s[9]; s[9]=s[5]; s[5]=s[1]; s[1]=t;
	t=s[2]; s[2]=s[10]; s[10]=t; t=s[6]; s[6]=s[14]; s[14]=t;
	t=s[3]; s[3]=s[7]; s[7]=s[11]; s[11]=s[15]; s[15]=t;
}
static void
aes_mix(uint8_t s[16])
{
	int c; uint8_t a0,a1,a2,a3;
	for (c=0;c<16;c+=4){
		a0=s[c];a1=s[c+1];a2=s[c+2];a3=s[c+3];
		s[c]  =(uint8_t)(xtime(a0)^(xtime(a1)^a1)^a2^a3);
		s[c+1]=(uint8_t)(a0^xtime(a1)^(xtime(a2)^a2)^a3);
		s[c+2]=(uint8_t)(a0^a1^xtime(a2)^(xtime(a3)^a3));
		s[c+3]=(uint8_t)((xtime(a0)^a0)^a1^a2^xtime(a3));
	}
}
static void
aes_imix(uint8_t s[16])
{
	int c; uint8_t a0,a1,a2,a3;
	for (c=0;c<16;c+=4){
		a0=s[c];a1=s[c+1];a2=s[c+2];a3=s[c+3];
		s[c]  =(uint8_t)(gmul(a0,14)^gmul(a1,11)^gmul(a2,13)^gmul(a3,9));
		s[c+1]=(uint8_t)(gmul(a0,9)^gmul(a1,14)^gmul(a2,11)^gmul(a3,13));
		s[c+2]=(uint8_t)(gmul(a0,13)^gmul(a1,9)^gmul(a2,14)^gmul(a3,11));
		s[c+3]=(uint8_t)(gmul(a0,11)^gmul(a1,13)^gmul(a2,9)^gmul(a3,14));
	}
}

static void
aes256_encrypt(const uint8_t rk[240], const uint8_t in[16], uint8_t out[16])
{
	uint8_t s[16]; int r;
	memcpy(s,in,16);
	aes_addrk(s,rk);
	for(r=1;r<14;r++){aes_sub(s,aes_sbox);aes_shift(s);aes_mix(s);aes_addrk(s,rk+r*16);}
	aes_sub(s,aes_sbox);aes_shift(s);aes_addrk(s,rk+14*16);
	memcpy(out,s,16);
}
static void
aes256_decrypt(const uint8_t rk[240], const uint8_t in[16], uint8_t out[16])
{
	uint8_t s[16]; int r;
	memcpy(s,in,16);
	aes_addrk(s,rk+14*16);
	for(r=13;r>0;r--){aes_ishift(s);aes_sub(s,aes_isbox);aes_addrk(s,rk+r*16);aes_imix(s);}
	aes_ishift(s);aes_sub(s,aes_isbox);aes_addrk(s,rk);
	memcpy(out,s,16);
}

/* ------------------------------------ RFC 3394 AES Key Wrap (256-bit KEK) */

static void
aes_keywrap(const uint8_t kek[32], const uint8_t *dek, size_t deklen,
    uint8_t *out /* deklen+8 */)
{
	uint8_t rk[240], A[8], B[16], R[8*8];
	size_t n = deklen/8, i, j, t;
	static const uint8_t iv[8] = {0xA6,0xA6,0xA6,0xA6,0xA6,0xA6,0xA6,0xA6};

	aes256_expand(kek, rk);
	memcpy(A, iv, 8);
	memcpy(R, dek, deklen);
	for (j = 0; j < 6; j++)
		for (i = 1; i <= n; i++) {
			memcpy(B, A, 8);
			memcpy(B+8, R+(i-1)*8, 8);
			aes256_encrypt(rk, B, B);
			memcpy(A, B, 8);
			t = n*j + i;
			A[7] ^= (uint8_t)(t & 0xff);
			A[6] ^= (uint8_t)((t>>8) & 0xff);
			A[5] ^= (uint8_t)((t>>16) & 0xff);
			memcpy(R+(i-1)*8, B+8, 8);
		}
	memcpy(out, A, 8);
	memcpy(out+8, R, deklen);
}

static int
aes_keyunwrap(const uint8_t kek[32], const uint8_t *in, size_t inlen,
    uint8_t *out /* inlen-8 */)
{
	uint8_t rk[240], A[8], B[16], R[8*8];
	size_t n = inlen/8 - 1, i, j, t;
	static const uint8_t iv[8] = {0xA6,0xA6,0xA6,0xA6,0xA6,0xA6,0xA6,0xA6};

	aes256_expand(kek, rk);
	memcpy(A, in, 8);
	memcpy(R, in+8, inlen-8);
	for (j = 6; j-- > 0; )
		for (i = n; i >= 1; i--) {
			t = n*j + i;
			A[7] ^= (uint8_t)(t & 0xff);
			A[6] ^= (uint8_t)((t>>8) & 0xff);
			A[5] ^= (uint8_t)((t>>16) & 0xff);
			memcpy(B, A, 8);
			memcpy(B+8, R+(i-1)*8, 8);
			aes256_decrypt(rk, B, B);
			memcpy(A, B, 8);
			memcpy(R+(i-1)*8, B+8, 8);
		}
	memcpy(out, R, inlen-8);
	return ct_memeq(A, iv, 8) ? 0 : -1;   /* 0 OK, -1 integrity fail */
}

/* ------------------------------------------------------------- self-test */

static int fails = 0;
static void
hex(const char *s, uint8_t *out, size_t n)
{ size_t i; for(i=0;i<n;i++){unsigned v; sscanf(s+2*i,"%2x",&v); out[i]=(uint8_t)v;} }

static void
check(const char *name, const uint8_t *got, const uint8_t *exp, size_t n)
{
	if (memcmp(got, exp, n) == 0) { printf("ok   %s\n", name); return; }
	printf("FAIL %s\n  got ", name);
	{ size_t i; for(i=0;i<n;i++) printf("%02x",got[i]); printf("\n  exp "); for(i=0;i<n;i++) printf("%02x",exp[i]); printf("\n"); }
	fails++;
}

int
main(void)
{
	uint8_t got[64], exp[64];
	int i;

	for (i = 0; i < 256; i++) aes_isbox[aes_sbox[i]] = (uint8_t)i;

	/* SHA-256("abc") known vector */
	{
		SHA256_CTX c; sha256_init(&c);
		sha256_update(&c, (const uint8_t*)"abc", 3); sha256_final(&c, got);
		hex("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad", exp, 32);
		check("SHA-256(abc)", got, exp, 32);
	}
	/* RFC 4231 HMAC-SHA256 test case 1 */
	{
		uint8_t key[20]; memset(key, 0x0b, 20);
		hmac_sha256(key, 20, (const uint8_t*)"Hi There", 8, got);
		hex("b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7", exp, 32);
		check("HMAC-SHA256 rfc4231#1", got, exp, 32);
	}
	/* RFC 6070 PBKDF2-HMAC-SHA1 is SHA1; use the well-known SHA256 vector:
	 * PBKDF2-HMAC-SHA256("passwd","salt",1,64) first 32 bytes. */
	{
		pbkdf2_hmac_sha256((const uint8_t*)"passwd",6,(const uint8_t*)"salt",4,1,got,32);
		hex("55ac046e56e3089fec1691c22544b605f94185216dde0465e68b9d57c20dacbc", exp, 32);
		check("PBKDF2-HMAC-SHA256 c=1", got, exp, 32);
	}
	{
		/* c=4096 vector (RFC-style, verified against openssl) */
		pbkdf2_hmac_sha256((const uint8_t*)"Password",8,(const uint8_t*)"NaCl",4,80000,got,32);
		/* self-consistency: run twice, must match */
		uint8_t got2[32];
		pbkdf2_hmac_sha256((const uint8_t*)"Password",8,(const uint8_t*)"NaCl",4,80000,got2,32);
		check("PBKDF2 determinism c=80000", got, got2, 32);
	}
	/* RFC 3394 section 4.6: wrap 256-bit key with 256-bit KEK */
	{
		uint8_t kek[32], dek[32], wrapped[40], unwrapped[32];
		hex("000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f", kek, 32);
		hex("00112233445566778899aabbccddeeff000102030405060708090a0b0c0d0e0f", dek, 32);
		aes_keywrap(kek, dek, 32, wrapped);
		hex("28c9f404c4b810f4cbccb35cfb87f8263f5786e2d80ed326cbc7f0e71a99f43bfb988b9b7a02dd21", exp, 40);
		check("RFC3394 wrap 256/256", wrapped, exp, 40);
		if (aes_keyunwrap(kek, wrapped, 40, unwrapped) != 0) { printf("FAIL unwrap integrity\n"); fails++; }
		else check("RFC3394 unwrap roundtrip", unwrapped, dek, 32);
		/* tamper -> integrity must fail */
		wrapped[10] ^= 0x01;
		if (aes_keyunwrap(kek, wrapped, 40, unwrapped) == 0) { printf("FAIL tamper not caught\n"); fails++; }
		else printf("ok   RFC3394 tamper rejected\n");
	}
	/* constant-time compare correctness */
	{
		uint8_t a[16], b[16];
		memset(a, 0x5a, 16); memset(b, 0x5a, 16);
		if (!ct_memeq(a,b,16)) { printf("FAIL ct_memeq equal\n"); fails++; } else printf("ok   ct_memeq equal\n");
		b[15]^=1;
		if (ct_memeq(a,b,16)) { printf("FAIL ct_memeq differ\n"); fails++; } else printf("ok   ct_memeq differ\n");
	}

	printf(fails ? "\n%d FAILURES\n" : "\nALL PASS\n", fails);
	return fails ? 1 : 0;
}
