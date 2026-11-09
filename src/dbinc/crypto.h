/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#ifndef	_DB_CRYPTO_H_
#define	_DB_CRYPTO_H_

#if defined(__cplusplus)
extern "C" {
#endif

#ifdef HAVE_CRYPTO_IPP
#include <ippcp.h>
#endif

/*
 * RFC 0019 key-provider / verifier / key-wrap sizes.
 */
#define	DB_CRYPTO_SALT_LEN	16	/* 128-bit random salt. */
#define	DB_CRYPTO_VRFY_LEN	32	/* PBKDF2-HMAC-SHA256 verifier. */
#define	DB_CRYPTO_DEK_LEN	32	/* 256-bit data-encryption key. */
#define	DB_CRYPTO_WRAP_LEN	40	/* RFC 3394 wrap of a 256-bit DEK. */
#define	DB_CRYPTO_KEK_LEN	32	/* 256-bit key-encryption key. */

/* Default PBKDF2 iteration count (OWASP 2023 floor for HMAC-SHA256). */
#define	DB_CRYPTO_KDF_ITERS	210000

/* CIPHER region format version: 1 = legacy (password copy), 2 = verifier. */
#define	DB_CRYPTO_FMT_VERIFIER	2

/* Built-in provider ids (stable; stored in the region and __db_keys). */
#define	DB_CRYPTO_PROV_PASSWD	1
#define	DB_CRYPTO_PROV_KEYFILE	2
#define	DB_CRYPTO_PROV_TPM	3

/*
 * !!!
 * These are the internal representations of the algorithm flags.
 * They are used in both the DB_CIPHER structure and the CIPHER
 * structure so we can tell if users specified both passwd and alg
 * correctly.
 *
 * CIPHER_ANY is used when an app joins an existing env but doesn't
 * know the algorithm originally used.  This is only valid in the
 * DB_CIPHER structure until we open and can set the alg.
 */
/*
 * We store the algorithm in an 8-bit field on the meta-page.  So we
 * use a numeric value, not bit fields.
 * now we are limited to 8 algorithms before we cannot use bits and
 * need numeric values.  That should be plenty.  It is okay for the
 * CIPHER_ANY flag to go beyond that since that is never stored on disk.
 */

/*
 * This structure is per-process, not in shared memory.
 */
struct __db_cipher {
	u_int	(*adj_size) __P((size_t));
	int	(*close) __P((ENV *, void *));
	int	(*decrypt) __P((ENV *, void *, void *, u_int8_t *, size_t));
	int	(*encrypt) __P((ENV *, void *, void *, u_int8_t *, size_t));
	int	(*init) __P((ENV *, DB_CIPHER *));

	u_int8_t mac_key[DB_MAC_KEY];	/* MAC key. */
	void	*data;			/* Algorithm-specific information */

	/*
	 * RFC 0019: the data-encryption key the algorithm actually uses, in
	 * process-private memory only.  dek_len == 0 means the legacy mode
	 * where the secret feeds __aes_derivekeys directly.
	 */
	u_int8_t	dek[DB_CRYPTO_DEK_LEN];
	u_int32_t	dek_len;

#define	CIPHER_AES	1		/* AES algorithm */
	u_int8_t	alg;		/* Algorithm used - See above */
	u_int8_t	spare[3];	/* Spares */

#define	CIPHER_ANY	0x00000001	/* Only for DB_CIPHER */
	u_int32_t	flags;		/* Other flags */
};

#ifdef HAVE_CRYPTO

#include "crypto/rijndael/rijndael-api-fst.h"

/*
 * Shared ciphering structure (RFC 0019 format, fmt_version 2).
 * No mutex needed because all information is read-only after creation.
 * The password is NO LONGER stored here -- only a salted verifier and the
 * KEK-wrapped DEK, so a file-backed region holds no cleartext secret.
 */
typedef struct __cipher {
	u_int32_t	flags;		/* Algorithm used - see above */
	u_int32_t	fmt_version;	/* CIPHER region format version. */
	u_int32_t	kdf_iters;	/* PBKDF2 iteration count. */
	u_int32_t	provider;	/* DB_CRYPTO_PROV_* that created it. */
	u_int32_t	dek_wrapped;	/* Boolean: wrapped_dek is valid. */
	u_int32_t	spare;
	u_int8_t	salt[DB_CRYPTO_SALT_LEN];
	u_int8_t	verifier[DB_CRYPTO_VRFY_LEN];
	u_int8_t	wrapped_dek[DB_CRYPTO_WRAP_LEN];
} CIPHER;

#define	DB_AES_KEYLEN	128	/* AES key length */
#define	DB_AES_CHUNK	16	/* AES byte unit size */

typedef struct __aes_cipher {
#ifdef	HAVE_CRYPTO_IPP
	void		*ipp_ctx;	/* IPP key instance */
#else
	keyInstance	decrypt_ki;	/* Decryption key instance */
	keyInstance	encrypt_ki;	/* Encryption key instance */
#endif
	u_int32_t	flags;		/* AES-specific flags */
} AES_CIPHER;

#include "dbinc_auto/crypto_ext.h"
#endif /* HAVE_CRYPTO */

#if defined(__cplusplus)
}
#endif
#endif /* !_DB_CRYPTO_H_ */
