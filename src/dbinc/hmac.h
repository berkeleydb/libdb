/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#ifndef	_DB_HMAC_H_
#define	_DB_HMAC_H_

#if defined(__cplusplus)
extern "C" {
#endif

/*
 * Algorithm specific information.
 */
/*
 * SHA1 checksumming
 */
typedef struct {
	u_int32_t	state[5];
	u_int32_t	count[2];
	unsigned char	buffer[64];
} SHA1_CTX;

/*
 * SHA-256 (RFC 0019: KDF/verifier/key-wrap only; the page/log MAC stays SHA1).
 */
#define	DB_SHA256_DIGEST	32
typedef struct {
	u_int8_t	buffer[64];
	u_int32_t	datalen;
	u_int64_t	bitlen;
	u_int32_t	state[8];
} SHA256_CTX;

/*
 * AES assumes the SHA1 checksumming (also called MAC)
 */
#define	DB_MAC_MAGIC	"mac derivation key magic value"
#define	DB_ENC_MAGIC	"encryption and decryption key value magic"

#if defined(__cplusplus)
}
#endif

#include "dbinc_auto/hmac_ext.h"
#endif /* !_DB_HMAC_H_ */
