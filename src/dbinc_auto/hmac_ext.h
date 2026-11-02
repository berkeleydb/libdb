/* DO NOT EDIT: automatically built by dist/s_include. */
#ifndef	_hmac_ext_h_
#define	_hmac_ext_h_

#if defined(__cplusplus)
extern "C" {
#endif

void __db_chksum __P((void *, u_int8_t *, size_t, u_int8_t *, u_int8_t *));
void __db_derive_mac __P((u_int8_t *, size_t, u_int8_t *));
int __db_check_chksum __P((ENV *, void *, DB_CIPHER *, u_int8_t *, void *, size_t, int));
void __db_SHA1Transform __P((u_int32_t *, unsigned char *));
void __db_SHA1Init __P((SHA1_CTX *));
void __db_SHA1Update __P((SHA1_CTX *, unsigned char *, size_t));
void __db_SHA1Final __P((unsigned char *, SHA1_CTX *));
void __db_SHA256Init __P((SHA256_CTX *));
void __db_SHA256Update __P((SHA256_CTX *, const u_int8_t *, size_t));
void __db_SHA256Final __P((u_int8_t *, SHA256_CTX *));
void __db_hmac_sha256 __P((const u_int8_t *, size_t, const u_int8_t *, size_t, u_int8_t *));
int __db_pbkdf2_sha256 __P((const u_int8_t *, size_t, const u_int8_t *, size_t, u_int32_t, u_int8_t *, size_t));
int __db_ct_memeq __P((const void *, const void *, size_t));
void __db_explicit_bzero __P((void *, size_t));

#if defined(__cplusplus)
}
#endif
#endif /* !_hmac_ext_h_ */
