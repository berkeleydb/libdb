/* DO NOT EDIT: automatically built by dist/s_include. */
#ifndef	_typed_ext_h_
#define	_typed_ext_h_

#if defined(__cplusplus)
extern "C" {
#endif

#ifdef HAVE_TYPED
void __typed_release __P((DB *));
#endif
#ifdef HAVE_TYPED
int __typed_open __P((DB *, DB_THREAD_INFO *, DB_TXN *, const char *, const char *, u_int32_t));
#endif
#ifdef HAVE_TYPED
int __typed_put_check __P((DBC *, DBT *, DBT *, u_int32_t));
#endif
#ifdef HAVE_TYPED
int __typed_fingerprint __P((DB *, const DB_CODEC *, u_int8_t *));
#endif
#ifdef HAVE_TYPED
int __typed_memcmp_order __P((const DBT *, const DBT *));
#endif
#ifdef HAVE_TYPED
int __typed_codec_cmp __P((const DB_CODEC *, const DBT *, const DBT *));
#endif
#ifdef HAVE_TYPED
int __typed_catalog_remove __P((DB *, DB_THREAD_INFO *, DB_TXN *, const char *, const char *));
#endif
#ifdef HAVE_TYPED
int __typed_catalog_rename __P((DB *, DB_THREAD_INFO *, DB_TXN *, const char *, const char *, const char *));
#endif

#if defined(__cplusplus)
}
#endif
#endif /* !_typed_ext_h_ */
