/* DO NOT EDIT: automatically built by dist/distrib. */
#ifndef _common_ext_h_
#define _common_ext_h_
int __db_byteorder __P((DB_ENV *, int));
int __db_fchk __P((DB_ENV *, const char *, u_int32_t, u_int32_t));
int __db_fcchk
   __P((DB_ENV *, const char *, u_int32_t, u_int32_t, u_int32_t));
int __db_ferr __P((const DB_ENV *, const char *, int));
int __db_pgerr __P((DB *, db_pgno_t));
int __db_pgfmt __P((DB *, db_pgno_t));
#ifdef DIAGNOSTIC
void __db_assert __P((const char *, const char *, int));
#endif
int __db_panic_msg __P((DB_ENV *));
int __db_panic __P((DB_ENV *, int));
#ifdef __STDC__
void __db_err __P((const DB_ENV *, const char *, ...));
#else
void __db_err();
#endif
void __db_real_err
    __P((const DB_ENV *, int, int, int, const char *, va_list));
#ifdef __STDC__
int __db_logmsg __P((DB_ENV *,
    DB_TXN *, const char *, u_int32_t, const char *, ...));
#else
int __db_logmsg();
#endif
int __db_getlong
    __P((DB *, const char *, char *, long, long, long *));
u_int32_t __db_log2 __P((u_int32_t));
#endif /* _common_ext_h_ */
