/* DO NOT EDIT: automatically built by dist/distrib. */
#ifndef _log_ext_h_
#define _log_ext_h_
int __log_open __P((DB_ENV *));
int __log_find __P((DB_LOG *, int, int *));
int __log_valid __P((DB_LOG *, u_int32_t, int));
int __log_close __P((DB_ENV *));
int __log_init_recover __P((DB_ENV *));
int __log_findckp __P((DB_ENV *, DB_LSN *));
int __log_get __P((DB_LOG *, DB_LSN *, DBT *, u_int32_t, int));
void __log_dbenv_create __P((DB_ENV *));
int __log_put __P((DB_ENV *, DB_LSN *, const DBT *, u_int32_t));
int __log_name __P((DB_LOG *,
    u_int32_t, char **, DB_FH *, u_int32_t));
int __log_register_recover
    __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __log_add_logid __P((DB_LOG *, DB *, u_int32_t));
int __db_fileid_to_db __P((DB_ENV *, DB **, int32_t, int));
void __log_close_files __P((DB_ENV *));
void __log_rem_logid __P((DB_LOG *, u_int32_t));
#endif /* _log_ext_h_ */
