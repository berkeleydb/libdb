/* DO NOT EDIT: automatically built by dist/s_include. */
#ifndef	_log_ext_h_
#define	_log_ext_h_
#if defined(__cplusplus)
extern "C" {
#endif
int __log_open __P((DB_ENV *));
int __log_find __P((DB_LOG *, int, int *, logfile_validity *));
int __log_valid __P((DB_LOG *, u_int32_t, int, logfile_validity *));
int __log_dbenv_refresh __P((DB_ENV *));
int __log_stat __P((DB_ENV *, DB_LOG_STAT **, u_int32_t));
int __log_lastckp __P((DB_ENV *, DB_LSN *));
void __log_region_destroy __P((DB_ENV *, REGINFO *));
int __log_vtruncate __P((DB_ENV *, DB_LSN *, DB_LSN *));
int __log_is_outdated __P((DB_ENV *dbenv, u_int32_t fnum, int *outdatedp));
int __log_archive __P((DB_ENV *, char **[], u_int32_t));
int __log_register_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, u_int32_t, const DBT *, const DBT *, int32_t, DBTYPE, db_pgno_t));
int __log_register_getpgnos __P((DB_ENV *, DBT *, DB_LSN *, db_recops, void *));
int __log_register_print __P((DB_ENV *, DBT *, DB_LSN *, db_recops, void *));
int __log_register_read __P((DB_ENV *, void *, __log_register_args **));
int __log_init_print __P((DB_ENV *, int (***)(DB_ENV *, DBT *, DB_LSN *, db_recops, void *), size_t *));
int __log_init_getpgnos __P((DB_ENV *, int (***)(DB_ENV *, DBT *, DB_LSN *, db_recops, void *), size_t *));
int __log_init_recover __P((DB_ENV *));
int __log_findckp __P((DB_ENV *, DB_LSN *));
int __log_cursor __P((DB_ENV *, DB_LOGC **, u_int32_t));
void __log_dbenv_create __P((DB_ENV *));
int __log_put __P((DB_ENV *, DB_LSN *, const DBT *, u_int32_t));
int __log_put_int __P((DB_ENV *, DB_LSN *, const DBT *, u_int32_t));
int __log_flush __P((DB_ENV *, const DB_LSN *));
int __log_file __P((DB_ENV *, const DB_LSN *, char *, size_t));
int __log_name __P((DB_LOG *, u_int32_t, char **, DB_FH *, u_int32_t));
int __log_register_recover __P((DB_ENV *, DBT *, DB_LSN *, db_recops, void *));
int __log_reopen_file __P((DB_ENV *, char *, int32_t, u_int8_t *, db_pgno_t, u_int32_t));
int __log_add_logid __P((DB_ENV *, DB_LOG *, DB *, int32_t));
int __db_fileid_to_db __P((DB_ENV *, DB **, int32_t, int));
void __log_close_files __P((DB_ENV *));
void __log_rem_logid __P((DB_LOG *, DB *, int32_t));
int __log_lid_to_fname __P((DB_LOG *, int32_t, FNAME **));
int __log_register __P((DB_ENV *, DB *, const char *));
int __log_register_int __P((DB_ENV *, DB *, const char *, u_int32_t));
int __log_unregister __P((DB_ENV *, DB *));
int __log_filelist_update __P((DB_ENV *, DB *, int32_t, const char *, int *));
int __log_file_lock __P((DB *));
#if defined(__cplusplus)
}
#endif
#endif /* _log_ext_h_ */
