/* DO NOT EDIT: automatically built by dist/distrib. */
#ifndef _db_ext_h_
#define _db_ext_h_
int __crdel_init_recover __P((DB_ENV *));
int __crdel_fileopen_recover
  __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __crdel_metasub_recover
  __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __crdel_metapage_recover
  __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __crdel_delete_recover
  __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __db_open __P((DB *,
    const char *, const char *, DBTYPE, u_int32_t, int));
int __db_close __P((DB *, u_int32_t));
int __db_remove __P((DB *, const char *, const char *, u_int32_t));
int __db_backup_name __P((const char *, char **, DB_LSN *));
int __db_testcopy __P((DB *, const char *));
int __db_cursor __P((DB *, DB_TXN *, DBC **, u_int32_t));
int __db_c_dup __P((DBC *, DBC **, u_int32_t));
int __db_cprint __P((DB *));
int __db_c_destroy __P((DBC *));
int __db_fd __P((DB *, int *));
int __db_get __P((DB *, DB_TXN *, DBT *, DBT *, u_int32_t));
int __db_put __P((DB *, DB_TXN *, DBT *, DBT *, u_int32_t));
int __db_sync __P((DB *, u_int32_t));
int __db_log_page __P((DB *,
    const char *, DB_LSN *, db_pgno_t, PAGE *));
int __db_init_recover __P((DB_ENV *));
int __db_pgin __P((db_pgno_t, void *, DBT *));
int __db_pgout __P((db_pgno_t, void *, DBT *));
void __db_metaswap __P((PAGE *));
int __db_byteswap __P((db_pgno_t, PAGE *, size_t, int));
int __db_dispatch __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __db_add_recovery __P((DB_ENV *,
   int (*)(DB_ENV *, DBT *, DB_LSN *, int, void *), u_int32_t));
int __db_txnlist_init __P((void *));
int __db_txnlist_add __P((void *, u_int32_t));
int __db_txnlist_close __P((void *, u_int32_t, u_int32_t));
int __db_txnlist_delete __P((void *, char *, u_int32_t, int));
void __db_txnlist_end __P((DB_ENV *, void *));
int __db_txnlist_find __P((void *, u_int32_t));
void __db_txnlist_gen __P((void *, int));
void __db_txnlist_print __P((void *));
int __db_dput __P((DBC *, DBT *, PAGE **, db_indx_t *));
int __db_drem __P((DBC *, PAGE **, u_int32_t));
int __db_dend __P((DBC *, db_pgno_t, PAGE **));
 int __db_ditem __P((DBC *, PAGE *, u_int32_t, u_int32_t));
int __db_pitem
    __P((DBC *, PAGE *, u_int32_t, u_int32_t, DBT *, DBT *));
int __db_relink __P((DBC *, u_int32_t, PAGE *, PAGE **, int));
int __db_ddup __P((DBC *, db_pgno_t));
int __db_dsearch __P((DBC *,
    int, DBT *, db_pgno_t, db_indx_t *, PAGE **, int *));
int __db_cursorchk __P((const DB *, u_int32_t, int));
int __db_cdelchk __P((const DB *, u_int32_t, int, int));
int __db_cgetchk __P((const DB *, DBT *, DBT *, u_int32_t, int));
int __db_cputchk __P((const DB *,
   const DBT *, DBT *, u_int32_t, int, int));
int __db_closechk __P((const DB *, u_int32_t));
int __db_delchk __P((const DB *, DBT *, u_int32_t, int));
int __db_getchk __P((const DB *, const DBT *, DBT *, u_int32_t));
int __db_joinchk __P((const DB *, u_int32_t));
int __db_putchk
   __P((const DB *, DBT *, const DBT *, u_int32_t, int, int));
int __db_statchk __P((const DB *, u_int32_t));
int __db_syncchk __P((const DB *, u_int32_t));
int __db_eopnotsup __P((const DB_ENV *));
int __db_removechk __P((const DB *, u_int32_t));
int __db_join __P((DB *, DBC **, DBC **, u_int32_t));
int __db_new __P((DBC *, u_int32_t, PAGE **));
int __db_free __P((DBC *, PAGE *));
int __db_lt __P((DBC *));
int __db_lget __P((DBC *,
    int, db_pgno_t, db_lockmode_t, int, DB_LOCK *));
int __dbh_am_chk __P((DB *, u_int32_t));
int __db_goff __P((DB *, DBT *,
    u_int32_t, db_pgno_t, void **, u_int32_t *));
int __db_poff __P((DBC *, const DBT *, db_pgno_t *));
int __db_ovref __P((DBC *, db_pgno_t, int32_t));
int __db_doff __P((DBC *, db_pgno_t));
int __db_moff __P((DB *, const DBT *, db_pgno_t, u_int32_t,
    int (*)(const DBT *, const DBT *), int *));
void __db_loadme __P((void));
int __db_dump __P((DB *, char *, char *));
int __db_prnpage __P((DB *, db_pgno_t));
int __db_prpage __P((DB *, PAGE *, u_int32_t));
int __db_isbad __P((PAGE *, int));
void __db_pr __P((u_int8_t *, u_int32_t));
int __db_prdbt __P((DBT *, int, const char *, FILE *, int));
void __db_prflags __P((u_int32_t, const FN *, FILE *));
int __db_addrem_recover
   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __db_split_recover __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __db_big_recover __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __db_ovref_recover __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __db_relink_recover
  __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __db_addpage_recover
   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __db_debug_recover __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __db_noop_recover __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __db_traverse_dup __P((DB *,
   db_pgno_t, int (*)(DB *, PAGE *, void *, int *), void *));
int __db_traverse_big __P((DB *,
    db_pgno_t, int (*)(DB *, PAGE *, void *, int *), void *));
int __db_reclaim_callback __P((DB *, PAGE *, void *, int *));
int __db_ret __P((DB *,
   PAGE *, u_int32_t, DBT *, void **, u_int32_t *));
int __db_retcopy __P((DB *, DBT *,
   void *, u_int32_t, void **, u_int32_t *));
int __db_upgrade __P((DB *, const char *, u_int32_t));
#endif /* _db_ext_h_ */
