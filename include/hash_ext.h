/* DO NOT EDIT: automatically built by dist/distrib. */
#ifndef _hash_ext_h_
#define _hash_ext_h_
int __ham_metachk __P((DB *, const char *, HMETA *));
int __ham_open __P((DB *, const char *, db_pgno_t));
int __ham_c_init __P((DBC *));
int __ham_c_dup __P((DBC *, DBC *));
u_int32_t __ham_call_hash __P((HASH_CURSOR *, u_int8_t *, int32_t));
int __ham_init_dbt __P((DBT *, u_int32_t, void **, u_int32_t *));
void __ham_c_update
   __P((HASH_CURSOR *, db_pgno_t, u_int32_t, int, int));
int __ham_get_clist __P((DB *,
    db_pgno_t, u_int32_t, HASH_CURSOR ***));
int __ham_init_recover __P((DB_ENV *));
int __ham_pgin __P((db_pgno_t, void *, DBT *));
int __ham_pgout __P((db_pgno_t, void *, DBT *));
int __ham_mswap __P((void *));
int __ham_add_dup __P((DBC *, DBT *, u_int32_t));
int __ham_dup_convert __P((DBC *));
int __ham_make_dup __P((const DBT *, DBT *d, void **, u_int32_t *));
void __ham_move_offpage __P((DBC *, PAGE *, u_int32_t, db_pgno_t));
void __ham_dsearch __P((DBC *, DBT *, u_int32_t *, int *));
void __ham_ca_split __P((DB *,
   db_pgno_t, db_pgno_t, db_pgno_t, u_int32_t, int));
int __ham_cprint __P((DB *));
u_int32_t __ham_func2 __P((const void *, u_int32_t));
u_int32_t __ham_func3 __P((const void *, u_int32_t));
u_int32_t __ham_func4 __P((const void *, u_int32_t));
u_int32_t __ham_func5 __P((const void *, u_int32_t));
int __ham_get_meta __P((DBC *));
int __ham_release_meta __P((DBC *));
int __ham_dirty_meta __P((DBC *));
int __ham_db_create __P((DB *));
int __ham_db_close __P((DB *));
int __ham_item __P((DBC *, db_lockmode_t));
int __ham_item_reset __P((DBC *));
void __ham_item_init __P((HASH_CURSOR *));
int __ham_item_done __P((DBC *, int));
int __ham_item_last __P((DBC *, db_lockmode_t));
int __ham_item_first __P((DBC *, db_lockmode_t));
int __ham_item_prev __P((DBC *, db_lockmode_t));
int __ham_item_next __P((DBC *, db_lockmode_t));
void __ham_putitem __P((PAGE *p, const DBT *, int));
void __ham_reputpair
   __P((PAGE *p, u_int32_t, u_int32_t, const DBT *, const DBT *));
int __ham_del_pair __P((DBC *, int));
int __ham_replpair __P((DBC *, DBT *, u_int32_t));
void __ham_onpage_replace __P((PAGE *, size_t, u_int32_t, int32_t,
    int32_t,  DBT *));
int __ham_split_page __P((DBC *, u_int32_t, u_int32_t));
int __ham_add_el __P((DBC *, const DBT *, const DBT *, int));
void __ham_copy_item __P((size_t, PAGE *, u_int32_t, PAGE *));
int __ham_add_ovflpage __P((DBC *, PAGE *, int, PAGE **));
int __ham_put_page __P((DB *, PAGE *, int32_t));
int __ham_dirty_page __P((DB *, PAGE *));
int __ham_get_page __P((DB *, db_pgno_t, PAGE **));
db_pgno_t __bucket_to_page __P((HASH_CURSOR *, db_pgno_t));
int __ham_get_cpage __P((DBC *, db_lockmode_t));
int __ham_next_cpage __P((DBC *, db_pgno_t, int, u_int32_t));
int __ham_lock_bucket __P((DBC *, db_lockmode_t));
void __ham_dpair __P((DB *, PAGE *, u_int32_t));
int __ham_insdel_recover
    __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_newpage_recover
    __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_replace_recover
   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_newpgno_recover
   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_splitmeta_recover
   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_splitdata_recover
   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_ovfl_recover
    __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_copypage_recover
  __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_metagroup_recover
  __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_groupalloc_recover
  __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_reclaim __P((DB *, DB_TXN *txn));
int __ham_stat __P((DB *, void *, void *(*)(size_t), u_int32_t));
 int __ham_traverse __P((DB *, DBC *, db_lockmode_t,
    int (*)(DB *, PAGE *, void *, int *), void *));
int __ham_upgrade __P((DB *, int, char *, DB_FH *, char *));
#endif /* _hash_ext_h_ */
