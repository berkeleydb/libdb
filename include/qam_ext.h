/* DO NOT EDIT: automatically built by dist/distrib. */
#ifndef _qam_ext_h_
#define _qam_ext_h_
int __qam_pitem
    __P((DBC *,  QPAGE *, u_int32_t, db_recno_t, DBT *));
int __qam_put __P((DB *, DB_TXN *, DBT *, DBT *, u_int32_t));
int __qam_delete __P((DB *, DB_TXN *, DBT *, u_int32_t));
int __qam_c_dup __P((DBC *, DBC *));
int __qam_c_init __P((DBC *));
int __qam_init_recover __P((DB_ENV *));
int __qam_mswap __P((PAGE *));
int __qam_pgin_out __P((db_pgno_t, void *, DBT *));
int __qam_db_create __P((DB *));
int __qam_db_close __P((DB *));
int __qam_open __P((DB *, const char *, db_pgno_t));
int __qam_metachk __P((DB *, const char *, QMETA *));
int __qam_inc_recover __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __qam_incfirst_recover
  __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __qam_mvptr_recover
  __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __qam_del_recover __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __qam_add_recover __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __qam_stat __P((DB *, void *, void *(*)(size_t), u_int32_t));
#endif /* _qam_ext_h_ */
