/* DO NOT EDIT: automatically built by dist/distrib. */
#ifndef _txn_ext_h_
#define _txn_ext_h_
int __txn_xa_begin __P((DB_ENV *, DB_TXN *));
int __txn_end __P((DB_TXN *, int));
int __txn_is_ancestor __P((DB_ENV *, size_t, size_t));
int __txn_activekids __P((DB_TXN *));
int __txn_init_recover __P((DB_ENV *));
int __txn_regop_recover
   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __txn_xa_regop_recover
   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __txn_ckp_recover __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __txn_child_recover
   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
void __txn_dbenv_create __P((DB_ENV *));
int __txn_open __P((DB_ENV *));
int __txn_close __P((DB_ENV *));
#endif /* _txn_ext_h_ */
