/* DO NOT EDIT: automatically built by dist/distrib. */
#ifndef	_mutex_ext_h_
#define	_mutex_ext_h_
#if defined(__cplusplus)
extern "C" {
#endif
int __db_fcntl_mutex_init __P((DB_ENV *, MUTEX *, u_int32_t));
int __db_fcntl_mutex_lock __P((MUTEX *, DB_FH *));
int __db_fcntl_mutex_unlock __P((MUTEX *));
int __db_pthread_mutex_init __P((DB_ENV *, MUTEX *, u_int32_t));
int __db_pthread_mutex_lock __P((MUTEX *));
int __db_pthread_mutex_unlock __P((MUTEX *));
int __db_tas_mutex_init __P((DB_ENV *, MUTEX *, u_int32_t));
int __db_tas_mutex_lock __P((MUTEX *));
int __db_tas_mutex_unlock __P((MUTEX *));
int __db_mutex_alloc __P((DB_ENV *, REGINFO *, MUTEX **));
void __db_mutex_free __P((DB_ENV *, REGINFO *, MUTEX *));
#if defined(__cplusplus)
}
#endif
#endif /* _mutex_ext_h_ */
