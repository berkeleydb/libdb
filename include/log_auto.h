/* Do not edit: automatically built by gen_rec.awk. */

#ifndef log_AUTO_H
#define log_AUTO_H

#define	DB_log_register	(DB_log_BEGIN + 1)

typedef struct _log_register_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	u_int32_t	opcode;
	DBT	name;
	DBT	uid;
	u_int32_t	id;
	DBTYPE	ftype;
} __log_register_args;

int __log_register_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, u_int32_t, const DBT *, const DBT *, u_int32_t, DBTYPE));
int __log_register_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __log_register_read __P((void *, __log_register_args **));
int __log_init_print __P((DB_ENV *));
#endif
