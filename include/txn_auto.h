/* Do not edit: automatically built by gen_rec.awk. */

#ifndef txn_AUTO_H
#define txn_AUTO_H

#define	DB_txn_regop	(DB_txn_BEGIN + 1)

typedef struct _txn_regop_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	u_int32_t	opcode;
} __txn_regop_args;

int __txn_regop_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, u_int32_t));
int __txn_regop_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __txn_regop_read __P((void *, __txn_regop_args **));

#define	DB_txn_ckp	(DB_txn_BEGIN + 2)

typedef struct _txn_ckp_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	DB_LSN 	ckp_lsn;
	DB_LSN 	last_ckp;
} __txn_ckp_args;

int __txn_ckp_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, DB_LSN *, DB_LSN *));
int __txn_ckp_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __txn_ckp_read __P((void *, __txn_ckp_args **));

#define	DB_txn_xa_regop	(DB_txn_BEGIN + 3)

typedef struct _txn_xa_regop_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	u_int32_t	opcode;
	DBT	xid;
	int32_t	formatID;
	u_int32_t	gtrid;
	u_int32_t	bqual;
} __txn_xa_regop_args;

int __txn_xa_regop_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, u_int32_t, const DBT *, int32_t, u_int32_t, u_int32_t));
int __txn_xa_regop_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __txn_xa_regop_read __P((void *, __txn_xa_regop_args **));

#define	DB_txn_child	(DB_txn_BEGIN + 4)

typedef struct _txn_child_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	u_int32_t	opcode;
	u_int32_t	parent;
} __txn_child_args;

int __txn_child_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, u_int32_t, u_int32_t));
int __txn_child_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __txn_child_read __P((void *, __txn_child_args **));
int __txn_init_print __P((DB_ENV *));
#endif
