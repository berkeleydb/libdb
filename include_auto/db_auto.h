/* Do not edit: automatically built by gen_rec.awk. */

#ifndef	db_AUTO_H
#define	db_AUTO_H
#define	DB_db_addrem	41
typedef struct _db_addrem_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	u_int32_t	opcode;
	int32_t	fileid;
	db_pgno_t	pgno;
	u_int32_t	indx;
	u_int32_t	nbytes;
	DBT	hdr;
	DBT	dbt;
	DB_LSN	pagelsn;
} __db_addrem_args;

#define	DB_db_big	43
typedef struct _db_big_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	u_int32_t	opcode;
	int32_t	fileid;
	db_pgno_t	pgno;
	db_pgno_t	prev_pgno;
	db_pgno_t	next_pgno;
	DBT	dbt;
	DB_LSN	pagelsn;
	DB_LSN	prevlsn;
	DB_LSN	nextlsn;
} __db_big_args;

#define	DB_db_ovref	44
typedef struct _db_ovref_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	int32_t	fileid;
	db_pgno_t	pgno;
	int32_t	adjust;
	DB_LSN	lsn;
} __db_ovref_args;

#define	DB_db_relink	45
typedef struct _db_relink_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	u_int32_t	opcode;
	int32_t	fileid;
	db_pgno_t	pgno;
	DB_LSN	lsn;
	db_pgno_t	prev;
	DB_LSN	lsn_prev;
	db_pgno_t	next;
	DB_LSN	lsn_next;
} __db_relink_args;

#define	DB_db_debug	47
typedef struct _db_debug_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	DBT	op;
	int32_t	fileid;
	DBT	key;
	DBT	data;
	u_int32_t	arg_flags;
} __db_debug_args;

#define	DB_db_noop	48
typedef struct _db_noop_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	int32_t	fileid;
	db_pgno_t	pgno;
	DB_LSN	prevlsn;
} __db_noop_args;

#define	DB_db_pg_alloc	49
typedef struct _db_pg_alloc_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	int32_t	fileid;
	DB_LSN	meta_lsn;
	db_pgno_t	meta_pgno;
	DB_LSN	page_lsn;
	db_pgno_t	pgno;
	u_int32_t	ptype;
	db_pgno_t	next;
} __db_pg_alloc_args;

#define	DB_db_pg_free	50
typedef struct _db_pg_free_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	int32_t	fileid;
	db_pgno_t	pgno;
	DB_LSN	meta_lsn;
	db_pgno_t	meta_pgno;
	DBT	header;
	db_pgno_t	next;
} __db_pg_free_args;

#endif
