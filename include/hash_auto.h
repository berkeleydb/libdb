/* Do not edit: automatically built by gen_rec.awk. */

#ifndef ham_AUTO_H
#define ham_AUTO_H

#define	DB_ham_insdel	(DB_ham_BEGIN + 1)

typedef struct _ham_insdel_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	u_int32_t	opcode;
	int32_t	fileid;
	db_pgno_t	pgno;
	u_int32_t	ndx;
	DB_LSN 	pagelsn;
	DBT	key;
	DBT	data;
} __ham_insdel_args;

int __ham_insdel_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, u_int32_t, int32_t, db_pgno_t, u_int32_t, DB_LSN *, const DBT *, const DBT *));
int __ham_insdel_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_insdel_read __P((void *, __ham_insdel_args **));

#define	DB_ham_newpage	(DB_ham_BEGIN + 2)

typedef struct _ham_newpage_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	u_int32_t	opcode;
	int32_t	fileid;
	db_pgno_t	prev_pgno;
	DB_LSN 	prevlsn;
	db_pgno_t	new_pgno;
	DB_LSN 	pagelsn;
	db_pgno_t	next_pgno;
	DB_LSN 	nextlsn;
} __ham_newpage_args;

int __ham_newpage_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, u_int32_t, int32_t, db_pgno_t, DB_LSN *, db_pgno_t, DB_LSN *, db_pgno_t, DB_LSN *));
int __ham_newpage_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_newpage_read __P((void *, __ham_newpage_args **));

#define	DB_ham_splitmeta	(DB_ham_BEGIN + 3)

typedef struct _ham_splitmeta_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	int32_t	fileid;
	u_int32_t	bucket;
	u_int32_t	ovflpoint;
	u_int32_t	spares;
	DB_LSN 	metalsn;
} __ham_splitmeta_args;

int __ham_splitmeta_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, int32_t, u_int32_t, u_int32_t, u_int32_t, DB_LSN *));
int __ham_splitmeta_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_splitmeta_read __P((void *, __ham_splitmeta_args **));

#define	DB_ham_splitdata	(DB_ham_BEGIN + 4)

typedef struct _ham_splitdata_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	int32_t	fileid;
	u_int32_t	opcode;
	db_pgno_t	pgno;
	DBT	pageimage;
	DB_LSN 	pagelsn;
} __ham_splitdata_args;

int __ham_splitdata_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, int32_t, u_int32_t, db_pgno_t, const DBT *, DB_LSN *));
int __ham_splitdata_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_splitdata_read __P((void *, __ham_splitdata_args **));

#define	DB_ham_replace	(DB_ham_BEGIN + 5)

typedef struct _ham_replace_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	int32_t	fileid;
	db_pgno_t	pgno;
	u_int32_t	ndx;
	DB_LSN 	pagelsn;
	int32_t	off;
	DBT	olditem;
	DBT	newitem;
	u_int32_t	makedup;
} __ham_replace_args;

int __ham_replace_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, int32_t, db_pgno_t, u_int32_t, DB_LSN *, int32_t, const DBT *, const DBT *, u_int32_t));
int __ham_replace_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_replace_read __P((void *, __ham_replace_args **));

#define	DB_ham_newpgno	(DB_ham_BEGIN + 6)

typedef struct _ham_newpgno_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	u_int32_t	opcode;
	int32_t	fileid;
	db_pgno_t	pgno;
	db_pgno_t	free_pgno;
	u_int32_t	old_type;
	db_pgno_t	old_pgno;
	u_int32_t	new_type;
	DB_LSN 	pagelsn;
	DB_LSN 	metalsn;
} __ham_newpgno_args;

int __ham_newpgno_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, u_int32_t, int32_t, db_pgno_t, db_pgno_t, u_int32_t, db_pgno_t, u_int32_t, DB_LSN *, DB_LSN *));
int __ham_newpgno_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_newpgno_read __P((void *, __ham_newpgno_args **));

#define	DB_ham_ovfl	(DB_ham_BEGIN + 7)

typedef struct _ham_ovfl_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	int32_t	fileid;
	db_pgno_t	start_pgno;
	u_int32_t	npages;
	db_pgno_t	free_pgno;
	u_int32_t	ovflpoint;
	DB_LSN 	metalsn;
} __ham_ovfl_args;

int __ham_ovfl_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, int32_t, db_pgno_t, u_int32_t, db_pgno_t, u_int32_t, DB_LSN *));
int __ham_ovfl_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_ovfl_read __P((void *, __ham_ovfl_args **));

#define	DB_ham_copypage	(DB_ham_BEGIN + 8)

typedef struct _ham_copypage_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	int32_t	fileid;
	db_pgno_t	pgno;
	DB_LSN 	pagelsn;
	db_pgno_t	next_pgno;
	DB_LSN 	nextlsn;
	db_pgno_t	nnext_pgno;
	DB_LSN 	nnextlsn;
	DBT	page;
} __ham_copypage_args;

int __ham_copypage_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, int32_t, db_pgno_t, DB_LSN *, db_pgno_t, DB_LSN *, db_pgno_t, DB_LSN *, const DBT *));
int __ham_copypage_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_copypage_read __P((void *, __ham_copypage_args **));

#define	DB_ham_metagroup	(DB_ham_BEGIN + 9)

typedef struct _ham_metagroup_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	int32_t	fileid;
	u_int32_t	bucket;
	db_pgno_t	pgno;
	DB_LSN 	metalsn;
	DB_LSN 	pagelsn;
} __ham_metagroup_args;

int __ham_metagroup_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, int32_t, u_int32_t, db_pgno_t, DB_LSN *, DB_LSN *));
int __ham_metagroup_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_metagroup_read __P((void *, __ham_metagroup_args **));

#define	DB_ham_groupalloc	(DB_ham_BEGIN + 10)

typedef struct _ham_groupalloc_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	int32_t	fileid;
	db_pgno_t	pgno;
	DB_LSN 	metalsn;
	DB_LSN 	mmetalsn;
	db_pgno_t	start_pgno;
	u_int32_t	num;
} __ham_groupalloc_args;

int __ham_groupalloc_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, int32_t, db_pgno_t, DB_LSN *, DB_LSN *, db_pgno_t, u_int32_t));
int __ham_groupalloc_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __ham_groupalloc_read __P((void *, __ham_groupalloc_args **));
int __ham_init_print __P((DB_ENV *));
#endif
