/* Do not edit: automatically built by gen_rec.awk. */

#ifndef crdel_AUTO_H
#define crdel_AUTO_H

#define	DB_crdel_fileopen	(DB_crdel_BEGIN + 1)

typedef struct _crdel_fileopen_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	DBT	name;
	u_int32_t	mode;
} __crdel_fileopen_args;

int __crdel_fileopen_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, const DBT *, u_int32_t));
int __crdel_fileopen_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __crdel_fileopen_read __P((void *, __crdel_fileopen_args **));

#define	DB_crdel_metasub	(DB_crdel_BEGIN + 2)

typedef struct _crdel_metasub_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	u_int32_t	fileid;
	db_pgno_t	pgno;
	DBT	page;
	DB_LSN 	lsn;
} __crdel_metasub_args;

int __crdel_metasub_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, u_int32_t, db_pgno_t, const DBT *, DB_LSN *));
int __crdel_metasub_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __crdel_metasub_read __P((void *, __crdel_metasub_args **));

#define	DB_crdel_metapage	(DB_crdel_BEGIN + 3)

typedef struct _crdel_metapage_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	u_int32_t	fileid;
	DBT	name;
	db_pgno_t	pgno;
	DBT	page;
} __crdel_metapage_args;

int __crdel_metapage_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, u_int32_t, const DBT *, db_pgno_t, const DBT *));
int __crdel_metapage_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __crdel_metapage_read __P((void *, __crdel_metapage_args **));

#define	DB_crdel_delete	(DB_crdel_BEGIN + 4)

typedef struct _crdel_delete_args {
	u_int32_t type;
	DB_TXN *txnid;
	DB_LSN prev_lsn;
	DBT	name;
} __crdel_delete_args;

int __crdel_delete_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t, const DBT *));
int __crdel_delete_print __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
int __crdel_delete_read __P((void *, __crdel_delete_args **));
int __crdel_init_print __P((DB_ENV *));
#endif
