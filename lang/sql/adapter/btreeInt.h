/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 */

#include <errno.h>

#include "sqliteInt.h"
#include <db.h>

#ifdef BDBSQL_SHARE_PRIVATE
/* BDBSQL_SHARE_PRIVATE implies BDBSQL_OMIT_SHARING */
#define	BDBSQL_OMIT_SHARING
#endif

#define	INTKEY_BUFSIZE	(sizeof(i64) + 2) /* We add 2 bytes to negatives. */
#define	MULTI_BUFSIZE	8 * SQLITE_DEFAULT_PAGE_SIZE
#define	DBNAME_SIZE	20
#define	NUMMETA		16

/*
 * The default maximum number of locking entities available in
 * Berkeley DB environments created by the sql engine.  This value is
 * passed to DB_ENV->set_lk_max_lockers.
 */
#ifndef BDBSQL_MAX_LOCKERS
# define BDBSQL_MAX_LOCKERS 1000
#endif

/*
 * The default maximum number of locks supported in Berkeley DB
 * environments created by the sql engine.  This value is passed to
 * DB_ENV->set_lk_max_locks.
 */
#ifndef BDBSQL_MAX_LOCKS
# define BDBSQL_MAX_LOCKS 10000
#endif

/*
 * The default maximum number of locked objects supported in Berkeley
 * DB environments created by the sql engine.  This value is passed to
 * DB_ENV->set_lk_max_objects.
 */
#ifndef BDBSQL_MAX_LOCK_OBJECTS
# define BDBSQL_MAX_LOCK_OBJECTS 10000
#endif

/*
 * The default size of the Berkeley DB environment's logging area, in
 * bytes.
 */
#ifndef BDBSQL_LOG_REGIONMAX
# define BDBSQL_LOG_REGIONMAX (300 * 1024)
#endif

/*
 * The default maximum number of transactions in Berkeley DB environment.
 */
#ifndef BDBSQL_MAX_TRANSACTIONS
# define BDBSQL_MAX_TRANSACTIONS 200
#endif

/*
 * The default policy for enabling the transactional bulk insert
 * optimization.
 */
#ifndef BDBSQL_TXN_BULK_DEFAULT
# define BDBSQL_TXN_BULK_DEFAULT 0
#endif

#ifndef UINT32_MAX                      /* Maximum 32-bit unsigned. */
#define	UINT32_MAX      4294967295U
#endif

#define	MAP_ERR(rc, ret)					\
	((rc != SQLITE_OK) ? rc : (ret == 0) ? SQLITE_OK : dberr2sqlite(ret))

#define	MAP_ERR_LOCKED(rc, ret)					\
	((rc != SQLITE_OK) ? rc : (ret == 0) ? SQLITE_OK :	\
	    dberr2sqlitelocked(ret))

typedef int (*compareFunc)(void*,int,const void*,int,const void*);

typedef struct {
#define	CACHE_KEY_SIZE 9		/* 8 hex characters + NUL */
	char key[CACHE_KEY_SIZE];
	DB *dbp;
	db_lockmode_t lock_mode;
	int created;
} CACHED_DB;

typedef struct {
	u32 value;
	u8 cached;
} CACHED_META;

typedef struct DELETED_TABLE DELETED_TABLE;
struct DELETED_TABLE {
	int iTable;
	DB_TXN *txn;
#ifdef BDBSQL_FILE_PER_TABLE
	int flag;
#define	DTF_DELETE	0x00
#define	DTF_DROP	0x01
#endif
	DELETED_TABLE *next;
};

#ifndef BDBSQL_SINGLE_THREAD
typedef struct {
	BtShared *pBt;
	KeyInfo *pKeyInfo;
	int iTable;
} TableInfo;
#endif

#ifdef BDBSQL_SHARE_PRIVATE
typedef struct {
	int fd;
	void *mapAddr;
	int generation;
	int readlock_count;
	int writelock_count;
	int write_waiting;
	int in_env_open;
	sqlite3_mutex *mutex;
} LockFileInfo;
#endif

typedef enum { CLEANUP_COMMIT, CLEANUP_ABORT, CLEANUP_CLOSE,
    CLEANUP_DROP_LOCKS, CLEANUP_GET_LOCKS } cleanup_mode_t;
/* There are three possible table types in SQLite. */
typedef enum { DB_STORE_NAMED, DB_STORE_TMP, DB_STORE_INMEM } storage_mode_t;
typedef enum { TRANS_NONE, TRANS_READ, TRANS_WRITE } txn_mode_t;
typedef enum { LOCKMODE_NONE, LOCKMODE_READ, LOCKMODE_WRITE } lock_mode_t;
typedef enum { NO_LSN_RESET, LSN_RESET_FILE, LSN_RESET_DIR } lsn_reset_t;

/* Declarations for functions that are shared by adapter source files. */
void *btreeCreateIndexKey(BtCursor *pCur);
Index *btreeGetIndex(Btree *p, int iTable);
int btreeGetPageCount(Btree *p, int **tables, u32 *pageCount, DB_TXN *txn);
int btreeGetUserTable(Btree *p, DB_TXN *pTxn, DB **pDb, int iTable);
int btreeGetTables(Btree *, int **, DB_TXN *);
int btreeLockSchema(Btree *p, lock_mode_t lockMode);
int btreeOpenEnvironment(Btree *p, int needLock);
int btreeOpenMetaTables(Btree *p, int *pCreating);
#ifndef SQLITE_OMIT_VACUUM
int btreeIncrVacuum(Btree *p);
int btreeVacuum(Btree *p, char **pzErrMsg);
#endif
int dberr2sqlite(int);
int closeDB(Btree *p, DB *dbp, u_int32_t flags);
void *allocateCursorIndex(BtCursor *pCur, u_int32_t amount);
int splitIndexKey(BtCursor *pCur);
int isDupIndex(int flags, int storage, KeyInfo *keyInfo, DB *db);
int btreeCleanupEnv(const char *home);
#ifdef BDBSQL_SHARE_PRIVATE
int btreeScopedFileLock(Btree *p, int iswrite, int dontreopen);
int btreeScopedFileUnlock(Btree *p, int iswrite);
int btreeHasFileLock(Btree *p, int iswrite);
#endif
#ifdef SQLITE_HAS_CODEC
int sqlite3CodecAttach(sqlite3*, int, const void*, int);
#endif

#define	CLEAR_PWD(pBt)	do {						\
	memset((pBt)->encrypt_pwd, 0xff, (pBt)->encrypt_pwd_len);	\
	free((pBt)->encrypt_pwd);				\
	(pBt)->encrypt_pwd_len = 0;					\
	(pBt)->encrypt_pwd = NULL;					\
} while (0)

#ifdef BDBSQL_FILE_PER_TABLE
/* Name of the metadata table in BDBSQL_FILE_PER_TABLE */
#define	BDBSQL_META_DATA_TABLE "metadata"
int getMetaDataFileName(const char *full_name, char **filename);
#endif

struct BtShared {
	char *dir_name;
	char *full_name;
	char *short_name; /* A pointer into orig_name memory. */
	char *orig_name;
	u_int8_t fileid[DB_FILE_ID_LEN];
	char *encrypt_pwd;
	lsn_reset_t lsn_reset;
	storage_mode_t dbStorage;
	u_int32_t env_oflags;
	DB_ENV *dbenv;
	int env_opened, encrypted, encrypt_pwd_len, last_table, need_open;
	/*
	 * Handles for the metadata DB, which holds the SQLite metadata for a
	 * file, and the tables DB, which is the Berkeley DB-internal database
	 * of sub-databases in a file.
	 */
	DB *metadb, *tablesdb;
	CACHED_META meta[NUMMETA];
	Hash db_cache;
#ifdef BDBSQL_SHARE_PRIVATE
	LockFileInfo lockfile;
	u_int32_t mp_mutex_count;
#endif
	/*
	 * A unique name is assigned to each in memory table. This value is
	 * used to ensure that each BtShared object gets a unique identifier.
	 * NOTE: For DB_STORE_INMEM tables, despite sharing the same environment
	 * handle, the internal table name is unique because it comprises of
	 * both the uid and iTable.
	 */
	u_int32_t uid;
	u_int32_t flags;
	u_int32_t panic; /* If the environment is not in a usable state. */
	u_int32_t db_oflags;
	u_int32_t transactional;
	u_int32_t pageSize;
	u_int32_t pageCount;
	u_int32_t pageSizeFixed;
	u_int32_t cacheSize;
	u_int32_t logFileSize; /* In bytes */
	u_int32_t database_existed; /* Did the database file exist on open. */
	u8 autoVacuum; /* Is auto-vacuum enabled? */
	u8 incrVacuum; /* Is incremental vacuum enabled? */
	u8 resultsBuffer; /* Query results are stored in a in-memory buffer */
	u8 secureDelete; /* Overwrite deleted data */
	/* Non-recursive mutex required to access this struct */
	sqlite3_mutex *mutex;
	BtCursor *first_cursor;

	/* Fields used to maintain the linked list of shared objects. */
	BtShared *pNextDb;
	BtShared *pPrevDb;
	Btree *btrees; /* A linked list of btrees that have been opened in this BtShared. */
	int nRef;
	int readonly;
};

struct BtCursor {
	Btree *pBtree;
	int tableIndex;
	u_int32_t flags;
	u8 isDupIndex, isFirst, isIncrblobHandle, wrFlag;
	CACHED_DB *cached_db;
	DBC *dbc;
	DB_TXN *txn;
	struct KeyInfo *keyInfo;
	enum {
		CURSOR_INVALID, CURSOR_VALID, CURSOR_REQUIRESEEK, CURSOR_FAULT
	} eState;
	int error, lastRes;
	i64 cachedRowid, savedIntKey, lastKey;
	DBT key, data, index;
	u8 nKeyBuf[INTKEY_BUFSIZE];
	DBT multiData;
	void *multiGetPtr, *multiPutPtr;
	int skipMulti;
	BtCursor *next;
};

struct Btree {
	struct BtShared *pBt;
	sqlite3 *db;

	int connected;		/* Set up with an open environment */
	DB_TXN *family_txn;	/* Makes txns and cursors lock-compatible. */
	DB_TXN *main_txn;	/* Base transaction for read and savepoint. */
	DB_TXN *read_txn;
	DB_TXN *savepoint_txn;
	int nSavepoint;		/* The number of open savepoints. */
#ifdef BDBSQL_SHARE_PRIVATE
	int maintxn_is_write;
#endif
	u_int32_t cached_dbs;
	int vfsFlags;

	void* schema;		/* Opaque schema handle used by SQLite */
	void (*free_schema)(void*);	/* Destructor for schema */

	DELETED_TABLE *deleted_tables;

	txn_mode_t inTrans;
	lock_mode_t schemaLockMode;
	DBC *schemaLock;
	DBC *compact_cursor; /* Walks over table names during vacuum. */
	u8 inVacuum;	/* True if vacuum is in progress */
	u8 sharable;	/* True if we can share pBt with another db */
	u8 locked;	/* True if db currently has pBt locked */
	u8 txn_excl;	/* True if in an exclusive transaction */
	u8 txn_bulk;	/* True to enable the bulk loading optimization */
	u32 txn_priority;	/* Transaction priority. */
	int wantToLock;	/* Number of nested calls to sqlite3BtreeEnter() */
	int nBackup;	/* Number of backup operations reading this btree */
	u32 updateDuringBackup; /* An update was performed during a backup. */
	int readonly;
	Btree *pNext;
	Btree *pPrev;
};

/* Shared by btree.c and btmutex.c */
typedef enum {
	LOG_VERBOSE, LOG_DEBUG, LOG_NORMAL, LOG_RELEASE, LOG_NONE
} loglevel_t;

#define	CURRENT_LOG_LEVEL LOG_RELEASE

#ifdef NDEBUG
#define	log_msg(...)
#else
/* Utility functions. */
static void log_msg(loglevel_t level, const char *fmt, ...)
{
	if (level >= CURRENT_LOG_LEVEL) {
		va_list ap;
		va_start(ap, fmt);
		vfprintf(stdout, fmt, ap);
		fputc('\n', stdout);
		fflush(stdout);
		va_end(ap);
	}
}
#endif

/*
 * Integer compression
 *
 *  First byte | Next | Maximum
 *  byte       | bytes| value
 * ------------+------+---------------------------------------------------------
 * [0 xxxxxxx] | 0    | 2^7 - 1
 * [10 xxxxxx] | 1    | 2^14 + 2^7 - 1
 * [110 xxxxx] | 2    | 2^21 + 2^14 + 2^7 - 1
 * [1110 xxxx] | 3    | 2^28 + 2^21 + 2^14 + 2^7 - 1
 * [11110 xxx] | 4    | 2^35 + 2^28 + 2^21 + 2^14 + 2^7 - 1
 * [11111 000] | 5    | 2^40 + 2^35 + 2^28 + 2^21 + 2^14 + 2^7 - 1
 * [11111 001] | 6    | 2^48 + 2^40 + 2^35 + 2^28 + 2^21 + 2^14 + 2^7 - 1
 * [11111 010] | 7    | 2^56 + 2^48 + 2^40 + 2^35 + 2^28 + 2^21 + 2^14 + 2^7 - 1
 * [11111 011] | 8    | 2^64 + 2^56 + 2^48 + 2^40 + 2^35 + 2^28 + 2^21 + 2^14 +
 *	       |      |	2^7 - 1
 *
 * NOTE: this compression algorithm depends
 * on big-endian order, so swap if necessary.
 */
extern int __db_isbigendian(void);

#define	CMP_INT_1BYTE_MAX 0x7F
#define	CMP_INT_2BYTE_MAX 0x407F
#define	CMP_INT_3BYTE_MAX 0x20407F
#define	CMP_INT_4BYTE_MAX 0x1020407F

#if defined(_MSC_VER) && _MSC_VER < 1300
#define	CMP_INT_5BYTE_MAX 0x081020407Fi64
#define	CMP_INT_6BYTE_MAX 0x01081020407Fi64
#define	CMP_INT_7BYTE_MAX 0x0101081020407Fi64
#define	CMP_INT_8BYTE_MAX 0x010101081020407Fi64
#else
#define	CMP_INT_5BYTE_MAX 0x081020407FLL
#define	CMP_INT_6BYTE_MAX 0x01081020407FLL
#define	CMP_INT_7BYTE_MAX 0x0101081020407FLL
#define	CMP_INT_8BYTE_MAX 0x010101081020407FLL
#endif

#define	CMP_INT_2BYTE_VAL 0x80
#define	CMP_INT_3BYTE_VAL 0xC0
#define	CMP_INT_4BYTE_VAL 0xE0
#define	CMP_INT_5BYTE_VAL 0xF0
#define	CMP_INT_6BYTE_VAL 0xF8
#define	CMP_INT_7BYTE_VAL 0xF9
#define	CMP_INT_8BYTE_VAL 0xFA
#define	CMP_INT_9BYTE_VAL 0xFB

#define	CMP_INT_2BYTE_MASK 0x3F
#define	CMP_INT_3BYTE_MASK 0x1F
#define	CMP_INT_4BYTE_MASK 0x0F
#define	CMP_INT_5BYTE_MASK 0x07

static const u_int8_t __dbsql_marshaled_int_size[] = {
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,

	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
	0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,

	0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03,
	0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03,
	0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03,
	0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03,

	0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
	0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,

	0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,
	0x06, 0x07, 0x08, 0x09, 0xFF, 0xFF, 0xFF, 0xFF
};

/*
 * __dbsql_compress_int --
 *	Compresses the integer into the buffer, returning the number of
 *	bytes occupied.
 *
 * PUBLIC: int __dbsql_compress_int __P((u_int8_t *, u_int64_t));
 */
static int __dbsql_compress_int(u_int8_t *buf, u_int64_t i)
{
	if (i <= CMP_INT_1BYTE_MAX) {
		/* no swapping for one byte value */
		buf[0] = (u_int8_t)i;
		return 1;
	} else {
		u_int8_t *p = (u_int8_t*)&i;
		if (i <= CMP_INT_2BYTE_MAX) {
			i -= CMP_INT_1BYTE_MAX + 1;
			if (__db_isbigendian() != 0) {
				buf[0] = p[6] | CMP_INT_2BYTE_VAL;
				buf[1] = p[7];
			} else {
				buf[0] = p[1] | CMP_INT_2BYTE_VAL;
				buf[1] = p[0];
			}
			return 2;
		} else if (i <= CMP_INT_3BYTE_MAX) {
			i -= CMP_INT_2BYTE_MAX + 1;
			if (__db_isbigendian() != 0) {
				buf[0] = p[5] | CMP_INT_3BYTE_VAL;
				buf[1] = p[6];
				buf[2] = p[7];
			} else {
				buf[0] = p[2] | CMP_INT_3BYTE_VAL;
				buf[1] = p[1];
				buf[2] = p[0];
			}
			return 3;
		} else if (i <= CMP_INT_4BYTE_MAX) {
			i -= CMP_INT_3BYTE_MAX + 1;
			if (__db_isbigendian() != 0) {
				buf[0] = p[4] | CMP_INT_4BYTE_VAL;
				buf[1] = p[5];
				buf[2] = p[6];
				buf[3] = p[7];
			} else {
				buf[0] = p[3] | CMP_INT_4BYTE_VAL;
				buf[1] = p[2];
				buf[2] = p[1];
				buf[3] = p[0];
			}
			return 4;
		} else if (i <= CMP_INT_5BYTE_MAX) {
			i -= CMP_INT_4BYTE_MAX + 1;
			if (__db_isbigendian() != 0) {
				buf[0] = p[3] | CMP_INT_5BYTE_VAL;
				buf[1] = p[4];
				buf[2] = p[5];
				buf[3] = p[6];
				buf[4] = p[7];
			} else {
				buf[0] = p[4] | CMP_INT_5BYTE_VAL;
				buf[1] = p[3];
				buf[2] = p[2];
				buf[3] = p[1];
				buf[4] = p[0];
			}
			return 5;
		} else if (i <= CMP_INT_6BYTE_MAX) {
			i -= CMP_INT_5BYTE_MAX + 1;
			if (__db_isbigendian() != 0) {
				buf[0] = CMP_INT_6BYTE_VAL;
				buf[1] = p[3];
				buf[2] = p[4];
				buf[3] = p[5];
				buf[4] = p[6];
				buf[5] = p[7];
			} else {
				buf[0] = CMP_INT_6BYTE_VAL;
				buf[1] = p[4];
				buf[2] = p[3];
				buf[3] = p[2];
				buf[4] = p[1];
				buf[5] = p[0];
			}
			return 6;
		} else if (i <= CMP_INT_7BYTE_MAX) {
			i -= CMP_INT_6BYTE_MAX + 1;
			if (__db_isbigendian() != 0) {
				buf[0] = CMP_INT_7BYTE_VAL;
				buf[1] = p[2];
				buf[2] = p[3];
				buf[3] = p[4];
				buf[4] = p[5];
				buf[5] = p[6];
				buf[6] = p[7];
			} else {
				buf[0] = CMP_INT_7BYTE_VAL;
				buf[1] = p[5];
				buf[2] = p[4];
				buf[3] = p[3];
				buf[4] = p[2];
				buf[5] = p[1];
				buf[6] = p[0];
			}
			return 7;
		} else if (i <= CMP_INT_8BYTE_MAX) {
			i -= CMP_INT_7BYTE_MAX + 1;
			if (__db_isbigendian() != 0) {
				buf[0] = CMP_INT_8BYTE_VAL;
				buf[1] = p[1];
				buf[2] = p[2];
				buf[3] = p[3];
				buf[4] = p[4];
				buf[5] = p[5];
				buf[6] = p[6];
				buf[7] = p[7];
			} else {
				buf[0] = CMP_INT_8BYTE_VAL;
				buf[1] = p[6];
				buf[2] = p[5];
				buf[3] = p[4];
				buf[4] = p[3];
				buf[5] = p[2];
				buf[6] = p[1];
				buf[7] = p[0];
			}
			return 8;
		} else {
			i -= CMP_INT_8BYTE_MAX + 1;
			if (__db_isbigendian() != 0) {
				buf[0] = CMP_INT_9BYTE_VAL;
				buf[1] = p[0];
				buf[2] = p[1];
				buf[3] = p[2];
				buf[4] = p[3];
				buf[5] = p[4];
				buf[6] = p[5];
				buf[7] = p[6];
				buf[8] = p[7];
			} else {
				buf[0] = CMP_INT_9BYTE_VAL;
				buf[1] = p[7];
				buf[2] = p[6];
				buf[3] = p[5];
				buf[4] = p[4];
				buf[5] = p[3];
				buf[6] = p[2];
				buf[7] = p[1];
				buf[8] = p[0];
			}
			return 9;
		}
	}
}

/*
 * __dbsql_decompress_int --
 *	Decompresses the compressed integer pointer to by buf into i,
 *	returning the number of bytes read.
 */
static int __dbsql_decompress_int(const u_int8_t *buf, u_int64_t *i)
{
	int len;
	u_int64_t tmp;
	u_int8_t *p;
	u_int8_t c;

	tmp = 0;
	p = (u_int8_t*)&tmp;
	c = buf[0];
	len = __dbsql_marshaled_int_size[c];

	switch (len) {
	case 1:
		*i = c;
		return 1;
	case 2:
		if (__db_isbigendian() != 0) {
			p[6] = (c & CMP_INT_2BYTE_MASK);
			p[7] = buf[1];
		} else {
			p[1] = (c & CMP_INT_2BYTE_MASK);
			p[0] = buf[1];
		}
		tmp += CMP_INT_1BYTE_MAX + 1;
		break;
	case 3:
		if (__db_isbigendian() != 0) {
			p[5] = (c & CMP_INT_3BYTE_MASK);
			p[6] = buf[1];
			p[7] = buf[2];
		} else {
			p[2] = (c & CMP_INT_3BYTE_MASK);
			p[1] = buf[1];
			p[0] = buf[2];
		}
		tmp += CMP_INT_2BYTE_MAX + 1;
		break;
	case 4:
		if (__db_isbigendian() != 0) {
			p[4] = (c & CMP_INT_4BYTE_MASK);
			p[5] = buf[1];
			p[6] = buf[2];
			p[7] = buf[3];
		} else {
			p[3] = (c & CMP_INT_4BYTE_MASK);
			p[2] = buf[1];
			p[1] = buf[2];
			p[0] = buf[3];
		}
		tmp += CMP_INT_3BYTE_MAX + 1;
		break;
	case 5:
		if (__db_isbigendian() != 0) {
			p[3] = (c & CMP_INT_5BYTE_MASK);
			p[4] = buf[1];
			p[5] = buf[2];
			p[6] = buf[3];
			p[7] = buf[4];
		} else {
			p[4] = (c & CMP_INT_5BYTE_MASK);
			p[3] = buf[1];
			p[2] = buf[2];
			p[1] = buf[3];
			p[0] = buf[4];
		}
		tmp += CMP_INT_4BYTE_MAX + 1;
		break;
	case 6:
		if (__db_isbigendian() != 0) {
			p[3] = buf[1];
			p[4] = buf[2];
			p[5] = buf[3];
			p[6] = buf[4];
			p[7] = buf[5];
		} else {
			p[4] = buf[1];
			p[3] = buf[2];
			p[2] = buf[3];
			p[1] = buf[4];
			p[0] = buf[5];
		}
		tmp += CMP_INT_5BYTE_MAX + 1;
		break;
	case 7:
		if (__db_isbigendian() != 0) {
			p[2] = buf[1];
			p[3] = buf[2];
			p[4] = buf[3];
			p[5] = buf[4];
			p[6] = buf[5];
			p[7] = buf[6];
		} else {
			p[5] = buf[1];
			p[4] = buf[2];
			p[3] = buf[3];
			p[2] = buf[4];
			p[1] = buf[5];
			p[0] = buf[6];
		}
		tmp += CMP_INT_6BYTE_MAX + 1;
		break;
	case 8:
		if (__db_isbigendian() != 0) {
			p[1] = buf[1];
			p[2] = buf[2];
			p[3] = buf[3];
			p[4] = buf[4];
			p[5] = buf[5];
			p[6] = buf[6];
			p[7] = buf[7];
		} else {
			p[6] = buf[1];
			p[5] = buf[2];
			p[4] = buf[3];
			p[3] = buf[4];
			p[2] = buf[5];
			p[1] = buf[6];
			p[0] = buf[7];
		}
		tmp += CMP_INT_7BYTE_MAX + 1;
		break;
	case 9:
		if (__db_isbigendian() != 0) {
			p[0] = buf[1];
			p[1] = buf[2];
			p[2] = buf[3];
			p[3] = buf[4];
			p[4] = buf[5];
			p[5] = buf[6];
			p[6] = buf[7];
			p[7] = buf[8];
		} else {
			p[7] = buf[1];
			p[6] = buf[2];
			p[5] = buf[3];
			p[4] = buf[4];
			p[3] = buf[5];
			p[2] = buf[6];
			p[1] = buf[7];
			p[0] = buf[8];
		}
		tmp += CMP_INT_8BYTE_MAX + 1;
		break;
	default:
		break;
	}

	*i = tmp;
	return len;
}
