/*-
 * test/fuzz/fuzz_api.c --
 *	Stateful public-API fuzzer: interpret the fuzz bytes as a bytecode
 *	program of DB operations and run it against a fresh private env.
 *
 *	This is the libdb analogue of a stateful/model fuzzer (TigerBeetle's
 *	VOPR drives operation sequences; SQLite fuzzes SQL programs).  It
 *	finds edge cases that only surface from a *sequence* of operations:
 *	cursor after delete, txn abort after put, get on a just-deleted key,
 *	reopen mid-stream, etc.
 *
 *	Bytecode: the input is a stream of (opcode, operands) records.  The
 *	first byte selects the access method for the initial open; then each
 *	record's first byte is an opcode (mod OP_COUNT) followed by inline
 *	operand bytes.  Sizes are bounded so each input runs in well under a
 *	second.
 *
 *	Goal: assertion failures, crashes, or leaks from operation-sequence
 *	edge cases.  Non-crash error returns are expected and ignored.
 *
 *	Isolation: one private env per input, closed and its dir removed
 *	before returning.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#include "fuzz_util.h"
#include "fuzz_driver.h"

#define DBNAME		"api.db"
#define MAX_OPS		2000		/* bound work per input */
#define MAX_KEYLEN	64
#define MAX_VALLEN	256

/* Opcodes.  Kept small and mapped via modulo so any byte is a valid op. */
enum {
	OP_PUT = 0,
	OP_GET,
	OP_DEL,
	OP_CUR_OPEN,
	OP_CUR_NEXT,
	OP_CUR_PREV,
	OP_CUR_DEL,
	OP_CUR_CLOSE,
	OP_TXN_BEGIN,
	OP_TXN_COMMIT,
	OP_TXN_ABORT,
	OP_SYNC,
	OP_COUNT
};

/* A tiny byte-stream reader over the fuzz input. */
struct rd {
	const unsigned char *p;
	unsigned long n, i;
};

static unsigned char
rd_u8(struct rd *r)
{
	if (r->i >= r->n)
		return (0);
	return (r->p[r->i++]);
}

/* Read a length-prefixed blob (len is one byte, capped at max). */
static unsigned int
rd_blob(struct rd *r, unsigned char *out, unsigned int max)
{
	unsigned int len, j;

	len = rd_u8(r) % (max + 1);
	for (j = 0; j < len; j++)
		out[j] = rd_u8(r);
	return (len);
}

static void
quiet_errcall(const DB_ENV *env, const char *pfx, const char *msg)
{
	(void)env; (void)pfx; (void)msg;
}

int
LLVMFuzzerTestOneInput(const unsigned char *data, unsigned long size)
{
	char dir[256];
	DB_ENV *env = NULL;
	DB *db = NULL;
	DBC *dbc = NULL;		/* at most one live cursor */
	DB_TXN *txn = NULL;		/* at most one live txn */
	struct rd r;
	DBT key, val, out;
	unsigned char kbuf[MAX_KEYLEN], vbuf[MAX_VALLEN];
	/* USERMEM output buffers so libdb never malloc's return data the
	 * harness would leak; keeps the harness leak-clean under LSan so
	 * only genuine engine leaks surface. */
	static unsigned char okey[1 << 16], oval[1 << 16];
	unsigned int klen, vlen;
	DBTYPE type;
	int ops;

	if (size > (1u << 16))
		size = (1u << 16);
	if (fuzz_scratch_make(dir, sizeof(dir)) != 0)
		return (0);

	r.p = data; r.n = size; r.i = 0;

	if (db_env_create(&env, 0) != 0) {
		fuzz_scratch_rm(dir);
		return (0);
	}
	env->set_errcall(env, quiet_errcall);
	if (env->open(env, dir, DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_PRIVATE, 0600) != 0) {
		(void)env->close(env, 0);
		fuzz_scratch_rm(dir);
		return (0);
	}

	/* First byte picks the access method for the initial open. */
	type = (rd_u8(&r) & 1) ? DB_HASH : DB_BTREE;
	if (db_create(&db, env, 0) != 0 ||
	    db->open(db, NULL, DBNAME, NULL, type,
	    DB_CREATE | DB_AUTO_COMMIT, 0600) != 0) {
		if (db != NULL)
			(void)db->close(db, 0);
		(void)env->close(env, 0);
		fuzz_scratch_rm(dir);
		return (0);
	}

	for (ops = 0; ops < MAX_OPS && r.i < r.n; ops++) {
		int op = rd_u8(&r) % OP_COUNT;

		memset(&key, 0, sizeof(key));
		memset(&val, 0, sizeof(val));
		memset(&out, 0, sizeof(out));
		/* Output DBTs use USERMEM so no return buffer is malloc'd. */
		out.data = oval; out.ulen = sizeof(oval);
		out.flags = DB_DBT_USERMEM;

		switch (op) {
		case OP_PUT:
			klen = rd_blob(&r, kbuf, MAX_KEYLEN);
			vlen = rd_blob(&r, vbuf, MAX_VALLEN);
			if (klen == 0)	/* BDB rejects zero-length keys */
				break;
			key.data = kbuf; key.size = klen;
			val.data = vbuf; val.size = vlen;
			(void)db->put(db, txn, &key, &val, 0);
			break;
		case OP_GET:
			klen = rd_blob(&r, kbuf, MAX_KEYLEN);
			if (klen == 0)
				break;
			key.data = kbuf; key.size = klen;
			(void)db->get(db, txn, &key, &out, 0);
			break;
		case OP_DEL:
			klen = rd_blob(&r, kbuf, MAX_KEYLEN);
			if (klen == 0)
				break;
			key.data = kbuf; key.size = klen;
			(void)db->del(db, txn, &key, 0);
			break;
		case OP_CUR_OPEN:
			if (dbc == NULL)
				(void)db->cursor(db, txn, &dbc, 0);
			break;
		case OP_CUR_NEXT:
			if (dbc != NULL) {
				key.data = okey; key.ulen = sizeof(okey);
				key.flags = DB_DBT_USERMEM;
				val.data = oval; val.ulen = sizeof(oval);
				val.flags = DB_DBT_USERMEM;
				(void)dbc->get(dbc, &key, &val, DB_NEXT);
			}
			break;
		case OP_CUR_PREV:
			if (dbc != NULL) {
				key.data = okey; key.ulen = sizeof(okey);
				key.flags = DB_DBT_USERMEM;
				val.data = oval; val.ulen = sizeof(oval);
				val.flags = DB_DBT_USERMEM;
				(void)dbc->get(dbc, &key, &val, DB_PREV);
			}
			break;
		case OP_CUR_DEL:
			if (dbc != NULL)
				(void)dbc->del(dbc, 0);
			break;
		case OP_CUR_CLOSE:
			if (dbc != NULL) {
				(void)dbc->close(dbc);
				dbc = NULL;
			}
			break;
		case OP_TXN_BEGIN:
			/* A cursor must be closed before its txn ends; keep
			 * the model simple: no nested txns. */
			if (txn == NULL && dbc == NULL)
				(void)env->txn_begin(env, NULL, &txn, 0);
			break;
		case OP_TXN_COMMIT:
			if (txn != NULL && dbc == NULL) {
				(void)txn->commit(txn, 0);
				txn = NULL;
			}
			break;
		case OP_TXN_ABORT:
			if (txn != NULL && dbc == NULL) {
				(void)txn->abort(txn);
				txn = NULL;
			}
			break;
		case OP_SYNC:
			(void)db->sync(db, 0);
			break;
		}
	}

	/* Tear down in dependency order: cursor, then txn, then db, env. */
	if (dbc != NULL)
		(void)dbc->close(dbc);
	if (txn != NULL)
		(void)txn->abort(txn);
	(void)db->close(db, 0);
	(void)env->close(env, 0);
	fuzz_scratch_rm(dir);
	return (0);
}
