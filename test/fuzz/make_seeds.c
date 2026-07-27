/*-
 * test/fuzz/make_seeds.c --
 *	Generate the seed-corpus inputs the fuzzers bootstrap from.  Run
 *	once (via run.sh or by hand) to produce:
 *	  corpus/dbfile/valid.db   -- a real 2-record btree .db file
 *	  corpus/recover/valid.log -- a real first log file from a txn env
 *	  corpus/api/valid.ops     -- a hand-built valid op-bytecode program
 *
 *	Building the .db and .log with libdb itself guarantees the seeds are
 *	structurally valid, so the fuzzer starts from real coverage and
 *	mutates outward.  Not part of CI's build; the committed corpus files
 *	are what CI uses.  Kept in-tree so anyone can regenerate them.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

static int
make_dbfile(const char *scratch, const char *outpath)
{
	DB_ENV *env;
	DB *db;
	DBT key, val;
	char cmd[512], src[512];
	int i;

	(void)snprintf(cmd, sizeof(cmd), "rm -rf '%s' && mkdir -p '%s'",
	    scratch, scratch);
	(void)system(cmd);

	if (db_env_create(&env, 0) != 0)
		return (1);
	if (env->open(env, scratch, DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE,
	    0600) != 0)
		return (1);
	if (db_create(&db, env, 0) != 0)
		return (1);
	if (db->open(db, NULL, "seed.db", NULL, DB_BTREE, DB_CREATE, 0600)
	    != 0)
		return (1);
	for (i = 0; i < 2; i++) {
		char k[16], v[16];
		(void)snprintf(k, sizeof(k), "key%d", i);
		(void)snprintf(v, sizeof(v), "val%d", i);
		memset(&key, 0, sizeof(key));
		memset(&val, 0, sizeof(val));
		key.data = k; key.size = (u_int32_t)strlen(k) + 1;
		val.data = v; val.size = (u_int32_t)strlen(v) + 1;
		(void)db->put(db, NULL, &key, &val, 0);
	}
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	(void)snprintf(src, sizeof(src), "%s/seed.db", scratch);
	(void)snprintf(cmd, sizeof(cmd), "cp '%s' '%s'", src, outpath);
	return (system(cmd));
}

static int
make_logfile(const char *scratch, const char *outpath)
{
	DB_ENV *env;
	DB *db;
	DBT key, val;
	char cmd[512], src[512];

	(void)snprintf(cmd, sizeof(cmd), "rm -rf '%s' && mkdir -p '%s'",
	    scratch, scratch);
	(void)system(cmd);

	if (db_env_create(&env, 0) != 0)
		return (1);
	/* Small log file so the seed corpus stays compact (default is a
	 * 10MB preallocated log; a fuzz seed only needs the header + a few
	 * records). */
	(void)env->set_lg_max(env, 65536);
	if (env->open(env, scratch, DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_PRIVATE, 0600) != 0)
		return (1);
	if (db_create(&db, env, 0) != 0)
		return (1);
	if (db->open(db, NULL, "seed.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600) != 0)
		return (1);
	memset(&key, 0, sizeof(key));
	memset(&val, 0, sizeof(val));
	key.data = (void *)"k"; key.size = 1;
	val.data = (void *)"v"; val.size = 1;
	(void)db->put(db, NULL, &key, &val, 0);
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	(void)snprintf(src, sizeof(src), "%s/log.0000000001", scratch);
	(void)snprintf(cmd, sizeof(cmd), "cp '%s' '%s'", src, outpath);
	return (system(cmd));
}

/* A short valid op-bytecode: btree, a few puts/gets, a cursor scan, a
 * txn that commits.  See fuzz_api.c for the opcode numbering. */
static int
make_ops(const char *outpath)
{
	static const unsigned char prog[] = {
		0x00,			/* access method selector -> BTREE */
		/* OP_PUT klen=3 "aaa" vlen=3 "AAA" */
		0, 3, 'a','a','a', 3, 'A','A','A',
		/* OP_PUT klen=3 "bbb" vlen=2 "BB" */
		0, 3, 'b','b','b', 2, 'B','B',
		/* OP_GET klen=3 "aaa" */
		1, 3, 'a','a','a',
		/* OP_CUR_OPEN */
		3,
		/* OP_CUR_NEXT, OP_CUR_NEXT */
		4, 4,
		/* OP_CUR_CLOSE */
		7,
		/* OP_TXN_BEGIN */
		8,
		/* OP_PUT klen=3 "ccc" vlen=1 "C" (inside txn) */
		0, 3, 'c','c','c', 1, 'C',
		/* OP_TXN_COMMIT */
		9,
		/* OP_DEL klen=3 "aaa" */
		2, 3, 'a','a','a',
		/* OP_SYNC */
		11,
	};
	FILE *f = fopen(outpath, "wb");
	if (f == NULL)
		return (1);
	(void)fwrite(prog, 1, sizeof(prog), f);
	(void)fclose(f);
	return (0);
}

int
main(int argc, char **argv)
{
	const char *base = argc > 1 ? argv[1] : ".";
	char scratch[256], p[512];
	int rc = 0;

	(void)snprintf(scratch, sizeof(scratch), "%s/.seedtmp", base);

	(void)snprintf(p, sizeof(p), "%s/corpus/dbfile/valid.db", base);
	if (make_dbfile(scratch, p) != 0) {
		fprintf(stderr, "make_dbfile failed\n"); rc = 1;
	} else
		printf("wrote %s\n", p);

	(void)snprintf(p, sizeof(p), "%s/corpus/recover/valid.log", base);
	if (make_logfile(scratch, p) != 0) {
		fprintf(stderr, "make_logfile failed\n"); rc = 1;
	} else
		printf("wrote %s\n", p);

	(void)snprintf(p, sizeof(p), "%s/corpus/api/valid.ops", base);
	if (make_ops(p) != 0) {
		fprintf(stderr, "make_ops failed\n"); rc = 1;
	} else
		printf("wrote %s\n", p);

	(void)snprintf(p, sizeof(p), "rm -rf '%s'", scratch);
	(void)system(p);
	return (rc);
}
