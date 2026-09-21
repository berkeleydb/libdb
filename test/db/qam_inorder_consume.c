/*
 * P7 reproducer: DB_INORDER + DB_CONSUME across a deleted record.
 *
 * Queue with DB_INORDER, N records via DB_APPEND, delete one in the middle,
 * then drain with DB_CONSUME. Without the flag the drain returns every survivor
 * and then DB_NOTFOUND. With it, the consume reaches the hole and spins.
 *
 * Emits a VERDICT line either way so a hang is distinguishable from a failure:
 * a timeout kill leaves NO verdict, which the runner treats as the hang.
 */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

#define NREC    20
#define HOLE    10

static void die(int ret, const char *what)
{
	fprintf(stderr, "FATAL %s: %s\n", what, db_strerror(ret));
	exit(2);
}

int main(int argc, char **argv)
{
	DB_ENV *env;
	DB *dbp;
	DBT key, data;
	db_recno_t rno;
	char vbuf[32];
	const char *home;
	int i, ret, consumed, inorder;

	home = argc > 1 ? argv[1] : "/nvme/P7DIR";
	inorder = argc > 2 ? atoi(argv[2]) : 1;

	if ((ret = db_env_create(&env, 0)) != 0) die(ret, "env_create");
	env->set_errfile(env, stderr);
	env->set_errpfx(env, "p7");
	if ((ret = env->open(env, home, DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_TXN | DB_INIT_LOG | DB_INIT_LOCK, 0644)) != 0)
		die(ret, "env->open");

	if ((ret = db_create(&dbp, env, 0)) != 0) die(ret, "db_create");
	(void)dbp->set_re_len(dbp, sizeof(vbuf));
	if (inorder && (ret = dbp->set_flags(dbp, DB_INORDER)) != 0)
		die(ret, "set_flags(DB_INORDER)");
	if ((ret = dbp->open(dbp, NULL, "p7.db", NULL, DB_QUEUE,
	    DB_CREATE | DB_AUTO_COMMIT, 0644)) != 0) die(ret, "db->open");

	/* Append NREC records. */
	memset(vbuf, 'v', sizeof(vbuf));
	for (i = 0; i < NREC; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		rno = 0;
		key.data = &rno; key.size = key.ulen = sizeof(rno);
		key.flags = DB_DBT_USERMEM;
		data.data = vbuf; data.size = sizeof(vbuf);
		if ((ret = dbp->put(dbp, NULL, &key, &data, DB_APPEND)) != 0)
			die(ret, "put(DB_APPEND)");
	}

	/* Punch a hole in the middle. */
	memset(&key, 0, sizeof(key));
	rno = HOLE;
	key.data = &rno; key.size = sizeof(rno);
	if ((ret = dbp->del(dbp, NULL, &key, 0)) != 0)
		die(ret, "del");

	printf("setup: %d records, record %d deleted, inorder=%d\n",
	    NREC, HOLE, inorder);
	fflush(stdout);

	/*
	 * Drain. Under the defect this never returns, so the caller's timeout
	 * is what ends the process -- and the absence of the VERDICT below is
	 * the signal.
	 */
	consumed = 0;
	for (;;) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		rno = 0;
		key.data = &rno; key.size = key.ulen = sizeof(rno);
		key.flags = DB_DBT_USERMEM;
		data.flags = DB_DBT_MALLOC;
		ret = dbp->get(dbp, NULL, &key, &data, DB_CONSUME);
		if (ret == DB_NOTFOUND)
			break;
		if (ret != 0)
			die(ret, "get(DB_CONSUME)");
		if (data.data != NULL)
			free(data.data);
		if (++consumed > NREC * 4) {
			printf("VERDICT p7 FAIL consumed=%d exceeds %d -- "
			    "consume is looping\n", consumed, NREC * 4);
			goto out;
		}
	}
	printf("VERDICT p7 %s consumed=%d expected=%d inorder=%d\n",
	    consumed == NREC - 1 ? "PASS" : "FAIL", consumed, NREC - 1, inorder);

out:	(void)dbp->close(dbp, 0);
	(void)env->close(env, 0);
	return (0);
}
