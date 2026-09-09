/*-
 * Regression test for issue #159 -- the queue cursor read path probed the
 * filesystem once per record.
 *
 * A queue's first_recno/cur_recno come from the meta page.  QAM_OUTSIDE_QUEUE()
 * treats cur < first as a wrapped queue, so a corrupt or hostile meta page can
 * describe a range covering most of the recno space, and a cursor walking it
 * asks for one page per record.  Each miss cost a filename construction plus a
 * stat(2) -- billions of them.  Bounded (it terminates, corrupts nothing) but a
 * denial of service.
 *
 * The bound comes from the extent files actually present, cached per handle and
 * consulted only when an extent is not already open.
 *
 * Both directions are asserted, because a bound that is too tight silently
 * truncates real data -- which is worse than the DoS:
 *
 *	1. A legitimate multi-extent queue reads back EVERY record.
 *	2. Consuming (which unlinks extents) then re-appending still works, so
 *	   the cached bound cannot pin a queue shut once its extents are gone.
 *	3. A queue whose meta page claims a wrapped, near-UINT32_MAX range is
 *	   bounded promptly rather than walking to the end of the recno space.
 *
 * Case 3 is the DoS itself; cases 1 and 2 are what a careless bound breaks.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <db.h>

#define	DBNAME		"q159.db"
#define	NRECS		20000
#define	RECLEN		32
#define	PAGESZ		512
#define	EXTENTSZ	2
/*
 * Ceiling for the hostile walk.  The point is to separate BOUNDED from RUNAWAY,
 * not to track machine speed: unbounded this ran for minutes to hours, while a
 * correctly bounded walk is tens of seconds.
 *
 * Measured on one machine: 25-27s idle, but 66s while a parallel build was
 * running, and 74-120s under an ASan-instrumented library.  A 60s limit failed
 * on load alone, so the ceiling is generous by design -- a real regression
 * reverts to unbounded and blows past any of these numbers.  Override with
 * QAM_DOS_LIMIT_SECS on very slow or heavily instrumented builds.
 */
#define	DOS_LIMIT_SECS	600

static int failures = 0;
static int checks = 0;

#define	CHECK(cond, ...) do {						\
	checks++;							\
	if (!(cond)) {							\
		failures++;						\
		printf("  FAIL: ");					\
		printf(__VA_ARGS__);					\
		printf("\n");						\
	}								\
} while (0)

static DB *
open_queue(const char *path, u_int32_t flags)
{
	DB *dbp;
	int ret;

	if ((ret = db_create(&dbp, NULL, 0)) != 0) {
		fprintf(stderr, "db_create: %s\n", db_strerror(ret));
		exit(EXIT_FAILURE);
	}
	/* 770 extents will not fit in the default cache. */
	(void)dbp->set_cachesize(dbp, 0, 64 * 1024 * 1024, 1);
	(void)dbp->set_re_len(dbp, RECLEN);
	(void)dbp->set_q_extentsize(dbp, EXTENTSZ);
	(void)dbp->set_pagesize(dbp, PAGESZ);
	dbp->set_errfile(dbp, NULL);
	if ((ret = dbp->open(dbp,
	    NULL, path, NULL, DB_QUEUE, flags, 0600)) != 0) {
		fprintf(stderr, "open %s: %s\n", path, db_strerror(ret));
		exit(EXIT_FAILURE);
	}
	return (dbp);
}

static void
append_recs(DB *dbp, int n, const char *tag)
{
	DBT key, data;
	db_recno_t recno;
	char buf[RECLEN];
	int i, ret;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	data.data = buf;
	data.size = RECLEN;
	for (i = 0; i < n; i++) {
		(void)snprintf(buf, sizeof(buf), "%s-%08d", tag, i);
		key.data = &recno;
		key.ulen = sizeof(recno);
		key.flags = DB_DBT_USERMEM;
		if ((ret = dbp->put(dbp, NULL, &key, &data, DB_APPEND)) != 0) {
			fprintf(stderr, "put %d: %s\n", i, db_strerror(ret));
			exit(EXIT_FAILURE);
		}
	}
}

static long
consume_all(DB *dbp)
{
	DBT key, data;
	db_recno_t recno;
	long n;
	int ret;

	for (n = 0;;) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &recno;
		key.ulen = sizeof(recno);
		key.flags = DB_DBT_USERMEM;
		data.flags = DB_DBT_MALLOC;
		ret = dbp->get(dbp, NULL, &key, &data, DB_CONSUME);
		if (ret == DB_NOTFOUND)
			break;
		if (ret != 0) {
			fprintf(stderr, "consume: %s\n", db_strerror(ret));
			exit(EXIT_FAILURE);
		}
		if (data.data != NULL)
			free(data.data);
		n++;
	}
	return (n);
}

static long
walk_all(DB *dbp)
{
	DBC *dbc;
	DBT key, data;
	long n;
	int ret;

	if ((ret = dbp->cursor(dbp, NULL, &dbc, 0)) != 0) {
		fprintf(stderr, "cursor: %s\n", db_strerror(ret));
		exit(EXIT_FAILURE);
	}
	for (n = 0;;) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		if ((ret = dbc->get(dbc, &key, &data, DB_NEXT)) != 0)
			break;
		n++;
	}
	(void)dbc->close(dbc);
	return (n);
}

/*
 * corrupt_meta_wrapped --
 *	Rewrite the queue meta page to claim a wrapped, near-UINT32_MAX range:
 *	first_recno high, cur_recno low.  QMETA offsets: first_recno at 72,
 *	cur_recno at 76 (see QMETA in dbinc/db_page.h).
 */
static int
corrupt_meta_wrapped(const char *path)
{
	FILE *fp;
	u_int32_t first, cur;

	if ((fp = fopen(path, "r+b")) == NULL)
		return (1);
	first = 4000000000U;
	cur = 10;
	if (fseek(fp, 72L, SEEK_SET) != 0 ||
	    fwrite(&first, sizeof(first), 1, fp) != 1 ||
	    fseek(fp, 76L, SEEK_SET) != 0 ||
	    fwrite(&cur, sizeof(cur), 1, fp) != 1) {
		(void)fclose(fp);
		return (1);
	}
	(void)fclose(fp);
	return (0);
}

int
main(int argc, char *argv[])
{
	DB *dbp;
	time_t start;
	double elapsed;
	long got;
	int limit;
	char *lp;

	(void)argc; (void)argv;

	/* A previous run leaves a corrupted db and its extents behind. */
	(void)system("rm -f " DBNAME " __dbq." DBNAME ".* 2>/dev/null");

	/* --- Case 1: a legitimate multi-extent queue reads back in full. */
	dbp = open_queue(DBNAME, DB_CREATE);
	append_recs(dbp, NRECS, "rec");
	got = walk_all(dbp);
	printf("  legitimate queue: walked %ld of %d records\n", got, NRECS);
	CHECK(got == NRECS,
	    "a legitimate multi-extent queue returned %ld of %d records -- the "
	    "extent bound is truncating real data", got, NRECS);

	/* --- Case 2: consume (unlinking extents) then re-append and read. */
	got = consume_all(dbp);
	CHECK(got == NRECS,
	    "consumed %ld of %d records", got, NRECS);
	append_recs(dbp, 500, "post");
	got = consume_all(dbp);
	printf("  after consuming all extents: re-appended and read %ld of 500\n",
	    got);
	CHECK(got == 500,
	    "re-append after every extent was unlinked returned %ld of 500 -- a "
	    "stale cached bound is pinning the queue shut", got);
	(void)dbp->close(dbp, 0);

	/*
	 * --- Case 3: the hostile meta page.  Walking must terminate quickly
	 * instead of probing the filesystem across the recno space.
	 */
	if (corrupt_meta_wrapped(DBNAME) != 0) {
		printf("qam_readpath_bound: FAIL (could not rewrite meta)\n");
		return (EXIT_FAILURE);
	}
	printf("  rewrote meta: first_recno=4000000000 cur_recno=10 (wrapped)\n");

	limit = DOS_LIMIT_SECS;
	if ((lp = getenv("QAM_DOS_LIMIT_SECS")) != NULL && atoi(lp) > 0)
		limit = atoi(lp);

	start = time(NULL);
	dbp = open_queue(DBNAME, 0);
	(void)walk_all(dbp);
	(void)dbp->close(dbp, 0);
	elapsed = difftime(time(NULL), start);
	printf("  hostile meta page: walk finished in %.0f seconds\n", elapsed);
	CHECK(elapsed < limit,
	    "a walk over a hostile wrapped meta page took %.0f seconds "
	    "(limit %d) -- the read path is still probing one extent per recno",
	    elapsed, limit);

	printf("qam_readpath_bound: %d checks, %d failures\n", checks, failures);
	printf("qam_readpath_bound: %s\n", failures == 0 ? "PASS" : "FAIL");
	return (failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
}
