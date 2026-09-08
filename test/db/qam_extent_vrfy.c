/*-
 * Regression test for the queue extent verification bound.
 *
 * PR #160 bounded __qam_vrfy_walkqueue()'s scan with
 *	if (stop > vdp->last_pgno) stop = vdp->last_pgno;
 * which looks obviously safe but is not: vdp->last_pgno comes from
 * __memp_get_last_pgno() on the MAIN .db file, and a queue with extents keeps
 * only its meta page there -- every data page lives in a separate
 * __dbq.<name>.<extid> file.  So last_pgno was 0, the bound collapsed to 0, and
 * the extent walk was skipped entirely: db_verify reported success on a queue
 * with a corrupted extent page.  That shipped in 5.3.35.
 *
 * The bug survived review because the only things checked were "legitimate
 * queues still verify OK" and "the DoS input finishes fast" -- and a verifier
 * that silently does nothing satisfies BOTH.  So this test asserts both
 * directions:
 *
 *	1. A legitimate many-extent queue verifies CLEAN (no false positive:
 *	   the bound must not reject real extent pages).
 *	2. The same queue with ONE corrupted extent page is DETECTED (the walk
 *	   is actually running).
 *
 * Case 2 is the one that fails if the bound ever degenerates again.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <db.h>

#define	DBNAME		"qext.db"
#define	NRECS		20000
#define	RECLEN		32
#define	PAGESZ		512
#define	EXTENTSZ	2

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

/*
 * build_queue --
 *	Build a queue with tiny extents and a small page size, so the data
 *	lands in many separate extent files and the main .db holds only its
 *	meta page -- the shape that made the 5.3.35 bound degenerate.
 */
static int
build_queue(const char *path)
{
	DB *dbp;
	DBT key, data;
	db_recno_t recno;
	char buf[RECLEN];
	int i, ret;

	if ((ret = db_create(&dbp, NULL, 0)) != 0) {
		fprintf(stderr, "db_create: %s\n", db_strerror(ret));
		return (ret);
	}
	(void)dbp->set_re_len(dbp, RECLEN);
	(void)dbp->set_q_extentsize(dbp, EXTENTSZ);
	(void)dbp->set_pagesize(dbp, PAGESZ);

	if ((ret = dbp->open(dbp,
	    NULL, path, NULL, DB_QUEUE, DB_CREATE, 0600)) != 0) {
		fprintf(stderr, "open %s: %s\n", path, db_strerror(ret));
		return (ret);
	}

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	data.data = buf;
	data.size = RECLEN;

	for (i = 0; i < NRECS; i++) {
		(void)snprintf(buf, sizeof(buf), "record-%08d", i);
		key.data = &recno;
		key.ulen = sizeof(recno);
		key.flags = DB_DBT_USERMEM;
		if ((ret = dbp->put(dbp, NULL, &key, &data, DB_APPEND)) != 0) {
			fprintf(stderr, "put %d: %s\n", i, db_strerror(ret));
			(void)dbp->close(dbp, 0);
			return (ret);
		}
	}
	if ((ret = dbp->close(dbp, 0)) != 0) {
		fprintf(stderr, "close: %s\n", db_strerror(ret));
		return (ret);
	}
	return (0);
}

/*
 * count_extents --
 *	How many __dbq.<name>.<id> files exist, and the name of the first.
 */
static int
count_extents(char *firstp, size_t firstlen)
{
	FILE *fp;
	char line[512];
	int n;

	n = 0;
	*firstp = '\0';
	/*
	 * The extent files sit alongside the database in the current
	 * directory; listing them with the shell keeps this test free of
	 * platform directory-reading differences.
	 */
	if ((fp = popen("ls __dbq." DBNAME ".* 2>/dev/null", "r")) == NULL)
		return (0);
	while (fgets(line, (int)sizeof(line), fp) != NULL) {
		line[strcspn(line, "\r\n")] = '\0';
		if (line[0] == '\0')
			continue;
		if (n == 0)
			(void)snprintf(firstp, firstlen, "%s", line);
		n++;
	}
	(void)pclose(fp);
	return (n);
}

/*
 * corrupt_extent_page --
 *	Set the page-type byte of the first page of an extent file to an
 *	invalid value.  Offset 25 is the type field of a Berkeley DB page.
 */
static int
corrupt_extent_page(const char *path)
{
	FILE *fp;
	unsigned char bad;

	if ((fp = fopen(path, "r+b")) == NULL) {
		fprintf(stderr, "open %s for corruption failed\n", path);
		return (1);
	}
	if (fseek(fp, 25L, SEEK_SET) != 0) {
		(void)fclose(fp);
		return (1);
	}
	bad = 255;
	if (fwrite(&bad, 1, 1, fp) != 1) {
		(void)fclose(fp);
		return (1);
	}
	(void)fclose(fp);
	return (0);
}

/*
 * verify_db --
 *	DB->verify() consumes the handle on both success and failure.
 */
static int
verify_db(const char *path)
{
	DB *dbp;
	int ret;

	if ((ret = db_create(&dbp, NULL, 0)) != 0) {
		fprintf(stderr, "db_create: %s\n", db_strerror(ret));
		return (ret);
	}
	/* The corruption diagnostics in case 2 are expected; stay quiet. */
	dbp->set_errfile(dbp, NULL);
	return (dbp->verify(dbp, path, NULL, NULL, 0));
}

int
main(int argc, char *argv[])
{
	char first[512];
	int nextents, ret;

	(void)argc; (void)argv;

	/*
	 * Start from a clean slate.  This test writes the database and its
	 * extent files into the current directory, and a previous run leaves a
	 * deliberately corrupted extent behind -- reusing it would make case 1
	 * fail for the wrong reason.  The runner only clears $HOME_DIR/*.db, so
	 * do the extents here.
	 */
	(void)system("rm -f " DBNAME " __dbq." DBNAME ".* 2>/dev/null");

	if (build_queue(DBNAME) != 0)
		return (EXIT_FAILURE);

	nextents = count_extents(first, sizeof(first));
	printf("  built %s: %d records, %d extent file(s)\n",
	    DBNAME, NRECS, nextents);
	CHECK(nextents > 1,
	    "expected many extent files, got %d -- this test needs the "
	    "multi-extent shape to be meaningful", nextents);
	if (nextents < 1) {
		printf("qam_extent_vrfy: FAIL (no extents built)\n");
		return (EXIT_FAILURE);
	}

	/* Case 1: the untouched queue must verify clean. */
	ret = verify_db(DBNAME);
	printf("  clean many-extent queue:   ret=%d (%s)\n",
	    ret, ret == 0 ? "success" : db_strerror(ret));
	CHECK(ret == 0,
	    "a legitimate queue with %d extents failed verification (%d) -- "
	    "the extent bound is too tight", nextents, ret);

	/* Case 2: one corrupted extent page must be DETECTED. */
	if (corrupt_extent_page(first) != 0) {
		printf("qam_extent_vrfy: FAIL (could not corrupt %s)\n", first);
		return (EXIT_FAILURE);
	}
	printf("  corrupted %s page type -> 255\n", first);

	ret = verify_db(DBNAME);
	printf("  corrupted extent page:     ret=%d (%s)\n",
	    ret, ret == 0 ? "SUCCESS -- NOT DETECTED" : db_strerror(ret));
	CHECK(ret != 0,
	    "a corrupted extent page was NOT detected: the extent walk is "
	    "being skipped, so queue verification is silently doing nothing");

	printf("qam_extent_vrfy: %d checks, %d failures\n", checks, failures);
	printf("qam_extent_vrfy: %s\n", failures == 0 ? "PASS" : "FAIL");
	return (failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
}
