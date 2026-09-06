/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 Oracle and/or its affiliates.  All rights reserved.
 *
 * hash_unsorted_cmp.c --
 *	Regression driver for the Hash lookup defect reported as
 *	https://github.com/berkeleydb/libdb/issues/139:
 *	__ham_getindex_unsorted() (src/hash/hash_page.c) called the
 *	application's DB->set_h_compare comparator for an inline (H_KEYDATA)
 *	key but discarded its result, so an equal key on a legacy
 *	P_HASH_UNSORTED page was reported as absent.  DB->get returned
 *	DB_NOTFOUND for a key that was present, and DB->put(DB_NOOVERWRITE)
 *	returned success and stored a second record with identical key bytes
 *	in a database where duplicates are disabled.
 *
 *	The defect needs three things at once, which is why it hid: a Hash
 *	database with a page still in the pre-4.6 P_HASH_UNSORTED format (5.3
 *	reads those without requiring DB->upgrade), an inline key on that
 *	page, and an explicitly configured custom comparator (a new handle's
 *	h_compare is NULL, so simply opening an old file is not enough).
 *	Modern sorted pages take __ham_getindex_sorted, which does record its
 *	comparison result.
 *
 * The legacy fixture is built synthetically -- no old library and no
 * committed binary blob.  A P_HASH_UNSORTED page and a P_HASH page have
 * identical byte layouts; the only difference is that P_HASH keeps its
 * key/data pairs in comparison order, which is a *subset* of what
 * P_HASH_UNSORTED allows.  So a current-format Hash file whose bucket pages
 * have their PAGE.type byte (offset 25) rewritten from P_HASH (13) to
 * P_HASH_UNSORTED (2), and whose metadata version (offset 16) is set back to
 * the 4.5.20 hash version 8, is a legitimate legacy file: it is exactly what
 * __ham_getindex dispatches to __ham_getindex_unsorted.  (This is the same
 * technique test/db/run_upgrade.sh uses to build old-format fixtures.)
 *
 * Checks, each run with the comparator (trigger) and without it (control),
 * so a failure is attributable to the comparator path and not the fixture:
 *	1. DB->get of a stored inline key must succeed and return its value.
 *	2. DB->get of a stored off-page key must succeed (the H_OFFPAGE
 *	   branch next door, which passes &res to __db_moff, must stay right).
 *	3. DB->put(DB_NOOVERWRITE) for a stored key must return DB_KEYEXIST
 *	   and must not add a record.
 *	4. A search key that merely PREFIXES a stored key, and one that
 *	   EXTENDS it, must both be reported absent.  The old code built the
 *	   stored-key DBT with the *search* key's length instead of the stored
 *	   key's, so the comparator saw a truncated or over-long view of the
 *	   page item: a prefix key compared equal (a false match returning
 *	   another record's data), and an over-long key made the comparator
 *	   read past the stored item.
 * Before the fix, every "trigger" case failed; the controls passed.
 */

#include <sys/types.h>
#include <sys/stat.h>

#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

#define	HOME		"HASH_UNSORTED_TESTDIR"
#define	PAGESIZE	512		/* Small pages: many bucket pages. */
#define	NRECS		20
#define	PAGE_TYPE_OFF	25		/* PAGE.type. */
#define	META_VERSION_OFF 16		/* DBMETA.version. */
#define	P_HASH_UNSORTED_T 2		/* Pre-4.6 hash page. */
#define	P_HASH_T	13		/* Sorted hash page. */
#define	HASH_VERSION_45	8		/* On-disk hash version of 4.5.20. */
#define	BIGKEYLEN	200		/* > PAGESIZE/4 => off-page key. */
#define	ALARM_SECS	120

static int fails = 0;
static unsigned long cmp_calls, cmp_equalities;

#define	CHK0(call) do {							\
	int _r = (call);						\
	if (_r != 0) {							\
		fprintf(stderr, "FAIL: %s:%d: %s => %d (%s)\n",		\
		    __FILE__, __LINE__, #call, _r, db_strerror(_r));	\
		fails++;						\
	}								\
} while (0)

#define	CHKEQ(got, want, what) do {					\
	if ((got) != (want)) {						\
		fprintf(stderr, "FAIL: %s:%d: %s: got %d, want %d\n",	\
		    __FILE__, __LINE__, (what), (int)(got), (int)(want));\
		fails++;						\
	}								\
} while (0)

/*
 * byte_compare --
 *	The comparison Berkeley DB itself used before 4.6 introduced
 *	DB->set_h_compare: unsigned byte order, shorter key first on a
 *	common prefix.  DB->set_h_compare on an existing database requires
 *	exactly this -- a comparator that reproduces the ordering the
 *	database was created with.
 */
static int
byte_compare(DB *dbp, const DBT *a, const DBT *b)
{
	size_t len;
	int ret;

	(void)dbp;
	cmp_calls++;
	len = a->size < b->size ? a->size : b->size;
	if ((ret = memcmp(a->data, b->data, len)) == 0)
		ret = a->size < b->size ? -1 : (a->size > b->size ? 1 : 0);
	if (ret == 0)
		cmp_equalities++;
	return (ret);
}

static void
inline_key(char *buf, int n)
{
	(void)snprintf(buf, 16, "acct%04d", n);
}

static void
big_key(char *buf)
{
	memset(buf, 'K', BIGKEYLEN);
	memcpy(buf, "offpage", 7);
	buf[BIGKEYLEN] = '\0';
}

static void
set_dbt(DBT *dbt, void *data, u_int32_t size)
{
	memset(dbt, 0, sizeof(*dbt));
	dbt->data = data;
	dbt->size = size;
}

/*
 * open_db --
 *	Open the Hash database, optionally configuring the comparator.
 */
static int
open_db(DB **dbpp, const char *path, int custom, u_int32_t flags)
{
	DB *dbp;
	int ret;

	if ((ret = db_create(&dbp, NULL, 0)) != 0) {
		fprintf(stderr, "db_create: %s\n", db_strerror(ret));
		return (ret);
	}
	if (custom && (ret = dbp->set_h_compare(dbp, byte_compare)) != 0) {
		fprintf(stderr, "set_h_compare: %s\n", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return (ret);
	}
	if ((ret = dbp->open(dbp,
	    NULL, path, NULL, DB_HASH, flags, 0644)) != 0) {
		fprintf(stderr, "open %s: %s\n", path, db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return (ret);
	}
	*dbpp = dbp;
	return (0);
}

/*
 * produce --
 *	Build a current-format Hash database with inline and off-page keys.
 */
static int
produce(const char *path)
{
	DB *dbp;
	DBT key, data;
	char kbuf[16], vbuf[32], bkbuf[BIGKEYLEN + 1];
	int i;

	(void)unlink(path);
	if (db_create(&dbp, NULL, 0) != 0)
		return (1);
	CHK0(dbp->set_pagesize(dbp, PAGESIZE));
	CHK0(dbp->open(dbp,
	    NULL, path, NULL, DB_HASH, DB_CREATE | DB_EXCL, 0644));
	for (i = 0; i < NRECS; i++) {
		inline_key(kbuf, i);
		(void)snprintf(vbuf, sizeof(vbuf), "balance=%d", 100 * i);
		set_dbt(&key, kbuf, (u_int32_t)strlen(kbuf));
		set_dbt(&data, vbuf, (u_int32_t)strlen(vbuf));
		CHK0(dbp->put(dbp, NULL, &key, &data, 0));
	}
	big_key(bkbuf);
	set_dbt(&key, bkbuf, (u_int32_t)BIGKEYLEN);
	set_dbt(&data, "offpage-value", 13);
	CHK0(dbp->put(dbp, NULL, &key, &data, 0));
	CHK0(dbp->close(dbp, 0));
	return (fails);
}

/*
 * make_legacy --
 *	Rewrite every sorted hash page into the pre-4.6 unsorted page type
 *	and set the metadata version back to the 4.5.20 hash version.
 *	Returns the number of pages converted, -1 on error.
 */
static int
make_legacy(const char *path)
{
	struct stat sb;
	off_t off;
	u_int32_t pagesize, version;
	int converted, fd;
	u_int8_t type;

	if ((fd = open(path, O_RDWR)) < 0) {
		perror("open fixture");
		return (-1);
	}
	if (fstat(fd, &sb) != 0) {
		perror("fstat fixture");
		(void)close(fd);
		return (-1);
	}
	if (pread(fd, &pagesize, sizeof(pagesize), 20) !=
	    (ssize_t)sizeof(pagesize) || pagesize == 0 ||
	    sb.st_size % (off_t)pagesize != 0) {
		fprintf(stderr, "fixture is not page aligned\n");
		(void)close(fd);
		return (-1);
	}
	version = HASH_VERSION_45;
	if (pwrite(fd, &version, sizeof(version), META_VERSION_OFF) !=
	    (ssize_t)sizeof(version)) {
		perror("pwrite version");
		(void)close(fd);
		return (-1);
	}
	converted = 0;
	for (off = 0; off < sb.st_size; off += (off_t)pagesize) {
		if (pread(fd, &type, 1, off + PAGE_TYPE_OFF) != 1) {
			perror("pread page type");
			(void)close(fd);
			return (-1);
		}
		if (type != P_HASH_T)
			continue;
		type = P_HASH_UNSORTED_T;
		if (pwrite(fd, &type, 1, off + PAGE_TYPE_OFF) != 1) {
			perror("pwrite page type");
			(void)close(fd);
			return (-1);
		}
		converted++;
	}
	if (close(fd) != 0) {
		perror("close fixture");
		return (-1);
	}
	return (converted);
}

/*
 * count_unsorted --
 *	How many P_HASH_UNSORTED pages does the file still hold?
 */
static int
count_unsorted(const char *path)
{
	struct stat sb;
	off_t off;
	u_int32_t pagesize;
	int count, fd;
	u_int8_t type;

	if ((fd = open(path, O_RDONLY)) < 0 || fstat(fd, &sb) != 0 ||
	    pread(fd, &pagesize, sizeof(pagesize), 20) !=
	    (ssize_t)sizeof(pagesize) || pagesize == 0) {
		perror("inspect fixture");
		if (fd >= 0)
			(void)close(fd);
		return (-1);
	}
	for (count = 0, off = 0; off < sb.st_size; off += (off_t)pagesize)
		if (pread(fd, &type, 1, off + PAGE_TYPE_OFF) == 1 &&
		    type == P_HASH_UNSORTED_T)
			count++;
	(void)close(fd);
	return (count);
}

static int
copy_file(const char *src, const char *dst)
{
	FILE *in, *out;
	char buf[4096];
	size_t n;

	if ((in = fopen(src, "rb")) == NULL)
		return (1);
	if ((out = fopen(dst, "wb")) == NULL) {
		(void)fclose(in);
		return (1);
	}
	while ((n = fread(buf, 1, sizeof(buf), in)) > 0)
		if (fwrite(buf, 1, n, out) != n) {
			(void)fclose(in);
			(void)fclose(out);
			return (1);
		}
	(void)fclose(in);
	return (fclose(out) != 0);
}

/*
 * count_records --
 *	Total records, and how many carry the target key bytes.  Read with
 *	the built-in comparison so the count is independent of the code
 *	under test.
 */
static void
count_records(const char *path, int *records, int *dups)
{
	DB *dbp;
	DBC *dbc;
	DBT key, data;
	char kbuf[16];
	int ret;

	*records = *dups = 0;
	if (open_db(&dbp, path, 0, DB_RDONLY) != 0) {
		fails++;
		return;
	}
	CHK0(dbp->cursor(dbp, NULL, &dbc, 0));
	inline_key(kbuf, 0);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	while ((ret = dbc->get(dbc, &key, &data, DB_NEXT)) == 0) {
		(*records)++;
		if (key.size == strlen(kbuf) &&
		    memcmp(key.data, kbuf, key.size) == 0)
			(*dups)++;
	}
	if (ret != DB_NOTFOUND)
		CHK0(ret);
	CHK0(dbc->close(dbc));
	CHK0(dbp->close(dbp, 0));
}

/*
 * check_get --
 *	DB->get must find a key that is on the legacy page.
 */
static void
check_get(const char *path, int custom, int big)
{
	DB *dbp;
	DBT key, data;
	char kbuf[BIGKEYLEN + 1], vbuf[32];
	const char *label;
	u_int32_t ksize;
	int ret;

	label = custom ? "trigger" : "control";
	cmp_calls = cmp_equalities = 0;
	if (count_unsorted(path) < 1) {
		fprintf(stderr,
		    "FAIL: %s: no P_HASH_UNSORTED page left in %s\n",
		    label, path);
		fails++;
		return;
	}
	if (open_db(&dbp, path, custom, DB_RDONLY) != 0) {
		fails++;
		return;
	}
	if (big) {
		big_key(kbuf);
		ksize = BIGKEYLEN;
		(void)snprintf(vbuf, sizeof(vbuf), "offpage-value");
	} else {
		inline_key(kbuf, 0);
		ksize = (u_int32_t)strlen(kbuf);
		(void)snprintf(vbuf, sizeof(vbuf), "balance=0");
	}
	set_dbt(&key, kbuf, ksize);
	memset(&data, 0, sizeof(data));
	ret = dbp->get(dbp, NULL, &key, &data, 0);
	printf("  get %s %s key: ret=%d (%s) cmp_calls=%lu equal=%lu\n",
	    label, big ? "off-page" : "inline", ret,
	    ret == 0 ? "success" : db_strerror(ret), cmp_calls,
	    cmp_equalities);
	CHKEQ(ret, 0, "DB->get of a stored key");
	if (ret == 0 && (data.size != strlen(vbuf) ||
	    memcmp(data.data, vbuf, data.size) != 0)) {
		fprintf(stderr, "FAIL: %s: wrong value for stored key\n",
		    label);
		fails++;
	}
	if (custom && cmp_calls == 0) {
		fprintf(stderr, "FAIL: %s: comparator was never called\n",
		    label);
		fails++;
	}
	CHK0(dbp->close(dbp, 0));
}

/*
 * check_missing --
 *	A key that is not stored must be reported absent.  "prefix" asks for
 *	a proper prefix of a stored key, otherwise for a stored key with
 *	extra bytes appended.  Both probe whether the stored-key DBT handed
 *	to the comparator carries the stored item's real length.
 */
static void
check_missing(const char *path, int custom, int prefix)
{
	DB *dbp;
	DBT key, data;
	char kbuf[32];
	const char *label;
	u_int32_t ksize;
	int ret;

	label = custom ? "trigger" : "control";
	cmp_calls = cmp_equalities = 0;
	if (open_db(&dbp, path, custom, DB_RDONLY) != 0) {
		fails++;
		return;
	}
	inline_key(kbuf, 0);			/* "acct0000" */
	if (prefix)
		ksize = 4;			/* "acct" */
	else {
		(void)strncat(kbuf, "XXXX", sizeof(kbuf) - strlen(kbuf) - 1);
		ksize = (u_int32_t)strlen(kbuf);
	}
	set_dbt(&key, kbuf, ksize);
	memset(&data, 0, sizeof(data));
	ret = dbp->get(dbp, NULL, &key, &data, 0);
	printf("  get %s %s key (%.*s): ret=%d (%s) cmp_calls=%lu equal=%lu\n",
	    label, prefix ? "prefix-of-stored" : "extends-stored",
	    (int)ksize, kbuf, ret,
	    ret == 0 ? "success" : db_strerror(ret), cmp_calls,
	    cmp_equalities);
	CHKEQ(ret, DB_NOTFOUND, "DB->get of a key that is not stored");
	CHK0(dbp->close(dbp, 0));
}

/*
 * check_nooverwrite --
 *	DB->put(DB_NOOVERWRITE) for a key that is present must return
 *	DB_KEYEXIST and leave the record count alone.
 */
static void
check_nooverwrite(const char *base, int custom)
{
	DB *dbp;
	DBT key, data;
	char path[256], kbuf[16];
	const char *label;
	int dups, records, ret;

	label = custom ? "trigger" : "control";
	(void)snprintf(path, sizeof(path), "%s/put_%s.db", HOME, label);
	if (copy_file(base, path) != 0) {
		fprintf(stderr, "FAIL: %s: cannot copy fixture\n", label);
		fails++;
		return;
	}
	cmp_calls = cmp_equalities = 0;
	if (count_unsorted(path) < 1) {
		fprintf(stderr,
		    "FAIL: %s: no P_HASH_UNSORTED page in the copy\n", label);
		fails++;
		return;
	}
	if (open_db(&dbp, path, custom, 0) != 0) {
		fails++;
		return;
	}
	inline_key(kbuf, 0);
	set_dbt(&key, kbuf, (u_int32_t)strlen(kbuf));
	set_dbt(&data, "balance=999999", 14);
	ret = dbp->put(dbp, NULL, &key, &data, DB_NOOVERWRITE);
	CHK0(dbp->close(dbp, 0));
	count_records(path, &records, &dups);
	printf("  put(DB_NOOVERWRITE) %s: ret=%d (%s) records=%d "
	    "with_target_key=%d cmp_calls=%lu equal=%lu\n", label, ret,
	    ret == 0 ? "success" : db_strerror(ret), records, dups,
	    cmp_calls, cmp_equalities);
	CHKEQ(ret, DB_KEYEXIST, "DB->put(DB_NOOVERWRITE) over a live key");
	CHKEQ(records, NRECS + 1, "record count after DB_NOOVERWRITE");
	CHKEQ(dups, 1, "records carrying the target key");
}

int
main(int argc, char *argv[])
{
	char base[256];
	int converted;

	(void)argc;
	(void)argv;

	(void)signal(SIGALRM, SIG_DFL);
	(void)alarm(ALARM_SECS);

	(void)mkdir(HOME, 0755);
	(void)snprintf(base, sizeof(base), "%s/legacy.db", HOME);
	(void)unlink(base);

	if (produce(base) != 0) {
		fprintf(stderr, "FAIL: could not build the base database\n");
		return (EXIT_FAILURE);
	}
	if ((converted = make_legacy(base)) < 1) {
		fprintf(stderr,
		    "FAIL: no hash page converted to P_HASH_UNSORTED\n");
		return (EXIT_FAILURE);
	}
	printf("legacy fixture: %s (%d page(s) -> P_HASH_UNSORTED, "
	    "hash version %d)\n", base, converted, HASH_VERSION_45);

	/* Controls first: they prove the fixture itself is sound. */
	check_get(base, 0, 0);
	check_get(base, 0, 1);
	check_missing(base, 0, 1);
	check_missing(base, 0, 0);
	check_nooverwrite(base, 0);

	/* Triggers: the same operations with a custom comparator. */
	check_get(base, 1, 0);
	check_get(base, 1, 1);
	check_missing(base, 1, 1);
	check_missing(base, 1, 0);
	check_nooverwrite(base, 1);

	if (fails != 0) {
		fprintf(stderr,
		    "hash_unsorted_cmp: %d check(s) FAILED\n", fails);
		return (EXIT_FAILURE);
	}
	printf("hash_unsorted_cmp: PASS\n");
	return (EXIT_SUCCESS);
}
