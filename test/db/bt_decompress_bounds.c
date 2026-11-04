/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * bt_decompress_bounds.c --
 *	Regression test for the out-of-bounds read in __bam_defdecompress
 *	(src/btree/bt_compress.c), the default compressed-btree key/data
 *	decoder.  The decoder read each varint's leading byte -- and the magic
 *	byte *s at offset 0 -- BEFORE checking the compressed buffer was long
 *	enough:
 *
 *	    s = compressed->data;
 *	    if (*s == CMP_INT_SPARE_VAL) { ... }            (*s, no size check)
 *	    size = __db_decompress_count_int(s);            (reads s[0])
 *	    if (size > compressed->size) return (EINVAL);   (check is AFTER)
 *
 *	A compressed btree's pages come from disk, so a corrupt or truncated
 *	compressed item -- a trust boundary -- over-reads up to one byte past
 *	compressed->size at each varint site, and reads the first byte of a
 *	zero-length item with no check at all.  This was proved over every
 *	bounded input by test/cbmc/harness_bt_decompress.c; the fix adds a
 *	bound-before-read check at each site.
 *
 *	To catch the over-read on ANY build -- not only under AddressSanitizer
 *	-- the compressed item is placed flush against the end of a mapped page
 *	whose NEXT page is PROT_NONE (a guard page).  Any read at byte offset
 *	>= csize then faults with SIGSEGV, which the parent process observes as
 *	the child dying on a signal.  With the fix the decoder returns EINVAL
 *	before any such read and the child exits 0.  (An ASan build traps the
 *	same over-read as a heap-buffer-overflow even earlier; either way the
 *	defect is caught and the fix is clean.)
 *
 *	Cases (the shapes the CBMC counterexample found):
 *	  case A: a zero-length compressed item     -> *s reads guard[0].
 *	  case B: a one-byte item, one 1-byte varint -> the first int32 consumes
 *	          the buffer, then the second count_int reads guard[0].
 *	  case C: every varint fits but prefix + suffix wraps (normal path), so
 *	          the size checks pass and memcpy runs with ~4 GB.
 *	  case D: the same wrap on the duplicate (0xFC) path.
 *	  case E: a real compressed btree whose first record's length varint is
 *	          corrupted on disk; __bamc_start_decompress copied that length
 *	          out of a 4 KB page.  Uses only the public API.
 *
 *	Each case runs in a forked child; the parent checks the child returned
 *	the EINVAL verdict and did NOT die on a signal.
 *
 *	__bam_defdecompress is an internal (non-public) entry point, so its
 *	prototype is declared here; the DB * argument is only passed to
 *	COMPQUIET and is never dereferenced, so a dummy pointer is fine.
 */
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "db.h"

/* Smallest byte value the integer codec reserves as the duplicate magic. */
#define	CMP_INT_SPARE_VAL	0xFC

/* Internal entry point (btree_ext.h); declared here so a public-header test
 * can call it.  Signature copied from src/dbinc_auto/btree_ext.h. */
int __bam_defdecompress __P((DB *, const DBT *, const DBT *,
    DBT *, DBT *, DBT *));
int __db_compress_int __P((u_int8_t *, u_int64_t));

/*
 * Run __bam_defdecompress against a compressed item of EXACTLY csize bytes,
 * placed flush against a PROT_NONE guard page so any over-read SIGSEGVs.
 * Returns the decoder's rc; a defect's over-read never returns (the child
 * dies on SIGSEGV, or ASan aborts).
 */
static int
decode_truncated(const u_int8_t *bytes, u_int32_t csize)
{
	DBT prevKey, prevData, compressed, destKey, destData;
	u_int8_t kbuf[64], dbuf[64], pk[8], pd[8];
	u_int8_t *region, *guard, *cbuf;
	long pg;
	int rc;

	pg = sysconf(_SC_PAGESIZE);
	if (pg <= 0)
		pg = 4096;

	/* Two pages: a readable first page, a PROT_NONE guard second page. */
	region = mmap(NULL, (size_t)pg * 2, PROT_READ | PROT_WRITE,
	    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (region == MAP_FAILED)
		return (ENOMEM);
	guard = region + pg;
	if (mprotect(guard, (size_t)pg, PROT_NONE) != 0) {
		munmap(region, (size_t)pg * 2);
		return (ENOMEM);
	}

	/* Place the csize-byte item so its last byte is the last readable byte
	 * before the guard page; a read of item[csize] hits the guard. */
	cbuf = guard - csize;
	if (csize != 0)
		memcpy(cbuf, bytes, csize);

	memset(&prevKey, 0, sizeof(prevKey));
	memset(&prevData, 0, sizeof(prevData));
	memset(&compressed, 0, sizeof(compressed));
	memset(&destKey, 0, sizeof(destKey));
	memset(&destData, 0, sizeof(destData));

	memset(pk, 0, sizeof(pk));
	memset(pd, 0, sizeof(pd));
	prevKey.data = pk; prevKey.size = sizeof(pk);
	prevData.data = pd; prevData.size = sizeof(pd);

	compressed.data = cbuf;
	compressed.size = csize;
	compressed.ulen = csize;

	destKey.data = kbuf; destKey.ulen = sizeof(kbuf);
	destData.data = dbuf; destData.ulen = sizeof(dbuf);

	rc = __bam_defdecompress((DB *)NULL, &prevKey, &prevData,
	    &compressed, &destKey, &destData);

	munmap(region, (size_t)pg * 2);
	return (rc);
}

/* Run one case in a child; return 0 if the child returned the EINVAL verdict
 * cleanly, non-zero if it died on a signal (the over-read) or returned the
 * wrong code. */
static int
run_case(const char *name, const u_int8_t *bytes, u_int32_t csize)
{
	pid_t pid;
	int status, rc;

	pid = fork();
	if (pid < 0) {
		printf("%s: fork failed\n", name);
		return (1);
	}
	if (pid == 0) {
		rc = decode_truncated(bytes, csize);
		/* The child signals the verdict via its exit code: 0 == EINVAL
		 * (the fix's clean rejection), 2 == any other return. */
		_exit(rc == EINVAL ? 0 : 2);
	}
	if (waitpid(pid, &status, 0) != pid) {
		printf("%s: waitpid failed\n", name);
		return (1);
	}
	if (WIFSIGNALED(status)) {
		printf("%s: child died on signal %d (the over-read)\n",
		    name, WTERMSIG(status));
		return (1);
	}
	if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
		printf("%s: child exited %d (expected EINVAL verdict)\n",
		    name, WIFEXITED(status) ? WEXITSTATUS(status) : -1);
		return (1);
	}
	return (0);
}

/*
 * Case E: a compressed btree whose first record's data-length varint is
 * corrupted on disk to ~257 MB.  __bamc_start_decompress copied that many
 * bytes out of a 4 KB page.  The fix returns EINVAL from the cursor get.
 */
#define	PAGE_MARK	"VVVVVVVVVVVVVVVV"
static int
run_page_case(void)
{
	DB *dbp;
	DBT key, data;
	FILE *fp;
	char dir[] = "/tmp/btdecompXXXXXX", path[64];
	u_int8_t *buf, *p;
	long len;
	pid_t pid;
	int found, ret, status;

	if (mkdtemp(dir) == NULL)
		return (1);
	snprintf(path, sizeof(path), "%s/c.db", dir);

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = "k"; key.size = 1;
	data.data = PAGE_MARK; data.size = sizeof(PAGE_MARK) - 1;
	if ((ret = db_create(&dbp, NULL, 0)) != 0 ||
	    (ret = dbp->set_bt_compress(dbp, NULL, NULL)) != 0 ||
	    (ret = dbp->open(dbp, NULL, path, NULL, DB_BTREE,
	    DB_CREATE, 0600)) != 0 ||
	    (ret = dbp->put(dbp, NULL, &key, &data, 0)) != 0 ||
	    (ret = dbp->close(dbp, 0)) != 0) {
		printf("case E: setup failed: %s\n", db_strerror(ret));
		return (1);
	}

	/* The first record is stored as [varint 16]["VVVV..."]. */
	if ((fp = fopen(path, "r+b")) == NULL)
		return (1);
	(void)fseek(fp, 0, SEEK_END);
	len = ftell(fp);
	rewind(fp);
	buf = malloc((size_t)len);
	if (buf == NULL || fread(buf, 1, (size_t)len, fp) != (size_t)len)
		return (1);
	found = 0;
	for (p = buf; p + 17 <= buf + len; p++)
		if (p[0] == 16 && memcmp(p + 1, PAGE_MARK, 16) == 0) {
			p[0] = 0xEF;	/* 4-byte varint: ~0x0F565656 */
			found++;
		}
	if (found != 1) {
		printf("case E: found %d copies of the record\n", found);
		return (1);
	}
	rewind(fp);
	(void)fwrite(buf, 1, (size_t)len, fp);
	(void)fclose(fp);
	free(buf);

	pid = fork();
	if (pid < 0)
		return (1);
	if (pid == 0) {
		if (db_create(&dbp, NULL, 0) != 0 ||
		    dbp->set_bt_compress(dbp, NULL, NULL) != 0 ||
		    dbp->open(dbp, NULL, path, NULL, DB_BTREE, 0, 0) != 0)
			_exit(3);
		memset(&data, 0, sizeof(data));
		data.flags = DB_DBT_MALLOC;
		ret = dbp->get(dbp, NULL, &key, &data, 0);
		_exit(ret == EINVAL ? 0 : 2);
	}
	if (waitpid(pid, &status, 0) != pid)
		return (1);
	(void)unlink(path);
	(void)rmdir(dir);
	if (WIFSIGNALED(status)) {
		printf("case E (corrupt first-record length): child died on "
		    "signal %d (the over-read)\n", WTERMSIG(status));
		return (1);
	}
	if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
		printf("case E (corrupt first-record length): child exited %d "
		    "(expected EINVAL)\n",
		    WIFEXITED(status) ? WEXITSTATUS(status) : -1);
		return (1);
	}
	return (0);
}

int
main(int argc, char *argv[])
{
	u_int8_t caseA[1];	/* unused payload; csize 0 */
	u_int8_t caseB[1], caseC[32], caseD[32];
	u_int32_t n;
	int nfail;

	(void)argc; (void)argv;

	nfail = 0;

	/*
	 * Case A: a zero-length compressed item.  The decoder reads *s with no
	 * size check -- that byte is on the guard page.  The fix returns
	 * EINVAL (compressed->size < 1) before the read.
	 */
	caseA[0] = 0;
	nfail += run_case("case A (size 0)", caseA, 0);

	/*
	 * Case B: a one-byte item holding a single 1-byte varint (0x05 < 0x80).
	 * The non-dup path's first count_int=1, size=1 (== compressed->size, so
	 * the "size > size" check passes), int32 reads data[0] and advances s;
	 * then the SECOND count_int reads data[1] -- on the guard page.  The fix
	 * rejects at "size >= compressed->size" before that read.
	 */
	caseB[0] = 0x05;
	nfail += run_case("case B (size 1)", caseB, 1);

	/*
	 * Cases C and D: every varint fits, but prefix + suffix wraps.  With
	 * prefix 4 and suffix 0xFFFFFFFE the destination size computes to 2
	 * and "size += suffix" wraps back under compressed->size, so the old
	 * decoder passed every check and then copied ~4 GB.  C is the normal
	 * path, D the duplicate (0xFC) path.
	 */
	n = 0;
	n += __db_compress_int(caseC + n, 4);
	n += __db_compress_int(caseC + n, 0xFFFFFFFEu);
	n += __db_compress_int(caseC + n, 0);
	memset(caseC + n, 'z', 8);
	nfail += run_case("case C (prefix+suffix wraps)", caseC, n + 8);

	n = 0;
	caseD[n++] = CMP_INT_SPARE_VAL;
	n += __db_compress_int(caseD + n, 4);
	n += __db_compress_int(caseD + n, 0xFFFFFFFEu);
	memset(caseD + n, 'z', 8);
	nfail += run_case("case D (dup prefix+suffix wraps)", caseD, n + 8);

	/* Case E: the first record's length on a real page (public API). */
	nfail += run_page_case();

	printf("VERDICT bt_decompress_bounds %s\n", nfail ? "FAIL" : "PASS");
	return (nfail != 0);
}
