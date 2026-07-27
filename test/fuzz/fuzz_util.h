/*-
 * test/fuzz/fuzz_util.h --
 *	Tiny shared helpers for the libdb fuzz harnesses: a fresh per-input
 *	scratch directory (so nothing leaks between inputs) and a helper to
 *	drop the fuzz bytes into a file inside it.  Header-only; each harness
 *	includes it once.
 *
 *	Determinism / isolation contract:
 *	  - fuzz_scratch_make() returns a unique dir under TMPDIR and mkdir's
 *	    it; fuzz_scratch_rm() removes it recursively.
 *	  - Each LLVMFuzzerTestOneInput() call makes its own dir and removes
 *	    it before returning, so no global state survives an input.
 */

#ifndef FUZZ_UTIL_H
#define FUZZ_UTIL_H

#include <sys/stat.h>
#include <sys/types.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* Unique scratch dir for one input.  buf must be >= 256 bytes. */
static int
fuzz_scratch_make(char *buf, size_t buflen)
{
	const char *tmp = getenv("TMPDIR");
	static unsigned long counter = 0;

	if (tmp == NULL || *tmp == '\0')
		tmp = "/tmp";
	(void)snprintf(buf, buflen, "%s/libdb_fuzz_%d_%lu_XXXXXX",
	    tmp, (int)getpid(), counter++);
	if (mkdtemp(buf) == NULL)
		return (-1);
	return (0);
}

/* Recursively remove a scratch dir made by fuzz_scratch_make(). */
static void
fuzz_scratch_rm(const char *dir)
{
	char cmd[512];

	/* dir is our own mkdtemp output, never attacker-controlled. */
	(void)snprintf(cmd, sizeof(cmd), "rm -rf '%s'", dir);
	(void)system(cmd);
}

/* Write size bytes to dir/name.  Returns 0 on success. */
static int fuzz_write_file(const char *dir, const char *name,
    const unsigned char *data, unsigned long size) __attribute__((unused));
static int
fuzz_write_file(const char *dir, const char *name,
    const unsigned char *data, unsigned long size)
{
	char path[512];
	FILE *f;

	(void)snprintf(path, sizeof(path), "%s/%s", dir, name);
	if ((f = fopen(path, "wb")) == NULL)
		return (-1);
	if (size > 0 && fwrite(data, 1, (size_t)size, f) != (size_t)size) {
		(void)fclose(f);
		return (-1);
	}
	(void)fclose(f);
	return (0);
}

#endif /* FUZZ_UTIL_H */
