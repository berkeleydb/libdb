/*-
 * test/fuzz/fuzz_driver.h --
 *	Standalone driver so the harnesses run WITHOUT libFuzzer.
 *
 *	When compiled with `-fsanitize=fuzzer`, libFuzzer supplies main() and
 *	drives LLVMFuzzerTestOneInput.  When libFuzzer is unavailable (or you
 *	just want to replay one saved input under ASan/UBSan), define
 *	FUZZ_STANDALONE at compile time to get a main() that reads each file
 *	argument and feeds its bytes to LLVMFuzzerTestOneInput once.  This is
 *	exactly the OSS-Fuzz "reproduce a testcase" contract and lets the
 *	CI smoke run and crash-replay work with a plain clang -fsanitize=
 *	address,undefined build (no libFuzzer runtime required).
 */

#ifndef FUZZ_DRIVER_H
#define FUZZ_DRIVER_H

int LLVMFuzzerTestOneInput(const unsigned char *data, unsigned long size);

#ifdef FUZZ_STANDALONE
#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char **argv)
{
	int i;

	if (argc < 2) {
		fprintf(stderr, "usage: %s FILE [FILE ...]\n", argv[0]);
		return (2);
	}
	for (i = 1; i < argc; i++) {
		FILE *f;
		long n;
		unsigned char *buf;

		if ((f = fopen(argv[i], "rb")) == NULL) {
			perror(argv[i]);
			return (2);
		}
		(void)fseek(f, 0, SEEK_END);
		n = ftell(f);
		if (n < 0) {
			(void)fclose(f);
			continue;
		}
		(void)fseek(f, 0, SEEK_SET);
		buf = n == 0 ? (unsigned char *)malloc(1)
		    : (unsigned char *)malloc((size_t)n);
		if (buf == NULL) {
			(void)fclose(f);
			return (2);
		}
		if (n > 0 && fread(buf, 1, (size_t)n, f) != (size_t)n) {
			perror(argv[i]);
			free(buf);
			(void)fclose(f);
			return (2);
		}
		(void)fclose(f);
		(void)LLVMFuzzerTestOneInput(buf, (unsigned long)n);
		free(buf);
		fprintf(stderr, "[fuzz-standalone] %s: ok\n", argv[i]);
	}
	return (0);
}
#endif /* FUZZ_STANDALONE */

#endif /* FUZZ_DRIVER_H */
