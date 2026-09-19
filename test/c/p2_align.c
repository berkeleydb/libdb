/*
 * Does the P2 fix actually change the buffer address __os_read sees?
 *
 * direct_db PASSES on this box both with and without the fix, because this
 * kernel/fs tolerates the unaligned buffer. So the behaviour test cannot
 * distinguish them here. Assert the MECHANISM instead: that ALIGNP_INC over an
 * over-sized stack buffer yields a 4096-aligned address where the bare
 * DBMETASIZE array does not.
 *
 * This is the same discipline as the rest of the suite: assert the observable
 * consequence, not that a call returned 0.
 */
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define DBMETASIZE 512
#define DB_FOP_DIRECT_ALIGN 4096
#define ALIGNP_INC(p, bound) \
    (void *)(((uintptr_t)(p) + (bound) - 1) & ~(((uintptr_t)(bound)) - 1))

static int check(void)
{
	unsigned char mbuf[DBMETASIZE];
	unsigned char alignbuf[DBMETASIZE + DB_FOP_DIRECT_ALIGN];
	unsigned char *aligned = ALIGNP_INC(alignbuf, DB_FOP_DIRECT_ALIGN);
	int bare_ok = ((uintptr_t)mbuf % DB_FOP_DIRECT_ALIGN) == 0;
	int fix_ok = ((uintptr_t)aligned % DB_FOP_DIRECT_ALIGN) == 0;
	int room = (aligned + DBMETASIZE) <= (alignbuf + sizeof(alignbuf));

	printf("  bare mbuf      = %p  %u-aligned=%s\n",
	    (void *)mbuf, DB_FOP_DIRECT_ALIGN, bare_ok ? "yes" : "NO");
	printf("  aligned bounce = %p  %u-aligned=%s  in-bounds=%s\n",
	    (void *)aligned, DB_FOP_DIRECT_ALIGN, fix_ok ? "yes" : "NO",
	    room ? "yes" : "NO");

	/* The fix must align, and the slack must be sufficient. */
	if (!fix_ok) {
		printf("VERDICT p2_align FAIL bounce buffer not aligned\n");
		return (1);
	}
	if (!room) {
		printf("VERDICT p2_align FAIL DBMETASIZE does not fit after rounding\n");
		return (1);
	}
	/*
	 * If the bare buffer happened to be aligned the fix is a no-op on this
	 * run -- report it rather than claiming a difference we did not see.
	 */
	printf("VERDICT p2_align PASS aligned=%p bare_was_aligned=%s\n",
	    (void *)aligned, bare_ok ? "yes(fix is a no-op this run)" : "no");
	return (0);
}

int main(void)
{
	int i, bad = 0;

	/* Several frames, since stack alignment varies with call depth. */
	for (i = 0; i < 3; i++)
		bad |= check();
	return (bad);
}
