/*
 * P3's mechanism check: does __log_write_direct's staging arithmetic actually
 * satisfy all THREE O_DIRECT constraints, and does the file end up holding the
 * same bytes the plain unaligned write would have left there?
 *
 * WHY THIS EXISTS.  flag_behaviour's direct_log mode asserts O_DIRECT is set on
 * the log fd and that the transactional open completes -- the right assertion,
 * and on this hardware it does distinguish fixed from broken (the unfixed path
 * fails EINVAL here, which P2's direct_db did NOT on the same box).  But that
 * is luck of the kernel, and /nvme's logical sector size is 512, so a passing
 * run only proves 512-alignment.  The code claims 4096.  And no behaviour test
 * can prove the restaged bytes are the RIGHT bytes -- a staging loop that wrote
 * correctly-aligned garbage would pass every alignment check and silently
 * corrupt the log.
 *
 * So this asserts the mechanism directly, on a model of the loop in
 * src/log/log_put.c:__log_write_direct, over a simulated file:
 *
 *   1. buffer address:  ALIGNP_INC over a 2*BLOCK stack buffer is BLOCK-aligned
 *                       and leaves a whole block of room, where a bare array is
 *                       not aligned.
 *   2. file offset:     every emitted write starts on a BLOCK multiple.
 *   3. length:          every emitted write is exactly BLOCK bytes.
 *   4. recoverability:  after a sequence of appends at arbitrary byte offsets
 *                       and lengths, the file's first w_off+len bytes are
 *                       byte-for-byte what the unaligned writes would have
 *                       produced, and everything past the frontier is zero
 *                       (the padding, which the reader already treats as
 *                       end-of-log).
 *
 * Point 4 is the one the behaviour test cannot reach, and it is the one whose
 * failure loses committed data.
 *
 * Model risk is acknowledged: this duplicates the arithmetic rather than calling
 * it (__log_write_direct is static and needs a live DB_LOG and region).  The
 * runner pairs it with an strace gate that asserts the REAL library's writes to
 * the real log file are block-aligned in offset and length, which cannot drift.
 */
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define	BLOCK		4096		/* DB_LG_DIRECT_ALIGN */
#define	FILEBYTES	(64 * 1024)
#define	ALIGNP_INC(p, bound) \
    (void *)(((uintptr_t)(p) + (bound) - 1) & ~(((uintptr_t)(bound)) - 1))

/* The simulated log file, and the reference copy built by plain writes. */
static unsigned char sim[FILEBYTES], ref[FILEBYTES];
static int nwrites, bad_addr, bad_off, bad_len;

/*
 * emit -- stand in for __os_io(DB_IO_WRITE) on an O_DIRECT descriptor, checking
 * what the kernel would check and refusing to record a write that violates it.
 */
static void
emit(unsigned char *buf, unsigned int off, unsigned int len)
{
	nwrites++;
	if (((uintptr_t)buf % BLOCK) != 0) { bad_addr++; return; }
	if ((off % BLOCK) != 0)            { bad_off++;  return; }
	if (len != BLOCK)                  { bad_len++;  return; }
	if (off + len > FILEBYTES) { printf("  model overflow\n"); exit(2); }
	memcpy(sim + off, buf, len);
}

/*
 * stage_write -- the body of __log_write_direct, kept deliberately parallel to
 * src/log/log_put.c so a reader can diff them by eye.
 */
static void
stage_write(unsigned int w_off, const unsigned char *addr, unsigned int len)
{
	unsigned char stagebuf[2 * BLOCK];
	unsigned char *stage = ALIGNP_INC(stagebuf, BLOCK);
	unsigned int base, head, n, off, total;

	/*
	 * The real function's stack contents at this address are indeterminate,
	 * so the model must represent "arbitrary", not "whatever the previous
	 * call left".  Without this poison, dropping the read-back below still
	 * passed every check -- stagebuf lands at the same stack address each
	 * call and happened to still hold the block we wanted.  That mutant is
	 * the one that would corrupt already-durable log bytes, so the test must
	 * be able to see it.
	 */
	memset(stagebuf, 0xA5, sizeof(stagebuf));

	base = w_off & ~(unsigned int)(BLOCK - 1);
	head = w_off - base;
	total = head + len;

	/* Read back the leading block (the file already holds those bytes). */
	if (head != 0)
		memcpy(stage, sim + base, BLOCK);

	for (off = 0; off < total; off += n) {
		n = total - off;
		if (n > BLOCK)
			n = BLOCK;
		if (off == 0)
			memcpy(stage + head, addr, n - head);
		else
			memcpy(stage, addr + (off - head), n);
		if (n != BLOCK)
			memset(stage + n, 0, BLOCK - n);
		emit(stage, base + off, BLOCK);
	}
}

int
main(void)
{
	/* Constraint 1 -- the buffer address, in the shape p2_align asserts. */
	unsigned char bare[BLOCK];
	unsigned char stagebuf[2 * BLOCK];
	unsigned char *stage = ALIGNP_INC(stagebuf, BLOCK);
	int bare_ok = ((uintptr_t)bare % BLOCK) == 0;
	int stage_ok = ((uintptr_t)stage % BLOCK) == 0;
	int room = (stage + BLOCK) <= (stagebuf + sizeof(stagebuf));

	/* A run of appends: sub-block, block-crossing, multi-block, exact. */
	static const unsigned int lens[] =
	    { 131, 12, 4000, 4096, 1, 8192, 37, 12000, 4095, 4097, 500 };
	unsigned char rec[16384];
	unsigned int w_off, i, j, frontier;

	printf("  bare array     = %p  %d-aligned=%s\n",
	    (void *)bare, BLOCK, bare_ok ? "yes" : "NO");
	printf("  staged buffer  = %p  %d-aligned=%s  block-of-room=%s\n",
	    (void *)stage, BLOCK, stage_ok ? "yes" : "NO", room ? "yes" : "NO");
	if (!stage_ok || !room) {
		printf("VERDICT p3_align FAIL staging buffer not %d-aligned "
		    "or short of room\n", BLOCK);
		return (1);
	}

	/* Constraints 2-4 -- offsets, lengths, and the resulting bytes. */
	memset(sim, 0, sizeof(sim));
	memset(ref, 0, sizeof(ref));
	w_off = 0;
	for (i = 0; i < sizeof(lens) / sizeof(lens[0]); i++) {
		unsigned int len = lens[i];

		if (w_off + len > FILEBYTES)
			break;
		/* Distinguishable payload, so a misplaced copy shows up. */
		for (j = 0; j < len; j++)
			rec[j] = (unsigned char)(1 + ((i * 7 + j) % 251));
		memcpy(ref + w_off, rec, len);	/* what a plain write leaves */
		stage_write(w_off, rec, len);
		w_off += len;			/* exactly as __log_write does */
	}
	frontier = w_off;

	printf("  %d staged writes, frontier=%u: bad_addr=%d bad_off=%d "
	    "bad_len=%d\n", nwrites, frontier, bad_addr, bad_off, bad_len);
	if (bad_addr != 0 || bad_off != 0 || bad_len != 0) {
		printf("VERDICT p3_align FAIL a staged write violated "
		    "O_DIRECT (addr/off/len)\n");
		return (1);
	}
	if (nwrites == 0) {
		printf("VERDICT p3_align FAIL no writes were staged -- "
		    "nothing was measured\n");
		return (1);
	}

	/* Every byte below the frontier must match the unaligned reference. */
	for (i = 0; i < frontier; i++)
		if (sim[i] != ref[i]) {
			printf("VERDICT p3_align FAIL log byte %u is %02x, "
			    "expected %02x -- restaging LOST DATA\n",
			    i, sim[i], ref[i]);
			return (1);
		}
	/* Past the frontier only padding, and padding must be zero. */
	for (i = frontier; i < ((frontier + BLOCK - 1) / BLOCK) * BLOCK; i++)
		if (sim[i] != 0) {
			printf("VERDICT p3_align FAIL pad byte %u is %02x, "
			    "not zero -- the reader would see garbage past "
			    "end-of-log\n", i, sim[i]);
			return (1);
		}

	printf("VERDICT p3_align PASS %d writes all %d-aligned in "
	    "addr/off/len, %u bytes below the frontier identical to the "
	    "unaligned reference, padding zero, bare_was_aligned=%s\n",
	    nwrites, BLOCK, frontier, bare_ok ? "yes" : "no");
	return (0);
}
