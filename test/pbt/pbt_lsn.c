/*-
 * test/pbt/pbt_lsn.c
 *	Property-based tests for the log-sequence-number (LSN) wire codec
 *	in src/dbinc/db_swap.h: LOGCOPY_FROMLSN / LOGCOPY_TOLSN (and the
 *	LOGCOPY_32 primitive they are built on).
 *
 * A DB_LSN is a structured record of two 32-bit fields (file, offset).
 * The log is written in a fixed little-endian-on-disk wire form so a
 * database is portable across endianness (source comment: "We write logs
 * in little endian format to minimize disruption on x86 ...").  The
 * codec chooses per-field between a byte-reversing copy (P_32_COPYSWAP)
 * and a straight memcpy based on LOG_SWAPPED(env) == !ENV_LITTLEENDIAN.
 *
 * This extends the scalar-swap coverage in pbt_byteswap.c to a whole
 * *structured record*: the two-field LSN, marshalled to and from an
 * (optionally unaligned) 8-byte wire buffer.
 *
 * Contracts (grounded in the macro bodies, not restated):
 *   lsn_roundtrip       -- FROMLSN(write) then TOLSN(read) is the identity
 *                          for any (file,offset), in EITHER endian mode.
 *   wire_is_canonical   -- when the env flag correctly describes the host,
 *                          the on-wire bytes are little-endian regardless
 *                          of the host's native byte order -- that is what
 *                          makes a log portable.  (The host endianness is
 *                          detected at run time so the env flag is honest;
 *                          you cannot fake the other endianness on one host
 *                          because the swap is relative to native order.)
 *   field_order         -- TOLSN puts wire bytes [0..3] into .file and
 *                          [4..7] into .offset: swapping the two LSN fields
 *                          swaps the two 4-byte halves of the wire buffer.
 *   lsn_roundtrip_unaligned -- the round-trip still holds when the wire
 *                          buffer sits at an arbitrary byte offset (the
 *                          codec is byte-at-a-time, so it must not assume
 *                          4-byte alignment).
 *
 * These are header macros (no libdb symbol), like pbt_byteswap.c, but
 * real on-disk-format code exercised on every cross-endian log replay.
 * F_ISSET(env, ENV_LITTLEENDIAN) is the only field the macros read, so a
 * one-field local struct stands in for ENV exactly.
 */

#include <string.h>

#include "db.h"		/* DB_LSN, u_int8_t / u_int32_t */

#include "dbinc/db_swap.h"

#include "pbt_common.h"

/*
 * The LOGCOPY_* macros only ever read `env->flags` (via F_ISSET /
 * LOG_SWAPPED).  A single-field struct is a faithful stand-in; using it
 * avoids pulling in the full ENV definition from db_int.h.  ENV_LITTLEENDIAN
 * is 0x4 (src/dbinc/db_int.in).
 */
#ifndef ENV_LITTLEENDIAN
#define ENV_LITTLEENDIAN 0x00000004
#endif
#ifndef F_ISSET
#define F_ISSET(p, f) ((p)->flags & (f))
#endif
typedef struct { u_int32_t flags; } pbt_env;

#if defined(PBT_HAVE_HEGEL)

static void
draw_lsn(hegel_test_case *tc, DB_LSN *lsn)
{
	lsn->file = (u_int32_t)hegel_draw_int(tc,
	    hegel_integers(0, 0xFFFFFFFFLL));
	lsn->offset = (u_int32_t)hegel_draw_int(tc,
	    hegel_integers(0, 0xFFFFFFFFLL));
}

/*
 * The env flag must reflect the TRUE host byte order: LOG_SWAPPED(env) is
 * relative to native order, so faking the other endianness on one host
 * produces a lie.  Detect it once at run time.
 */
static u_int32_t
host_env_flags(void)
{
	u_int32_t probe = 1;
	return (*(u_int8_t *)&probe == 1) ? ENV_LITTLEENDIAN : 0;
}

/* P1: FROMLSN then TOLSN is the identity, in either endian mode. */
static void
prop_lsn_roundtrip(hegel_test_case *tc, void *u)
{
	pbt_env env;
	DB_LSN in, out;
	u_int8_t wire[8];
	(void)u;

	env.flags = hegel_draw_bool(tc, hegel_booleans()) ?
	    ENV_LITTLEENDIAN : 0;
	draw_lsn(tc, &in);

	LOGCOPY_FROMLSN(&env, wire, &in);
	LOGCOPY_TOLSN(&env, &out, wire);
	PBT_CHECK(out.file == in.file && out.offset == in.offset,
	    "LSN wire round-trip lost data");
}

/*
 * P2: with an HONEST env flag (matching the host), the wire form of an
 * LSN is little-endian: file in bytes [0..3] LE, offset in [4..7] LE.
 * This is the portable on-disk contract -- an x86 and a big-endian host
 * that both describe themselves correctly emit these same bytes.
 */
static void
prop_wire_is_canonical(hegel_test_case *tc, void *u)
{
	pbt_env env;
	DB_LSN in;
	u_int8_t wire[8], want[8];
	(void)u;

	env.flags = host_env_flags();
	draw_lsn(tc, &in);

	LOGCOPY_FROMLSN(&env, wire, &in);

	/* Expected little-endian marshalling, computed independently. */
	want[0] = (u_int8_t)(in.file & 0xFF);
	want[1] = (u_int8_t)((in.file >> 8) & 0xFF);
	want[2] = (u_int8_t)((in.file >> 16) & 0xFF);
	want[3] = (u_int8_t)((in.file >> 24) & 0xFF);
	want[4] = (u_int8_t)(in.offset & 0xFF);
	want[5] = (u_int8_t)((in.offset >> 8) & 0xFF);
	want[6] = (u_int8_t)((in.offset >> 16) & 0xFF);
	want[7] = (u_int8_t)((in.offset >> 24) & 0xFF);

	PBT_CHECK(memcmp(wire, want, 8) == 0,
	    "LSN wire form is not little-endian canonical");
}

/*
 * P3: field order -- .file occupies wire[0..3] and .offset wire[4..7],
 * so swapping the two LSN fields swaps the two 4-byte halves of the wire.
 * (Independent of endianness: it only checks which field lands where.)
 */
static void
prop_field_order(hegel_test_case *tc, void *u)
{
	pbt_env env;
	DB_LSN in, swapped;
	u_int8_t w1[8], w2[8];
	(void)u;

	env.flags = host_env_flags();
	draw_lsn(tc, &in);
	swapped.file = in.offset;
	swapped.offset = in.file;

	LOGCOPY_FROMLSN(&env, w1, &in);
	LOGCOPY_FROMLSN(&env, w2, &swapped);

	/* first half of w2 == second half of w1, and vice versa */
	PBT_CHECK(memcmp(w2, w1 + 4, 4) == 0 && memcmp(w2 + 4, w1, 4) == 0,
	    "LSN fields not marshalled file-then-offset");
}

/* P4: round-trip still holds through an unaligned wire buffer. */
static void
prop_lsn_roundtrip_unaligned(hegel_test_case *tc, void *u)
{
	pbt_env env;
	DB_LSN in, out;
	u_int8_t buf[16];
	u_int8_t *wire;
	int off;
	(void)u;

	env.flags = host_env_flags();
	off = (int)hegel_draw_int(tc, hegel_integers(0, 8));
	wire = buf + off;
	draw_lsn(tc, &in);

	LOGCOPY_FROMLSN(&env, wire, &in);
	LOGCOPY_TOLSN(&env, &out, wire);
	PBT_CHECK(out.file == in.file && out.offset == in.offset,
	    "unaligned LSN wire round-trip lost data");
}

static const pbt_entry_t tests[] = {
	{ "lsn_roundtrip",           prop_lsn_roundtrip,           500 },
	{ "wire_is_canonical",       prop_wire_is_canonical,       500 },
	{ "field_order",             prop_field_order,             500 },
	{ "lsn_roundtrip_unaligned", prop_lsn_roundtrip_unaligned, 500 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "lsn_roundtrip",           NULL, 0 },
	{ "wire_is_canonical",       NULL, 0 },
	{ "field_order",             NULL, 0 },
	{ "lsn_roundtrip_unaligned", NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("lsn", tests)
