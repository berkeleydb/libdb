/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * sim_os_hooks.c --
 *	The bridge between the __os_* I/O layer (src/os/os_rw.c,
 *	os_fsync.c) and the DST core (sim_core.c).  These are the exact
 *	functions the guarded #ifdef HAVE_DST call sites in the os layer
 *	invoke.  Each is a single relaxed load (__db_sim_active) in the
 *	common no-sim case, so an --enable-dst library that is NOT running
 *	a sim is behaviourally and ~performance identical to stock.
 *
 *	Only linked into the library under --enable-dst (HAVE_DST).  When
 *	off, the os-layer call sites are #ifdef'd out and this file is not
 *	compiled, so production has zero overhead and no new symbols.
 *
 *	This is the ONE place that translates a DB_FH into the write-back
 *	model's file key: a stable hash of the file name, so the durable
 *	frontier tracks a logical file across libdb's close/reopen (libdb
 *	re-opens log and db files freely; keying on fd would lose the
 *	frontier).
 */

#include "db_config.h"

#include "db_int.h"
#include "sim_rng.h"
#include "sim_fault.h"

/*
 * db_sim_fkey --
 *	Stable 64-bit key for a file handle's logical file (FNV-1a of the
 *	name).  Handles with no name (anonymous temp) get key 0, which the
 *	write-back table treats as a single shared bucket -- fine, since
 *	anonymous files are not the durability-critical WAL/db files a
 *	crash test tracks.
 *
 * PUBLIC: #ifdef HAVE_DST
 * PUBLIC: uint64_t db_sim_fkey __P((DB_FH *));
 * PUBLIC: #endif
 */
uint64_t
db_sim_fkey(fhp)
	DB_FH *fhp;
{
	uint64_t h = 1469598103934665603ull;   /* FNV-1a offset basis */
	const unsigned char *p;

	if (fhp == NULL || fhp->name == NULL)
		return (0);
	for (p = (const unsigned char *)fhp->name; *p != '\0'; p++) {
		h ^= (uint64_t)*p;
		h *= 1099511628211ull;              /* FNV-1a prime */
	}
	return (h);
}

/*
 * __db_sim_io_write_hook --
 *	Called from the write path AFTER the real bytes have been written.
 *	Records the written high-water in the write-back model (so a later
 *	crash can drop everything past the last fsync).  `end_off` is the
 *	byte just past the end of this write.
 *
 * PUBLIC: #ifdef HAVE_DST
 * PUBLIC: void __db_sim_io_write_hook __P((DB_FH *, u_int32_t));
 * PUBLIC: #endif
 */
void
__db_sim_io_write_hook(fhp, end_off)
	DB_FH *fhp;
	u_int32_t end_off;
{
	if (!__db_sim_active() || !__db_sim_wb_active())
		return;
	__db_sim_wb_wrote(db_sim_fkey(fhp), (uint64_t)end_off);
}

/*
 * __db_sim_io_write_off_hook --
 *	As above but takes the 64-bit absolute end offset directly (the
 *	page write path in __os_io computes (pgno*pgsize + io_len)).
 *
 * PUBLIC: #ifdef HAVE_DST
 * PUBLIC: void __db_sim_io_write_off_hook __P((DB_FH *, u_int64_t));
 * PUBLIC: #endif
 */
void
__db_sim_io_write_off_hook(fhp, end_off)
	DB_FH *fhp;
	u_int64_t end_off;
{
	if (!__db_sim_active() || !__db_sim_wb_active())
		return;
	__db_sim_wb_wrote(db_sim_fkey(fhp), end_off);
}

/*
 * __db_sim_io_sync_hook --
 *	Called from __os_fsync after a successful fsync/fdatasync: promotes
 *	this file's written extent to durable in the write-back model.
 *
 * PUBLIC: #ifdef HAVE_DST
 * PUBLIC: void __db_sim_io_sync_hook __P((DB_FH *));
 * PUBLIC: #endif
 */
void
__db_sim_io_sync_hook(fhp)
	DB_FH *fhp;
{
	if (!__db_sim_active() || !__db_sim_wb_active())
		return;
	__db_sim_wb_synced(db_sim_fkey(fhp));
}

/*
 * __db_sim_io_read_hook --
 *	Called from the read path AFTER bytes land in the caller's buffer.
 *	On a seeded corrupt-read coin, flips one bit of one returned byte --
 *	a silent corrupt read the engine's page checksum must catch.  A
 *	no-op unless a sim armed the corrupt knob.
 *
 * PUBLIC: #ifdef HAVE_DST
 * PUBLIC: void __db_sim_io_read_hook __P((void *, size_t));
 * PUBLIC: #endif
 */
void
__db_sim_io_read_hook(buf, len)
	void *buf;
	size_t len;
{
	int fb;

	if (!__db_sim_active() || buf == NULL || len == 0)
		return;
	fb = __db_sim_io_flip_byte((int)(len > 0x7fffffff ? 0x7fffffff : len));
	if (fb >= 0)
		((unsigned char *)buf)[fb] ^= 0x40;
}
