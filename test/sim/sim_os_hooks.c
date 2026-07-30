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
#include "sim_clock.h"

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
	uint64_t key;

	if (!__db_sim_active() || !__db_sim_wb_active())
		return;
	key = db_sim_fkey(fhp);
	__db_sim_wb_wrote(key, end_off);
	if (fhp != NULL && fhp->name != NULL)
		__db_sim_wb_note_name(key, fhp->name);
}

/*
 * __db_sim_io_write_fault_hook --
 *	Called from the write fast path BEFORE the real bytes are written,
 *	with the intended transfer length.  Consults the seeded write-side
 *	fault knobs and returns:
 *	   -1  ENOSPC: the write must fail, nothing persists;
 *	  >=0  the number of bytes to actually persist (a torn write
 *	       persists a strict prefix < len but the caller still reports
 *	       full success, leaving a latent bad tail a checksum must catch;
 *	       len itself means "write it all", the common case).
 *	A no-op (returns len) unless a sim armed the knobs.
 *
 * PUBLIC: #ifdef HAVE_DST
 * PUBLIC: int __db_sim_io_write_fault_hook __P((DB_FH *, u_int32_t));
 * PUBLIC: #endif
 */
int
__db_sim_io_write_fault_hook(fhp, len)
	DB_FH *fhp;
	u_int32_t len;
{
	COMPQUIET(fhp, NULL);
	if (!__db_sim_active())
		return ((int)len);
	if (__db_sim_io_enospc())
		return (-1);
	/* Short-transfer / EIO: a seeded whole-write failure distinct from
	 * ENOSPC (models a transient device error).  Returns -2 => the
	 * caller reports EIO, nothing persists. */
	if (__db_sim_io_should_fault())
		return (-2);
	return (__db_sim_io_torn_prefix((int)len));
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
__db_sim_io_read_hook(fhp, off, buf, len)
	DB_FH *fhp;
	u_int64_t off;
	void *buf;
	size_t len;
{
	int fb, ilen;

	if (!__db_sim_active() || buf == NULL || len == 0)
		return;
	ilen = (int)(len > 0x7fffffff ? 0x7fffffff : len);
	/* Stale read: return a prior (superseded) version of this exact
	 * (file,offset) if one is in the ring and the seeded coin fires. */
	(void)__db_sim_io_stale_read(db_sim_fkey(fhp), off, buf, ilen);
	/* Corrupt read: silently flip a byte the checksum must catch. */
	fb = __db_sim_io_flip_byte(ilen);
	if (fb >= 0)
		((unsigned char *)buf)[fb] ^= 0x40;
}

/*
 * __db_sim_io_presnapshot_hook --
 *	Called from the write fast path BEFORE overwriting (fhp, off, len).
 *	When the stale-read model is armed it reads the bytes CURRENTLY on
 *	disk at this (file,offset) and records them in the stale-read ring,
 *	so a later seeded stale read can hand back this now-prior version.
 *	A no-op (no pread, no cost) unless stale injection is armed -- so it
 *	never perturbs a non-stale run.
 *
 * PUBLIC: #ifdef HAVE_DST
 * PUBLIC: void __db_sim_io_presnapshot_hook __P((DB_FH *, u_int64_t, u_int32_t));
 * PUBLIC: #endif
 */
void
__db_sim_io_presnapshot_hook(fhp, off, len)
	DB_FH *fhp;
	u_int64_t off;
	u_int32_t len;
{
	unsigned char snap[512];
	size_t cp;
	ssize_t nr;

	/* Only pay for the read-before-write when stale injection is armed
	 * (the ring is otherwise inert), and only for a real named file. */
	if (!__db_sim_active() || !__db_sim_io_stale_armed() ||
	    fhp == NULL || fhp->fd < 0 || len == 0)
		return;
	cp = len < sizeof(snap) ? (size_t)len : sizeof(snap);
	nr = pread(fhp->fd, snap, cp, (off_t)off);
	if (nr <= 0)
		return;                 /* nothing there yet: no prior version */
	__db_sim_io_stale_record(db_sim_fkey(fhp), off, snap, (int)nr);
}

/*
 * __db_sim_io_latency_hook --
 *	Consume the seeded per-I/O latency knob (a tiny capped sleep when
 *	armed; a no-op otherwise).
 *
 * PUBLIC: #ifdef HAVE_DST
 * PUBLIC: void __db_sim_io_latency_hook __P((void));
 * PUBLIC: #endif
 */
void
__db_sim_io_latency_hook()
{
	if (!__db_sim_active())
		return;
	__db_sim_io_sleep_latency();
}

/*
 * __db_sim_clock_hook --
 *	Called from __os_gettime AFTER a real clock reading has landed in
 *	`tp`.  When the clock-skew fault is armed, applies the seeded skew
 *	(fixed offset + jitter + occasional forward/backward jump) in place;
 *	a no-op (leaves tp untouched, ~zero cost) otherwise.  This is the
 *	one seam every lock/txn timeout deadline, the deadlock detector's
 *	expiry scan, checkpoint scheduling and the replication timers read
 *	their notion of "now" through.
 *
 * PUBLIC: #ifdef HAVE_DST
 * PUBLIC: void __db_sim_clock_hook __P((db_timespec *, int));
 * PUBLIC: #endif
 */
void
__db_sim_clock_hook(tp, monotonic)
	db_timespec *tp;
	int monotonic;
{
	time_t sec;
	long nsec;

	if (tp == NULL || !__db_sim_clock_armed())
		return;
	sec = tp->tv_sec;
	nsec = (long)tp->tv_nsec;
	__db_sim_clock_skew(&sec, &nsec, monotonic);
	tp->tv_sec = sec;
	tp->tv_nsec = nsec;
}
