/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * sim_inject.h --
 *	DST bug-injection harness -- the "bug-detection latency" yardstick.
 *
 *	FoundationDB / TigerBeetle back the claim "deterministic simulation
 *	finds real bugs" by PLANTING a known bug and proving the simulator
 *	catches it within a small number of seeds, deterministically, with
 *	a replayable seed.  That -- not a coverage percentage -- is the
 *	metric that inspires confidence: "break a safety invariant and DST
 *	catches it fast, handing you the exact seed."
 *
 *	In a normal build DB_DST_INJECT_BUG is undefined and every check
 *	compiles to 0, so the harness is absent from production AND from the
 *	default --enable-dst build.  Each planted bug lives at a real
 *	library site, so activating it requires (re)building the LIBRARY
 *	with -DDB_DST_INJECT_BUG=<n>; scripts/dst-bug-inject.sh does exactly
 *	that (a dedicated build tree per bug) and asserts the matching
 *	scenario catches it within K seeds.  A planted bug the sweep does
 *	NOT catch is a hole in the DST coverage of that safety property.
 *
 *	Bug ids (each plants a KNOWN safety violation at a real library
 *	site whose failure a specific scenario's invariant detects):
 *
 *	  1  NODURABLE  -- src/log/log_put.c __log_flush_int SKIPS the log
 *	                   fsync but still acks the commit as durable.  The
 *	                   write-back model's durable frontier never advances
 *	                   past the record, so __db_sim_wb_crash() drops it
 *	                   and the "committed" txn is LOST after recovery.
 *	                   test_sim_crash_recover's "every committed txn
 *	                   survives" invariant fires.  (The headline
 *	                   ack-before-fsync catch.)
 *	  2  NOCKSUM    -- src/hmac/hmac.c __db_check_chksum ignores a
 *	                   checksum mismatch and accepts the page.  A
 *	                   torn/corrupt page then flows silently into the
 *	                   tree; test_sim_torn's "never silently wrong"
 *	                   invariant fires (a get returns bytes that do not
 *	                   match what was stored, with no error).
 *	  3  LOSTUPDATE -- src/mp/mp_bh.c __memp_pgwrite SKIPS the page write
 *	                   but reports success, so the buffer clears BH_DIRTY
 *	                   as if the page reached disk.  A checkpoint then
 *	                   believes a dirty page is durable when it is not;
 *	                   after a crash, recovery trusts the checkpoint and
 *	                   the update is lost.  test_sim_ckp_crash's
 *	                   "post-checkpoint committed data survives"
 *	                   invariant fires.
 *	  4  ABORTNOUNDO -- src/txn/txn.c __txn_abort SKIPS the __txn_undo
 *	                   rollback pass, so an aborting txn's dirty page
 *	                   changes are left in place yet the txn reports
 *	                   aborted.  The aborted records survive;
 *	                   test_sim_abort_atomic's "aborted txns leave no
 *	                   trace" invariant fires.
 *	  5  CKPBADLSN  -- src/txn/txn_chkpt.c __txn_updateckp records a
 *	                   checkpoint LSN advanced far past the true one, so
 *	                   recovery starts too LATE and skips replaying
 *	                   committed log records written after the real
 *	                   checkpoint.  Post-checkpoint committed txns are
 *	                   lost after a crash; test_sim_ckp_lsn's
 *	                   "post-checkpoint committed data survives"
 *	                   invariant fires.
 *
 *	When you add a safety invariant, add a bug id here, plant it at the
 *	real site, and add a case to scripts/dst-bug-inject.sh so DST must
 *	prove it catches it.
 */

#ifndef _DB_SIM_INJECT_H_
#define _DB_SIM_INJECT_H_

/*
 * DB_DST_BUG(n) is 1 iff the build activated planted bug n.  Zero in
 * every normal build (DB_DST_INJECT_BUG undefined), so all injection
 * sites vanish.  Exactly one bug is active per injected build.
 */
#if defined(DB_DST_INJECT_BUG)
# define DB_DST_BUG(n)  ((DB_DST_INJECT_BUG) == (n))
#else
# define DB_DST_BUG(n)  (0)
#endif

#define DB_DST_BUG_NODURABLE   1   /* log_put.c: skip log fsync, still ack */
#define DB_DST_BUG_NOCKSUM     2   /* hmac.c: accept a checksum-mismatch page */
#define DB_DST_BUG_LOSTUPDATE  3   /* mp_bh.c: skip a dirty-page write */
#define DB_DST_BUG_ABORTNOUNDO 4   /* txn.c: skip an abort's undo pass */
#define DB_DST_BUG_CKPBADLSN   5   /* txn_chkpt.c: record a wrong checkpoint LSN */

#endif /* !_DB_SIM_INJECT_H_ */
