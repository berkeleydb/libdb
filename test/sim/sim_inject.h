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
 *	default --enable-dst build.  A dedicated build passes
 *	-DDB_DST_INJECT_BUG=<n> (via `make dst_tests DSTBUG=<n>`) to activate
 *	exactly ONE planted bug; the DST capstone is then expected to FAIL
 *	within a bounded seed count.  A planted bug the sweep does NOT catch
 *	is a hole in the DST coverage of that safety property.
 *
 *	Bug ids (each targets a DST-reachable safety-critical site whose
 *	violation a specific pilot detects):
 *
 *	  1  NODURABLE  -- the crash-recover pilot skips truncating the DB
 *	                   to the durable frontier before recovery, i.e. it
 *	                   trusts bytes the write-back model says were never
 *	                   fsync'd.  Models a writer that ACKs a commit
 *	                   without making it durable.  The capstone's
 *	                   "every committed txn present after recovery, DB
 *	                   verifies clean" invariant fires.  (v1: modelled
 *	                   in the harness; a later phase plants it inside
 *	                   __log_flush itself.)
 *	  2  NOCKSUM    -- the torn/iofault pilot accepts a page whose
 *	                   checksum mismatches instead of erroring.  Silent
 *	                   corruption; the pilot's "engine errors cleanly or
 *	                   detects, never silently corrupts" invariant fires.
 *
 *	When you add a safety invariant, add a bug id here and a case to
 *	the pilot so DST must prove it catches it.
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

#define DB_DST_BUG_NODURABLE  1   /* trust un-fsync'd bytes across crash */
#define DB_DST_BUG_NOCKSUM    2   /* accept a checksum-mismatched page */

#endif /* !_DB_SIM_INJECT_H_ */
