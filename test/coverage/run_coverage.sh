#!/usr/bin/env bash
#
# run_coverage.sh -- build libdb with gcov instrumentation, run a bounded
# representative test subset, and report line + branch coverage (SQLite-style:
# measure branch coverage, then aim new tests at the gaps).
#
# See test/coverage/README.md for how to read the report and find the
# least-covered files. Tier B3 of .agents/test-suite-maturity-plan.md.
#
# Usage:
#   test/coverage/run_coverage.sh              # subset build + report
#   COV_TESTS="lock001 txn001" test/coverage/run_coverage.sh   # custom tests
#   COV_JOBS=4 test/coverage/run_coverage.sh   # limit build parallelism
#
# Requires: gcc, gcov (matching gcc), lcov + genhtml, tclsh. In the nix dev
# shell gcov comes from gcc; lcov/genhtml are pulled via `nix run nixpkgs#lcov`
# automatically if not on PATH. On CI (ubuntu) install `lcov` from apt.
#
# Everything lands in build_unix/ (gitignored build tree); the HTML report is
# written to build_unix/coverage-html/ and a text summary to
# build_unix/coverage-summary.txt.

set -euo pipefail

# --- locate the repo (this script lives in test/coverage/) -------------------
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../.." && pwd)"
bld="$root/build_unix"

# --- config ------------------------------------------------------------------
# Bounded, representative subset. Runs in a few minutes, exercises the core
# engine AND every access method plus the verify/salvage paths.
#
# Two entry forms, space-separated:
#   test:arg      -> `eval test arg` (arg blank for tests taking none).
#                    Used for the lock/txn/SSI/recovery core.
#   method/test   -> `run_method method test`, which runs the access-method
#                    test AND then verify_dir + salvage_dir on the databases
#                    it leaves behind -- so this one form covers the hash/
#                    queue/recno/heap access methods (src/hash, src/qam,
#                    src/btree/bt_recno.c, src/heap) *and* db verification
#                    (src/db/db_vrfy.c, src/hash/hash_verify.c,
#                    src/qam/qam_verify.c, src/heap/heap_verify.c) *and*
#                    salvage. Before this, only test001:btree ran, leaving
#                    hash/queue/heap/recno + all verify code at 0%.
#   proc@test@method -> `run_range_partition`/`run_partition_callback`
#                    (or any run_* proc) with (test method) -- runs an
#                    existing Tcl test under range partitioning / partition
#                    callbacks, lighting up src/db/partition.c.
#
# The access-method matrix is deliberately curated (a few high-value tests per
# method: basic put/get, cursors, dups, delete/renumber, partial) rather than
# the whole suite, to keep the run bounded while lighting up the red files.
# btree/test111 adds compaction coverage (src/btree/bt_compact.c); the
# run_range_partition/run_partition_callback entries add src/db/partition.c.
# env020 + statprint001 drive every *_stat_print / stat_print(DB_STAT_ALL)
# path -- the verbose stat formatters that functional tests never reach.
# env020 covers the Tcl bindings (lock/log/mpool/mutex/txn/rep/repmgr/db/seq),
# statprint001 adds the two cold spots env020 misses (heap_stat.c, and
# dbreg_stat.c's __dbreg_print_all which needs DB_STAT_ALL|DB_STAT_SUBSYSTEM
# together with open DBs) plus a db_stat *utility* flag sweep (the read-only
# on-disk __*_stat_print entry path). Together: env_stat 17->80, lock_stat
# 17->74, rep_stat 19->72, log_stat 23->87, mut_stat 29->87, db_stati 22->62,
# seq_stat 0->63, dbreg_stat 0->70, heap_stat 0->80.
#
# mvcc001 forces the two cold multiversion/cache paths the functional suite
# never applies pressure to: mp_mvcc.c freeze/thaw (a tiny multiversion cache
# with snapshot readers pinning old versions while a writer churns pages, so
# old versions spill to the __db.freezer file and thaw back) and mp_resize.c
# region growth (a small multi-region cache grown past region boundaries via
# resize_cache with a larger cache_max).  Lifts mp_mvcc.c 0->~70% and
# mp_resize.c 0->~58%.  (Cache SHRINK is intentionally not exercised: it hits
# a SIGSEGV off-by-one in __memp_remove_region -- see
# test/coverage/MVCC-RESIZE-COVERAGE.md.)
#
# sec001 + sec002 run the ENCRYPTION path (AES page/log encryption + HMAC-SHA1
# checksums + the mt19937 IV generator).  No other subset test opens an
# encrypted env/db, so crypto/ + hmac/ were the coldest reachable surface:
# sec001 drives the create/open/join interface plus every wrong-password /
# empty-password / algorithm-mismatch error branch; sec002 drives the
# page-encryption round-trip (encrypted put/get across pages) and the
# metadata/root-page checksum-error + DB_RUNRECOVERY paths.  Lift:
# mt19937db.c 0->~96, hmac.c 10->~91, rijndael-alg-fst.c 19->~84,
# crypto.c 51->~78, aes_method.c 28->~44, rijndael-api-fst.c 6->~30 (capped:
# BDB only uses AES MODE_CBC, so the ECB/CFB1/pad* halves are dead code here).
# lock007 + test143:btree light up three cold lock/codec files:
#   lock/lock_method.c   -- the DB_ENV lock-config setters/getters.  Cold
#     because the rest of the suite runs with default lock sizing.  lock007
#     sets every knob (set_lk_max_locks/lockers/objects, set_lk_partitions,
#     set_lk_tablesize, the DB_MEM_LOCK/LOCKER/LOCKOBJECT init counts, and
#     set_lk_detect for all nine deadlock-detection policies) and reads them
#     back through the getters (10->~38%).
#   lock/lock_alloc.incl -- the lock-region object/locker/lock allocator
#     (included into lock_region.c).  Cold because default runs never exhaust
#     the initial free lists.  lock007's many-locker workload (200 lockers x
#     40 distinct objects) forces the region-growth loop (14->~82%).
#   common/db_compint.c  -- the compressed-integer (varint) codec used by
#     btree compression (bt_compress.c).  Cold because no default-subset test
#     opens a -compress btree.  test143:btree stores records whose data sizes
#     span the codec's 1/2/3-byte size classes and reads them back, driving
#     __db_compress_int / __db_decompress_int32 (0->~16%, the full
#     tcl-reachable ceiling).  NOTE: the 64-bit __db_decompress_int and the
#     4-9 byte size classes are unreachable from any Tcl workload (btree
#     compression only ever marshals 32-bit lengths); they are exhaustively
#     property-tested by test/pbt/pbt_compint.c, a separate tier not in this
#     subset -- so db_compint's ceiling here is ~24%, not 100%.
#
# recd001/002/016 are NOT listed here: the recd recovery-record group is run
# by the dedicated COV_RECD block below (driver-per-test, timeout-guarded),
# which covers *_rec.c branches across all access methods.
: "${COV_TESTS:=lock001: txn001: ssi001: ssi002: env007: \
  lock007: \
  btree/test001 btree/test111 test143:btree \
  hash/test001 hash/test006 hash/test010 hash/test025 hash/test077 \
  queue/test001 queue/test007 queue/test025 \
  recno/test001 recno/test006 recno/test024 recno/test025 \
  heap/test001 heap/test013 heap/test024 \
  run_range_partition@test001@btree \
  run_partition_callback@test001@btree \
  logverify001: logverify002: \
  env020: statprint001: mvcc001: \
  sec001: sec002:}"
: "${COV_JOBS:=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)}"
: "${TCLSH:=tclsh}"
# TCL lib dir: nix store on this box, /usr/lib/tcl8.6 on ubuntu CI.
: "${TCL_LIB:=}"
if [ -z "$TCL_LIB" ]; then
  for d in /nix/store/*tcl-8.6*/lib /usr/lib/tcl8.6 /usr/lib; do
    [ -d "$d" ] && { TCL_LIB="$d"; break; }
  done
fi

# gcov MUST match the compiler that built the .gcno files. We force CC=gcc
# (BDB's configure otherwise picks `cc`, which in the nix shell is clang and
# emits LLVM-format .gcda that gcc's gcov cannot read -- the "B11 vs B52"
# version mismatch). gcov ships alongside gcc.
export CC=gcc
GCOV="${GCOV:-gcov}"

# lcov/genhtml: prefer PATH, else pull via nix (dev shell has neither on PATH).
LCOV=lcov
GENHTML=genhtml
if ! command -v lcov >/dev/null 2>&1; then
  if command -v nix >/dev/null 2>&1; then
    LCOV_BIN="$(nix build nixpkgs#lcov --no-link --print-out-paths 2>/dev/null)/bin"
    LCOV="$LCOV_BIN/lcov"
    GENHTML="$LCOV_BIN/genhtml"
  else
    echo "error: lcov not found on PATH and nix unavailable; install lcov" >&2
    exit 1
  fi
fi

# lcov error classes we tolerate: version (LLVM/gcc tag noise), source/mismatch
# (generated headers), inconsistent (gcov line/branch quirks), empty/unused.
IGN="mismatch,source,gcov,unused,negative,empty,inconsistent,version"

echo "== libdb coverage =="
echo "  repo:    $root"
echo "  CC:      $CC ($($CC -dumpversion 2>/dev/null || echo '?'))"
echo "  gcov:    $GCOV ($($GCOV --version 2>/dev/null | head -1))"
echo "  tcl lib: $TCL_LIB"
echo "  tests:   $COV_TESTS"
echo

# --- clean any prior coverage artifacts (idempotent) -------------------------
find "$bld" \( -name '*.gcda' -o -name '*.gcno' \) -delete 2>/dev/null || true
rm -f "$bld/coverage.info" "$bld/coverage-src.info" 2>/dev/null || true

# --- configure + build with instrumentation ---------------------------------
echo "== configure (--coverage) =="
cd "$bld"
CC=gcc ../dist/configure --enable-test --with-tcl="$TCL_LIB" \
  CFLAGS="-O0 -g --coverage" LDFLAGS="--coverage" >/tmp/cov-configure.log 2>&1 \
  || { echo "configure failed:"; tail -30 /tmp/cov-configure.log; exit 1; }

echo "== build (-j$COV_JOBS) =="
make -j"$COV_JOBS" >/tmp/cov-build.log 2>&1 \
  || { echo "build failed:"; tail -40 /tmp/cov-build.log; exit 1; }
echo "  .gcno files: $(find . -name '*.gcno' | wc -l)"

# --- run the test subset (produces .gcda) ------------------------------------
echo "== run tests =="
runtcl="$bld/.cov-run.tcl"
{
  echo 'source ../test/tcl/test.tcl'
  for pair in $COV_TESTS; do
    case "$pair" in
    *@*)  # proc@test@method form -> `run_proc test method` (partition runners)
      p="${pair%%@*}"; rest="${pair#*@}"; t="${rest%%@*}"; m="${rest#*@}"
      printf 'source ../test/tcl/%s.tcl\n' "$t"
      printf 'if {[catch {%s %s %s 0 1} res]} { puts "FAIL %s/%s/%s: $res"; exit 1 }\n' "$p" "$t" "$m" "$p" "$t" "$m"
      printf 'puts "PASS %s/%s/%s"\n' "$p" "$t" "$m"
      ;;
    */*)  # method/test form -> run_method (test + verify_dir + salvage_dir)
      m="${pair%%/*}"; t="${pair#*/}"
      printf 'source ../test/tcl/%s.tcl\n' "$t"
      printf 'if {[catch {run_method %s %s 0 1} res]} { puts "FAIL %s/%s: $res"; exit 1 }\n' "$m" "$t" "$m" "$t"
      printf 'puts "PASS %s/%s"\n' "$m" "$t"
      ;;
    *)    # test:arg form -> eval test arg
      t="${pair%%:*}"; a="${pair#*:}"
      printf 'source ../test/tcl/%s.tcl\n' "$t"
      printf 'if {[catch {eval %s %s} res]} { puts "FAIL %s: $res"; exit 1 }\n' "$t" "$a" "$t"
      printf 'puts "PASS %s"\n' "$t"
      ;;
    esac
  done
} > "$runtcl"
# tclsh8.6 preferred if present (nix); fall back to tclsh.
TCLBIN="$TCLSH"; command -v tclsh8.6 >/dev/null 2>&1 && TCLBIN=tclsh8.6
timeout "${COV_TIMEOUT:-2400}" "$TCLBIN" "$runtcl" 2>&1 | tee /tmp/cov-tests.log \
  | grep -E '^PASS|^FAIL' || true
rm -f "$runtcl"
if grep -q '^FAIL' /tmp/cov-tests.log; then
  echo "warning: a test FAILED; coverage still aggregated below" >&2
fi
echo "  .gcda files: $(find . -name '*.gcda' | wc -l)"

# --- optional replication tests (COV_REP=1) ----------------------------------
# Replication is the single biggest cold surface (rep/ + repmgr/ ~= 12.4k lines
# at ~0.8%). These tests are NOT in the default COV_TESTS because each one wants
# its own tclsh (they reset TESTDIR and a few election/lease tests hang), so
# they are run driver-per-test with a per-test timeout instead of one big tclsh.
# Set COV_REP=1 to include them. They use the in-process message-shuffling
# harness (rep0NN) and single-process real-socket repmgr (repmgrNN) -- no
# external orchestration. See test/coverage/REPLICATION-COVERAGE.md.
#
# NOT included (need real multi-process): the rep*script.tcl subprocess tests
# and the repmgr 100-series (need the db_repsite utility, absent from this fork)
# and a few election/lease tests that hang (rep016, repmgr024/026).
if [ "${COV_REP:-0}" = 1 ]; then
  echo "== run replication tests (COV_REP=1) =="
  : "${COV_REP_TIMEOUT:=300}"
  : "${COV_REP_TESTS:=rep001 rep002 rep003 rep005 rep006 rep007 rep008 rep009 \
 rep010 rep011 rep012 rep013 rep014 rep015 rep019 rep020 rep021 rep022 rep023 \
 rep024 rep025 rep026}"
  : "${COV_REPMGR_TESTS:=repmgr009 repmgr010 repmgr011 repmgr012 repmgr013 \
 repmgr017 repmgr018 repmgr023 repmgr025 repmgr027 repmgr030 repmgr031 \
 repmgr032 repmgr033 repmgr034}"
  reptcl="$bld/.cov-rep-one.tcl"
  for t in $COV_REP_TESTS $COV_REPMGR_TESTS; do
    case "$t" in
    rep[0-9]*) call="$t btree" ;;   # rep0NN take a method arg
    *)         call="$t" ;;          # repmgrNN use defaults
    esac
    printf 'source ../test/tcl/test.tcl\nsource ../test/tcl/reputils.tcl\nif {[catch {%s} r]} { puts "FAIL %s: $r"; exit 3 }\nputs "PASS %s"\n' \
      "$call" "$t" "$t" > "$reptcl"
    timeout "$COV_REP_TIMEOUT" "$TCLBIN" "$reptcl" >/tmp/cov-rep-$t.log 2>&1
    rc=$?
    if [ $rc -eq 124 ]; then echo "HANG $t"
    elif [ $rc -eq 0 ] && grep -q "^PASS $t" /tmp/cov-rep-$t.log; then echo "PASS $t"
    else echo "FAIL $t (rc=$rc)"; fi
    pkill -f "$reptcl" 2>/dev/null || true
    find TESTDIR -mindepth 1 -delete 2>/dev/null || true
  done
  rm -f "$reptcl"
  echo "  .gcda files after rep: $(find . -name '*.gcda' | wc -l)"
fi

# --- optional XA + on-disk-upgrade drivers (COV_XA_UPG=1, default on) --------
# These are NOT Tcl tests: they are standalone drivers that light up two cold
# subsystems the Tcl suite never reaches.
#   xa/xa.c + xa_map.c  -- the X/Open XA resource-manager switch (db_xa_switch).
#     test/xa/run_xa_direct.sh compiles test/xa/xa_direct.c (a Tuxedo-free TM
#     that drives the switch entry points + the internal 2PC / recovery path)
#     and runs it under a timeout.  Lifts xa.c 0%->~57%, xa_map.c 0%->~78%.
#   db/db_upg.c         -- the on-disk-format upgrade path.
#     test/db/run_upgrade.sh runs the db_upgrade utility + DB->upgrade over the
#     committed old-format fixture, freshly-created current-format dbs of every
#     access method, AND synthetic old-format fixtures (metadata pages rewritten
#     into BTMETA2X/BTMETA30/HASHHDR/HMETA30/QMETA30/QMETA31 layouts).  Lifts
#     db_upg.c 0%->~57%, qam_upgrade.c 0%->~97%, hash_upgrade.c 0%->~68%,
#     bt_upgrade.c 0%->~82%.  (db_upg_opd.c stays cold: needs a genuine 3.0-era
#     off-page-duplicate page chain -- see test/coverage/README.md.)
# Both self-clean their home dir and run under a hard timeout, so they cannot
# hang.  Set COV_XA_UPG=0 to skip.
if [ "${COV_XA_UPG:-1}" = 1 ]; then
  echo "== run XA + upgrade drivers (COV_XA_UPG=1) =="
  if sh "$root/test/xa/run_xa_direct.sh" >/tmp/cov-xa.log 2>&1; then
    echo "PASS xa_direct"
  else
    echo "FAIL xa_direct (rc=$?)"; tail -5 /tmp/cov-xa.log
  fi
  if sh "$root/test/db/run_upgrade.sh" >/tmp/cov-upg.log 2>&1; then
    echo "PASS db_upgrade"
  else
    echo "FAIL db_upgrade (rc=$?)"; tail -5 /tmp/cov-upg.log
  fi
  # os_aio async-I/O backends: the buffer pool reaches os_aio only via
  # DB_ENV->set_flags(DB_MPOOL_AIO) and then picks a SINGLE backend at
  # runtime (io_uring first on Linux), so no Tcl workload can light up the
  # pool + posix backends.  This driver drives EACH configured backend
  # directly plus a real DB_MPOOL_AIO checkpoint workload.  Lifts os_aio.c
  # 0%->~84%, os_aio_pool.c 0%->~73%, os_aio_posix.c 0%->~84%,
  # os_aio_uring.c 0%->~81%, common/os_method.c 0%->100%.
  if sh "$root/test/os/run_os_aio.sh" >/tmp/cov-osaio.log 2>&1; then
    echo "PASS os_aio_direct"
  else
    echo "FAIL os_aio_direct (rc=$?)"; tail -5 /tmp/cov-osaio.log
  fi
  echo "  .gcda files after xa/upg: $(find . -name '*.gcda' | wc -l)"
fi

# --- optional backup-API + compaction-recovery drivers (COV_BACKUP=1) --------
# Two more standalone C drivers (same self-clean + hard-timeout shape as the
# XA/upgrade drivers above) that light up code the Tcl suite cannot reach:
#   env/env_backup.c   -- the hot-backup config + callback API
#     (set/get_backup_config for all four enums, set/get_backup_callbacks).
#     backup.tcl drives db_hotbackup, which calls DB_ENV->backup() with a NULL
#     backup_handle, so env_backup.c stays 0%.  test/backup/run_backup_direct.sh
#     compiles backup_direct.c, which calls those public entry points directly
#     and then runs DB_ENV->backup()/dbbackup() with write callbacks installed
#     -- lifting env_backup.c 0%->~97% and the backup->open/write/close callback
#     branches of db/db_backup.c.
#   db/db_rec.c        -- the btree-compaction + page-truncation recovery
#     handlers __db_merge_recover / __db_pgno_recover / __db_pg_trunc_recover.
#     No recd0NN test runs compaction under recovery, so those ~330 lines are
#     cold.  test/db/run_recd_compact.sh compiles recd_compact.c, which fills+
#     sparsifies a btree, compacts with DB_FREE_SPACE (logging merge/pgno/
#     pg_trunc records) and re-opens under DB_RECOVER_FATAL to replay them.
#   bt_rec.c / db_rec.c -- four per-operation recovery handlers no recd0NN
#     test reaches (recd003 does NOT unlock them -- they "need scenarios no
#     bounded recd test hits"): __bam_root_recover (subdb-create root update),
#     __bam_irep_recover (compaction internal-record replace),
#     __bam_rcuradj_recover (rrecno child-txn cursor adjust) and
#     __db_ovref_recover (truncate of a btree with overflow items).
#     test/db/run_recd_handlers.sh compiles recd_handlers.c, which builds each
#     scenario and replays it under DB_RECOVER / DB_RECOVER_FATAL (and txn
#     abort for the undo paths).  Lifts those four handlers 0 -> covered
#     (__bam_irep 60% br, __bam_root 53%, __bam_rcuradj 74%, __db_ovref 68%).
# Set COV_BACKUP=0 to skip.
if [ "${COV_BACKUP:-1}" = 1 ]; then
  echo "== run backup + compaction-recovery drivers (COV_BACKUP=1) =="
  if sh "$root/test/backup/run_backup_direct.sh" >/tmp/cov-backup.log 2>&1; then
    echo "PASS backup_direct"
  else
    echo "FAIL backup_direct (rc=$?)"; tail -5 /tmp/cov-backup.log
  fi
  if sh "$root/test/db/run_recd_compact.sh" >/tmp/cov-recdcompact.log 2>&1; then
    echo "PASS recd_compact"
  else
    echo "FAIL recd_compact (rc=$?)"; tail -5 /tmp/cov-recdcompact.log
  fi
  if sh "$root/test/db/run_recd_handlers.sh" >/tmp/cov-recdhandlers.log 2>&1; then
    echo "PASS recd_handlers"
  else
    echo "FAIL recd_handlers (rc=$?)"; tail -5 /tmp/cov-recdhandlers.log
  fi
  echo "  .gcda files after backup/compact: $(find . -name '*.gcda' | wc -l)"
fi

# --- optional deadlock-detector + DB_REGISTER drivers (COV_DEAD_REG=1) -------
# The deadlock DETECTOR (lock/lock_deadlock.c, __lock_detect) and the
# process-registration + failchk-on-crash path (env/env_register.c,
# __envreg_register/isalive) are both multi-process: the `dead` group and
# env012 spawn worker tclsh via wrap.tcl (ddscript.tcl / envscript.tcl) that
# contend for locks in a cycle (detector picks a victim) or open the env with
# DB_REGISTER, crash, and let a survivor recover.  They cannot run in the main
# single-tclsh COV_TESTS loop (they reset TESTDIR and a hung worker would wedge
# the whole run), so -- like COV_REP -- they run driver-per-test with a
# per-test timeout and orphan-worker cleanup.  Lifts lock_deadlock.c 0.7%->~66%
# and env_register.c 0%->~55%.  proc counts are trimmed (default {2 4}) so each
# test finishes well inside the timeout; the full {2 4 10} matrix adds no new
# coverage, only minutes.  Set COV_DEAD_REG=0 to skip.
if [ "${COV_DEAD_REG:-1}" = 1 ]; then
  echo "== run deadlock + DB_REGISTER drivers (COV_DEAD_REG=1) =="
  : "${COV_DEAD_REG_TIMEOUT:=300}"
  # Each entry is "name:tcl-call"; trimmed proc counts keep each run bounded.
  # A bash array so the spaces inside a call ({2 4}) survive word-splitting.
  cov_dead_reg_tests=(
    "dead001:dead001 {2 4}"
    "dead002:dead002 {2 4}"
    "dead003:dead003 {2 4}"
    "dead004:dead004"
    "dead005:dead005 {4}"
    "dead006:dead006 {2 4}"
    "env007:env007"
    "env012:env012"
  )
  dregtcl="$bld/.cov-deadreg-one.tcl"
  for spec in "${cov_dead_reg_tests[@]}"; do
    name="${spec%%:*}"; call="${spec#*:}"
    printf 'source ../test/tcl/test.tcl\nif {[catch {%s} r]} { puts "FAIL %s: $r"; exit 3 }\nputs "PASS %s"\n' \
      "$call" "$name" "$name" > "$dregtcl"
    timeout "$COV_DEAD_REG_TIMEOUT" "$TCLBIN" "$dregtcl" >/tmp/cov-dreg-$name.log 2>&1
    rc=$?
    # kill orphan workers a hung/killed test may have left behind
    pkill -f 'wrap.tcl' 2>/dev/null || true
    pkill -f 'ddscript' 2>/dev/null || true
    pkill -f 'envscript' 2>/dev/null || true
    pkill -f 'db_deadlock' 2>/dev/null || true
    if [ $rc -eq 124 ]; then echo "HANG $name"
    elif [ $rc -eq 0 ] && grep -q "^PASS $name" /tmp/cov-dreg-$name.log; then echo "PASS $name"
    else echo "FAIL $name (rc=$rc)"; fi
    find TESTDIR -mindepth 1 -delete 2>/dev/null || true
  done
  rm -f "$dregtcl"
  echo "  .gcda files after dead/register: $(find . -name '*.gcda' | wc -l)"
fi

# --- optional recovery-record handlers (COV_RECD=1, default on) --------------
# The per-operation recovery handlers in db/db_rec.c, hash/hash_rec.c,
# btree/bt_rec.c and qam/qam_rec.c (__xxx_recover: redo / undo / abort /
# do-nothing branches) are the single biggest cold BRANCH surface
# (db_rec.c 3189 branches, hash_rec.c 2069, bt_rec.c 2366, qam_rec.c 574).
# The main COV_TESTS loop runs only recd001/002/016 on btree, so the hash and
# queue recover handlers sat at 0% branch and most redo/undo branches of the
# btree/db handlers were cold.
#
# The `recd` group drives exactly those branches: op_recover (testutils.tcl)
# crashes the env at every log-record boundary and replays recovery forward
# (redo) AND backward (undo/abort).  They CANNOT share one tclsh -- several
# recd tests use conflicting globals (e.g. recd006 sets `kvals` scalar,
# recd010 uses it as an array) and each spawns recdscript.tcl subprocesses --
# so, like COV_DEAD_REG, they run driver-per-test with a per-test timeout and
# orphan-subprocess cleanup.
#
# The set is curated for coverage-per-minute across the FOUR access methods
# each test supports (splits recd002, fileid-reuse recd005, nested-txn recd006,
# deep/many-child recd008, recnum recd009, off-page-dup splits recd010, cursor
# adjust recd013, queue-extent create/delete recd014, checksum-error recd016,
# crypto recd017, checkpoint/commit recd018, txn-id-wrap recd019, intermediate
# dirs recd020, aborted-prepared page-alloc recd022, reverse-split recd023,
# streaming partial-put overflow recd024, TXN_BULK recd025, big-key-to-internal
# recd004).  Lifts (recd-driven branch %): db_rec.c 14.6->16.4, bt_rec.c
# 12.3->21.6, hash_rec.c 0->24.2, qam_rec.c 0->45.5.  Whole block ~10 min.
#
# DELIBERATELY EXCLUDED (too slow for the marginal branches they add):
#   recd001 (~6.5 min/method): the other tests already cover its per-op
#     redo/undo branches -- dropping it entirely changed the totals by ~1
#     branch.  recd001:btree/recno each add nothing the set doesn't have.
#   recd003 (~7.3 min, dup): does NOT unlock the cold __bam_root/irep/rcuradj
#     or __db_ovref handlers (they need scenarios no bounded recd test hits),
#     so its 7 minutes buy only a handful of already-adjacent branches.
#   recd015 (~4.6 min, many prepared txns): prepare/recover branches are
#     already exercised by recd002/recd022's prepare paths.
#   recd007 (~4.3 min, file create/delete): db_rec create/rename branches are
#     covered by the recd_compact + upgrade drivers already in the subset.
# Set COV_RECD=0 to skip.
if [ "${COV_RECD:-1}" = 1 ]; then
  echo "== run recovery-record handler tests (COV_RECD=1) =="
  : "${COV_RECD_TIMEOUT:=300}"
  # Each entry is "test:method:extra-arg" (extra blank for tests taking none).
  cov_recd_tests=(
    "recd002:btree:0" "recd002:hash:0" "recd002:queue:0" "recd002:recno:0"
    "recd004:btree:"
    "recd005:btree:" "recd005:hash:" "recd005:queue:" "recd005:recno:"
    "recd006:btree:" "recd006:hash:"
    "recd008:btree:" "recd009:btree:" "recd010:btree:"
    "recd013:btree:" "recd013:hash:"
    "recd014:queueext:"
    "recd016:btree:" "recd017:btree:" "recd018:btree:" "recd019:btree:"
    "recd020:btree:" "recd022:btree:" "recd023:btree:" "recd024:btree:"
    "recd025:btree:"
  )
  recdtcl="$bld/.cov-recd-one.tcl"
  for spec in "${cov_recd_tests[@]}"; do
    t="${spec%%:*}"; rest="${spec#*:}"; m="${rest%%:*}"; a="${rest#*:}"
    printf 'source ../test/tcl/test.tcl\nsource ../test/tcl/%s.tcl\nif {[catch {eval %s %s %s} r]} { puts "FAIL %s %s: $r"; exit 3 }\nputs "PASS %s %s"\n' \
      "$t" "$t" "$m" "$a" "$t" "$m" "$t" "$m" > "$recdtcl"
    timeout "$COV_RECD_TIMEOUT" "$TCLBIN" "$recdtcl" >/tmp/cov-recd-$t-$m.log 2>&1
    rc=$?
    # kill orphan recdscript.tcl subprocesses a hung/killed test may leave
    pkill -f "$recdtcl" 2>/dev/null || true
    pkill -f 'recdscript' 2>/dev/null || true
    if [ $rc -eq 124 ]; then echo "HANG $t $m"
    elif [ $rc -eq 0 ] && grep -q "^PASS $t $m" /tmp/cov-recd-$t-$m.log; then echo "PASS $t $m"
    else echo "FAIL $t $m (rc=$rc)"; fi
    find TESTDIR -mindepth 1 -delete 2>/dev/null || true
  done
  rm -f "$recdtcl"
  echo "  .gcda files after recd: $(find . -name '*.gcda' | wc -l)"
fi

# --- aggregate ---------------------------------------------------------------
echo "== capture (lcov) =="
# NOTE: capture from .libs (not .) -- libtool double-compiles, and only the
# .libs/*.gcda carry the merged replication counts; capturing "." drops repmgr.
"$LCOV" --capture --directory .libs --output-file coverage.info \
  --gcov-tool "$GCOV" --rc geninfo_unexecuted_blocks=1 --branch-coverage \
  --ignore-errors "$IGN" >/tmp/cov-lcov.log 2>&1 \
  || { echo "lcov capture failed:"; tail -20 /tmp/cov-lcov.log; exit 1; }

# Keep only the library sources under src/ (drop tcl harness, examples, system).
"$LCOV" --extract coverage.info "*/src/*" --output-file coverage-src.info \
  --branch-coverage --ignore-errors "$IGN" >/dev/null 2>&1

echo "== summary =="
"$LCOV" --summary coverage-src.info --branch-coverage --ignore-errors "$IGN" 2>&1 \
  | grep -E 'source files|lines|functions|branches' | tee coverage-summary.txt

# --- HTML report -------------------------------------------------------------
echo "== genhtml =="
rm -rf coverage-html
"$GENHTML" coverage-src.info --output-directory coverage-html --branch-coverage \
  --ignore-errors "empty,inconsistent,source,category,unmapped" >/tmp/cov-genhtml.log 2>&1 \
  && echo "  report: $bld/coverage-html/index.html" \
  || { echo "genhtml failed:"; tail -10 /tmp/cov-genhtml.log; }

# --- least-covered files (the actionable list) -------------------------------
echo
echo "== least-covered src files (>=50 lines) -- aim new tests here =="
"${PYTHON:-python3}" "$here/rank_coverage.py" coverage-src.info | head -20
echo
echo "Done. See test/coverage/README.md for how to read the report."
