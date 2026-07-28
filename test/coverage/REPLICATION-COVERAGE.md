# Replication coverage (rep/ + repmgr/)

The full-suite report (`FULL-COVERAGE-REPORT.md`) left replication at **0.8%**
because it "needs multi-process orchestration this run avoided." This note
records what actually runs, how, and the measured lift.

**Key finding:** most of the replication suite does *not* need external
multi-process orchestration. The `rep0NN` tests use the in-process
message-shuffling harness (`reputils.tcl`: `replsend` / `process_msgs` hand-carry
replication messages between multiple in-process envs in a single `tclsh`), and
the single-process `repmgrNN` tests spin up a master + 2 clients over real
localhost TCP inside one `tclsh`. Both run with no forked test driver — just
`source test.tcl; source reputils.tcl; <testproc> btree`.

## Measured lift (CC=gcc, -O0 -g --coverage, src/-only, this box)

| subsystem | before | after (line) | after (branch) | after (func) |
|-----------|-------:|-------------:|---------------:|-------------:|
| `src/rep/`    | 0.8% | **55.4%** (3935/7106)  | **41.0%** (3294/8039) | **71.5%** (148/207) |
| `src/repmgr/` | 0.8% | **58.3%** (3111/5339)  | **43.6%** (1813/4156) | **77.4%** (219/283) |
| **combined**  | 0.8% | **56.6%** (7046/12445) | **41.9%** (5107/12195) | **74.9%** (367/490) |

That is **+55.8 pts line** on ~12,445 previously-cold instrumented lines — the
single biggest coverage lever in the tree. Against the whole-`src/` denominator
(72,363 lines) this alone is roughly **+9.7 pts** of headline line coverage
(7,001 newly-covered lines / 72,363).

## Tests that run cleanly (single `tclsh`, timeout-guarded)

Run driver-per-test (own `tclsh`, own TESTDIR) via `COV_REP=1
test/coverage/run_coverage.sh`, or directly:

```sh
tclsh8.6 -c 'source ../test/tcl/test.tcl; source ../test/tcl/reputils.tcl; rep001 btree'
```

**rep (in-process message harness) — 22 pass:**
`rep001 rep002 rep003 rep005 rep006 rep007 rep008 rep009 rep010 rep011 rep012
rep013 rep014 rep015 rep019 rep020 rep021 rep022 rep023 rep024 rep025 rep026`

(includes the election tests `rep002 rep005 rep020 rep022 rep026`, which
exercise `rep_elect.c` — now 71%.)

**repmgr (single-process real localhost sockets) — 15 pass:**
`repmgr009 repmgr010 repmgr011 repmgr012 repmgr013 repmgr017 repmgr018
repmgr023 repmgr025 repmgr027 repmgr030 repmgr031 repmgr032 repmgr033
repmgr034`

These drive the real repmgr TCP transport (`repmgr_net.c` 63%, `repmgr_msg.c`
58%, `repmgr_sel.c` 66%, `repmgr_elect.c` 72%) in one process — `basic_repmgr_test`
starts an appointed master + two clients bound to localhost ports from
`available_ports` (base 30100).

## Still uncovered / needs real multi-process

- **`rep_lease.c` (0%)** — replication leases. The lease tests (`rep034`,
  `repmgr024`, `repmgr026`) **hang** under the in-process harness (lease-timeout
  polling loops that never satisfy in shuffled-message time). Left out.
- **`rep_stat.c` (19%)** — most of the miss is stat-print formatting; a
  `rep_stat -clear` / print-oriented test would lift it.
- **`rep016` hangs** — an election test that loops; excluded.
- **`repmgr007` FAILS** on its final "repmgr ignores unexpected input" errchk
  (`expected 1, got 0`) — but it runs to completion first, so its coverage is
  captured; it is simply not counted as a pass. Likely a behavioral drift in
  this fork, not a harness problem. Excluded from the clean set; re-triage on a
  non-coverage build before touching engine code.
- **`rep*script.tcl` subprocess tests** (`rep017 rep018 rep035 rep036 rep040
  rep042 rep043 rep045 rep048 rep065 rep078 rep092 rep095 rep097 rep102`) — these
  `exec` a second `tclsh` running a companion `*script.tcl`. They need real
  process fork/exec orchestration (a driver that launches the script child and
  shuttles the message queue on disk). Not run here.
- **repmgr 100-series** (`repmgr100 repmgr101 repmgr105 repmgr106 repmgr107
  repmgr108 …`) — genuine multi-process repmgr. They require the `db_repsite`
  test utility (`$(testdir)/repmgr/db_repsite.cpp`), which **is not present in
  this fork** (only the Windows `.vcxproj` stubs survive). To run them, restore
  `db_repsite.cpp` and add it to the `make` target, then run under a per-test
  timeout. Skipped as out-of-scope multi-process.

## How to reproduce

```sh
cd build_unix
CC=gcc ../dist/configure --enable-debug --enable-test \
  --with-tcl=<tcl-lib> CFLAGS="-O0 -g --coverage" LDFLAGS="--coverage"
make -j4
# then, from build_unix, per-test (own tclsh, own TESTDIR):
for t in rep001 rep002 ... repmgr009 ...; do
  timeout 300 tclsh8.6 -c \
    "source ../test/tcl/test.tcl; source ../test/tcl/reputils.tcl; $t btree" \
    || echo "hang/fail: $t"
done
# capture — MUST use .libs (libtool double-compiles; only .libs/*.gcda carry
# the merged replication counts):
lcov --capture --directory .libs --output-file cov.info \
  --gcov-tool gcov --branch-coverage --rc geninfo_unexecuted_blocks=1
lcov --extract cov.info '*/src/rep/*' '*/src/repmgr/*' -o rep.info --branch-coverage
lcov --summary rep.info --branch-coverage
```

Or simply: `COV_REP=1 test/coverage/run_coverage.sh` (adds the rep/repmgr sets
to the default subset and captures from `.libs`).
