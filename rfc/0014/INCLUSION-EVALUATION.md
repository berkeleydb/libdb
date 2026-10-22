# RFC 0014 Phase 1 — inclusion evaluation (2026-10-21)

Branch `feat/rfc14-typed`, a forward port of `rfc14-typed` (`0d744f7ba`) onto
post-LTS `master` `3e55513eb`. That makes 33 original commits plus 5 new ones
(below), linear on master. This note answers one question: **should the typed
layer go into libdb, and on which line?** Every number below was measured on
EC2 on 2026-10-21 unless it says otherwise.

## Recommendation

**Include it on the post-LTS development line, opt-in (`--enable-typed`, off by
default), in the next regular release. Do not ship it in the `2026.10.x` LTS
line.**

The LTS rule (VERSIONING.md, "LTS lines") is "fixes only; features go to
regular releases". The typed layer is a feature and adds public API, so it
belongs in the next regular release. Nothing in the evidence argues for
holding it further. The default build pays nothing, as shown below. The typed
build passes every gate on both architectures it was measured on. Every
sabotage arm makes its test fail. The one open D2 condition, the put-path cost,
measured inside build-to-build noise.

Two **pre-existing engine bugs** surfaced during this work. Neither is in the
typed layer: both reproduce on master and on `v2026.10.7`. They are LTS-line
fixes and should ship as a `2026.10.x` patch release on their own merits (see
"Found along the way").

## What it is

An application can declare, per Btree or Hash subdatabase, how its key and
data bytes are structured: a record codec plus per-field type codecs. libdb
then enforces NOT NULL, MAXLEN, VALID and UNIQUE on every put. The layer is a
**generator**: it turns the schema into the `bt_compare` / `h_hash` /
`h_compare` / `associate` callbacks libdb already accepts. It adds no new log
record, no new region state and no access-method format version. A typed file
carries one meta-page flag bit and a catalog subdatabase (`__db_typed`) in the
same file.

New API (present only in an `--enable-typed` build): `db_set_schema()`,
`db_get_schema()`, `db_record_fixed()`; error codes `DB_CONSTRAINT_VIOLATION`
(-30966) and `DB_CODEC_MISMATCH` (-30965). There are no new `DB` methods, so
`sizeof(DB)` is unchanged by design (D2).

Size: 2,544 lines in `src/typed/`, 497 lines of hooks and Phase 0 fixes in the
engine, and 8,333 lines of tests.

## Identity check (the north star)

| North star | Measured result |
|---|---|
| Embedded, no server | Unchanged; everything is in-process. |
| ACID, WAL, recovery | No new log records. `db_recover` from a typed build recovers a typed environment **with no codecs registered** (`typed_recover`, green on both platforms). |
| All access methods | Untyped databases are unchanged. Typed = Btree and Hash only (D4); Heap, Queue and Recno are refused at `db_set_schema`. |
| Multi-process shared regions | **Environment signature identical** across master, branch-default and branch-typed on both arches (x86_64 `0x841f4ea8`, aarch64 `0x56a06689`). Typed and default processes can share one environment. |
| On-disk format | Unchanged for untyped files. A typed subdatabase is refused (`EINVAL`) by any default or older library, read-only or not (`typed_old_reader`). |
| Tiny footprint | **The default build is byte-identical to a typed-free tree** (below). `--enable-typed` costs +17.6 KB `.text` (1.35%) on x86_64 and +15.3 KB (1.44%) on aarch64. |
| ABI | Additive only. All six handle sizes are pinned and unchanged in both builds (`run_handle_sizes` PASS ×4). |

## Qualification (2026-10-21)

Hosts: Linux x86_64 Debian 12, gcc, c5d.4xlarge spot; FreeBSD 14.5 aarch64,
clang 21.1.8, c7gd.4xlarge spot. Both are 16 vCPU, using the real git repo
(history present). The on-demand quota was full with other teams' work, so
both ran on spot. The Linux box was reclaimed twice; its results below come
from a complete run.

| Gate | Linux x86_64 | FreeBSD aarch64 |
|---|---|---|
| Builds (master default, branch default, typed+debug, ASan+UBSan typed, diag+TCL) | all ok, 0 warnings | all ok (9 warnings, same on master) |
| `test/db` tier, master default | 29/29 | 29/29 |
| `test/db` tier, branch default | 35/35 | 35/35 |
| `test/db` tier against the **typed** library | 35/35 | 35/35 |
| `typed` tier | 23/23 | 23/23 |
| `test/db` tier, ASan+UBSan typed build | 35/35 after the parity fix below; 0 ASan, 0 UBSan reports | — |
| `typed` tier, ASan+UBSan | 23/23, 0 reports | — |
| Footprint gate, items 0–4 | PASS | PASS |
| Guard lint (new item 0) | 0 unguarded references | 0 |
| **Sabotage: 33 arms** | **33/33 made their test FAIL**, 0 not detected, 0 vacuous | **33/33** |
| `run_handle_sizes` (default and typed) | PASS | PASS |
| TCL lock001–006, dead001–007, ssi001–003, txn001 | 17/17 | 17/17 |
| `s_validate`, `s_message_id`, `s_execbits`, `s_chk_copyright`, `s_chk_licence` | all rc=0 | (bash-only; Linux covers) |
| meson/autoconf parity | PASS | — |

The TCL `test001` row is missing on purpose. The harness passed it the wrong
arguments. Rerun the way `run_targeted.sh` runs it, `test001 btree` passes on
Linux for master and branch alike. On aarch64 it crashed for master, the
branch, `v2026.10.7` and `v2026.10.5` alike. That crash is the second
pre-existing bug below. With the fix, it passes 3/3.

### D2: the default build costs nothing

Measured as `.text` size and sha256 of `.text`:

| | Linux x86_64 | FreeBSD aarch64 |
|---|---|---|
| master | 1,302,060 | 1,061,292 |
| typed-free base (master + Phase 0) | 1,303,084 | 1,061,980 |
| **branch, default build** | **1,303,084** | **1,061,980** (sha256 identical to the base) |
| branch, `--enable-typed` | 1,320,684 | 1,077,292 |

The branch's default build differs from master by exactly the three Phase 0
bug fixes. Those were approved separately and are typed-independent: the R1
equal-key revive fix and the two `associate_foreign` leak fixes. Phase 0 adds
**+1,024 B** on x86_64 and **+688 B** on aarch64. The typed layer itself adds
**0 bytes** to a default build on both architectures.

### D2's revisit condition: the untyped put path in a typed build

Phase 1 adds one branch to `__dbc_put`: `F2_ISSET(dbp, DB2_AM_TYPED)` is
short-circuited on every untyped handle. D2 said to revisit the side-table
design if that branch shows above noise. Measured with
`test/bench/typed_putcost_ab.sh`: Linux x86_64 c5d.4xlarge, 7 reps × 10 s, 1M
prepopulated keys, overwrite puts, arms alternated within each rep, plus a
default-vs-second-default noise floor. This is a 16-vCPU directional result,
**not** a metal release number.

| threads | default (median puts/s) | typed build | spread of 5 default-build medians | typed vs pooled default |
|---|---|---|---|---|
| 1 | 525,080 | 523,096 | 480,890–525,080 (8.9%) | +5.0%, **inside** the band |
| 8 | 959,070 | 970,037 | 941,807–960,376 (2.0%) | +2.1%, just above the band, and **faster** |

The typed build was never slower than the default-build noise allows. The t=8
excursion is in the favourable direction, which is a layout effect between two
binaries and not a cost. **D2 stands; no revisit.** A metal A/B with more
reps is still worth running before quoting a number externally.

The first attempt at this measurement was invalid, and the harness had a bug
that also mattered for the record. `typed_putcost.c` called `set_cachesize()`
*after* `DB_ENV->open`, where it is silently ignored. Every arm therefore ran
on the default 256 KB cache, and the t=8 run spun in `__memp_alloc` for 12
minutes. The driver also shared handles across threads without `DB_THREAD`.
Both are fixed in the forward port (commit 5 below), and the numbers above are
from the fixed driver.

## The commits added in the forward port

1. **`typed: renumber two message ids that master reused`.** BDB5316 and
   BDB5317 were allocated by typed before master used them for `log_put.c` and
   the RFC 0015 M1 notice. The cherry-pick merged cleanly and left each id on
   two messages. They move to 5382 and 5383, and a scan of both `DB_STR(` and
   `DB_STR_A(` finds no duplicates left.
2. **`typed: footprint gate item 0, a history-free guard lint`.** The footprint
   gate's base (the parent of the first `src/typed/` commit) means nothing once
   the layer is merged. A merge commit sits above the base and mixes in
   unrelated `.text`. In a fetch-depth 1 clone there is no base at all, and the
   `posix` matrix job that runs the `db` tier is fetch-depth 1. **As written,
   merging the branch would have turned that job red.**
   `test/db/typed_guard_lint.sh` checks the guarantee at the source: every
   typed identifier in `src/` and `util/` is inside `#ifdef HAVE_TYPED`. It
   needs no git, base or build, so it holds on master forever. Sabotage: an
   unguarded hook, an `#ifndef` arm and an `#else` arm each FAIL; the nested
   `#ifdef` control passes.
3. **`docs: list the typed API in the C index; document --enable-typed`.** The
   three API pages existed but were not in `api/c/index.md`, and the build
   guide never mentioned the option.
4. **`test: meson/autoconf parity drops the caller's compiler flags`.** The new
   `typed` CI job exports `CFLAGS=-fsanitize=…` for its test drivers. The
   parity gate inherited it, built its "default" autoconf arm instrumented, and
   then could not link its probe. Before: FAIL. After: PASS, both with and
   without the env flags.
5. **`bench: typed_putcost sizes its cache before open, opens DB_THREAD`.**
   The driver bug described in the put-cost section above.

PR #217's CI then found four more gaps. Each one is in the branch's test
harness or docs, and none is in `src/typed/`:

6. **docs: one link** in `db_set_schema.md` was one `../` short. The docs
   gate's offline lychee check caught it.
7. **macOS: `typed_footprint` item 3** counted symbols with ELF-only `nm -D`,
   which finds nothing in a Mach-O `.dylib`. It now uses `nm -gU` there.
   **Sanitizer timeout:** the `typed` job is the first to run the `db` tier
   under clang ASan+UBSan. `qam_extent_vrfy` and `qam_readpath_bound` exceeded
   the 180 s default, so the job now sets `TIMEOUT=900`.
8. **`typed_cmp_fuzz` under clang's `-fsanitize=function`.** That check
   (clang-only) flags every call through a libdb method pointer, because the
   definitions are K&R-style. With the runner's UBSan `halt_on_error` it
   stopped the test before any fuzzing. The flag now goes last, behind a
   compiler probe, since gcc rejects it.
9. **`typed_cmp_fuzz` halted on the engine's `SSZA` offsetof idiom**
   (`bt_open.c:94`), which clang UBSan flags as a "member access within null
   pointer". UBSan now reports without halting. ASan, which the sabotage arm
   relies on, still halts.

After these, **PR #217 CI is green at job level**: 67 success, 6 skipped and
0 failed, with the coverage ratchet still running at the time of writing.
That includes the `typed` job (clang ASan+UBSan, 58 manifest verdicts,
`typed_cmp_fuzz` 1,000,001 iterations with 0 ASan errors), every macOS cell,
and the fetch-depth 1 `posix` cells, which report
`VERDICT typed_footprint PASS items 0 3 4`.

The clang UBSan log is full of pre-existing engine reports: `SSZA`
null-member access (~600) and K&R function-type calls (~120). None is in
`src/typed/`. They are a separate project ("make the engine UBSan-clean
under clang"), and no master job currently runs the `db` tier under clang
UBSan.

## Resolved: RFC §4.1's deferred question (old-library `DB_TRUNCATE`)

The spec left one hole marked [UNSURE]: can a default library's `DB_TRUNCATE`
of a typed subdatabase rewrite its meta page without the typed bit, leaving a
database that a typed library then accepts unenforced? Probed on aarch64
against the committed fixtures, with an untyped control and with pure master:

- A default-library `DB->open(file, "typedsub", DB_CREATE|DB_TRUNCATE)`
  truncates the **whole file to 0 bytes** and then fails with `EBADF`. Every
  later open, typed or not, gets `EINVAL` ("unexpected file type or format").
- The typed library therefore refuses the file. **No constraint can be
  bypassed**, so the typed-layer question closes as fail-closed.
- The untyped control behaves identically, so this is not typed behaviour.
  See the first bug below.

## Found along the way (pre-existing, not typed; LTS-line fixes)

### 1. `DB_TRUNCATE` with a subdatabase name destroys the whole file

Present in Oracle 5.3.28, `v2026.10.7` and master. In an environment without
locking (the only place `DB_TRUNCATE` is allowed), `DB->open(dbp, NULL, file,
"sub", type, DB_CREATE|DB_TRUNCATE, …)` is not refused. `db_open.c:74-86`
`__memp_ftruncate()`s the file to 0 before the real open fails with `EBADF`.
Every other subdatabase in that file is gone. The documentation
(`dbopen.md:94`) says `DB_TRUNCATE` "cannot be used to discard databases
within a file", but the code never checks `dname`.

Reproducer: subdatabases `keep` (2 rows) and `victim` (1 row);
truncate-open `victim`; afterwards `keep` fails to open with `EINVAL` and the
file is 0 bytes. Control: `DB_TRUNCATE` with `dname == NULL` works correctly.

**Fix written** (branch `fix/truncate-subdb-refuse`). It refuses
`DB_TRUNCATE` with `EINVAL` (BDB5384) in `__db_open_arg`'s `dname != NULL`
block, before any file is touched. Named in-memory databases, which have no
file, are left alone. The original authors already called this "illegal when
opening for subdbs" (`test/tcl/sdb001.tcl:91`) but nothing enforced it. The
new `test/db/run_truncate_subdb` FAILs on master (EBADF; both subdatabases
destroyed) and PASSes with the fix. The no-subdatabase control passes on both.
This is data-loss prevention and is LTS-eligible.

### 2. A strict-aliasing miscompile in the Btree reverse walk (aarch64)

Present in Oracle 5.3.28, `v2026.10.5`, `v2026.10.7` and master.
`IS_CUR_DELETED` / `IS_DUPLICATE` / `IS_CUR_DUPLICATE` (`bt_cursor.c:136-161`)
read the cursor position through `DBC_INTERNAL *`, while `__bamc_prev` writes
it through `BTREE_CURSOR *`. The two structs share their prefix only by pasting
the `__DBC_INTERNAL` macro into each, so under C's aliasing rules they are
unrelated types. FreeBSD clang 21 -O2 on aarch64 hoists the load of `indx`
above the `cp->indx -= adjust` store, so the walk tests the slot it just left.

- **Default `-O2` library: `DB_PREV` silently skips live records** next to
  deleted ones. Oracle: 20,000 rows, every third deleted through a cursor kept
  open; `DB_NEXT` sees the true 13,333, `DB_PREV` sees **13,321**. 5/5 rounds
  wrong on master, the branch and the LTS.
- **Diagnostic build: SIGSEGV** at `bt_cursor.c:2554` in TCL `test001 btree`
  (Test001.d), 5/5. The FreeBSD aarch64 qualification never ran `test001`, so
  it was never caught.
- Proof of cause: the same build with `-fno-strict-aliasing` passes both
  checks, and its disassembly shows the load after the store.
- **Fix written** (branch `fix/bamc-strict-aliasing`, one file, +23/-6): the
  three macros read through `BTREE_CURSOR *`. Verified on aarch64 with strict
  aliasing still on: the load is now after the store, the oracle PASSes 5/5,
  and `test001 btree` PASSes 3/3. With the new regression runner
  `run_bamc_prev_alias` (FAILs on master, PASSes with the fix) the `db` tier is
  30/30. `.text` size and the environment signature are both equal to master.
- Linux x86_64 gcc was not observed to miscompile this, but the code is
  undefined behaviour on every compiler, so the fix is not aarch64-specific.

## What Phase 1 deliberately does not do

D1–D8 are in the RFC; this is the user-visible list.

- No Heap, Queue or Recno typed databases (D4).
- No duplicates on typed databases (C12) and no compression on them.
- Typed databases must be named subdatabases in a file (C6).
- `db_dump` / `db_load` of a typed subdatabase yields an untyped database,
  which a typed open then refuses. Loading as typed is Phase 2.
- `DB->rename` of a typed subdatabase must be done with a typed library; a
  default build strands the catalog record (documented, fail-closed).
- There is no compile-time feature macro in `db.h` for "this build has typed".
  An application finds out by linking: `db_set_schema` is absent from a
  default library. That is the stated fail-closed design, but it means the
  public header declares prototypes the default library does not export.
  **Suggested follow-up:** emit `#define DB_HAVE_TYPED 1` into `db.h` under
  `--enable-typed`.

## Not verified

- Windows/MSVC build of `--enable-typed` (the project lists carry the typed
  sources; the CI windows job is build-only and does not enable typed).
- macOS beyond CI (CI's macOS cells run the default-build `db` tier, including
  `typed_footprint`, and are green).
- A metal put-cost A/B.
- The ASan+UBSan tiers on aarch64 (Linux x86_64 only).
- The aliasing fix's full TCL suite (only test001 and the `db` tier).

## Merge checklist

- [ ] Rebase-free: the branch is linear on `master`, so open a PR as is.
- [x] CI: the new `typed` job is green, and the `posix` matrix (fetch-depth 1)
      is green with the footprint gate reporting "items 0 3 4" (PR #217).
- [ ] `REGION-SIG-CHANGE-INTENDED` is **not** needed: the signature is unchanged.
- [ ] Update `rfc/INDEX.md`: 0014 → "Implemented (Phase 1), opt-in".
- [ ] Release note in the next regular release, plus an upgrade-guide entry
      saying "no action needed; typed is opt-in".
- [ ] Separately: #218 (`fix/bamc-strict-aliasing`) and #219
      (`fix/truncate-subdb-refuse`), both green at job level (59/59), go to
      master and then a `2026.10.x` LTS patch release.
