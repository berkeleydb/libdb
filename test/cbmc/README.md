# CBMC formal-verification harnesses

This directory holds [CBMC](https://www.cprover.org/cbmc/) (C Bounded Model
Checker) harnesses that **formally verify the actual C** of libdb's
self-contained algorithmic cores. CBMC proves — over *all* inputs within a
stated bound — that a function has no array out-of-bounds, no invalid pointer
dereference, no (checked) arithmetic overflow, no failed reachable assertion,
and any user-written property. It is exactly the tool for small,
pointer/arithmetic-heavy functions, and it finds the same bug classes our
fuzzer and malloc-injection found (`__db_ret_okitem` OOB #67,
`__db_set_lastpgno` off-by-one).

These harnesses verify the **real, unmodified engine C**: the pure functions
are pulled in by `#include`-ing the actual `.c`; the two functions that are
`static` / drag in the whole tree (`__db_ret_okitem`, `__dd_find`) are copied
**verbatim** (logic byte-for-byte identical to `src/` — verified by
`diff`), with only the surrounding `#include`s replaced by the minimal
struct/macro definitions the function uses (which mirror the real headers).
Each harness's top comment lists exactly what was stubbed.

> **CBMC proves properties up to the bound.** Where a loop or buffer is
> bounded (e.g. varint over the full `uint64` range, page ≤ 64 bytes, ≤ 4
> lockers), the proof is *complete within that bound* — not a claim about
> unbounded inputs. Bounds are stated per harness below.

## Running

Inside the nix dev shell (CBMC 6.9.0 is in `flake.nix`
`devShells.default`):

```sh
nix develop . --command bash test/cbmc/run.sh
```

`run.sh` runs every harness with its unwind bound and prints
`PASS`/`FAIL` + runtime. Whole suite: ~20 s.

Run one harness directly:

```sh
cd test/cbmc
nix develop .. --command cbmc harness_varint.c -Istubs --function harness \
    --bounds-check --pointer-check --conversion-check
```

To see a counterexample trace, add `--trace` (and `--property <name>` to
isolate one assertion).

## Harnesses

| Harness | Target (`src/`) | Proves | Bound | Runtime | Result |
|---|---|---|---|---|---|
| `harness_varint.c`   | `common/db_compint.c` `__db_compress_int` / `__db_decompress_int` / `__db_decompress_int32` / `__db_compress_count_int` | round-trip `decompress(compress(x))==x`; count == bytes written == bytes read; length monotone in value; int32 agrees with int64; no OOB | full `uint64`, 9-byte buf (loop-free) | ~1 s | SUCCESSFUL |
| `harness_swap.c`     | `dbinc/db_swap.h` `P_16/32/64_SWAP`, `M_*_SWAP`, `P_*_COPYSWAP`, `P_*_COPY`, `SWAP16/32` | involution `swap∘swap==id`; copyswap reverses exactly its bytes; copy is identity; SWAP advances pointer by type size; no OOB | all inputs (loop-free) | <1 s | SUCCESSFUL |
| `harness_hash4.c`    | `hash/hash_func.c` `__ham_func4` (the `__db_chksum` plain-hash core) | reads exactly `len` key bytes (no OOB); deterministic; `len==0 ⇒ 0` | key ≤ 8 bytes, `--unwind 9` | ~15 s | SUCCESSFUL |
| `harness_getlong.c`  | `common/db_getlong.c` `__db_getlong` / `__db_getulong` | success ⇒ result in `[min,max]`; ulong `max==0` = "no upper bound"; no OOB on the parse | string ≤ 6 chars, `--unwind 8` | ~1 s | SUCCESSFUL |
| `harness_dd_find.c`  | `lock/lock_deadlock.c` `__dd_find` (waits-for bitmap cycle detection) | no OOB on the `nlockers×nalloc` bitmap matrix or deadlist; termination; deadlist entries point into the matrix and are NULL-terminated | 4 lockers, `--unwind 6` + `--unwinding-assertions` | <1 s | SUCCESSFUL |
| `harness_cmpvote.c`  | `rep/rep_elect.c` `__rep_cmp_vote` (election winner comparator) | **(b)** a priority-0 non-`REPCTL_ELECTABLE` vote never wins; **(c)** if any non-zero-priority vote is present the winner has non-zero priority; **(d)** among like-priority votes at the **same data_gen**, the winner maximises (LSN, priority, tiebreaker) and is order-independent. `-DCMPVOTE_SABOTAGE` swaps `LOG_COMPARE`'s order arms and must FAIL (d) | 3 votes, all fields nondet (loop-free) | ~2 s | SUCCESSFUL. **(a) order independence does NOT hold — see FINDING below** |
| `harness_tally.c`    | `rep/rep_elect.c` `__rep_tally` (per-election vote tally in the region) | each eid counted at most once; a lower/equal egen for a seen eid returns `DB_REP_IGNORE` with no count change, a higher egen updates in place with no count change; `*countp` == number of distinct eids and never exceeds capacity; no OOB on the tally array. `-DTALLY_SABOTAGE` turns the "seen this eid" test from `==` to `!=` (double-count) and must FAIL | tally CAP 5, ≤ 6 votes, 5 eids, egen ≤ 2, `--unwind 8` | ~1 s | SUCCESSFUL |
| `harness_majority.c` | `rep/rep_elect.c` `ELECTION_MAJORITY` (rep.h:534) + the `ack <= nsites/2` sub-majority warning in `__rep_elect_int` | **(1)** `2*majority > nsites` so any two majorities intersect (also proved constructively: two majorities of N=8 share a member); **(2)** the warning test `ack <= nsites/2` is exactly `ack < ELECTION_MAJORITY(nsites)`. `-DMAJORITY_SABOTAGE` uses `n/2` (floor) and must FAIL both | nsites 1–64, ack 0–64, constructive N=8 | ~1 s | SUCCESSFUL |
| `harness_ackpolicy.c` | `repmgr/repmgr_net.c` `got_acks` (:1117) + the `needed`/`quorum` computation in `__repmgr_send` (DB_REP_PERMANENT block) | **QUORUM:** durable => no electable-site majority (`ELECTION_MAJORITY(total electable incl. master)`) avoids a holder (master or acked electable peer) — "no election can lose it". **2-site:** STRICT => quorum 0 (master-only durable); non-strict => the single client must ack. **ALL_PEERS:** durable => every electable peer acked. **ONE_PEER:** durable => some electable peer acked. **Shortcuts:** `needed==0` is durability-safe (only when no electable peers exist); `available<needed` refuses, and `got_acks` would also refuse. `-DACKPOLICY_SABOTAGE` counts non-electable acks as peers and must FAIL QUORUM + ONE_PEER | ≤ 4 remote sites (+ master), all flags/acks nondet; electable-majority quantifier over a nondet membership bitmap | ~2–5 s/arm | SUCCESSFUL |
| `harness_okitem.c`   | `db/db_ret.c` `__db_ret_okitem` (the #67 guard) | **safety guarantee**: if okitem returns 0 ("safe"), the exact downstream `__db_ret`/`__db_retcopy` read stays on-page (`__CPROVER_r_ok`) | page ≤ 64 bytes (loop-free) | ~1 s | SUCCESSFUL. Found a real bug (below), fixed in `2ca81268a`; `-DHPAGE_PTYPE_SABOTAGE` restores the bug and must FAIL |
| `harness_bt_decompress.c` | `btree/bt_compress.c` `__bam_defdecompress` | **safety**: never reads outside `compressed->data[0..size)` nor `prevKey`/`prevData`, never writes past `destKey->ulen`/`destData->ulen`, for ANY input bytes | compressed ≤ 16, prev ≤ 8, dest ulen ≤ 16 (loop-free) | ~2 s | **BUG FOUND (below).** The real code FAILS (over-read); `-DB1_FIXED` builds the fix and VERIFIES. `run.sh` runs the real code as a must-FAIL arm (`b1-decompress`) until the engine fix lands |
| `harness_qam_recno.c` | `dbinc/qam.h` `QAM_RECNO_PAGE` / `QAM_RECNO_INDEX` / `QAM_OUTSIDE_QUEUE` / `QAM_BEFORE_FIRST` / `QAM_AFTER_CURRENT` / `QAM_NOT_VALID` | **MAP:** for recno ≥ 1, `QAM_RECNO_INDEX(QAM_RECNO_PAGE(r)) < rec_page` and `(page,index)` maps back to `r`. **RANGE:** the macros agree with an independent circular-window reference, incl. the wrapped queue (`cur < first`). **PARTITION:** for recno ≠ RECNO_OOB, exactly one of before-first / after-current / inside holds; `QAM_NOT_VALID == before \|\| after`. `-DQAM_SABOTAGE` drops the 1-based `- 1` in `QAM_RECNO_INDEX` and must FAIL | full `u_int32_t` recno/first/cur/q_root; rec_page 1..8 (loop-free) | ~1 s | SUCCESSFUL (macros agree with the reference; no bug) |

`run.sh` runs `okitem` twice: on the real engine macro (→ `VERIFICATION
SUCCESSFUL`) and with `-DHPAGE_PTYPE_SABOTAGE`, which restores the pre-fix
macro (→ `VERIFICATION FAILED`, the sabotage arm).

`run.sh` also runs `check_copies.sh`: a harness that copies a `static`
function proves a property of the copy, so the copy must still match `src/`.
It compares code tokens (comments and blank lines are ignored) from the
function's `name(` line to its closing `}`. The okitem harness drifted this way
once, keeping the unparenthesised macro for weeks after the engine fix.

## Proof that the harnesses have teeth

A harness that can never fail proves nothing. Each of these was checked by
temporarily asserting a *wrong* property and confirming CBMC produces a
counterexample:

* **varint** — asserting `decompress(compress(x)) == x + 1` →
  `VERIFICATION FAILED` (assertion "TEETH: deliberately wrong round-trip").
* **swap** — asserting a *single* `P_32_SWAP` equals the identity →
  `VERIFICATION FAILED`.
* **dd_find** — asserting a recorded deadlist entry lies *past* the bitmap
  matrix → `VERIFICATION FAILED`.
* **okitem** — the real engine macro already yields a genuine counterexample
  (the bug below); the `-DHPAGE_PTYPE_FIXED` build then verifies, so the
  property distinguishes correct from incorrect code.

## FINDING (not a safety bug) — `__rep_cmp_vote` is order-dependent, and is not the lexicographic order its comment claims

`harness_cmpvote.c` property **(a)** (order independence) FAILS on the real,
unmodified `__rep_cmp_vote` (src/rep/rep_elect.c:1101). Two sites that tally
the **same** set of VOTE1 messages in different arrival orders can pick
**different** phase-1 winners. CBMC gives two independent counterexamples.

### Counterexample 1 — the first-vote special case (all-tied mixed-version set)

Three votes, all fields zero except:

```
vote 0: priority=0, flags=0                 (not electable)
vote 1: priority=0, flags=0                 (not electable)
vote 2: priority=0, flags=REPCTL_ELECTABLE  (electable, mixed-version site)
```

`__rep_cmp_vote` treats the FIRST tallied vote specially: when `rep->sites==1`
it makes an electable vote the winner outright. When `rep->sites>1` it runs the
comparison arms, and for this all-tied set (`data_gen==w_datagen==0`,
`cmp==LOG_COMPARE(0,0)==0`, `priority==w_priority==0`,
`tiebreaker==w_tiebreaker==0`) **none** of the four arms fire. So:

* orders where vote 2 arrives first (`{2,0,1}`, `{2,1,0}`) → **winner = 2**;
* every other order → **winner stays `DB_EID_INVALID`**.

This is the exact "mixed version group ... priority 0 but ELECTABLE" case the
source comment describes, and it is order-dependent.

### Counterexample 2 — the last arm bypasses data_gen

The replace-the-winner condition is:

```c
(priority != 0 && w_priority == 0) ||                         /* A */
(like_pri && data_gen > w_datagen) ||                         /* B */
(like_pri && data_gen == w_datagen && cmp > 0) ||             /* C */
(cmp == 0 && (priority > w_priority ||                        /* D */
    (priority == w_priority && tiebreaker > w_tiebreaker)))
```

Arm **D** fires whenever the LSNs are equal (`cmp == 0`), **regardless of
data_gen**. So a vote with a *lower* data_gen but an equal LSN and a higher
tiebreaker beats a vote with a *higher* data_gen (which arm B had made the
winner). Example (all electable, all same priority, same LSN):

```
vote 0: data_gen=0x80000004, tiebreaker=0   (seen first, becomes winner)
vote 1: data_gen=0xFFFFFFFF, tiebreaker=0   (higher data_gen, arm B => winner)
vote 2: data_gen=0x80000005, tiebreaker=1   (lower data_gen, equal LSN,
                                             higher tiebreaker, arm D => winner)
```

Final winner = vote 2, whose data_gen (`0x80000005`) is **lower** than vote 1's
(`0xFFFFFFFF`). The comparator is therefore **not** the total order
"data_gen, then LSN, then priority, then tiebreaker" that the function's
comment claims; it is not even a consistent order (the relation is not
transitive), which is the root reason (a) fails. The harness proves (d) only in
the sub-domain where all data_gens are equal, where the comparator does reduce
to a clean `(LSN, priority, tiebreaker)` order and IS order-independent.

### Why this is NOT a safety bug (no two masters)

A diverging phase-1 winner cannot elect two masters, because of the **VOTE2**
phase. After phase 1 (`IS_PHASE1_DONE`: enough VOTE1s heard and a winner
chosen), each site sends a single VOTE2 for *its own* `rep->winner`. A site
becomes master only through `I_HAVE_WON` (rep_elect.c:26):

```c
#define I_HAVE_WON(rep, winner) \
    ((rep)->votes >= (rep)->nvotes && winner == (rep)->eid)
```

so a candidate is elected only when a **majority** (`nvotes`) of sites
independently chose it as their phase-1 winner and voted VOTE2 for it. Two
distinct candidates cannot each collect a majority of the same group (that is
exactly what H3 proves: any two majorities intersect). Order dependence can at
worst make an election **fail to settle** in a round (no candidate reaches a
majority of VOTE2s), which retries at the next egen — a liveness cost in a rare
all-tied case, not a split brain.

### Disposition

No engine change on `test/cbmc-expand` (per the workstream rules). This is a
real correctness-of-comment / order-consistency defect with a liveness, not
safety, impact. A follow-up could either (1) make the comparator a genuine
total order (gate arm D on `data_gen == w_datagen`, and give the first vote the
same comparison path as the rest) and add a TCL rep regression that drives an
all-tied mixed-version set, or (2) correct the comment to describe the actual
order. `lts/2026.10.7` has the identical `__rep_cmp_vote` (checked below), so a
fix would warrant an LTS backport. Flagged to the lead; left as a documented
finding here because it does not threaten the one-master-per-generation
invariant.

## BUG FOUND (fix branch `fix/bt-decompress-bounds`) — `__bam_defdecompress` over-reads a truncated compressed item

`harness_bt_decompress.c` on the **unmodified** engine code produces a
counterexample (`VERIFICATION FAILED`). It is a **real out-of-bounds read** on
hostile input: a compressed B-tree page comes from disk, so a corrupt or
truncated compressed key/data item reaches `__bam_defdecompress`
(`src/btree/bt_compress.c:483`).

### Root cause

The decoder reads each varint — and the leading magic byte — **before** it
checks the compressed buffer is long enough:

```c
s = (u_int8_t*)compressed->data;
if (*s == CMP_INT_SPARE_VAL) {        /* (1) reads s[0] with NO size check */
    ++s; size = 1;
    size += __db_decompress_count_int(s);   /* (2) reads s[0] (= data[1]) */
    if (size > compressed->size)            /*     ... the check is AFTER */
        return (EINVAL);
    s += __db_decompress_int32(s, &prefix);
    ...
```

`__db_decompress_count_int(buf)` returns `__db_marshaled_int_size[buf[0]]` — it
dereferences `buf[0]`. Both call sites (the duplicate path and the
non-duplicate path) call it, then test `size > compressed->size`, so the
lookahead byte is read one step before the bound that would have rejected it.
And `*s` at the very top is read with no check at all, so a **zero-length**
compressed item (`compressed->size == 0`) over-reads immediately.

The subsequent `__db_decompress_int32` reads *are* covered by the preceding
`size > compressed->size` check (the offset bookkeeping in `size` matches `s`),
so the over-read is exactly the `count_int` lookahead byte and the `*s` magic
byte — up to one byte past `compressed->size` at each of the three varint
sites, and the first byte of an empty buffer.

### Minimal reproducer (CBMC counterexample)

The cheapest counterexample is `compressed->size == 0`: `*s` reads
`compressed->data[0]`, one byte past a zero-length object. A one-byte item
whose leading byte is a multi-byte varint tag trips the `count_int` lookahead
at the second varint. CBMC reports two genuine failures on the real code:
`__bam_defdecompress … *s: pointer outside object bounds` and
`varint … buf[0]: pointer outside object bounds`.

### The fix (on `fix/bt-decompress-bounds`, built here as `-DB1_FIXED`)

Check that the next byte index is still inside `compressed->size` **before**
every `__db_decompress_count_int`, and that `compressed->size >= 1` before the
magic `*s`. `harness_bt_decompress.c -DB1_FIXED` builds this variant and
VERIFIES (no over-read, no over-write) over the whole input space.

### How this harness runs

`run.sh` runs two arms:

* `b1-fixed` — the `-DB1_FIXED` variant — must VERIFY.
* `b1-decompress` — the **unmodified engine code** — is registered as a
  must-FAIL arm (like `okitem` before `2ca81268a`): it documents the live bug
  and keeps the harness honest. When `fix/bt-decompress-bounds` lands on
  master, flip this arm to a normal `run` (verify) and drop `-DB1_FIXED`.

### Modelling note (varint helpers)

The harness models `__db_decompress_count_int` / `__db_decompress_int32` by
their exact **read footprint** (the count reads `buf[0]`; the int32 reads the
leading `width` bytes, `width` from the real `__db_marshaled_int_size` table),
rather than `#include`-ing `db_compint.c`. The helpers' arithmetic is already
proved against the real source by `harness_varint.c`; re-exploring it here
blows the SAT formula past the suite's 5-minute budget, and the property B1
proves depends only on *which bytes* the helpers touch, which the models
reproduce faithfully.

## BUG FOUND (fixed in `2ca81268a`) — `__db_ret_okitem` hash-offpage guard read the wrong byte

`harness_okitem.c` on the **unmodified** engine code produces a
counterexample (`VERIFICATION FAILED`). Investigation showed it is a **real
latent OOB read**, of the exact class bug #67 set out to close.

### Root cause

`HPAGE_PTYPE` (src/dbinc/db_page.h:583) had no parentheses around its
argument:

```c
#define HPAGE_PTYPE(p)  (*(u_int8_t *)p)     /* p is NOT parenthesised */
```

`__db_ret_okitem` (src/db/db_ret.c:102, 106) calls it with a **compound**
argument:

```c
if (HPAGE_PTYPE((u_int8_t *)h + off) == H_OFFPAGE && ...)   /* line 102 */
if (HPAGE_PTYPE((u_int8_t *)h + off) != H_OFFPAGE && ...)   /* line 106 */
```

Because `*` binds tighter than the `+ off` the caller passes, this expands to

```c
*(u_int8_t *)(u_int8_t *)h + off      ==   ((u_int8_t *)h)[0] + off
```

i.e. it reads **the page's first byte plus `off`**, *not* the item's type
byte at `page[off]`. So okitem's H_OFFPAGE detection on hash pages is broken:
it can classify an on-page `H_OFFPAGE` item as non-offpage and skip the
`off + HOFFPAGE_SIZE <= pgsize` bound, then `__db_ret` copies out a
`HOFFPAGE` header that runs past the end of the page — a heap OOB read on
corrupt/hostile input. (It can also misfire the other direction.)

These are the **only two call sites in `src/`** that pass a compound
expression to `HPAGE_PTYPE`; every other caller passes a bare pointer
variable, so the missing parens don't bite them. The defect is isolated to
`__db_ret_okitem` and was introduced by the #67 fix itself.

### Minimal reproducer (CBMC counterexample, hash page, pgsize 64)

```
page[25] = 8    (TYPE = P_HASH_UNSORTED)
page[20] = 1    (NUM_ENT = 1)
inp[0]   = 58   (page[26..27]; item offset off = 58)
page[58] = 3    (H_OFFPAGE)   → item's real type byte says "offpage"
                → HOFFPAGE header spans [58, 58+12=70), which is > 64
okitem(dbp, h, indx=0) returns 0 ("safe")   ← WRONG: HPAGE_PTYPE read
                                              page[0]+58 = 58, not page[58]=3,
                                              so the offpage bound was skipped
```

Confirmed by native `gcc` execution as well as CBMC (not a CBMC artifact):
`HPAGE_PTYPE((u_int8_t *)h + off)` returns `page[0] + off`, and okitem
returns 0 for an item whose on-page extent runs 6 bytes past the page.

### The fix (applied in `2ca81268a`)

One line, in `src/dbinc/db_page.h`:

```c
-#define HPAGE_PTYPE(p)  (*(u_int8_t *)p)
+#define HPAGE_PTYPE(p)  (*(u_int8_t *)(p))
```

The harness now verifies on the real macro. `-DHPAGE_PTYPE_SABOTAGE` puts the
old macro back, and that build must fail.

## Notes on the `okitem` harness bounds & flags

* The `okitem` verification uses `--bounds-check --no-pointer-check`. It keeps
  the array-bounds check (real OOB *reads*) and uses `__CPROVER_r_ok(ptr, len)`
  — CBMC's "this region is readable" predicate — to assert the exact
  downstream read is in-bounds. `--pointer-check`'s *pointer-relation* check is
  turned off only because `okitem` deliberately **forms** a past-the-end
  pointer (`(u_int8_t *)(inp + NUM_ENT(h))`) for its bounds arithmetic without
  dereferencing it; that formation is a modelling artifact, not the safety
  property.
* The harness also constrains `NUM_ENT(h) <= pgsize` and the offset-table slot
  `inp[indx]` to be on-page. The latter documents a **secondary observation**:
  the `P_HEAP` path reads `inp[indx]` for `indx` up to the untrusted
  `HEAP_HIGHINDX(h)` *without* an on-page bound on that slot (btree/hash do
  bound it). On a normal-size page this is benign; it is the same *class* as
  #67 and worth an engine review, but is not exercised as a hard failure here.

## Layout

```
test/cbmc/
  harness_*.c        one harness per target (self-documenting header comment)
  stubs/             empty db_config.h / db_int.h so a #include'd real .c
                     compiles with only the typedefs the harness supplies
  run.sh             run all harnesses, report PASS/FAIL + runtime
  cbmc.yml.workflow  advisory GitHub Actions workflow (see below)
  README.md          this file
```

## CI

`cbmc.yml.workflow` is a GitHub Actions workflow that runs the suite on push /
PR (advisory — it does not gate merges, and the known `okitem` bug is expected
to reproduce until the engine fix lands). It is staged as
`*.yml.workflow` rather than under `.github/workflows/` because pushing
workflow files requires an OAuth token with the `workflow` scope. To enable
it, a maintainer copies it into place:

```sh
cp test/cbmc/cbmc.yml.workflow .github/workflows/cbmc.yml
```
