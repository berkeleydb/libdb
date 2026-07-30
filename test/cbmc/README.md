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
| `harness_okitem.c`   | `db/db_ret.c` `__db_ret_okitem` (the #67 guard) | **safety guarantee**: if okitem returns 0 ("safe"), the exact downstream `__db_ret`/`__db_retcopy` read stays on-page (`__CPROVER_r_ok`) | page ≤ 64 bytes (loop-free) | ~1 s | **FAILS on real code → BUG FOUND (below)**; SUCCESSFUL with `-DHPAGE_PTYPE_FIXED` |

`run.sh` runs `okitem` twice: `-DHPAGE_PTYPE_FIXED` (the one-line fix →
`VERIFICATION SUCCESSFUL`, proving both the guarantee and the fix) and the
unmodified engine macro (→ `VERIFICATION FAILED`, reproducing the bug).

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

## BUG FOUND — `__db_ret_okitem` hash-offpage guard reads the wrong byte

`harness_okitem.c` on the **unmodified** engine code produces a
counterexample (`VERIFICATION FAILED`). Investigation showed it is a **real
latent OOB read**, of the exact class bug #67 set out to close.

### Root cause

`HPAGE_PTYPE` (src/dbinc/db_page.h:583) has no parentheses around its
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

### Suggested fix (engine — NOT applied here; harnesses are additive)

One line, in `src/dbinc/db_page.h`:

```c
-#define HPAGE_PTYPE(p)  (*(u_int8_t *)p)
+#define HPAGE_PTYPE(p)  (*(u_int8_t *)(p))
```

Building the harness with `-DHPAGE_PTYPE_FIXED` applies exactly this and the
proof then reports `VERIFICATION SUCCESSFUL` for all okitem safety
properties, validating the fix. (Equivalently, the two call sites could hoist
`(u_int8_t *)h + off` into a local, but fixing the macro closes the whole
class.)

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
