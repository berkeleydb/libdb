<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.

SPDX-License-Identifier: Sleepycat

See the file LICENSE for redistribution information.
-->

# T7 — `SH_LIST_INSERT_HEAD` into an empty list "fails" the TestQueue matrix

> **Preserved from `/tmp`.** The investigation behind T7: `shqueue.h`'s
> macros were correct and the TEST was undefined behaviour, which gcc's
> dead-store elimination acted on while clang did not. Kept because the
> reproduction requires gcc and the reasoning that distinguished a harness
> bug from an engine bug is not obvious from the one-line fix.


**Verdict: (a). The macros are correct. The TEST was invalid as written.**
The engine is **not** affected. `src/dbinc/shqueue.h` is unchanged by this fix.

The tracker framed this as "either the macros are wrong for separately-allocated
elements (then the test is invalid) or `INSERT_HEAD`'s empty-list arm is wrong
(then the engine is affected)". It is the first, but the reason is sharper than
"offsets don't reach": the offsets reach fine. The separate allocation makes the
pointer arithmetic **undefined**, and gcc ≥ -O1 exploits that undefinedness to
**delete the stores that initialise the element**.

---

## 1. Root cause

`shqueue.h` is offset-based. `SH_LIST_INSERT_HEAD` stores

```c
(head)->slh_first = SH_PTR_TO_OFF(head, elm);     /* (u_int8_t*)elm - (u_int8_t*)head */
```

and `SH_LIST_FIRSTP` recovers the element as `(u_int8_t *)head + slh_first`.

Forming a pointer by adding an integer to `head` is only defined when the result
lands inside **the same object** as `head` (C11 6.5.6p8). That is precisely the
situation the macros were designed for: in the engine, heads and elements are
both carved out of **one** mapped region and reached as region-base + offset via
`R_ADDR()` (`src/dbinc/region.h:300`).

`TestQueue.c` instead `calloc()`d the head and **every element separately** — 21
call sites. So `head + slh_first` walked out of the head's own object. Consequence
in gcc's optimiser:

1. Points-to analysis proves nothing derived from `head` can alias the
   `calloc()`d block (correctly — they are distinct objects).
2. Therefore the block is unreachable once `sh_l_insert_head()` returns.
3. Dead-store elimination removes the writes to `ele->content` **and** to
   `ele->sh_les.sle_next` / `sle_prev`.

The element reads back as **all zeroes**. Case 2 (`INSERT_HEAD` into an empty
`sh_list`) then verifies as a one-element list whose content is NUL, and
`sh_l_discard()` faults walking it — the list genuinely *is* corrupt, exactly as
the tracker observed. It is corrupt because the initialising stores were
compiled away, not because `INSERT_HEAD` computed a wrong link.

### The stores are visibly absent from the generated code

`ins()` compiled at gcc -O2 (full body). `calloc` is called, the *offset* is
computed and stored into the head (`movq %rax,(%rbx)`), but **no store to
`content`, `sle_next` or `sle_prev` is emitted**:

```asm
ins:
        movl    $24, %esi
        movl    $1, %edi
        call    calloc@PLT
        movq    (%rbx), %rdx
        subq    %rbx, %rax          # SH_PTR_TO_OFF(head, elm)
        cmpq    $-1, %rdx
        je      .L2                 # empty-list arm: falls straight through
        ...                         # non-empty arm fixes up the old first
.L2:
        movq    %rax, (%rbx)        # head->slh_first = offset
        ret                         # <-- element never initialised
```

---

## 2. Evidence distinguishing (a) from (b)

### 2.1 Macro-only reproducer — PASSES at every optimisation level

A standalone reproducer using *only* the macros (`/tmp/t7/repro.c`), which keeps
the element pointer live, passes identically at -O0/-O1/-O2. Measured for
`INSERT_HEAD` into an empty list:

```
after INSERT_HEAD(A) into empty: slh_first=32
   e[0] content=A next=-1 prev=-32   (SH_PTR_TO_OFF(head,e)=32)
   FOREACH got "A" want "A"  ok
   SH_LIST_PREV(e0)=0x...310 (head=0x...310)   <-- PREV lands exactly on the head
```

Per the brief: **the reproducer passes, so the bug is in the harness.** Stated
plainly rather than papered over with a macro change.

### 2.2 The empty and non-empty arms do agree on `sle_prev`

The brief asked specifically about this. They agree; verified numerically.

* Empty arm: `sle_prev = SH_PTR_TO_OFF(elm, &head->slh_first)` = `-32`.
  `SH_LIST_PREV` computes `elm - (*__SH_LIST_PREV_OFF)` and lands on the head
  (printed above) — correct, since for an empty list the predecessor's "next
  location" **is** `&head->slh_first`.
* Non-empty arm, after `INSERT_HEAD(A)` then `INSERT_HEAD(B)`:
  `B.prev = -64` (B → `&head->slh_first`), and A's prev was rewritten by
  `SH_LIST_NEXT_TO_PREV` to `+40` = A → `&B.sle_next`.
  `FOREACH` = `"BA"`; removing B leaves `"A"`.

Both arms maintain the same invariant: `sle_prev` is the offset from the element
to its predecessor's `sle_next` slot, with `&head->slh_first` standing in as the
head's "next" slot. `SH_LIST_REMOVE` consumes it correctly in both cases
(`REMOVE` of the only element restores `slh_first = -1`, measured). **No defect.**

### 2.3 Isolation: the elision is caused by the pointer not escaping

Three functions, byte-identical macro use, differing only in whether the
element pointer escapes (`/tmp/t7/mech.c`):

| variant | shape | gcc -O0 | gcc -O1/-O2 | clang -O1/-O2 |
|---|---|---|---|---|
| `insA` | pointer never escapes (**the old harness**) | present | **ELIDED** | present |
| `insB` | also stored to a global | present | present | present |
| `insC` | also passed to an opaque function (**the engine**) | present | present | present |

Adding a single escape makes the stores reappear with the macros untouched. That
localises the fault to the harness's allocation/escape shape, not to `shqueue.h`.

### 2.4 Control: same macros, one backing object → passes everywhere

The full 34-case × 2-structure matrix run against a pool allocator, so head and
elements share one object (`/tmp/t7/drv.c`), ascending **and** descending
addresses (elements below the head, i.e. negative offsets):

```
-O1 mode0: 2/2 suites 68/68      -O1 mode1: 2/2 suites 68/68
-O2 mode0: 2/2 suites 68/68      -O2 mode1: 2/2 suites 68/68
-O3 mode0: 2/2 suites 68/68      -O3 mode1: 2/2 suites 68/68
```

Negative offsets work, so this was never a direction or range problem. (As the
tracker noted, `db_ssize_t` is 64-bit here — confirmed `typedef ssize_t` at
`db.h:123`, `HAVE_MIXED_SIZE_ADDRESSING` off — so range was never the issue.)

### 2.5 Control: the engine's exact shape passes at every -O

Head *and* elements inside one object, both reached as base+offset from an
**opaque** region base — mirroring `R_ADDR(reginfo, off)` (`/tmp/t7/engshape.c`):

```
-O0 OK   -O1 OK   -O2 OK   -O3 OK      (FOREACH="BA")
```

### 2.6 Why this never showed up, and why my first build passed

**The nix dev shell defaults to `clang`**, which does not perform this
transform. The tracker reproduced on gcc. My first `cutest` build passed 68/68
for this reason; it was not a failed reproduction. Forcing `CC=gcc` reproduces
the tracker text byte-for-byte:

```
TESTING: sh_list
.....*
case 2 INSERT_HEAD in sh_list init: "" desired: "A" elem: "(null)" insert: "A" got: " - walking the list using the _FOREACH macro failed
exit=139
```

Compiler/opt sensitivity of the *unfixed* harness:

| | -O0 | -O1 | -O2 | -O3 | -O2 -flto |
|---|---|---|---|---|---|
| gcc | pass | **fail** | **fail** | **fail** | **fail** |
| clang | pass | pass | pass | pass | pass |

A test that passes at -O0 and fails at -O1 with no source change is a UB
signature, which is what pointed at dead-store elimination rather than at a
link-arithmetic bug.

### 2.7 ASan corroboration (A/B, gcc -O1)

```
PRE-FIX : ERROR: AddressSanitizer: heap-use-after-free ... in sh_l_discard
          SUMMARY: heap-use-after-free TestQueue.c:254 in sh_l_discard
POST-FIX: (no ASan findings)
```

The use-after-free is downstream: the zeroed element made `slh_first` point at a
block `sh_l_discard` had already freed.

---

## 3. The fix (test-only)

`test/c/suites/TestQueue.c` — the sole file changed, +98/-21.
**`src/dbinc/shqueue.h` is untouched** (`git diff --stat -- src/dbinc/shqueue.h`
is empty).

Give the head and its elements one backing object, which is what the macros
require and what a shared region actually looks like:

* `static db_ssize_t tq_pool[1024]` — typed as `db_ssize_t` so the pool is
  naturally aligned for the link fields in every `SH_LIST_ENTRY`/`SH_TAILQ_ENTRY`.
* `tq_alloc()` — bump-allocates zeroed, rounded-up space; `assert`s on exhaustion.
* `tq_pool_reset()` — called from `sh_l_init()`/`sh_t_init()`, so each of the 34
  cases starts fresh (the previous case is discarded by then).
* `tq_free()` — no-op; the pool is released wholesale.

All 21 `calloc`/`free` call sites were converted mechanically; the only remaining
occurrences of the word `calloc` are in the explanatory comment. A comment at the
pool records *why* separate allocation is wrong, so it does not get "cleaned up"
back into `calloc()`.

Note this keeps the test honest: it still exercises the real macros over the full
matrix, and it now exercises them in the memory shape the engine uses.

---

## 4. Verification

`cutest -s TestQueue`, gcc -O2 `--enable-diagnostic` — the config that failed:

```
TESTING: sh_list
....................................................................	100.00% passed (68/68).
TESTING: sh_tailq
....................................................................	100.00% passed (68/68).
.
OK (1 test)
exit=0
```

**Zero failures: 136/136 assertions, no `*` (post-op) or `+` (pre-op) markers,
`exit=0`, 3/3 repetitions.** Same result on the clang build.

Full matrix of the harness, both compilers, including sanitizers:

| | -O0 | -O1 | -O2 | -O3 | -O2 -flto | ASan+UBSan | -DVERBOSE |
|---|---|---|---|---|---|---|---|
| gcc | pass | pass | pass | pass | pass | pass | pass |
| clang | pass | pass | pass | pass | pass | pass¹ | pass |

¹ clang UBSan reports 11 `call to function ... through pointer to incorrect
function type` findings in the Oracle-era `qfns[]` cast table. **Pre-existing and
unrelated**: the count is *identical* (11) when built from `HEAD`'s unmodified
`TestQueue.c`, so this fix neither introduces nor masks them. Worth a separate
tracker item; not touched here to keep the diff to the actual defect.

Other suites, gcc build — no regressions:
`TestDbTuner exit=0`, `TestKeyExistErrorReturn exit=0`, `TestPartial exit=0 (4 tests)`,
`TestEnvMethod exit=0`.

---

## 5. Engine impact: none

Requested explicitly, so stated precisely.

The four `SH_LIST_INSERT_HEAD` call sites in the engine are:

| site | head | element |
|---|---|---|
| `src/lock/lock.c:1473` | `&sh_locker->heldby` | `newl` |
| `src/lock/lock.c:2407` | `&sh_parent->heldby` | — |
| `src/lock/lock.c:2635` | `&new_locker->heldby` | `lp` |
| `src/lock/lock_id.c:538` | `&mlockerp->child_locker` | `lockerp` |

Both `SH_LIST_HEAD` declarations in the engine (`child_locker`, `heldby`,
`src/dbinc/lock.h:200,208`) are **embedded members of `__db_locker`**, which
itself lives in the lock region. The elements (`__db_lock`, `__db_locker`) come
from `__env_alloc()` out of that same region and are reached via
`R_ADDR(&lt->reginfo, off)`. Head and elements are therefore inside one mapped
object — the defined case — and the pointers additionally escape through
region-offset round-trips the compiler cannot see through, so neither the UB nor
the elision applies. §2.5 confirms this shape empirically at -O0 through -O3.

**No shared-region, on-disk or in-region format consequence.** No layout, no
field width, no offset convention changed; `shqueue.h` is byte-identical. The
fix is confined to test code, so there is no ABI or region-compatibility impact
and no bump required.

---

## 6. Correction to the tracker entry

T7's framing should be updated: the hypothesis "the macros are wrong for
separately-allocated elements" is right that separate allocation is the trigger,
but the mechanism is **UB-driven dead-store elimination of the element's
initialising stores at gcc -O1+**, not a mis-computed link. The entry's
observation that 64-bit `db_ssize_t` rules out range overflow is confirmed and
was a useful exclusion. Add: **reproducing requires gcc; the nix dev shell
defaults to clang and passes either way**, which is worth recording so the next
reader does not conclude it is unreproducible.

---

## 7. Must-fail (sabotage) arm

A fix whose gate cannot fail proves nothing, so the pool was reverted to
separate `calloc()` at the **call sites only** (keeping `tq_alloc`/`tq_free`
defined) and rebuilt with the same gcc -O2 flags:

```
sabotage exit=139  (Segmentation fault)
case 2 INSERT_HEAD in sh_list init: "" desired: "A" elem: "(null)" insert: "A" got: " - walking the list using the _FOREACH macro failed
```

That is the tracker symptom reproduced on demand, and the same source with the
pool restored reports `=== clean ===`. So the single-object property is
demonstrably the thing carrying the fix.

**First sabotage attempt was itself broken** and is worth recording: a `sed` that
rewrote `tq_free(` → `free(` also renamed the *definition*, giving
`error: static declaration of 'free' follows non-static declaration`. The binary
never built and the arm reported `exit=127`, which is easy to misread as "failed
correctly". Re-running with a targeted `perl` that touches only call sites gave
the real `exit=139`. When a sabotage arm's exit code is 126/127, suspect the
harness before believing the result.
