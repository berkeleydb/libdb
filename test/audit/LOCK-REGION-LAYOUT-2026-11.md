# Lock-manager region layout: the final word (A3, 2026-11)

> Review item A3 (REVIEW-2026-11.md Sec 1.5; A/bostic.md action item 1): settle
> the lock-manager region layout **once** \u2014 final stripe count and padding \u2014 and
> stop re-breaking the region format release over release. Design Lesson 11: do
> the whole structural change, don't make a partial change and clean up later.

This note is **normative**: it records the layout the fork commits to, the
reasons, and a test (`test/db/run_lock_region_layout.sh`) that fails if the
layout moves without the `REGION-SIG-CHANGE-INTENDED` escape token.

## 1. The churn A3 is reacting to

| When | Commit | What changed | Region signature (LP64 x86_64) |
|------|--------|--------------|-------------------------------|
| before P12 | \u2014 | region-wide locker free list / counters under `mtx_locker_stripe[0]` | `0xb86f77f0` |
| rejected | `fa109c45a` | P12 (per-stripe locker alloc) implemented, measured **-18%**, **not merged** | (not shipped) |
| v2026.10.5 | `2d8397dfe` + `eb910d4cf` | P12 merged with P13; `sizeof(DB_LOCKREGION)` 1000 \u2192 5064 (64 padded stripes) | `0xb86f77f0` \u2192 `0x0b67cec8` |
| 32-bit fix | `553b649a1` | G19: the stripe pad was hand-written `pad[24]`; made it **computed** from the field list so ILP32 (24-byte fields) stops failing the one-cache-line assertion | (unchanged on LP64) |
| v2026.10.6 | `4a4cff4d9` | RFC 0015 S3/S4/M5/S7 scalability + aarch64 mutex; region layout touched again | `0xe7904e69` \u2192 `0x56a06689` |
| master today | `e1fe5c2c4` | (further field moves since .6) | **`0xe67a9de8`** |

Two breaks in two consecutive regular releases (.5 and .6), plus a 32-bit
pad fix in between. That is exactly the "nibbled to death by ducks" pattern
Design Lesson 11 warns against. **Nothing silently broke** \u2014 the region
signature gate turns every mismatch into a clean `DB_VERSION_MISMATCH` at attach
(env_region.c), and no release in this window is LTS \u2014 but the layout was tuned
in public, release over release, which is what A3 asks to stop.

## 2. The layout the fork commits to

Measured on the dev box (LP64 Linux x86_64, `--enable-diagnostic`,
master `e1fe5c2c4`):

```
DB_CACHE_LINE_BYTES       64
LOCK_LOCKER_STRIPES       64
sizeof(DB_LOCKERSTRIPE)   64      (exactly one cache line, asserted at build)
sizeof(DB_LOCKREGION)     5064
sizeof(DB_LOCKPART)       88
sizeof(struct __db_lock)  80
sizeof(DB_LOCKOBJ)        136
sizeof(DB_LOCKER)         176
region signature          0xe67a9de8
```

### 2.1 Stripe count: **64, fixed**

`LOCK_LOCKER_STRIPES = 64` (src/dbinc/lock.h:45), a compile-time constant, not a
tunable. It is **not** `lk_partitions` (the object-partition count, which stays
`10 * ncpu`); it is the fixed width of the locker-hash stripe array. 64 is the
right fixed choice and must not be re-tuned per release:

* **It is a power of two**, so `LOCK_LOCKER_STRIPE(id) = id & 63` (lock.h:46) is
  a mask, not a modulo \u2014 on the locker-get hot path.
* **It bounds the whole-table walks.** `LOCK_LOCKERS` takes all 64 stripes
  (deadlock detector, failchk, stat, free-list exhaustion). A larger count makes
  those walks proportionally more expensive; P12's own measurement is the
  cautionary tale (`fa109c45a`: removing the stripe-0 admission control cost
  -18% until P13 removed what it was gating). 64 is enough to spread the
  reader-count atomics across cache lines on today's 64\u2013128-vCPU parts without
  making the full-table latch acquisition a new bottleneck.
* **It is independent of CPU count**, so the region layout and signature are the
  same on every host \u2014 a machine-independent region is a hard requirement for
  the cross-attach contract (two processes on the same region must agree on the
  layout byte-for-byte). Tying the stripe count to `ncpu` (as `lk_partitions`
  is) would make the signature host-dependent and defeat the gate.

**Normative:** `LOCK_LOCKER_STRIPES` stays 64. Changing it is a
`REGION-SIG-CHANGE-INTENDED` event and requires re-justifying against this note,
not a release-over-release tuning knob.

### 2.2 Padding: **computed, one cache line, asserted**

`DB_LOCKERSTRIPE` is padded to exactly `DB_CACHE_LINE_BYTES` (64) so adjacent
stripes never share a line (false sharing between cores working neighbouring
stripes was the measured motivation). The padding is **computed** from the field
list, not hand-written:

```
u_int8_t pad[DB_CACHE_LINE_BYTES - sizeof(struct __db_lockerstripe_fields)];
```

with `struct __db_lockerstripe_fields` a shadow of the real fields kept
field-for-field in sync (src/dbinc/lock.h:130-168), and a build-time assertion
(`DB_LOCKERSTRIPE_SIZE_CHECK`, lock.h:183-190) that fails the build if
`sizeof(DB_LOCKERSTRIPE) != 64`. This is the G19 fix (`553b649a1`): the first
version hand-wrote `pad[24]` assuming 40-byte fields on every platform, which is
false on ILP32 (24-byte `SH_TAILQ_HEAD` pairs), so the struct was 48 bytes there
and the assertion fired. Computing the pad makes the one-cache-line invariant
hold on LP64 *and* ILP32 from one declaration.

**Normative:** the pad stays computed, never hand-written; the one-cache-line
static assertion stays; `DB_CACHE_LINE_BYTES` stays 64 (it is the line size of
every platform the fork qualifies; a platform with a different line would need a
`REGION-SIG-CHANGE-INTENDED` bump and a new qualification, not a silent edit).

### 2.3 The P12 A/B switch stays, off the hot path

`region->locker_shard` (lock.h, read once from `DB_NO_LOCKER_SHARD` at region
create) selects per-stripe locker allocation (the P12 fix, default on) vs the
pre-P12 stripe-0-guarded region-wide lists. It is stored in the region, not
consulted per call, so (a) every attacher inherits the creator's choice (a
locker freed to a different stripe than it came from would corrupt both lists),
and (b) one binary can A/B the subsystem over an identical region layout \u2014 the
only unconfounded way to measure it (per the `run_bench.sh` DB_PRIVATE-layout
warning). This switch is part of the committed layout; it does not change
`sizeof(DB_LOCKREGION)` and must not be removed without re-opening the P12
decision.

## 3. The pin

`test/db/run_lock_region_layout.sh` drives `test/db/lock_region_layout.c`, which
prints and asserts the sizes in Sec 2. The test **FAILS if any of these move**:

* `LOCK_LOCKER_STRIPES != 64`
* `sizeof(DB_LOCKERSTRIPE) != DB_CACHE_LINE_BYTES` (already a build assertion;
  the test confirms it at runtime too, so a non-DIAGNOSTIC build is covered)
* `sizeof(DB_LOCKREGION)`, `sizeof(DB_LOCKPART)`, `sizeof(struct __db_lock)`,
  `sizeof(DB_LOCKOBJ)`, `sizeof(DB_LOCKER)` differ from the recorded values

It does not compare the region signature with a recorded value. The signature
depends on the platform and the configure options (`0xe67a9de8` for an
`--enable-diagnostic --enable-test` build, `0x841f4ea8` for a plain one, on
LP64 x86_64), so one constant failed every other configuration, including
CI's plain jobs. The signature is gated where like is compared with like: the
`region signature` CI job (PR against base, same configure) and the LTS L1 gate.

A deliberate region change updates the recorded values **in the same commit that
carries `REGION-SIG-CHANGE-INTENDED`**, exactly as handle_sizes.c pins the
public struct sizes behind the soname rule. An *accidental* layout change \u2014 a
field added to `DB_LOCKREGION`, a stripe-count edit, a pad miscomputation \u2014 fails
this test in the commit that causes it, which is the whole point of A3: the
layout is settled, and moving it is now a loud, intentional act, not drift.

> The signature is platform-specific (each ABI and endianness has its own, and
> ILP32 differs from LP64). The runner records the LP64 x86_64 value as the
> canonical one and SKIPs the signature check on other platforms with a reason,
> while still pinning the stripe count, the cache-line assertion and the
> CPU-independent structural constants everywhere.
