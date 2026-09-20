# P3 — `DB_LOG_DIRECT` could not complete a transactional open

Defect P3 (`docs/design/perf-gate-gaps.md`, gap G15 family). Fixed in
`src/log/log_put.c:__log_write_direct`.

## The defect

`O_DIRECT` constrains **three** properties of every transfer: the buffer
address, the file offset, and the transfer length must each be a multiple of the
device block size. `__log_write` satisfied none of them:

```c
__os_io(env, DB_IO_WRITE, dblp->lfhp, 0, 0, lp->w_off, len, addr, &nw)
```

- `addr` is either the region log buffer or a caller's record — no alignment
  guarantee.
- `lp->w_off` is the byte-granular append frontier. Log records are packed end
  to end, so it is block-aligned essentially never.
- `len` is an arbitrary record length.

Under `--enable-o_direct` + `DB_LOG_DIRECT` the first transactional `DB->open`
failed `EINVAL`, so the flag could not be used at all. Two distinct failures
were observable on the development box:

```
flag_behaviour: BDB0137 write: 0x7ffee71da177,   1: Invalid argument
flag_behaviour: BDB0137 write: 0x7fd7d341a590, 131: Invalid argument
```

The 1-byte write is `__db_file_extend` (log preallocation), reached from
`__log_write` when `lp->w_off == 0`. The 131-byte write is `__log_write`
itself. **Both had to be fixed**; the brief named only the second.

## The three constraints, and how each is satisfied

`__log_write_direct` restages every write into a block-aligned window. With
`B = DB_LG_DIRECT_ALIGN = 4096`:

```
base  = w_off & ~(B-1)        /* round the frontier DOWN to a block */
head  = w_off - base          /* bytes of the first block that precede us */
total = head + len
```

and it emits `ceil(total / B)` writes of exactly `B` bytes each, starting at
`base`.

| Constraint | How it is satisfied |
|---|---|
| **buffer address** | `ALIGNP_INC(stagebuf, B)` over a `2*B` stack buffer. Same shape as the P2 fix: an over-sized stack array, no allocation, so no error path can leak — which matters on a path that can fail mid-loop. |
| **file offset** | Every write starts at `base + off` where `base` is `w_off` rounded down to `B` and `off` is a multiple of `B`. Aligned by construction. |
| **length** | Every write is exactly `B` bytes. The leading partial block is completed by reading it back; the trailing partial block is zero-padded. |

`DB_LG_DIRECT_ALIGN` is 4096, not a probe of the device's logical block size:
4096 covers every device in common use (512e and 4Kn alike) and over-aligning is
harmless. It is a `#define` in `src/dbinc/log.h`, consulted only under
`DBLOG_DIRECT`.

## The length decision, and why the log stays readable

**Chosen: pad to a block multiple, with the leading partial block read back.**

The brief listed three options. The other two were rejected:

- *Aligned prefix via `O_DIRECT`, unaligned tail through a buffered
  descriptor.* Two descriptors onto one file with different cache semantics.
  The tail would live in the page cache while the prefix bypassed it, so
  `__os_fsync` on the direct fd would not cover the tail, and the durable
  frontier would depend on which descriptor last touched a block. That is
  exactly the class of bug that produced the `__memp_aio_drain` false-durability
  incident. Rejected.
- *A persistent aligned staging buffer the size of the log buffer.* Needs a
  pointer in `DB_LOG`, which `src/env/env_sig.c` hashes via
  `__ADD(__db_log)` — adding a field would change `__env_struct_sig()` and make
  every existing environment refuse to attach (`BDB1539`), for an optional flag.
  Rejected on compatibility grounds; noted as the upgrade path in a
  `ponytail:` comment.

**Why the padding does not break the reader.** The padding occupies
`[w_off + len, roundup(w_off + len, B))` — strictly *beyond* the write frontier.
Three independent reasons it is inert:

1. `lp->w_off` still advances by exactly `len`. The next write's `base` lands
   back inside the padded block and re-covers it with real data. Padding is
   never durable state; it is always the tail of the last write.
2. The reader does not scan for records past the frontier. It follows
   `hdr->prev`/`hdr->len` chains and validates a per-record checksum
   (`__db_chksum` / `LOG_HDR_SUM`), so a zero region is not a candidate record.
   `__log_valid` explicitly treats a zero-filled header as `DB_LV_INCOMPLETE`
   rather than corruption.
3. A log file is *already* zero over that ground. `__db_file_extend` and
   `DBLOG_ZERO` preallocate log files zero-filled, and the padding writes the
   same zeros the preallocation would have. On a filesystem without
   preallocation the region is sparse, which reads as zero.

**LSN space is untouched.** Because `w_off` advances by `len` and not by the
padded amount, no offset, `f_lsn`, `s_lsn`, `b_off`, or `hdr->prev`
arithmetic anywhere in the log or txn subsystems changes meaning. Nothing
outside `__log_write_direct` is aware that the write was restaged.

**Verified, not asserted:** `db_log_verify` on a log written entirely through
this path reports `BDB2504 Log verification ended and SUCCEEDED`, and
`db_printlog` parses 1255 records with empty stderr.

## The offset decision

`lp->w_off` is **not** block-aligned in general — log records append end to end,
so it is aligned only by coincidence. Measured: instrumenting
`__log_write_direct` on a 3000-transaction run, `head != 0` on **1999 of 2000**
writes. The leading-block read-back is therefore the hot path, not an edge case.

That read-back is the one operation in the fix that can lose already-durable
data if it goes wrong, so it is guarded:

```c
if ((ret = __os_io(env, DB_IO_READ, ..., base, DB_LG_DIRECT_ALIGN, stage, &nio)) != 0)
        return (ret);
if (nio < (size_t)head) {
        __db_errx(env, "Short read of the log block preceding offset %lu", ...);
        return (EIO);
}
```

A short read means the file does not reach `w_off`, i.e. the log was truncated
under us. Rewriting the block from indeterminate stack contents would destroy a
durable record, so the function fails instead. Both `__os_io` calls propagate
their return unchanged; there is no path in the new code that discards an error.

## The preallocation helpers

`__db_file_extend` (a 1-byte write) and `__db_zero_extend`
(`buffer_size`-granular writes) cannot go out on an `O_DIRECT` descriptor. They
are now skipped under `DBLOG_DIRECT`. Their errors were *already* ignored by
design ("we may have run out of disk space, but that's no reason to quit"), so
the only thing lost is a sync-performance optimization on log creation — but the
`__db_syserr` each failed call logged blamed the wrong site on every single log
file creation, which is what made P3 look like one defect instead of two.

## Durability proof

`test/c/p3_durable.c` + `test/c/p3-durable-run.sh`, manifest tier `p3durable`.
No clean shutdown anywhere: the writer `_exit(0)`s and is then `SIGKILL`ed, with
no `DB_ENV->close`, no `DB->close`, and **no checkpoint** (a checkpoint would let
mpool satisfy the reads and take the log off the recovery path).

| Gate | Result |
|---|---|
| `p3durable@ack` | **PASS** — 300 commits acked `DB_TXN_SYNC` under `DB_LOG_DIRECT`, process killed, all 300 present after `DB_RECOVER` with byte-correct contents |
| `p3durable@verify` | **PASS** — `db_verify` clean on the recovered database |
| `p3durable@group` | **PASS** — 480 commits / 8 threads: 357 log syncs direct vs 225 buffered, `st_maxcommitperflush` = 6 in both arms |

The `@ack` gate takes its record count from the writer's own `DURABLE-ACK`
lines, not from the requested count, so a writer that died early cannot make the
gate easier by acking fewer commits.

**Group commit properties intact.** `st_maxcommitperflush` = 6 under the flag,
identical to buffered — waiters still share an fsync, so the
N-waiters-per-fsync property holds. The flush count is higher (357 vs 225)
because the restaging issues more, smaller physical writes; that is a throughput
cost, not a correctness or grouping change, and the runner's cross-arm check
fails if the flag ever degrades grouping by more than 3x. Absolute thresholds
alone would not catch a fix that serialized the log, which is why the buffered
arm is measured in the same run.

## Mechanism assertions

P2 taught that a passing behaviour test can be luck: `direct_db` passed both
before *and* after the P2 fix on this box, because the kernel tolerated the
unaligned buffer. Two mechanism gates were added.

**`test/c/p3_align.c`** (manifest `flag p3_align`). Models the staging
arithmetic and asserts all three constraints on every emitted write, plus the
property no behaviour test can reach: that the resulting file is **byte-identical
below the write frontier** to what the plain unaligned writes would have
produced, with zero padding above it. A staging loop that emitted
correctly-aligned *garbage* would satisfy every alignment check and silently
corrupt the log; only a content comparison catches it.

Verified to have teeth — three mutations of the modelled loop each turn it red:

| Mutation | Verdict |
|---|---|
| payload copied to `stage` instead of `stage + head` | `FAIL log byte 0 is 0f, expected 01 -- restaging LOST DATA` |
| head read-back removed (with the staging buffer poisoned) | `FAIL log byte 0 is a5, expected 01 -- restaging LOST DATA` |
| padding filled `0xff` instead of zero | `FAIL pad byte 37161 is ff, not zero` |

The poison (`memset(stagebuf, 0xA5, ...)` at the top of the modelled function)
is load-bearing and is commented as such: without it, removing the read-back
still passed, because `stagebuf` landed at the same stack address on each call
and happened to still hold the block being rewritten.

**`flag direct_log@io`.** `p3_align` models the arithmetic; this greps the real
library's syscalls so the model and the code cannot drift apart. Result:
**43982 of 43982** log `pread`/`pwrite` calls 4096-aligned in both offset and
length, zero violations.

## Honest reporting: does the unfixed path work on this box?

**No — P3 reproduces here**, unlike P2. On pristine master with
`--enable-o_direct`, `direct_log` fails with the two `EINVAL` writes quoted
above. So the behaviour test does distinguish fixed from broken on this
hardware, and the XFAIL→PASS transition is real signal rather than a no-op.

Two caveats stated rather than hidden:

- `/nvme` reports a 512-byte logical sector size (`lsblk LOG-SEC 512`), so a
  passing *behaviour* run only demonstrates 512-alignment. The code claims
  4096. That gap is why `direct_log@io` exists and asserts 4096 explicitly.
- Removing the head read-back from the **real library** did *not* fail the
  crash/recovery gate, because the uninitialised stack buffer happened to still
  contain the block being rewritten — the same accident the model poisons
  against. Poisoning the real staging buffer *and* removing the read-back
  aborts (`exit 134`), and dropping the final block of every multi-block write
  breaks the writer outright (0 commits acked). Reported as measured: the
  crash gate catches gross restaging errors, and `p3_align` is what covers the
  subtle head-bytes case.

## Verdicts: before and after

| Gate | Before (master) | After |
|---|---|---|
| `flag direct_log` | **XFAIL** (`DB->open under DB_LOG_DIRECT failed EINVAL`) | **PASS** (`040000 set on all 1 fd(s) of /log.`) |
| `flag direct_log`, `FLAGB_STRICT=1` | **FAIL** (XFAIL allowance refused) | **PASS** |
| `flag p3_align` | did not exist | **PASS** |
| `flag direct_log@io` | did not exist | **PASS** (43982/43982 aligned) |
| `p3durable@ack` / `@verify` / `@group` | did not exist | **PASS** / **PASS** / **PASS** |

`FLAGB_STRICT=1` full run: **ALL FLAG BEHAVIOUR TESTS PASS**, 15/15 verdicts,
no XFAIL anywhere.

The XFAIL allowance is **removed for `direct_log` only** — the branch is gone
from `flag_behaviour.c`, so a failed transactional open under `DB_LOG_DIRECT` is
now a hard FAIL, a regression rather than a recorded expectation. `direct_db`
keeps its P2 allowance untouched.

## The default build is behaviourally unchanged

`DB_LOG_DIRECT` stays optional and off by default.

1. **The flag cannot be set.** Without `--enable-o_direct`, `HAVE_O_DIRECT` is
   undefined, `__os_support_direct_io()` returns 0, and
   `log_method.c:461` refuses the flag. Asked directly:
   `log_set_config(DB_LOG_DIRECT) = 22 (Invalid argument)`. So
   `F_ISSET(dblp, DBLOG_DIRECT)` can never be true and every new branch is dead
   code.
2. **Every new branch is gated.** `__log_write_direct` is reached only through
   `F_ISSET(dblp, DBLOG_DIRECT) ? ... : __os_io(...)`; the unflagged arm is the
   original call, unchanged. The preallocation skip is gated on the same flag.
3. **The flag tier on a default build**: O_DIRECT modes SKIP with the reason
   printed, every other mode PASSES, `ALL FLAG BEHAVIOUR TESTS PASS`.

## ABI and environment-signature proofs

Both measured in **fresh build directories** (a stale `db_config.h` reports a
spurious mismatch), on both this branch and pristine master, in both
configurations:

| Tree | Config | `__env_struct_sig()` |
|---|---|---|
| `fix/p3-log-odirect` | default | `0xb86f77f0` |
| `fix/p3-log-odirect` | `--enable-o_direct` | `0xb86f77f0` |
| master (pristine) | default | `0xb86f77f0` |
| master (pristine) | `--enable-o_direct` | `0xb86f77f0` |

Unchanged. No field was added to any structure `env_sig.c` hashes —
`DB_LG_DIRECT_ALIGN` is a `#define` and the staging buffer is a stack array.

Public ABI, all four arms identical and matching the expected values:

```
DB=1744 DBC=552 DB_ENV=2088 DB_TXN=336
```

## Regression status

| Check | Result |
|---|---|
| `dist/s_execbits` | OK — every shebang-bearing `test/*.sh` executable |
| `test/check_manifest.sh --tier flag` | OK — 15 verdict lines, every entry satisfied |
| `test/check_manifest.sh --strict` | OK — manifest self-check clean |
| `db_log_verify` on a `DB_LOG_DIRECT` log | `BDB2504 ... SUCCEEDED` |
| `db_printlog` on the same log | 1255 records, empty stderr |
| DST capstones (`--enable-dst`) | 6/7 PASS; `test_sim_torn` FAILS |
| targeted TCL subset | fails to source |

Two failures are **pre-existing and not caused by this change** — both were
reproduced on pristine master in a fresh build directory with identical flags:

- `test_sim_torn`: `__env_panic` abort, `exit 134`, identical on master.
- TCL: `bad command "getconfig"` from `berkdb getconfig` at `test.tcl:117` — a
  stale system `libdb_tcl` shadowing the freshly built one. Identical on
  master, so the targeted TCL subset could not be used as a gate on this box.

## Residual cost, stated

`ponytail:` comment in the source names it: one `pwrite` per block, so a flush
larger than a block costs more I/O operations than the single unaligned write it
replaced, and a write not starting on a boundary costs one extra block read
(which is almost always — `head != 0` on 1999/2000 writes). Measured effect on
group commit: 357 log syncs vs 225 buffered for the same 480 commits. The
upgrade path is a persistent staging buffer cached across calls, which needs a
`DB_LOG` field and therefore an `__env_struct_sig()` change; worth doing only if
`DB_LOG_DIRECT` throughput ever matters more than on-disk environment
compatibility.
