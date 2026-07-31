# Fuzz crash / regression seeds — `test/fuzz/crashes/`

Every input that a fuzz harness crashed (or leaked) on is committed here as a
**regression seed**.  A commit that fixes the underlying engine bug should keep
the seed so CI (and any future OSS-Fuzz run) re-checks it forever.

## Convention

- File name: `<harness>_<short-slug>.seed`, e.g.
  `dbfile_oob_read_db_retcopy.seed`.
- One file = one reproducing input, byte-for-byte as the fuzzer saved it.
- Reproduce with the standalone driver (no libFuzzer needed):

  ```sh
  ./run.sh repro <harness> crashes/<file>.seed
  # <harness> = dbfile | recover | api
  ```

- To add a new one after a fuzz run:

  ```sh
  cp build/artifacts_<harness>/crash-XXXX crashes/<harness>_<slug>.seed
  ```

## Regression gate

`../check-crashes.sh` replays **every** seed here through its standalone
harness and asserts a clean (non-crash) outcome — the memory-safety property
the hardening fixes deliver.  Run it from `test/fuzz/`:

```sh
./check-crashes.sh
```

CI's `fuzz.yml` also replays each `crashes/<harness>_*.seed` on every PR, so
committing a seed here wires it into CI automatically.

## Findings from the bootstrap run (2026-07, clang 21, ASan+UBSan)

These are **engine bugs** the harnesses found on the first short run.  They are
reported here as regression seeds; fixing the engine is out of scope for the
harness PR (a maintainer fixes them separately).  Both are on the untrusted
`.db`-file page-parse path (`fuzz_dbfile`).

| Seed | Fault | Site | Trigger |
|------|-------|------|---------|
| `dbfile_oob_read_db_retcopy.seed` | ASan OOB read (`memcpy` src, ~64KB) | `__db_retcopy` `src/db/db_ret.c:158`, reached via `__dbc_iget` → cursor `DB_NEXT` | A malformed page gives an item a bogus large length; the return-copy trusts it and reads past the page buffer. **FIXED** (`__db_ret_okitem` bounds-check in `db_ret.c`). |
| `dbfile_fpe_memp_fopen.seed` | SIGFPE (integer divide-by-zero) | `__memp_fopen` `src/mp/mp_fopen.c:398` (`bytes % pagesize`), reached via `DB->open` → `__env_mpool` | The on-disk metadata pagesize is 0; the `DB_ASSERT(pagesize != 0)` on the preceding line does not guard non-DIAGNOSTIC builds, so the modulo divides by zero. **FIXED** (runtime EINVAL check). |

## Findings from the hardening re-fuzz (2026-07, this PR)

Two more surfaced once the fixes above stopped the earlier crashes from
masking them.  Both are the same trust-a-length-from-the-file class and are
**FIXED** in this PR.

| Seed | Fault | Site | Trigger | Fix |
|------|-------|------|---------|-----|
| `dbfile_fpe_bam_minkey.seed` | SIGFPE (divide-by-zero) | `__bamc_refresh` `src/btree/bt_cursor.c:285`, via `B_MINKEY_TO_OVFLSIZE` on btree open/cursor-init | The btree meta page's `minkey` field is 0; it is used as a divisor. | Reject `minkey < 2` when loaded in `__bam_read_root` (`bt_open.c`), matching what verify and the public setter already require. |
| `recover_oob_read_log_chksum.seed` | ASan OOB read | `__ham_func4` `src/hash/hash_func.c:171`, via `__db_check_chksum` ← `__log_valid` `src/log/log.c:818` | A corrupt log header's `hdr->len` makes the checksum hash read past the fixed-size `persist` record buffer.  The crypto path already bounded this; the non-crypto path did not. | Add the same `hdr->len - hdrsize == recsize` bound (and underflow guard) on the non-crypto path in `__log_valid`. |

## Findings from the security / pentest review (2026-07)

Five more, all on the untrusted `.db`-file parse/verify surface, found by
fuzzing an **ASan-instrumented** libdb (a heap-buffer-overflow *inside* libdb's
own allocations is invisible to a plain-lib harness -- see the ASan-gate note
below). All are **FIXED** in this PR; each ships a regression seed.

| Seed | Fault | Site | Fix |
|------|-------|------|-----|
| `dbfile_typeconf_part_verify.seed` | ASan heap-buffer-overflow (8-byte WRITE) | `__db_lget` via `__ham_get_meta` <- `__ham_open` <- `__part_verify` `partition.c` | Type confusion: a non-Btree (e.g. Heap) file with the partition flag was opened with the Hash AM but a Heap-sized cursor internal -> `LOCK_INIT(&hcp->hlock)` writes past the 88B alloc. Dispatch by exact type; reject non-Btree/Recno/Hash. |
| `dbfile_doublefree_heap_vrfy.seed` | ASan double-free / free of indeterminate ptr | `__heap_vrfy` `heap_verify.c` | `offsets` freed at `err:` while uninitialized (early `__db_vrfy_datapage` failure jumps there). `offsets = NULL;` at decl. (was the OPEN item in `fuzz-found-bugs.md`.) |
| `dbfile_infloop_bam_search.seed` | DoS -- infinite loop | `__bam_search` `bt_search.c` fast-path child fetch | A `P_IBTREE` child pointer to itself/an ancestor at same-or-higher level spins the descent forever. Guard `LEVEL(child) >= LEVEL(parent)` -> `DB_PAGE_NOTFOUND` (levels must strictly decrease). |
| `dbfile_fpe_heap_region_size.seed` | SIGFPE (divide-by-zero) | `__heap_vrfy_meta` `heap_verify.c` (`HEAP_REGION_NUM`) | heap meta `region_size` 0 or UINT32_MAX (`+1` wraps) used as divisor. Reject at open+verify. |
| `dbfile_fpe_qam_recpage.seed` | SIGFPE (divide-by-zero) | `__qam_vrfy_meta` `qam_verify.c` (`QAM_RECNO_PAGE`) | queue meta `rec_page` 0 used as divisor. Reject at open+verify. |

> **ASan gate:** `check-crashes.sh` now builds an ASan-only libdb under
> `build_asan_gate/` (gitignored) and links the standalone harnesses against
> it, so a memory fault *inside* libdb (e.g. the `__part_verify` OOB write) is
> caught -- a plain-lib harness cannot see it.  Set `LIBDB_ASAN=0` to skip.

A sixth finding (a **bounded** queue extent-scan DoS in `__qam_vrfy_walkqueue`
on a crafted huge `cur_recno`) is documented in `.agents/security-review.md`
and DEFERRED: it terminates, and a safe fix must be extent-aware so it does not
reject valid large/wrapped queues.

### Still open (documented, not fixed here)

`fuzz_recover` / verify-on-a-corrupt-file additionally show a **memory leak on
the cleanup path** (`DB_PRIVATE` mpool/region pages allocated when the verify
scratch DBs or a `DB_RECOVER` log open then fails on malformed input are not
freed on close; roots at `__bam_new_file`/`__memp_alloc` and
`__log_init`/`__env_attach`).  `fuzz_api` shows **session-accumulated leaks**
(some op sequences make `DB->close`/`DB_ENV->close` fail, leaving the handle
un-freed).  These are leaks, not memory-safety faults, and the fix lives in the
`DB_PRIVATE` region-teardown lifecycle — a separate, larger effort.  The
recover smoke run and `check-crashes.sh` disable LSan (`detect_leaks=0`) so
crashes are still caught without the known leak halting every input.
