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
