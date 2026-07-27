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

## Findings from the bootstrap run (2026-07, clang 21, ASan+UBSan)

These are **engine bugs** the harnesses found on the first short run.  They are
reported here as regression seeds; fixing the engine is out of scope for the
harness PR (a maintainer fixes them separately).  Both are on the untrusted
`.db`-file page-parse path (`fuzz_dbfile`).

| Seed | Fault | Site | Trigger |
|------|-------|------|---------|
| `dbfile_oob_read_db_retcopy.seed` | ASan OOB read (`memcpy` src, ~64KB) | `__db_retcopy` `src/db/db_ret.c:158`, reached via `__dbc_iget` → cursor `DB_NEXT` | A malformed page gives an item a bogus large length; the return-copy trusts it and reads past the page buffer. |
| `dbfile_fpe_memp_fopen.seed` | SIGFPE (integer divide-by-zero) | `__memp_fopen` `src/mp/mp_fopen.c:398` (`bytes % pagesize`), reached via `DB->open` → `__env_mpool` | The on-disk metadata pagesize is 0; the `DB_ASSERT(pagesize != 0)` on the preceding line does not guard non-DIAGNOSTIC builds, so the modulo divides by zero. |

`fuzz_recover` additionally shows a **memory leak on the corrupt-log
recovery-cleanup path** (env/log region allocated during a `DB_RECOVER` open
that then fails on a malformed log is not freed; roots at
`__log_init`/`__env_attach`).  `fuzz_api` shows **session-accumulated leaks**
(some op sequences make `DB->close`/`DB_ENV->close` fail, leaving the handle
un-freed).  Both are reported, not fixed here; the recover smoke run disables
LSan (`detect_leaks=0`) so crashes are still caught without the known leak
halting every input.
