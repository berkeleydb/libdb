# db_repsite — reconstruction TODO (repmgr 100-series enabler)

**Status: NOT reconstructed in this coverage PR (deliberately timeboxed).**
XA and on-disk-upgrade were the surer wins and shipped; `db_repsite` is the
hardest of the three and is documented here for a focused follow-up.

## What it is

`db_repsite` is a small **C++ command-line test utility** that the repmgr
100-series Tcl tests (`repmgr100`–`repmgr112`) drive as one or more child
processes over a stdin/stdout pipe. Each `db_repsite` process is one repmgr
*site* (a separate OS process sharing an env home), which is exactly the
multi-process repmgr topology the in-process `repmgrNN` (9–34) harness cannot
exercise. Restoring it unlocks ~12 tests and the multi-process repmgr paths.

- Referenced by build: `dist/Makefile.in` lines 1209, 1380–1383
  (`db_repsite@o@: $(testdir)/repmgr/db_repsite.cpp`,
  `DBREPSITE_OBJS`, `db_repsite: $(DBREPSITE_OBJS) $(DEF_LIB_CXX)`).
- Expected source path: **`test/repmgr/db_repsite.cpp`** (the `$(testdir)` in
  Makefile.in is `test`). **This file is absent from this fork** — only the
  Windows project stub `build_windows/VS10/db_repsite.vcxproj` survives, and it
  is *not* in git history as a `.cpp`.
- Tcl entry points: `setup_site_prog` / `open_site_prog` in
  `test/tcl/reputils.tcl` (~line 2585). `setup_site_prog` looks for
  `$util_path/db_repsite` and `error "Skipping: ..."` if absent — which is why
  the 100-series currently skip.

## Command protocol (reverse-engineered from the tests)

`db_repsite` reads one command per line from stdin, line-buffered
(`fconfigure $s -buffering line`), and writes sentinels to stdout that the
tests read with `gets`. Full verb set observed across
`repmgr100.tcl … repmgr112.tcl`:

| command | args | action | stdout expectation |
|---------|------|--------|--------------------|
| `home DIR` | env home path | remember home for `open_env` | (none) |
| `local PORT` | port | set this site's local address (127.0.0.1:PORT) | (none) |
| `remote HOST PORT` | | add a remote site (helper) | (none) |
| `output FILE` | | redirect env `set_errfile`/verbose to FILE | (none) |
| `open_env` | | `db_env_create` + `DB_ENV->open` with repmgr flags | (none) |
| `start master` / `start client` | role | `DB_ENV->repmgr_start(nthreads, role)` | line matching `*Successful*` |
| `open_db NAME` | db name | `db_create` + `DB->open` (btree, auto-commit) | (none) |
| `put KEY VALUE` | | `DB->put` (auto-commit) | (none) |
| `is_connected PORT` | | query repmgr site status; print connected/not | line the test matches (see repmgr101/112) |
| `echo TOKEN` | any token | write `TOKEN\n` back — the test's sync sentinel | echoes TOKEN verbatim |
| `exit` | | close db, close env, exit 0 | (none) |

Notes:
- `start` is the only command `open_site_prog` waits on (`gets $s` after
  `start`), expecting a line containing `Successful`. Everything else is
  fire-and-forget until an explicit `echo`.
- Site config uses **DB_CONFIG**, written by `make_dbconfig` (repmgr_site /
  rep_set_config lines), so `db_repsite` itself does NOT parse repmgr_site
  from stdin for the local/bootstrap sites in most tests — it just opens the
  env and the DB_CONFIG supplies the topology. `local`/`remote` stdin verbs
  are used by the few tests that set the address programmatically
  (`repmgr101`, `repmgr105`, `repmgr112`).

## API surface it must call

All public, all present in this fork's `db.h`:

- `db_env_create`, `DB_ENV->set_errfile`, `DB_ENV->set_verbose`,
  `DB_ENV->open` with `DB_CREATE|DB_INIT_REP|DB_INIT_LOCK|DB_INIT_LOG|
  DB_INIT_MPOOL|DB_INIT_TXN|DB_THREAD|DB_RECOVER`.
- `DB_ENV->repmgr_site` (add local/remote site, set DB_LOCAL_SITE /
  DB_BOOTSTRAP_HELPER), `DB_REPMGR_SITE->set_config`, `->close`.
- `DB_ENV->repmgr_start(dbenv, nthreads, DB_REP_MASTER|DB_REP_CLIENT)`.
- `DB_ENV->repmgr_site_list` / `DB_ENV->repmgr_stat` for `is_connected`.
- `db_create`, `DB->open`, `DB->put`, `DB->close`, `DB_ENV->close`.

The canonical upstream implementation is Oracle BDB 5.3's
`test/repmgr/db_repsite.cpp` (~250–300 lines). It is a straightforward
stdin-command dispatcher; reconstruction is *tractable* but must be validated
against the exact stdout sentinels the tests `gets`, and — critically — run
under a **per-test timeout**, because a mis-handled connection/sync will hang
the pipe (the tests block on `gets`).

## Steps to finish (follow-up PR)

1. Write `test/repmgr/db_repsite.cpp` implementing the verb table above.
   Model the dispatch loop on the existing `test/xa/xa_direct.c` style
   (assert-and-report), but C++ and long-lived (loop on `fgets`).
2. It already has a Makefile.in target (`db_repsite`); confirm `--enable-test`
   builds it (needs `--enable-cxx` / `DEF_LIB_CXX`). Add to the coverage
   `make` step if missing.
3. Run `repmgr100` first (simplest: 2-site master/client) under
   `timeout 300 tclsh8.6 -c 'source ../test/tcl/test.tcl;
   source ../test/tcl/reputils.tcl; repmgr100'`. Iterate on sentinels until it
   passes without hanging. Then 101, 102, 105, 106.
4. Register a `run_repmgr_100` set behind a `COV_REPMGR100=1` flag in
   `test/coverage/run_coverage.sh` (per-test tclsh + timeout + TESTDIR clean,
   mirroring the existing `COV_REP` block), NOT in the default subset (these
   are multi-process and can hang — always timeout-guard).

## Why deferred here

Reconstructing a multi-process repmgr CLI and making 100-series tests pass
*reliably and without hanging* is materially larger and riskier than the XA
and upgrade wins, which were shippable and measurable in-budget. This doc
captures the entire API + protocol so the follow-up is mechanical.
