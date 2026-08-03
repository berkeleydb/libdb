# Public DB_* flag reconcile (docs vs. engine)

One-time reconcile of the public `DB_*` flags the engine actually accepts
against what `docs_src/api/c` documents, focused on the transaction /
environment / database flag surfaces the SSI work touched. Sources of truth:

- `src/dbinc_auto/api_flags.in` — the public flag bit values.
- `src/dbinc/db.in` — public return codes.
- `src/txn/txn.c __txn_begin` `__db_fchk` mask — the valid `txn_begin` flags.
- `src/env/env_method.c __env_set_flags` `OK_FLAGS` — the valid env
  `set_flags` flags.

"Documented" = the flag token appears (word-boundary) in some
`docs_src/api/c/*.md` page.

## DB_ENV->txn_begin() flags (src/txn/txn.c mask)

| Flag                   | Before | After this change |
|------------------------|--------|-------------------|
| DB_IGNORE_LEASE        | doc    | doc               |
| DB_READ_COMMITTED      | doc    | doc               |
| DB_READ_UNCOMMITTED    | doc    | doc               |
| DB_TXN_BULK            | doc    | doc               |
| DB_TXN_FAMILY          | **MISSING** | **added** (txnbegin.md) |
| DB_TXN_NOSYNC          | doc    | doc               |
| DB_TXN_NOWAIT          | doc    | doc               |
| DB_TXN_SNAPSHOT        | doc    | doc (now = SSI)   |
| DB_TXN_SNAPSHOT_SAFE   | *removed* | flag removed — folded into DB_TXN_SNAPSHOT |
| DB_TXN_SYNC            | doc    | doc               |
| DB_TXN_WAIT            | doc    | doc               |
| DB_TXN_WRITE_NOSYNC    | doc    | doc               |

Every `txn_begin` flag the engine accepts is now documented.

## DB_ENV->set_flags() flags (src/env/env_method.c OK_FLAGS)

| Flag                     | Status |
|--------------------------|--------|
| DB_AUTO_COMMIT           | doc    |
| DB_CDB_ALLDB             | doc    |
| DB_DATABASE_LOCKING      | **MISSING** (pre-existing gap; not SSI-era; left as-is) |
| DB_DIRECT_DB             | doc    |
| DB_DSYNC_DB              | doc    |
| DB_MPOOL_AIO             | doc (engine work; already present) |
| DB_MULTIVERSION          | doc    |
| DB_NOFLUSH               | **MISSING** (pre-existing gap; not SSI-era; left as-is) |
| DB_NOLOCKING             | doc    |
| DB_NOMMAP                | doc    |
| DB_NOPANIC               | doc    |
| DB_OVERWRITE             | doc    |
| DB_PANIC_ENVIRONMENT     | doc    |
| DB_REGION_INIT           | doc    |
| DB_TIME_NOTGRANTED       | doc    |
| DB_TXN_NOSYNC            | doc    |
| DB_TXN_NOWAIT            | doc    |
| DB_TXN_SNAPSHOT          | doc    |
| DB_TXN_WRITE_NOSYNC      | doc    |
| DB_YIELDCPU              | doc    |
| DB_HOTBACKUP_IN_PROGRESS | doc    |

Note: `DB_TXN_SNAPSHOT` is now serializable snapshot isolation (SSI); the
separate `DB_TXN_SNAPSHOT_SAFE` flag was **removed** and its behavior folded
into `DB_TXN_SNAPSHOT`, which is a valid `set_flags` env flag (SSI can be set
environment-wide or per-transaction). `envset_flags.md` states this under
`DB_TXN_SNAPSHOT`.

## Public return codes (src/dbinc/db.in) — SSI-era

| Return code            | Value    | Before | After |
|------------------------|----------|--------|-------|
| DB_SNAPSHOT_UNSAFE     | -30967   | **undocumented** | **added** (program_errorret.md) |
| DB_SNAPSHOT_CONFLICT   | -30968   | **undocumented** | **added** (program_errorret.md) |

There is **no** `DB_TXN_SNAPSHOT_UNSAFE` *flag* — `DB_SNAPSHOT_UNSAFE` is a
return *code* an SSI transaction may get, not a `txn_begin` option.

## What this change fixed

- `DB_TXN_SNAPSHOT` — now serializable snapshot isolation (SSI); the earlier
  separate `DB_TXN_SNAPSHOT_SAFE` (0x800) flag was removed and folded in:
  documented on `api/c/txnbegin.md`, cross-noted on `api/c/envset_flags.md`,
  and explained in the transactions guide
  (`guides/gsg_txn/isolation.md`) and the programmer's reference
  (`guides/programmer_reference/transapp_read.md`).
- `DB_SNAPSHOT_UNSAFE` / `DB_SNAPSHOT_CONFLICT` return codes documented in
  `guides/programmer_reference/program_errorret.md`.
- `DB_TXN_FAMILY` — a legacy public `txn_begin` flag that was never
  documented; a short entry was added to complete the `txn_begin` flag list.
- `DB_READ_COMMITTED` / `DB_READ_UNCOMMITTED` / `DB_MPOOL_AIO` — verified
  already documented; no change needed.

## Out of scope (deliberately left)

`DB_DATABASE_LOCKING` and `DB_NOFLUSH` are valid `set_flags` flags that are
undocumented, but they are pre-existing Oracle-era gaps unrelated to the SSI
work; documenting them accurately needs their own audit and is not part of
this SSI-focused reconcile.
