# C-to-Rust mapping

A cross-walk from the libdb C API to its `libdb` Rust form, for a Berkeley DB C
user finding their way around. The C methods are grouped by handle. The Rust
column names the method on the safe type (`Env`, `EnvBuilder`, `Db`,
`DbBuilder`, `Txn`, `Cursor`, `Sequence`); where the raw symbol differs it is
the `libdb-sys` shim entry `libdb_<handle>_<method>`.

The binding wraps the surface a real transactional application uses (RFC 0016
§8). Replication/repmgr, the raw mpool API, log cursors and encryption config
beyond a passphrase are reachable through `libdb-sys` but have no safe wrapper
in v1.

## Top-level

| C | Rust |
|---|---|
| `db_env_create` | `Env::builder` |
| `db_create` | `Env::database` → `DbBuilder` |
| `db_sequence_create` | `Db::sequence` |
| `db_version` | `libdb::version` |
| `db_strerror` | internal (folded into `Error`'s `Display`) |
| compile-vs-runtime version check | `libdb::check_version` |

## DB_ENV

| C method | Rust |
|---|---|
| `dbenv->open` | `EnvBuilder::open` |
| `dbenv->close` | `Drop for Env` (and `EnvBuilder`) |
| `dbenv->set_cachesize` | `EnvBuilder::cachesize` |
| `dbenv->set_flags(f, onoff)` | `EnvBuilder::set_flag` |
| `dbenv->set_lk_detect` | `EnvBuilder::lock_detect` (`LockDetect`) |
| `dbenv->set_errpfx` | `EnvBuilder::errpfx` |
| `dbenv->set_errcall` | `EnvBuilder::capture_diagnostics` + `Env::last_diagnostic` |
| `dbenv->set_msgcall` | `EnvBuilder::log_messages` |
| `dbenv->txn_begin` | `Env::begin` / `Env::begin_opts` |
| `dbenv->txn_checkpoint` | `Env::checkpoint` |
| the deadlock retry loop | `Env::transact` / `Env::transact_opts` |

## DB

| C method | Rust |
|---|---|
| `db->open` | `DbBuilder::open` / `DbBuilder::open_txn` |
| `db->close` | `Drop for Db` |
| `db->set_pagesize` | `DbBuilder::pagesize` |
| `db->set_flags(DB_DUP[SORT])` | `DbBuilder::duplicates` / `sorted_duplicates` |
| `db->set_bt_compare` | `DbBuilder::bt_compare` |
| `db->set_dup_compare` | `DbBuilder::dup_compare` |
| `db->set_h_hash` | `DbBuilder::h_hash` |
| access method (`DBTYPE`) | `DbBuilder::btree`/`hash`/`recno`/`queue`/`db_type` |
| `db->put` | `Db::put` / `Db::tput` |
| `db->put(DB_NOOVERWRITE)` | `Db::put_no_overwrite` / `tput_no_overwrite` → `Put` |
| `db->get` (copy out) | `Db::get` / `Db::tget` → `Option<Vec<u8>>` |
| `db->get` (`DB_DBT_USERMEM`) | `Db::get_into` / `get_into_grow` → `GetInto` |
| `db->get` (borrowed window) | `Db::with_value(key, &#124;v&#124; ...)` |
| `db->del` | `Db::del` / `Db::tdel` → `bool` |
| `db->exists` | `Db::exists` / `Db::texists` → `bool` |
| `db->cursor` | `Db::cursor` / `Db::cursor_txn` |
| `db->associate` | `Db::associate` (extractor returns `None` for `DB_DONOTINDEX`) |
| `db->pget` | `Db::pget` → `(primary_key, value)` |
| `db->sync` | `Db::sync` |

## DBC (cursor)

| C method / opcode | Rust |
|---|---|
| `dbc->close` | `Drop for Cursor` |
| `dbc->get(DB_FIRST/LAST/NEXT/PREV/CURRENT)` | `Cursor::first`/`last`/`next`/`prev`/`current` |
| `dbc->get(DB_NEXT_DUP/NEXT_NODUP)` | `Cursor::next_dup` / `next_nodup` |
| `dbc->get(DB_SET)` | `Cursor::set` |
| `dbc->get(DB_SET_RANGE)` | `Cursor::set_range` |
| forward/backward scan | `Cursor::iter(Direction)` (a real `Iterator`) |
| `dbc->put(DB_KEYLAST)` | `Cursor::put` |
| `dbc->put(DB_NODUPDATA)` | `Cursor::put_dup` → `Put` |
| `dbc->del` | `Cursor::del` |
| `dbc->count` | `Cursor::count` |
| `dbc->dup` | `Cursor::dup` |
| `dbc->pget` | `Cursor::pget(opcode)` → `(skey, pkey, value)` |
| `dbc->get(DB_MULTIPLE_KEY)` + `DB_MULTIPLE_KEY_NEXT` | `Cursor::bulk(buf)` → `BulkPairs` iterator |

## DB_TXN

| C method | Rust |
|---|---|
| `txn->commit` | `Txn::commit` / `commit_nosync` (consumes `self`) |
| `txn->abort` | `Txn::abort` (consumes `self`) |
| `txn->discard` | `Txn::discard` |
| `txn->prepare(gid)` | `Txn::prepare(&[u8; 128])` |
| `txn->id` | `Txn::id` |
| `txn->set_name` | `Txn::set_name` |
| `txn->set_timeout(DB_SET_LOCK_TIMEOUT)` | `Txn::set_lock_timeout` |
| `txn->set_timeout(DB_SET_TXN_TIMEOUT)` | `Txn::set_txn_timeout` |
| nested (`txn_begin` with parent) | `Txn::begin_child` / `begin_child_opts` |

## DB_SEQUENCE

| C method | Rust |
|---|---|
| `seq->open` | `Db::sequence(key)` (create-if-absent, initial 1) |
| `seq->get(delta)` | `Sequence::next` / `Sequence::advance(delta)` |
| `seq->set_cachesize` | `Sequence::set_cache` |
| `seq->set_range` | (set via `libdb-sys` for now) |
| `seq->remove` | `Sequence::remove` (consumes `self`) |
| `seq->close` | `Drop for Sequence` |

## Error codes

| C return | Rust |
|---|---|
| `0` | `Ok` |
| `DB_NOTFOUND` | `Ok(None)` / iterator ends (not an error) |
| `DB_KEYEXIST` | `Ok(Put::Exists)` (not an error) |
| `DB_BUFFER_SMALL` | `GetInto::TooSmall(need)` (not an error) |
| `DB_LOCK_DEADLOCK` | `Error::Deadlock` (retried by `transact`) |
| `DB_LOCK_NOTGRANTED` | `Error::LockNotGranted` |
| `DB_RUNRECOVERY` | `Error::RunRecovery` |
| `DB_SECONDARY_BAD` | `Error::SecondaryBad` |
| other `< 0` | `Error::Db(code, message)` |
| `> 0` (OS errno) | `Error::Os(errno, message)` |
