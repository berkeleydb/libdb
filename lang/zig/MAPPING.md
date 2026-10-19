<!-- SPDX-License-Identifier: Sleepycat -->
# C → Zig cross-walk

For a Berkeley DB C user: the C method (via its handle) and the Zig binding form.
Everything routes through the flat `shim.c` symbols, so the "C method" column is
the real `handle->method` the shim calls.

## Top-level

| C | Zig |
|---|---|
| `db_env_create` | `Env.create()` |
| `db_create` | `Env.database().open(...)` / `Env.openBtree(...)` |
| `db_sequence_create` + `open` | `Sequence.open(...)` |
| `db_version(&maj,&min,&patch)` | `version()` → `.{major,minor,patch}` |
| compile-vs-runtime version check | `checkVersion()` |
| `db_strerror` | (decoded into `Error` + `env.lastError()`) |

## DB_ENV

| C | Zig |
|---|---|
| `dbenv->open(home, flags, mode)` | `env.open(home)` (opens transactional + `DB_THREAD`) |
| `dbenv->close` | `env.deinit()` |
| `dbenv->set_cachesize(gb,b,nc)` | `env.setCachesize(gb, b)` |
| `dbenv->set_flags(flags, onoff)` | `env.setFlags(flags, on)` |
| `dbenv->set_errpfx` | `env.setErrpfx(pfx)` |
| `dbenv->set_errcall` | wired automatically → `env.lastError()` |
| `dbenv->set_msgcall` | `env.enableMsgcall(on)` |
| `dbenv->txn_begin(parent, &txn, flags)` | `env.begin(parent, flags)` |
| the deadlock-retry loop | `env.transact(Ctx, ctx, op)` |

## DB

| C | Zig |
|---|---|
| `db->set_pagesize` | `env.database().pagesize(n)` |
| `db->set_flags(DB_DUP[\|DB_DUPSORT])` | `env.database().dup(sorted)` |
| `db->set_bt_compare` / `set_dup_compare` / `set_h_hash` | `env.database().openWith(file, &Callbacks{...})` |
| `db->open(txn, file, db, type, flags, mode)` | `builder.open(file)` / `builder.openTxn(file, txn)` |
| `db->close` | `db.deinit()` |
| `db->put(txn,k,d,0)` | `db.put(txn, k, d)` |
| `db->put(txn,k,d,DB_NOOVERWRITE)` | `db.putNew(txn, k, d)` → `bool` |
| `db->get(txn,k,d,0)` DB_DBT_MALLOC | `db.get(alloc, txn, k)` → `?[]u8` |
| `db->get(txn,k,d,0)` DB_DBT_USERMEM | `db.getInto(txn, k, buf, &need)` → `?usize` |
| `db->get` borrowed window | `db.with(txn, k, Ctx, ctx, fn)` |
| `db->del(txn,k,0)` | `db.del(txn, k)` → `bool` |
| `db->exists(txn,k,0)` | `db.exists(txn, k)` → `bool` |
| `db->cursor(txn,&dbc,0)` | `db.cursor(txn)` |
| `db->associate(txn, sec, cb, 0)` | `primary.associate(txn, &sec, &Extractor{...})` |
| `db->pget(txn, skey, pkey, data, 0)` | `sec.pget(alloc, txn, skey)` |
| bulk `DB_MULTIPLE_KEY` | `db.bulk(txn, buf)` → `BulkIter` |

## DBC (cursor)

| C | Zig |
|---|---|
| `dbc->get(k,d,op)` DB_DBT_MALLOC | `cur.get(alloc, op, key?, data?)` → `?Pair` |
| `dbc->put(k,d,op)` | `cur.put(k, d, op)` |
| `dbc->del` | `cur.del()` |
| `dbc->count(&n,0)` | `cur.count()` → `u32` |
| `dbc->dup(&new, DB_POSITION)` | `cur.dup(position)` |
| `dbc->pget(sk,pk,d,op)` | `cur.pget(alloc, op, skey?)` |
| FIRST/NEXT, LAST/PREV scan | `cur.iterator(.forward|.backward)` → `.next(alloc)` |

## DB_TXN

| C | Zig |
|---|---|
| `txn->commit(0)` | `txn.commit()` |
| `txn->abort` | `txn.abort()` |
| `txn->discard(0)` | `txn.discard()` |
| `txn->prepare(gid)` | `txn.prepare(&gid)` |
| `txn->id` | `txn.id()` |
| `txn->set_name` | `txn.setName(name)` |
| `txn->set_timeout(usec, DB_SET_TXN_TIMEOUT)` | `txn.setTimeout(usec)` |

## DB_SEQUENCE

| C | Zig |
|---|---|
| `db_sequence_create` + set_* + `open` | `Sequence.open(&db, txn, key, create, .{...})` |
| `seq->get(txn, delta, &v, 0)` | `seq.next(txn, delta)` → `i64` |
| `seq->close` | `seq.deinit()` |
| `seq->remove` | `seq.remove(txn)` |
