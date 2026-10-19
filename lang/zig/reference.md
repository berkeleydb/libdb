<!-- SPDX-License-Identifier: Sleepycat -->
# libdb Zig binding — reference

Organised like the std docs: top-level functions, then each type with its
declarations. Every item here has a matching `///` doc-comment in
`src/libdb.zig`; this file is the hand-written companion to Zig's autodoc.

Import: `const libdb = @import("libdb");`. Pinned to Zig 0.16.

---

## Top-level

### `version() -> struct { major, minor, patch: i32 }`
The library version at run time (`db_version`).

### `checkVersion() Error!void`
Fail (`error.Usage`) if the runtime library's major/minor disagree with the
`db.h` the binding was compiled against — the "built against the wrong header"
footgun. `header_major` / `header_minor` are the compile-time values.

### `const consts`
The frozen constant list (`src/consts.zig`): every `DB_*` value the binding
bakes in, verified against `db.h` and re-checked by the drift gate
(`zig build drift`). Use e.g. `consts.DB_SET_RANGE`, `consts.DB_TXN_SNAPSHOT`.

---

## `Error`

```zig
error{ Deadlock, RunRecovery, NotFound, KeyExist, BufferSmall,
       SecondaryBad, Db, System, Usage, OutOfMemory }
```

The decoder maps a C return code: `0` → ok; a DB-range negative → a typed
variant; a positive errno → `error.System`. The control-flow negatives
(`NotFound`, `KeyExist`, `BufferSmall`) are **demoted** on the paths where they
are not failures: `get` returns `?[]u8`, `putNew`/`del`/`exists` return `bool`,
`getInto` writes the needed length to `need` on `error.BufferSmall`. For
`error.Db`/`error.System` read `env.lastError()` for DB's diagnostic string.

---

## `Env`

A database environment. Create → configure → open; `defer env.deinit()`. Must
outlive every `Db`/`Txn`. Opened free-threaded (`DB_THREAD`).

- `create() Error!Env` — allocate the handle.
- `open(home: [*:0]const u8) Error!void` — open transactional + `DB_THREAD`,
  and configure the deadlock detector (`DB_LOCK_DEFAULT`) so a real lock cycle
  is broken rather than hanging (libdb does not auto-detect).
- `lkDetect() Error!u32` — the configured detector policy (a `DB_LOCK_*`).
- `deinit() void` — close; the handle is poisoned in safe builds afterwards.
- `setCachesize(gbytes, bytes: u32) Error!void`
- `setFlags(flags: u32, on: bool) Error!void` — e.g. `consts.DB_TXN_NOSYNC`.
- `setErrpfx(pfx: [*:0]const u8) Error!void`
- `enableMsgcall(on: bool) Error!void`
- `lastError() []const u8` — DB's last diagnostic string (captured via errcall).
- `database() DbBuilder` / `openBtree(file) Error!Db`
- `begin(parent: ?*Txn, flags: u32) Error!Txn`
- `transact(Ctx, ctx, op) Error!void` — run `op` in a txn, commit on success,
  abort+retry on `error.Deadlock` (bounded at 10 attempts). Safe to call
  concurrently: the inner txn uses `DB_TXN_NOWAIT`, so a lock conflict returns
  promptly for the retry loop instead of blocking.

Do **not** copy or move an `Env` after `open` (it stashes `&env` for the errcall
thunk); keep it at a stable address, or call `rebind()` after relocating.

---

## `DbBuilder` / `DbConfig`

Fluent builder from `env.database()`. Methods return a new builder:
`btree()`, `hash()`, `recno()`, `queue()`, `pagesize(n)`, `dup(sorted: bool)`.
Terminal: `open(file)`, `openTxn(file, txn)`, `openWith(file, &Callbacks)`.

## `Callbacks`

Comparator / hash callbacks (a comparator cannot fail):

```zig
Callbacks{ .ctx = &my, .bt_compare = fnPtr, .dup_compare = ..., .h_hash = ... }
```

Each takes `(ctx: *anyopaque, a, b: []const u8) c_int` (compare) or
`(ctx, data: []const u8) u32` (hash). Stashed behind `db->api_internal`; the
`extern-C` thunk casts it back.

---

## `Db`

A key/value store. `defer db.deinit()`.

- `put(txn: ?*Txn, key, data: []const u8) Error!void`
- `putNew(txn, key, data) Error!bool` — `false` if the key existed.
- `del(txn, key) Error!bool` — `false` if absent.
- `exists(txn, key) Error!bool`
- `get(alloc, txn, key) Error!?[]u8` — copy-out (form 1, the safe default).
- `getInto(txn, key, buf: []u8, need: *usize) Error!?usize` — zero-alloc (form
  2, the hot path); `error.BufferSmall` sets `need`.
- `with(txn, key, Ctx, ctx, fn) Error!bool` — borrowed window (form 3); the
  slice is invalid once the closure returns.
- `cursor(txn) Error!Cursor`
- `associate(txn, secondary: *Db, &Extractor) Error!void`
- `pget(alloc, txn, skey) Error!?struct{pkey, data: []u8}` — secondary lookup.
- `bulk(txn, buf: []align(1024) u8) Error!BulkIter` — one `DB_MULTIPLE_KEY` get
  as a zero-allocation iterator; `buf` ≥ page size, a multiple of 1024.

## `Extractor`

```zig
Extractor{ .ctx = &my, .extract = fn (ctx, pkey, pdata) ?[]const u8 }
```

Returns the secondary key to index, or `null` to skip the record
(`DB_DONOTINDEX`). Stashed behind the **secondary** `db->api_internal`.

## `BulkIter`

`next() Error!?Pair` yields `(key, value)` slices into the bulk buffer (no
per-record allocation); `deinit()` closes the underlying cursor.

---

## `Txn`

A transaction; resolve exactly once. Not free-threaded.

- `commit() Error!void` — in safe builds asserts no cursor opened on this txn
  remains open.
- `abort() void`, `discard() Error!void`
- `prepare(gid: *const [128]u8) Error!void`
- `id() u32`, `setName(name) Error!void`, `setTimeout(usec: u32) Error!void`

Begin flags (OR-combine): `consts.DB_TXN_SNAPSHOT`, `DB_TXN_NOWAIT`,
`DB_READ_COMMITTED`, `DB_READ_UNCOMMITTED`.

---

## `Cursor`

`defer cur.deinit()`. If opened on a txn, close before the txn resolves (safe
builds assert via a count on the txn).

- `get(alloc, op: u32, key: ?[]const u8, data: ?[]const u8) Error!?Pair`
- `put(key, data, op) Error!void`, `del() Error!void`
- `count() Error!u32`, `dup(position: bool) Error!Cursor`
- `pget(alloc, op, skey: ?[]const u8) Error!?struct{skey, pkey, data}`
- `iterator(dir: Direction) Iterator` with `Direction = .{forward, backward}`;
  `Iterator.next(alloc) Error!?Pair`.

Op codes come from `consts`: `DB_FIRST`, `DB_LAST`, `DB_NEXT`, `DB_PREV`,
`DB_SET`, `DB_SET_RANGE`, `DB_NEXT_DUP`, `DB_GET_BOTH`, … Cursor-put ops:
`DB_KEYFIRST`, `DB_KEYLAST`, `DB_AFTER`, `DB_BEFORE`, `DB_CURRENT`.

---

## `Sequence`

A 64-bit persistent counter backed by a key in a `Db`. `defer seq.deinit()`.

- `open(db: *Db, txn: ?*Txn, key, create: bool, Config) Error!Sequence`
  with `Config{ initial, cachesize, inc, wrap, range }`.
- `next(txn, delta: i32) Error!i64` — advance and return the block's first value.
- `remove(txn) Error!void`

---

## The lifecycle, in safe builds

`Debug`/`ReleaseSafe` poison each handle on close and `std.debug.assert` it is
not reused; `ReleaseFast`/`ReleaseSmall` omit the check (the real artifact). In
the release builds a double-`deinit` is a double-free — the second `close` runs
on an already-closed handle — just as a use-after-close is; the safe builds
catch both, the release artifact does not. `std.testing.allocator` leak-checks
catch a missed `deinit`/`free`.
