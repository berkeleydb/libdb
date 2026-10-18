<!-- SPDX-License-Identifier: Sleepycat -->
# libdb — Zig binding

Idiomatic Zig bindings for libdb (the community Berkeley DB fork), per
[RFC 0016](../../rfc/0016-rust-zig-bindings.md). Pinned to **Zig 0.16**.

Three layers:

- **Layer 0** — the C library (`db.h` + `libdb-2026.0.so`).
- **Layer 1** — `shim.c`/`shim.h` (flat `libdb_<handle>_<method>` entry points so
  the C compiler resolves method-pointer offsets) + `src/consts.zig` (the frozen
  constant list) + the mirrored `DBT`.
- **Layer 2** — `src/libdb.zig` — owning `Env`/`Db`/`Txn`/`Cursor`/`Sequence`,
  error unions, allocator passed in, `defer deinit()`.

A caller uses only Layer 2.

## Build & test

```sh
export LD_LIBRARY_PATH=/usr/local/BerkeleyDB.2026.0/lib
zig build test --summary all                     # Debug
zig build test -Doptimize=ReleaseSafe --summary all
zig build test -Doptimize=ReleaseFast --summary all
zig build drift                                  # the constant drift gate
zig build example-kv                             # key/value CLI
zig build example-counter                        # transactional counter + retry
zig build example-address                        # secondary-index address book
```

Override the install prefix with `-Dlibdb-prefix=/path/to/BerkeleyDB`.

## A first program

```zig
const libdb = @import("libdb");

var env = try libdb.Env.create();
defer env.deinit();
try env.open("/tmp/myenv");

var db = try env.openBtree("data.db");
defer db.deinit();

try db.put(null, "hello", "world");
const a = std.heap.page_allocator;
if (try db.get(a, null, "hello")) |v| {
    defer a.free(v);
    std.debug.print("{s}\n", .{v}); // world
}
```

## Documentation

- [`reference.md`](reference.md) — the full API reference, organised like the std docs.
- [`guide/`](guide/) — getting-started chapters (open env, put/get, txns + retry,
  cursors, secondary indices, bulk).
- [`MAPPING.md`](MAPPING.md) — a C-method → Zig cross-walk for Berkeley DB C users.

Every public declaration carries a `///` doc-comment; `zig build-obj -femit-docs`
renders them, and `reference.md` is the hand-written companion.
