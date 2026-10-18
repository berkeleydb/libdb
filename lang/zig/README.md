# libdb — Zig bindings

Idiomatic Zig bindings for **libdb** (the community Berkeley DB fork).

**Status: Phase 0** (the proof-of-shape from [RFC 0016](../../rfc/0016-rust-zig-bindings.md)).
What works today: an owning `Env` and `Db` (`defer env.deinit()`), the
error-union model (the control-flow negatives are modelled in return types —
an absent key is `null`, not an error), and a borrowed-in / copy-out
`put`/`get` round trip. Transactions, cursors, callbacks, secondary indices and
bulk ops land in later phases — see the RFC.

## Layout

```
build.zig     Builds the `libdb` module; links the C library + the method shim.
src/libdb.zig Layer 2: the safe, idiomatic API you import.
shim.c/shim.h Layer 1 seam: flat C entry points for the handle methods, so Zig
              never assumes a handle struct offset (the C compiler resolves
              db->put via the real db.h). Shared concept with the Rust binding.
src/test.zig  The Phase-0 test block (round trip).
```

## Build & test

Requires an installed libdb (`libdb-2026.0.so` + `db.h`). Default prefix is
`/usr/local/BerkeleyDB.2026.0`; override with `-Dlibdb-prefix=...`.

```sh
export LD_LIBRARY_PATH=/usr/local/BerkeleyDB.2026.0/lib
zig build test --summary all
```

Pinned to **Zig 0.16**. Verified on FreeBSD 14 against libdb at the first LTS,
`v2026.10.7`.

## Example

```zig
var env = try libdb.Env.create();
defer env.deinit();
try env.open("db-home");
var db = try env.openBtree("data.db");
defer db.deinit();
try db.put("key", "value");
const v = (try db.get(allocator, "key")).?;
defer allocator.free(v);
```
