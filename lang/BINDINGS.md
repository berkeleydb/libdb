# libdb language bindings

Idiomatic bindings that expose the libdb C API in other languages. Each is a
pure consumer of the installed C library (`libdb-2026.0.so` + `db.h`) — they do
not change the engine, the on-disk/log/region formats, the soname, or the ABI.

| Language | Location | Status | Docs |
|---|---|---|---|
| Rust | [`rust/`](rust/) | RFC 0016 Phases 1–4 complete | rustdoc (`cargo doc`), [`rust/guide/`](rust/guide/), [`rust/MAPPING.md`](rust/MAPPING.md) |
| Zig | [`zig/`](zig/) | RFC 0016 Phases 1–4 complete | [`zig/reference.md`](zig/reference.md), [`zig/guide/`](zig/guide/), [`zig/MAPPING.md`](zig/MAPPING.md) |

Both follow the same three-layer design (raw C → a 1:1 FFI layer with opaque
handles + a C method-shim → the idiomatic safe API) described in
[`../rfc/0016-rust-zig-bindings.md`](../rfc/0016-rust-zig-bindings.md), with the
C API surface mapped in [`../rfc/0016/capi-survey.md`](../rfc/0016/capi-survey.md).
Each ships a **drift gate** that fails the build if any constant the binding
bakes in disagrees with the installed `db.h`.

The older bindings — [`cxx/`](cxx/), [`java/`](java/), [`tcl/`](tcl/),
[`sql/`](sql/) and the compatibility shims — remain as before.

## Building

Both bindings build against an installed libdb. The default install prefix is
`/usr/local/BerkeleyDB.2026.0`; override with `LIBDB_DIR` (Rust) or
`-Dlibdb-prefix=` (Zig). See each binding's README.
