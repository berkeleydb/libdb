# 1. Opening an environment

Everything starts with an `Env` — the database environment that owns the cache,
the lock manager, the log and the transaction subsystem. You build it, open it
at a home directory, then open databases in it.

```rust
let env = libdb::Env::builder()
    .cachesize(0, 64 << 20) // 64 MiB cache
    .open("db-home")?;
# Ok::<(), libdb::Error>(())
```

`open` creates a *transactional, free-threaded* environment (it passes
`DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_TXN |
DB_THREAD`). Because it is free-threaded, `Env` is `Send + Sync` and you can
share it across threads behind an `Arc`.

## Configuration

The builder methods set everything that must be configured *before* open:

- `.cachesize(gbytes, bytes)` — the buffer-pool size.
- `.lock_detect(mode)` — turn on the deadlock detector. **Set this on any
  environment used by more than one concurrent writer.** libdb does not detect
  deadlocks on its own; without a detector (or a `nowait` transaction) two
  deadlocked transactions wait forever. `LockDetect::Default` is a sound choice.
- `.set_flag(flag, on)` — toggle a behaviour bit, e.g.
  `libdb::flag::TXN_NOSYNC` for faster, less durable commits.
- `.errpfx("myapp")` and `.capture_diagnostics()` — prefix libdb's diagnostics
  and capture the last one into the returned `Error` (and `env.last_diagnostic()`).

```rust
let env = libdb::Env::builder()
    .cachesize(0, 16 << 20)
    .lock_detect(libdb::LockDetect::Default)
    .capture_diagnostics()
    .open("db-home")?;
# Ok::<(), libdb::Error>(())
```

## Lifetime

The `Env` closes when it drops. Every `Db`, `Txn` and `Cursor` borrows it, so
the compiler guarantees the environment outlives everything opened in it — the
C ownership rule, enforced for free.

Next: [Put and get](02-put-get.md).
