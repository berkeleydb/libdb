<!-- SPDX-License-Identifier: Sleepycat -->
# 1. Opening an environment

Everything starts with an `Env`: a directory that holds the shared cache, the
lock table, and the transaction log. Create it, configure it, open it, and tie
its lifetime to a `defer`.

```zig
const std = @import("std");
const libdb = @import("libdb");

var env = try libdb.Env.create();
defer env.deinit();           // closes the handle; dead afterwards
try env.setCachesize(0, 16 << 20);  // 16 MiB cache (optional)
try env.open("/tmp/myenv");   // creates it if absent, transactional + DB_THREAD
```

`env.open` opens with `DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK | DB_INIT_LOG |
DB_INIT_TXN | DB_THREAD`, so the environment is transactional and free-threaded.
The home directory must already exist (create it with your OS's `mkdir`).

**Lifecycle:** the `Env` must outlive every `Db`, `Txn`, `Cursor` and `Sequence`
opened in it. In `Debug`/`ReleaseSafe` builds a closed handle is poisoned and any
reuse trips an assert.

**Diagnostics:** DB's human-readable messages are captured automatically; after a
failing call read `env.lastError()`:

```zig
db.put(null, "k", "v") catch |e| {
    std.debug.print("{s}: {s}\n", .{ @errorName(e), env.lastError() });
    return e;
};
```
