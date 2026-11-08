---
title: "Upgrading to compatibility level 2026.1 (libdb-2026.1)"
api-name: "Upgrading to compatibility level 2026.1"
---
## Upgrading to compatibility level 2026.1 (`libdb-2026.1`)

The first `master` release after 2026.10.8 moves libdb's **compatibility level** from `2026.0.9` to `2026.1.0` (RFC 0020). The shared library is renamed from `libdb-2026.0.so` to **`libdb-2026.1.so`**.

**Your databases and log files do not change.** A database or log written by any 2026.0 release (2026.10.8, 2026.10.7.1, 2026.10.7 and earlier fork releases) opens and recovers with a 2026.1 release. What changes is the library's name and the shared-memory environment.

### What you must do

1. **Relink.** A program dynamically linked against `libdb-2026.0.so` will not load `libdb-2026.1.so`; that is the point of the change. Rebuild it against the new release. Change `-ldb-2026.0` to `-ldb-2026.1`, and the install prefix `/usr/local/BerkeleyDB.2026.0` to `/usr/local/BerkeleyDB.2026.1`.
2. **Recreate the environment's region files.** A 2026.1 process refuses to join an environment created at 2026.0:

        BDB1538 Program version 2026.1 doesn't match environment version 2026.0
        DB_ENV->open: DB_VERSION_MISMATCH

   Nothing is read or written when this happens. To move an environment:
   1. shut down every process using it, including utilities such as `db_stat`;
   2. run recovery with the **old** release (`db_recover`, or open with `DB_RECOVER`), or confirm the environment was closed cleanly;
   3. remove the region files (`DB_ENV->remove`, or delete `__db.0*`), but **not** the databases or the logs;
   4. open the environment with the 2026.1 release, with `DB_RECOVER` if you like. It recreates the regions and reads the existing logs and databases.

### Why

Between the fork's first release and 2026.10.8 the `DB` handle grew from 1456 to 1744 bytes and the `DBC` handle gained a field (concurrent-read cursor sharding). The library allocates these handles, so a single program never noticed. But two different libdb builds loaded into one process, through a plugin and its host for example, could hand each other a `DB *` of the wrong size, under the same library name. The new name says plainly that the two are not interchangeable.

### Who is not affected

- **The 2026.10.7 long-term-support line** stays at compatibility level 2026.0 and `libdb-2026.0.so` for its whole life. Install 2026.10.7.N patch releases in place, as before.
- **Programs that link libdb statically** need only a rebuild, plus the environment step above if they share an environment with programs on the other level.

### Checking which level you have

`db_version(&major, &minor, &patch)` returns `2026, 1, 0` at the new level and `2026, 0, 9` before it. `db.h` defines `DB_VERSION_MINOR` as `1` or `0` accordingly.
