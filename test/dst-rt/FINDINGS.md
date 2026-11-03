# RFC 0024 runtime fault-injection: findings

Each finding is an issue-style write-up with a reproducer.  Engine fixes go on
separate branches from `origin/master`, one per bug, each with a test that
fails first.  They are **not** fixed on `test/dst-rt`.

---

## F1-001: libdb's region file-management writes fail under a BLANKET O_DIRECT (libdirectio), but the NATIVE direct-I/O path is clean

**Severity:** low (preload-only; the supported `DB_DIRECT_DB`/`DB_LOG_DIRECT`
path is alignment-correct).  Not a data-loss bug.

**Status:** CHARACTERISED.  The native path is correct; the preload gap is a
small robustness improvement, optional.

**Verdict (lead-directed diagnostic, 2026-11).**  Two configurations were
compared on ext4 (/data), a real block-backed fs:

| Configuration | env open | data file | log file | region files | unaligned writes |
|---|---|---|---|---|---|
| **native** `--enable-o_direct`, `DB_DIRECT_DB` + `log_set_config(DB_LOG_DIRECT)` | **works, 50/50 commits** | O_DIRECT (fincore 0) | O_DIRECT (fincore 0) | buffered (by design) | **none** |
| **preload** libdirectio (blanket O_DIRECT on every regular file) | **FAILS, EINVAL** | O_DIRECT | O_DIRECT | O_DIRECT (forced) | 3 (see below) |

So libdb's **supported** direct-I/O path is alignment-correct: it opens the
data and log *page* files O_DIRECT with aligned, block-sized I/O (the log
header is padded on that path -- 50/50 commits with `DB_LOG_DIRECT` and
`fincore` shows the log 0-cached), and it deliberately keeps the region files
(`__db.00N`), temp files and the backup/verify utilities on **buffered**
handles.  The bug surfaces only when libdirectio forces O_DIRECT onto those
buffered-by-design files, whose management writes are not block-aligned:

| Call site | Write | Why O_DIRECT rejects it |
|---|---|---|
| `src/env/env_file.c:__db_file_extend` | `write(fd, &buf, 1)` where `buf` is a `char` on the stack | length 1 (not a 512/4096 multiple), buffer stack-unaligned |
| `src/os/os_map.c:__db_file_write` (DB_ENV_REGION_INIT path) | a `MEGABYTE`-chunked write ending in a `rp->size % MEGABYTE` tail | the tail length is not block-aligned |
| log file header write (`log.0000000001`) | `pwrite64(fd, hdr, 131, 0)` / `write(fd, hdr, 131)` | length 131, heap buffer unaligned |

`__db_file_extend` (env/env_file.c) is the clearest:

```c
char buf;
buf = '\0';
pages = (db_pgno_t)((size - sizeof(buf)) / MEGABYTE);
relative = (u_int32_t)((size - sizeof(buf)) % MEGABYTE);
if ((ret = __os_seek(env, fhp, pages, MEGABYTE, relative)) == 0)
	ret = __os_write(env, fhp, &buf, sizeof(buf), &nw);  /* 1-byte, stack */
```

**This is the alignment class the RFC's §1 storage row and the F1 brief
predicted**, but scoped: it is the region/temp/utility files that libdb keeps
buffered, NOT the page I/O path.  The native `DB_DIRECT_DB`/`DB_LOG_DIRECT`
configuration -- the supported way to get direct I/O -- is alignment-clean, so
F1-001 is a **preload artifact**, not a supported-config bug.

**Reproducer (dev box, Debian 12, ext4 on /data):**

```sh
# build libdb --enable-diagnostic; build the harness rtload
LD_PRELOAD=/opt/dst-rt/lib/libdirectio.so \
  ./rtload --home /data/dstrt/h --ack /tmp/a.log --node a \
           --seed 9 --ops 10 --keys 10 --valsz 200
# => BDB0137 write: 0x7ffef78d3f77, 1: Invalid argument
#    env->open ...: Invalid argument
```

strace pinpoints the call:

```
openat(..., "home/__db.001", O_RDWR|O_CREAT|O_EXCL, 0644) = 4
write(5, "\0", 1)  = -1 EINVAL (Invalid argument)   # __db_file_extend
...
openat(..., "log.0000000001", O_RDWR|O_CREAT, 0644) = 3
pwrite64(3, <131 bytes>, 0) = -1 EINVAL             # log header
```

tmpfs ignores `O_DIRECT`, so the bug only shows on a real block-backed fs
(ext4/xfs); `fincore` on `__db.001` confirms 0 cached pages, i.e. the open did
reach the O_DIRECT path before failing.

**Suggested fix (engine branch `fix/direct-io-alignment`, NOT here).**  Optional
robustness: make the region/temp management writes O_DIRECT-tolerant so a
blanket preload (or a user who sets O_DIRECT on the whole fs) does not trip
them.  Smallest change: size the region file with `ftruncate` /
`posix_fallocate` instead of the 1-byte `write` in `__db_file_extend`
(`__os_truncate` already prefers `ftruncate`), and make the `__db_file_write`
MEGABYTE-tail block-aligned.  The log header already pads correctly on the
native direct path, so no log-format change is needed.  Must NOT touch log or
region format.  Correctness fix for robustness -> normal LTS backport rules if
it lands.  Given the native path is clean, this is low priority.

**Impact on the harness.**  S1 gets TWO directio arms:
  - **native:** `--enable-o_direct` build + `DB_DIRECT_DB`/`DB_LOG_DIRECT`, run
    under kill + recovery -- the supported path, verified clean here;
  - **preload:** libdirectio, which still trips F1-001 at env-open, so that arm
    SKIPs-with-reason (naming F1-001) until the optional fix lands.
The S1 kill+recovery durability check (incl. the `--nosync` sabotage) runs on
the native arm now.

---

## F1-002 (tool note, not a libdb bug): libumem `UMEM_DEBUG=default` aborts on the dev box

**Status:** tool limitation, documented; the harness works around it.

On the dev box (Debian 12, x86_64, libumem built from the pinned commit),
`UMEM_DEBUG=default` (which includes the `audit` per-buffer stack-capture
component) aborts **any** program at startup -- including a 1-line
`malloc(16);free(p)` program -- with no detection report (SIGABRT, rc=134).
So it is not a libdb heap bug; it is the `audit` component failing on this
host.  Confirmed by isolating the flags:

| UMEM_DEBUG | clean 1-malloc prog | 64-into-16 overflow |
|---|---|---|
| `default` / `audit` / `audit,guards` | ABORT (rc 134/1) | ABORT |
| `guards` | rc 0 | rc 0 (NOT detected at free) |
| `contents` / `firewall` | rc 0 | rc 0 (NOT detected) |
| **`guards,contents`** | **rc 0** | **ABORT (detected)** |

The S3 `s3-umem` arm therefore probes `default` on a trivial program first and,
when it aborts spuriously, falls back to `UMEM_DEBUG=guards,contents` -- the
detection half of `default` (redzone + fill-pattern).  That mode runs the libdb
workload clean AND catches both the planted overflow and a use-after-free (the
`s3-umem-sabotage` arm proves the teeth).  The only thing lost versus `default`
is the audit backtraces, which are diagnostic, not detection.

**libdb result under redzone+contents checking:** CLEAN over an 800-op
transactional workload -- no redzone corruption, no use-after-free, no
fill-pattern violation.  rtcheck verifies the data afterwards.

**Resolution for the harness (lead decision 2026-11): NO engine fix.**  Changing
engine code to satisfy a tool that forces O_DIRECT on files libdb keeps
buffered on purpose is not worth the risk.  F1-001 stays a documented finding.
Instead the S1 preload arm uses a tiny test-only shim,
`test/dst-rt/tools/directio_match.c`, an `open`/`openat` LD_PRELOAD that sets
O_DIRECT only for paths matching `$DIRECTIO_MATCH` (default `*.db`).  Combined
with the loader's `--direct` (native `DB_LOG_DIRECT`) for the log, the preload
arm now runs uncached over the data+log files AND the utilities
(db_verify/db_recover/db_backup, which never take DB_DIRECT_DB), without
tripping the buffered-region writes.  `s1-preload-directio` PASSES.
