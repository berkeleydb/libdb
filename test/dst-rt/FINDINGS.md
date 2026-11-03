# RFC 0024 runtime fault-injection: findings

Each finding is an issue-style write-up with a reproducer.  Engine fixes go on
separate branches from `origin/master`, one per bug, each with a test that
fails first.  They are **not** fixed on `test/dst-rt`.

---

## F1-001: libdb's region and log file-extend writes fail under O_DIRECT (EINVAL)

**Severity:** medium (blocks the S1 directio durability scenario from running
against an unmodified engine; not a data-loss bug by itself).

**Status:** OPEN, needs an engine fix on its own branch.

**Summary.** Under `libdirectio` (which sets `O_DIRECT` on every regular file
opened via `open`/`openat`), libdb cannot even open a transactional
environment.  Three distinct writes return `EINVAL` because they violate
`O_DIRECT`'s alignment contract (length a multiple of the device block size,
buffer aligned to the block size):

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
predicted.**  libdb's *page* I/O (the `DB_DIRECT_DB` path, `__os_io`) is
page-aligned and page-sized and so is designed to be O_DIRECT-clean; these
file-*management* writes (region sizing, log header) are not.  Note: in the
`--enable-diagnostic` build tested here, `DB_ENV->set_flags(DB_DIRECT_DB)`
reported "direct I/O either not configured or not supported" and ran buffered,
so libdb's own direct-I/O mode was not even reachable without a configure flag
(`--enable-o_direct` / O_DIRECT detection) -- a separate observation.  The
point stands regardless: a blanket `O_DIRECT` via libdirectio hits the
management writes, which are unaligned by construction, before any page I/O.

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

**Suggested fix (engine branch, NOT here).**  Make the region/log
file-management writes O_DIRECT-tolerant when the fd happens to be direct:
either (a) size the file with `ftruncate`/`posix_fallocate` instead of a 1-byte
write (`__db_file_extend` already prefers `ftruncate` in `__os_truncate`; the
region path should too), or (b) bounce these small management writes through a
block-aligned bounce buffer, or (c) detect `EINVAL` on a management write and
retry on a buffered dup of the fd.  Option (a) is the smallest: on a host with
`HAVE_FTRUNCATE`, extend the region file with `ftruncate` and drop the 1-byte
write entirely.

**Impact on the harness.**  Until the engine is fixed, the S1 `direct` arm
cannot run an unmodified libdb end-to-end.  The S1 scenario therefore:
  - detects the EINVAL-on-open and records it as the known finding F1-001
    (SKIP with a reason that names this finding), rather than a false PASS;
  - still runs its kill+recovery durability check WITHOUT directio, so the
    sabotage arm (`--nosync` loses acked writes) is still exercised;
  - will flip to a full directio run the moment F1-001 is fixed on master.
