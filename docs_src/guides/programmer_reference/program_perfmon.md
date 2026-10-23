---
title: "Performance Event Monitoring"
api-name: "Performance Event Monitoring"
source: docs/programmer_reference/program_perfmon.html
---
## Performance Event Monitoring

<span class="sect2"> [Platforms](program_perfmon.md#program_perfmon_platforms) </span>

<span class="sect2"> [Building](program_perfmon.md#program_perfmon_build) </span>

<span class="sect2"> [Using the probes with DTrace](program_perfmon.md#program_perfmon_dtrace) </span>

<span class="sect2"> [Using the probes on Linux](program_perfmon.md#program_perfmon_linux) </span>

<span class="sect2"> [A worked example: commit latency](program_perfmon.md#program_perfmon_example) </span>

<span class="sect2"> [Example scripts](program_perfmon.md#program_perfmon_examples) </span>

<span class="sect2"> [Performance events reference](program_perfmon.md#program_perfmon_probes) </span>

A library configured with `--enable-dtrace` contains user-level statically defined tracing (USDT) probes, in a provider named `bdb`. They mark database operations (open, cursor, get, put, delete), page allocation, transactions, waits for locks and mutexes, buffer-cache reads, writes and evictions, log flushes, and a few rare code paths. A tracer can attach to any process that uses the library. The application needs no changes.

### Platforms

| Platform | Build tool | What the probes are | Reading them |
|---|---|---|---|
| FreeBSD | base system `dtrace(1)` | DOF, linked in by `dtrace -G` | `dtrace`, as root |
| illumos, Solaris | base system `dtrace(1)` | DOF, linked in by `dtrace -G` | `dtrace`, with the `dtrace_proc` and `dtrace_user` privileges or as root |
| macOS | Xcode `dtrace(1)` | resolved by the linker, no `dtrace -G` | `dtrace`, as root, with System Integrity Protection allowing DTrace |
| Linux | SystemTap's `dtrace` wrapper and `<sys/sdt.h>` | ELF `.note.stapsdt` entries | `bpftrace`, `perf`, or `stap` |

FreeBSD 14.5/amd64 and Debian 12/amd64 were built and tested for this page. The illumos and macOS rows follow from reading `dist/aclocal/perfmon.m4` and `dist/Makefile.in` and have **not been tested**.

DTrace for Windows and Oracle Linux's DTrace port are not supported build targets. The Windows build does not use `configure`, and on Oracle Linux `--enable-dtrace` uses whatever `dtrace` is first on the `PATH`. Neither has been tried.

### Building

```
../dist/configure --enable-dtrace [--enable-perfmon-statistics]
make
```

`--enable-systemtap` is accepted as a synonym for `--enable-dtrace`. `--enable-dtrace` cannot be combined with `--disable-statistics`.

configure prints which kind of `dtrace` it found:

```
checking which kind of dtrace dtrace is... native, -G     (FreeBSD)
checking which kind of dtrace dtrace is... sdt            (Linux)
```

- **FreeBSD and illumos.** `dtrace -G` is run over every object file before the library is linked. It rewrites each probe site and produces `db_provider.o`, which carries the DOF. That object is linked into both `libdb-2026.0.so` and `libdb-2026.0.a`. A program that links the static library gets the probes too.
- **Linux.** Install only `systemtap-sdt-dev` (Debian, Ubuntu) or `systemtap-sdt-devel` (Fedora, RHEL). It provides `<sys/sdt.h>` and `/usr/bin/dtrace`. The SystemTap runtime (`stap`, kernel debuginfo) is not needed to build or to use the probes with `bpftrace` or `perf`.
- **macOS.** The probes are resolved when the library is linked, with no `dtrace -G` step.

A library configured without `--enable-dtrace` contains no probes and no probe code. The default build is unchanged by this feature. On FreeBSD 14.5 and Debian 12 the `.text` section of a default `libdb-2026.0.so` was compared with master's and was byte-identical, as was the region signature (`dist/env_sig_print.sh`).

`make listprobes` prints the probes compiled into the library.

### Using the probes with DTrace

The probes of a process appear under the provider `bdb<pid>`. `$target` names the process started with `-c` or given with `-p`.

```
# kldload dtraceall                      # FreeBSD, once per boot
# dtrace -l -n 'bdb$target:::' -c './db_load -V'
   ID   PROVIDER            MODULE                          FUNCTION NAME
73417   bdb23789   libdb-2026.0.so                       __bam_split alloc-btree_split
73418   bdb23789   libdb-2026.0.so                         __db_free alloc-free
73419   bdb23789   libdb-2026.0.so                          __db_new alloc-new
73420   bdb23789   libdb-2026.0.so                        __db_close db-close
...
```

To trace every process that uses the library:

```
# dtrace -Zn 'bdb*::: { @[probename] = count(); }'
```

DTrace needs root on FreeBSD and macOS. On illumos and Solaris the `dtrace_proc` and `dtrace_user` privileges (`usermod -K defaultpriv=basic,dtrace_proc,dtrace_user <user>`) are enough to trace your own processes. When `LD_LIBRARY_PATH` points at an uninstalled build, pass it through `sudo env LD_LIBRARY_PATH=...`, because `sudo` drops it.

### Using the probes on Linux

The probes are ELF notes in the library. Reading them needs no privilege:

```
$ readelf -n .libs/libdb-2026.0.so | grep -c 'Provider: bdb'
46
```

`bpftrace` and `perf` attach to the library by path. Attaching needs root, or `CAP_BPF` and `CAP_PERFMON`.

```
# bpftrace -l 'usdt:/usr/local/BerkeleyDB.2026.0/lib/libdb-2026.0.so:bdb:*'
# perf probe -x /usr/local/BerkeleyDB.2026.0/lib/libdb-2026.0.so sdt_bdb:txn__commit
```

In a USDT name the `-` of a DTrace probe name becomes `__`. So `txn-commit` is `bdb:txn__commit`. bpftrace 0.17 could not resolve a probe in a library that its `-c` child had not loaded yet. Attach by library path first, then start the program, as the example below does.

The SystemTap scripts in `util/systemtap` are kept as they were and have not been tested with this release.

### A worked example: commit latency

`util/dtrace/commitlat.d` compares how long a durable commit takes with how long the log fsync behind it takes. It also counts both, so commits divided by fsyncs is the group-commit batch size. If commit latency follows fsync latency, the device sets the pace. If commits are much slower than fsyncs, they are queueing for the log.

The driver is `test/db/dtrace_drv.c`. It runs N threads, each doing begin, put and synchronous commit, then a checkpoint. This is a real run on a c6id.4xlarge running FreeBSD 14.5, with the environment on a UFS file system on the local NVMe device:

```
# dtrace -q -s ../util/dtrace/commitlat.d -c "./dtrace_drv /nvme/ex 8 2000"
VERDICT dtrace_drv PASS commits=16000

  commit latency (us)
           value  ------------- Distribution ------------- count
               4 |                                         0
               8 |                                         1
              16 |                                         0
              32 |@                                        385
              64 |@@@@@@@@@@@@@@@@@@@@@@@@@@               10241
             128 |@@@@@@@@@@@@@                            5374
             256 |                                         1
             512 |                                         0

  log fsync latency (us)
           value  ------------- Distribution ------------- count
              16 |                                         0
              32 |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  5180
              64 |@                                        102
             128 |                                         12
             256 |                                         0

fsync errors: 0
commits 16001, log fsyncs 5294
```

Eight threads produced about three commits per fsync. Most fsyncs took 32-64 µs, and most commits took 64-256 µs, one or two fsyncs' worth: each commit waited for the fsync already running, then for its own. The one extra commit is the checkpoint's internal transaction.

The same measurement with bpftrace on Debian 12 (Linux 6.1, ext4 on local NVMe, bpftrace 0.17) and the same driver:

```
# bpftrace -q -e '
  usdt:/path/to/build_unix/.libs/libdb-2026.0.so:bdb:log__flush_begin { @s[tid] = nsecs; }
  usdt:/path/to/build_unix/.libs/libdb-2026.0.so:bdb:log__flush_end /@s[tid]/ {
      @fsync_us = hist((nsecs - @s[tid]) / 1000); delete(@s[tid]); }
  usdt:/path/to/build_unix/.libs/libdb-2026.0.so:bdb:txn__commit { @commits = count(); }
  interval:s:12 { exit(); }' &
$ ./dtrace_drv /nvme/bx 8 2000

@commits: 16002
@fsync_us:
[16, 32)              41 |                                                    |
[32, 64)            3502 |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@|
[64, 128)              3 |                                                    |
[128, 256)           871 |@@@@@@@@@@@@                                        |
[256, 512)             1 |                                                    |
[512, 1K)              1 |                                                    |
```

That run did 4,419 fsyncs for 16,002 commits.

**Overhead.** On FreeBSD the same driver (8 threads × 4,000 durable commits) took 0.62-0.63 s in each of five runs against the default library, and the same against the `--enable-dtrace` library with no tracer attached. At that resolution the difference was not measurable. This page makes no claim about the cost while a tracer is attached, because that depends on the script.

### Example scripts

The DTrace examples are in `util/dtrace`. All of them trace one process, given with `-p <pid>` or `-c "<program> [<args>]"`. They run until interrupted, until that process exits, or until an optional maximum count is reached. On FreeBSD 14.5 every script compiled and ran against `db_load`. `cache.d` needs a library built with `--enable-perfmon-statistics`, because `mpool-hit` and `mpool-miss` are statistics probes. Without them it stops with `probe description ... does not match any probes`.

<span class="term">commitlat</span>  
Commit latency against log-fsync latency, described above.

<span class="term">apicalls</span>  
Counts the main API calls, grouped by thread. It uses the `pid` provider, not the `bdb` probes.

<span class="term">apitimes</span>  
Time spent in the main API calls, grouped by thread (`pid` provider).

<span class="term">apitrace</span>  
Entry to and return from each of the main API calls (`pid` provider).

<span class="term">cache</span>  
Cache hits, misses and evictions, overall and per file, every *N* (default 10) seconds for *M* (default 60) intervals. Needs `--enable-perfmon-statistics`.

<span class="term">dbdefs</span>  
D declarations of the Berkeley DB structures some probes pass, `#include`d by `dist/db_provider.d` when native `dtrace` builds the provider.

<span class="term">locktimes</span>  
Time spent waiting for page locks, grouped by file name, page number and lock mode.

<span class="term">locktimesid</span>  
The same, grouped by the 20-byte file id instead of the file name. This helps when several environments are traced, or when files are recreated.

<span class="term">mutex</span>  
Mutex and shared-latch wait times, grouped by mutex, mode and thread, and by mutex class (the `MTX_*` values in `src/dbinc/mutex.h`).

<span class="term">showerror</span>  
Prints the application stack when an error routine (`__db_err`, `__db_errx`, `__env_panic`) is called (`pid` provider).

### Performance events reference

The authoritative list is `dist/db_provider.d`, generated by `dist/s_include` from `dist/events.in`. Each probe is declared with its C argument types. A DTrace probe `class__name` is named `class-name` in a D script, and `bdb:class__name` for bpftrace and perf. The classes are:

- `alloc`: on-disk page allocation and free, and btree splits.
- `db`: open, close, cursor, get, put and delete, with the file and database names.
- `lock`: `suspend` and `resume` around a wait for a conflicting transactional lock, and lock release.
- `log`: `flush_begin` and `flush_end` around the log fsync. `flush_end` carries its return value.
- `mpool`: page `read`, `write` and `evict`, region allocation, and allocator wrap-around. `mpool-write` fires for synchronous writes and for the asynchronous checkpoint writes made with `DB_MPOOL_AIO`.
- `mutex`: `suspend` and `resume` around a mutex or shared-latch wait.
- `race`: rarely taken retry paths, for diagnosing races.
- `rep`: replication counters.
- `txn`: begin, commit, prepare and abort.
- `error`, `warning`: an error first noticed deep in the library.

About 40 of the 88 declared probes, mostly in the `lock`, `mpool`, `mutex`, `rep` and `txn` classes, are updates of the counters reported by the `*_stat` methods. They are compiled in only with `--enable-perfmon-statistics`. Their first argument is the counter's new value.

Some fork-era code has no probes of its own: the buffer-pool lookup (`__memp_fget`), the btree cursor and `src/typed`. Their time shows up in `db-get`, `db-put`, `mpool-read` and the lock and mutex wait probes. For finer detail use the `pid` provider (DTrace) or `uprobe` (bpftrace).
