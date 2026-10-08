<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.

SPDX-License-Identifier: Sleepycat

See the file LICENSE for redistribution information.
-->

# libdb on FreeBSD 14.5-STABLE — build/portability, S5, G15

> **Preserved from `/tmp`.** This is the full report from the FreeBSD 14.5
> run that produced the S5 fix, the `queue.h`/kqueue finding (P10), the ten
> hardcoded test paths (P11), and the G15 flag measurements. The EC2 host is
> terminated and the file lived only in `/tmp`, so the commit messages were
> the only surviving record of a 7,600-second investigation. Kept whole,
> including the self-corrections, because the retracted first G15 probe is
> itself the useful part.


**Host** FreeBSD 14.5-STABLE (`stable/14-n275257-b32597a90dfb`) amd64, 16 CPU,
clang 21.1.8, **ZFS** root (`zroot/home`, `/home/ec2-user/w`).
**Tree** `https://github.com/berkeleydb/libdb.git` master, fetched as a tarball
(no `git` on the host — see P-7). Configured in a **sibling** build dir
(`/home/ec2-user/w/build` against `/home/ec2-user/w/libdb-master`), plus a
second `--enable-o_direct` build in `/home/ec2-user/w/build-od`.

**Nothing was committed or pushed, and `/home/gburd/ws/libdb` was not modified.**
All patches are inline below as unified diffs.

> **Privilege note, because it bounds three results.** The host has no `sudo`,
> no `doas`, and `root` SSH is refused (`Permission denied (publickey)`), so
> `pkg install` is impossible (`pkg: Insufficient privileges to install
> packages`; `pkg -r` fails on ownership). The base system therefore had to be
> enough — it was, using base `make`, `clang`, `ktrace`/`kdump`, `lldb`. The
> consequences: **no TCL** (so `ssi009.tcl` could not be run — the C driver the
> brief specifies was written instead), **no `javac`** and **no `meson`** (3
> legitimate tier SKIPs), and `python3` existed only as `python3.12`.

---

# Item 1 — Build and test on FreeBSD

## Summary

| | result |
|---|---|
| `dist/configure` | **OK** with base `make` — no GNU-make dependency found |
| First `make` | **FAILED** — 1 compile error (P-1), in the kqueue backend's include path |
| `make` after P-1 fix | **clean, 0 errors** |
| `HAVE_AIO_KQUEUE` | **confirmed defined** (`db_config.h:35`), backend is real code |
| `test/db/run_all.sh`, as found | **9 FAIL / 3 PASS / 3 SKIP** |
| `test/db/run_all.sh`, after fixes | **12 PASS / 0 FAIL / 3 SKIP**, `rc=0` |

Portability problems found: **7**. **Two are genuinely ours and are fixed**
(P-1 the build break, P-2 the sibling-build-dir break). Three more are ours but
left for your decision because the right fix is a judgement call, not a bug fix
(P-3 timeout tuning, P-4 interpreter probing, P-5 cosmetic warnings). Two are
host-environment facts with no libdb defect behind them (P-6, P-7).

## P-1 — `src/dbinc/queue.h` poisons the system `<sys/queue.h>`; the kqueue backend cannot compile

**This is the first time `os_aio_kqueue.c` has ever been compiled, and it did
not compile.** Exact error:

```
--- os_aio_kqueue.o ---
In file included from ../libdb-master/src/os/os_aio_kqueue.c:37:
In file included from /usr/include/sys/event.h:33:
/usr/include/sys/queue.h:121:2: error: invalid preprocessing directive
  121 | #warn Use QUEUE_MACRO_DEBUG_xxx instead (TRACE, TRASH and/or ASSERTIONS)
      |  ^
1 warning and 1 error generated.
*** [os_aio_kqueue.o] Error code 1
```

**Root cause — ours, not FreeBSD's.** `src/dbinc/queue.h:187` does
`#define QUEUE_MACRO_DEBUG 0`. FreeBSD's `<sys/queue.h>:120` tests that name
with **`#ifdef`**, so defining it to `0` still trips the branch, and the branch
body is `#warn`, which is not a valid directive (clang rejects it; it is not a
spelling of `#warning`). Any TU that includes `db_int.h` and then any system
header reaching `<sys/queue.h>` fails. `os_aio_kqueue.c` is that TU, via
`<sys/event.h>`.

The name is private to our file — only its own `#if` reads it (verified:
`grep -rn QUEUE_MACRO_DEBUG` finds 4 hits, all in `src/dbinc/queue.h`) — so
prefixing it is sufficient and changes no behaviour. **Fixed; the full build is
then clean.** This is the whole reason the backend had never built, and it would
bite every future BSD build.

```diff
--- a/src/dbinc/queue.h
+++ b/src/dbinc/queue.h
@@ -184,8 +184,29 @@
 #undef TRACEBUF
 #undef TRASHIT
 
-#define	QUEUE_MACRO_DEBUG 0
-#if QUEUE_MACRO_DEBUG
+/*
+ * Renamed from QUEUE_MACRO_DEBUG, which POISONED the system <sys/queue.h>.
+ *
+ * This header #undefs the system queue macros above and then defines its own,
+ * so it must not leave a macro behind whose NAME the system header also tests.
+ * FreeBSD's <sys/queue.h> has
+ *
+ *	#ifdef QUEUE_MACRO_DEBUG
+ *	#warn Use QUEUE_MACRO_DEBUG_xxx instead (TRACE, TRASH and/or ASSERTIONS)
+ *
+ * and `#warn' is not a valid preprocessing directive -- clang rejects it
+ * outright.  Because the test is #ifdef, defining the macro to 0 still trips
+ * it.  Any translation unit that included db_int.h (hence this file) and then
+ * a system header reaching <sys/queue.h> failed to compile:
+ *
+ *	/usr/include/sys/queue.h:121:2: error: invalid preprocessing directive
+ *
+ * On FreeBSD that is src/os/os_aio_kqueue.c, via <sys/event.h>.  The name is
+ * private to this file (only the #if below reads it), so prefixing it is
+ * sufficient and changes no behaviour.
+ */
+#define	DB_QUEUE_MACRO_DEBUG 0
+#if DB_QUEUE_MACRO_DEBUG
 /* Store the last 2 places the queue element or head was altered */
 struct qm_trace {
 	char * lastfile;
@@ -216,7 +237,7 @@
 #define	QMD_TRACE_HEAD(head)
 #define	TRACEBUF
 #define	TRASHIT(x)
-#endif	/* QUEUE_MACRO_DEBUG */
+#endif	/* DB_QUEUE_MACRO_DEBUG */
 
 /*
  * Singly-linked List declarations.
```

## Sub-item 1b — your `s_chk_include` removal of `<sys/types.h>`, `<errno.h>`, `<unistd.h>`: **SAFE, verified, and here is why**

**Answer: the removal is safe on FreeBSD.** Both files compile, the full library
builds, and the backend still works at runtime. But two things about how this
was checked matter more than the verdict.

### Your change is not on GitHub master — I tested it by hand

"Pull/rebase to current master" would have **silently tested the old file**.
Checked two ways:

```
$ git log --oneline origin/master -1
750fb0302 docs(test): repair the tracker table -- three rows I inserted corrupted it

$ git show origin/master:src/os/os_aio_kqueue.c | sed -n 36,41p
#include <sys/event.h>
#include <sys/types.h>
#include <aio.h>
#include <errno.h>
#include <unistd.h>
```

A freshly-downloaded `codeload` tarball of `refs/heads/master` has all five
includes too. So the edit is local/unpushed. I applied it by hand to get the
post-change state you described:

```
$ grep -n '^#include' src/os/os_aio_kqueue.c
30:#include "db_config.h"
32:#include "db_int.h"
33:#include "dbinc/os_aio.h"
37:#include <sys/event.h>
38:#include <aio.h>
$ grep -n '^#include' src/os/os_aio_posix.c
31:#include "db_config.h"
33:#include "db_int.h"
34:#include "dbinc/os_aio.h"
38:#include <aio.h>
```

### `HAVE_AIO_KQUEUE` really is defined — not assumed

You asked me not to accept a vacuous pass. Both the header text and the
preprocessor agree:

```
$ grep -n 'define HAVE_AIO_KQUEUE' db_config.h
35:#define HAVE_AIO_KQUEUE 1
$ cc -E -I. -I../libdb-master/src -dM .../os_aio_kqueue.c | grep HAVE_AIO_
#define HAVE_AIO_KQUEUE 1
#define HAVE_AIO_POSIX 1
```

### The compile result

```
KQUEUE_CC_RC=0
POSIX_CC_RC=0
=== errors only ===
(no error: lines)
```

and the full `make -j16` is `BUILD_RC=0` with `0` matches for `error:`.

### Proof the TU is not empty — the teeth

`#ifdef` is true for `-D X=0`, so forcing the flag off needs a real `#undef`
injected after `db_config.h`. With that:

| | text/data syms | `aio_read`/`aio_write`/`kqueue`/`kevent` undefs |
|---|---|---|
| `HAVE_AIO_KQUEUE` **on** | 5 | **4** |
| `HAVE_AIO_KQUEUE` **undef'd** | 1 | **0** |

So the compile that passed was compiling the real backend, not a stub.

### Why it is safe — and the conditional you were right to worry about

`db_int.h` already provides all three, transitively, on this platform. From the
preprocessed output of the post-change file:

```
"/usr/include/errno.h"
"/usr/include/sys/types.h"
"/usr/include/unistd.h"
```

**Your concern about conditionality was well founded** — those three includes
in `src/dbinc/db_int.in` *are* inside a conditional:

```
#ifdef HAVE_SYSTEM_INCLUDE_FILES
#include <sys/types.h>   (line 16)
#include <errno.h>       (line 67)
#include <unistd.h>      (line 75)
#endif /* !HAVE_SYSTEM_INCLUDE_FILES */
```

It is safe because `HAVE_SYSTEM_INCLUDE_FILES` is defined in the **`else` arm of
the Windows test** in `dist/configure.ac:675` — i.e. for every non-Windows
build, unconditionally — and this build has `#define HAVE_SYSTEM_INCLUDE_FILES 1`
at `db_config.h:552`.

**The three headers are load-bearing, just already supplied.** Control: a TU
with only the two remaining includes fails exactly as expected —
`error: use of undeclared identifier 'errno'`. The file does use bare `errno`
(`os_aio_kqueue.c:215`, `if (errno == EINTR)`), plus `close()` and `ssize_t`.

So: safe **on FreeBSD/non-Windows**. The one place this would break is a build
where `HAVE_SYSTEM_INCLUDE_FILES` is off (the `DB_WIN32` arm), and
`os_aio_kqueue.c` is not built there anyway.

### Runtime proof, since "it links" is not "it works"

`__os_aio_init` calls every probe as `(void)__os_aio_*_init(...)`, so a failed
`kqueue()` leaves the context on the synchronous fallback **silently**. I wrote
`test/c/aio_kqueue_probe.c` (inline at the end of this section) which writes
8000 records against a deliberately small 512 KB cache to force real eviction,
checkpoints, then verifies every record byte-exact plus `db_verify`:

```
  DB_MPOOL_AIO=1
  wrote 8000 records of 256 bytes against a 524288 byte cache
VERDICT aiokq_sync PASS checkpoint+sync returned 0 with DB_MPOOL_AIO=1
VERDICT aiokq_content PASS all 8000 records read back byte-exact after eviction and checkpoint
VERDICT aiokq_verify PASS db_verify clean
```

And `ktrace` proves the backend actually ran, against a control with the flag
off:

| `DB_MPOOL_AIO` | `kqueue` | `aio_write` | `kevent` | `aio_return` | `aio_error` | `pwrite`+`write` |
|---|---|---|---|---|---|---|
| **1** | **1** | **155** | **29** | **155** | **155** | — |
| **0** | 0 | 0 | 0 | 0 | 0 | 8941 |

Zero aio syscalls without the flag, 155 `aio_write` with it. **The kqueue+aio
backend is genuinely exercised on FreeBSD and is correct.**

## P-2 — 10 `test/db` runners hardcode `../test/db/`, so a sibling build dir cannot run them

8 of the 9 original failures were this. Exact error, ×8:

```
cc: error: no such file or directory: '../test/db/hash_unsorted_cmp.c'
run_all.sh: run_hash_unsorted_cmp FAILED (exit 1)
```

`run_all.sh` itself resolves paths correctly (`HERE=$(CDPATH= cd -- "$(dirname
-- "$0")" && pwd)`), and so do the runners that passed
(`run_qam_inorder_consume.sh`, `run_windows_srclist.sh`, …). The 8 failing ones
use `SRC=${SRC:-../test/db/<x>.c}`, which only resolves when cwd is the in-tree
`build_unix`. The brief asked for a sibling build dir, which is also what
`configure` supports. Fixed the same way the working runners already do it —
one representative diff shown, the other 8 are identical in shape:

```diff
--- a/test/db/run_hash_unsorted_cmp.sh
+++ b/test/db/run_hash_unsorted_cmp.sh
@@ -27,8 +27,9 @@
 
 set -e
 
+HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
 BUILD=${BUILD:-.}
-SRC=${SRC:-../test/db/hash_unsorted_cmp.c}
+SRC=${SRC:-$HERE/hash_unsorted_cmp.c}
 HOME_DIR=${HOME_DIR:-HASH_UNSORTED_TESTDIR}
 TIMEOUT=${TIMEOUT:-180}
 
```

Same change applied to `run_recd_compact.sh`, `run_recd_handlers.sh`,
`run_lock_priority_nullderef.sh`, `run_null_method_slots.sh`,
`run_qam_extent_vrfy.sh`, `run_qam_readpath_bound.sh`,
`run_curadj_dup_partition.sh`, and `run_upgrade.sh` (which needed
`FIXTURE=${FIXTURE:-$HERE/fixtures/bdb4.7.db}`):

```diff
--- a/test/db/run_upgrade.sh
+++ b/test/db/run_upgrade.sh
@@ -70,8 +70,9 @@
 
 set -e
 
+HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
 BUILD=${BUILD:-.}
-FIXTURE=${FIXTURE:-../test/db/fixtures/bdb4.7.db}
+FIXTURE=${FIXTURE:-$HERE/fixtures/bdb4.7.db}
 WORK=${WORK:-UPGTEST}
 TIMEOUT=${TIMEOUT:-60}
 PYTHON=${PYTHON:-python3}
```

## P-3 — `run_qam_extent_vrfy.sh`: not a hang, a 180 s timeout that is far too tight on ZFS

The 9th original failure, and the only one that was not a path bug:

```
Running qam_extent_vrfy (timeout 180s)
run_qam_extent_vrfy.sh: FAIL (rc=124)
```

`rc=124` is a timeout, so this needed to be told apart from a real hang. It is
**not** a hang. Measured directly, the test **passes in 637 s**:

```
  built qext.db: 20000 records, 770 extent file(s)
  clean many-extent queue:   ret=0 (success)
  corrupted __dbq.qext.db.0 page type -> 255
  corrupted extent page:     ret=-30970 (BDB0090 DB_VERIFY_BAD: ...)
qam_extent_vrfy: 3 checks, 0 failures
qam_extent_vrfy: PASS
      637.47 real         0.06 user         0.04 sys
```

**0.06 s user + 0.04 s sys against 637 s wall**: the process is asleep, not
spinning. Sampled progress is a near-perfect 1 extent/second (229 extents at
60 s, 287 at 120 s, 343 at 180 s, … 770 at completion), and `lldb` on the live
process shows why:

```
frame #3: __os_yield(env=..., secs=..., usecs=...) at os_yield.c:48
frame #4: __memp_alloc(...) at mp_alloc.c:267
frame #5: __memp_mpf_alloc(... path="./__dbq.qext.db.210" ...) at mp_fopen.c:787
frame #7: __qam_fprobe(dbc=..., pgno=421, ...) at qam_files.c:311
frame #8: __qam_append(...) at qam.c:434
```

That `__os_yield` is `mp_alloc.c`'s `aggressive >= 3` arm, which does
`__memp_sync_int(DB_SYNC_ALLOC)` then **`__os_yield(env, 1, 0)` — a full
second** — once it has scanned the cache without freeing space. The test asks
for 20000 records with `set_q_extentsize(2)` and a 512-byte page, i.e. ~770
extent files, each needing an `MPOOLFILE` allocation from a cache that is too
small, so nearly every new extent pays one of those 1-second sleeps.

**Verdict: a test-tuning problem, not an engine defect, but a real portability
finding** — the default timeout is below the runtime on a stock ZFS host.
Running the tier with `TIMEOUT=900` makes it pass. I did **not** change the
committed default, because the right fix is a judgement call for you: either
raise the runner's default `TIMEOUT`, or shrink the fixture
(`NRECS`/`EXTENTSZ`), or raise the test's cachesize so `__memp_alloc` stops
going aggressive. Shrinking the fixture is probably best — the bound this test
checks does not need 770 extents to be meaningful.

## P-4 — `run_db_verify_multifile.sh` hard-FAILs where `python3` is spelled `python3.12`

```
FAIL: python3 needed to corrupt a page
run_all.sh: run_db_verify_multifile FAILED (exit 1)
```

The host has `/usr/local/bin/python3.12` but no `python3`. The script does
`PYTHON=${PYTHON:-python3}` then `command -v` and **fails**, where sibling
runners SKIP with a reason for a missing tool. Worked around for this run with
a `python3` symlink on `PATH` (after which it PASSES). **Not patched** — it is
your call whether a missing interpreter should be FAIL (the test cannot verify
the thing it exists to verify) or SKIP (consistent with `javac`/`meson`). I
lean FAIL-is-correct here, so the only change I would suggest is probing
`python3`, then `python3.12`, then `python`, before giving up.

## P-5 — stray `/*` inside a block comment, 2 files, warns on every TU that includes it

```
../libdb-master/src/dbinc/os_aio.h:109:64: warning: '/*' within block comment [-Wcomment]
  109 |  * Caveat, deliberately recorded: dist/s_sig scans ../src/dbinc/*.h, which
```

`../src/dbinc/*.h` inside a `/* */` comment opens a nested comment. Harmless,
but it fires on **every** file including `os_aio.h` (7 TUs), and the same shape
is at `test/db/qam_extent_vrfy.c:202` (`$HOME_DIR/*.db`). Cosmetic, so not
patched; trivially fixed by writing `*.h` as `\*.h` or rewording. Flagged
because it is noise that hides real warnings.

## P-6 / P-7 — host-environment facts, not libdb bugs

- **P-6: no procfs.** `ls /proc` is empty and no procfs appears in `mount`.
  This is what breaks the existing G15 probe — see Item 3.
- **P-7: no `git`, no `gmake`, no TCL, no `javac`, no `meson`, no `strace`.**
  All unavailable and unobtainable without root. `gmake` turned out to be
  **unnecessary**: `dist/Makefile.in` contains no GNU-make constructs
  (checked for `ifeq`/`ifdef`/`$(shell`/`$(wildcard`/`$(patsubst`/pattern
  rules — zero hits) and base `make` configures and builds the whole tree.
  **No GNU-make, glibc, or Linux-header assumption was found in the build.**

## Final regression tier

```
$ cd build && TIMEOUT=900 sh ../libdb-master/test/db/run_all.sh
RUN_ALL_RC=0
     12 PASS
      3 SKIP
```

| runner | verdict |
|---|---|
| `run_hash_unsorted_cmp.sh` | PASS |
| `run_recd_compact.sh` | PASS |
| `run_recd_handlers.sh` | PASS |
| `run_upgrade.sh` | PASS |
| `run_lock_priority_nullderef.sh` | PASS |
| `run_null_method_slots.sh` | PASS |
| `run_qam_extent_vrfy.sh` | PASS (needs `TIMEOUT=900`, see P-3) |
| `qam_inorder_consume.sh` | PASS |
| `run_windows_srclist.sh` | PASS |
| `run_db_verify_multifile.sh` | PASS (needs a `python3` on PATH, see P-4) |
| `run_qam_readpath_bound.sh` | PASS |
| `run_curadj_dup_partition.sh` | PASS |
| `run_u9_serializable.sh` | SKIP — `no javac on this host` |
| `run_u8_backup_config.sh` | SKIP — `no javac on this host` |
| `run_meson_autoconf_parity.sh` | SKIP — `no meson on this host` |

The 3 SKIPs are legitimate missing-toolchain skips with stated reasons, not
silent passes.

---

# Item 2 — S5: `lk_partitions=1` multi-process locker teardown

## Verdict: **REPRODUCED and FIXED** — but the tracker entry is wrong on two counts

> Tracker: *"A second, independent `lk_partitions=1` failure in multi-process
> locker teardown (`ssi009` / `BDB2047`)."*

| tracker claim | measured |
|---|---|
| symptom is **BDB2047** | **No.** BDB2047 never fired in any run. The symptom is an **infinite loop** emitting **BDB2053** |
| specific to **`lk_partitions=1`** | **No.** Reproduces identically at `lk_partitions=1` **and** `lk_partitions=10` |
| needs multi-process churn (`ssi009`) | Partly — it needs a **dead process**, but only **one**, and it is fully deterministic |

**BDB2047 is `"Freeing locker with locks"`** (`src/lock/lock_id.c:565`, in
`__lock_freelocker_int`, returning `EINVAL`). I instrumented for it two ways —
the error text via `DB_ENV->set_errcall` **and** the return code of `failchk` —
because `DB_STR()` compiles to the bare string in a non-NLS build, so the
literal "BDB2047" never appears on stderr. **It did not fire.**

## What actually happens

First multi-process run, 4 children, `lk_partitions=1`:

```
$ ls -la /tmp/s5e.log
-rw-r--r--  1 ec2-user wheel  3169938352 Oct  7 15:07 /tmp/s5e.log
$ grep -c BDB2053 /tmp/s5e.log
45284819
```

**A 3.1 GB log containing 45,284,819 identical BDB2053 lines** for a single
locker id, and `DB_ENV->failchk` never returned:

```
BDB2053 Freeing read locks for locker 0x80000023: 9863/69200630648848
   ... ×45,284,819, all identical ...
```

## Root cause, measured rather than inferred

I wrote a driver that walks `locker_tab` with the *same* predicates
`__lock_failchk` uses and prints the three facts that decide each locker's fate.
Minimal repro is **one** child: a **read-only** `DB_TXN_SERIALIZABLE`
transaction, `SIGKILL`ed with the txn open.

```
  lk_partitions=1
  child pid 10425 died on signal 9 (txn left open)
  [before] locker 0x2        pid=10425 txnal=0 nlocks=1 nwrites=0 nheld=1 alive=0 releasable_by_PUT_READ=some modes=READ   | would_free=yes
  [before] locker 0x80000005 pid=10425 txnal=1 nlocks=1 nwrites=0 nheld=1 alive=0 releasable_by_PUT_READ=NONE modes=SIREAD | would_free=NO (txnal)
  [before] ^^ NON-PROGRESS: failchk emits BDB2053, calls PUT_READ (releases
           nothing), does NOT free (txnal), then `goto retry` -> same state
```

The loop in `src/lock/lock_failchk.c` cannot make progress on locker
`0x80000005`:

1. **The skip test does not fire.** It needs `heldby` empty **or**
   `nlocks == nwrites`. Here `nlocks=1`, `nwrites=0` — because a `DB_LOCK_SIREAD`
   marker counts in `nlocks` but is not a write lock (`IS_WRITELOCK` is
   `WRITE|WWRITE|IWRITE|IWR`, `dbinc/lock.h:76`).
2. **`heldby` is non-empty**, so it logs BDB2053 and calls
   `__lock_vec(DB_LOCK_PUT_READ)`.
3. **That releases nothing.** The `PUT_READ` pass (`writes == 0`) releases only
   `DB_LOCK_READ` and `DB_LOCK_READ_UNCOMMITTED` (`lock.c:569-571`). `SIREAD` is
   **deliberately retained** there — the issue #140 comment says so explicitly:
   *"SIREAD markers are handled just before it by `__lock_sicommit`, which is
   why they must be RETAINED (not released) on this path."*
4. **`__lock_freelocker` is skipped**, because it is guarded on
   `lip->id < TXN_MINIMUM` and `0x80000005 >= 0x80000000`.
5. **`goto retry`** — identical state, forever.

**The deeper point:** the locker genuinely is not `__lock_failchk`'s to clean.
`__txn_failchk` aborts the transaction and that releases the marker. But
`__env_failchk_int` calls them **in this order**:

```
src/env/env_failchk.c:94:  if (LOCKING_ON(env) && (ret = __lock_failchk(env)) != 0)
src/env/env_failchk.c:98:  ((ret = __txn_failchk(env)) != 0 ||
```

so `__lock_failchk` spins waiting for a state only the *later* pass can produce.
It never gets there.

## The fix

Skip a dead **transactional** locker that holds nothing this function can
release. Minimal, at the one place all paths route through:

```diff
--- a/src/lock/lock_failchk.c
+++ b/src/lock/lock_failchk.c
@@ -31,8 +31,9 @@
 	DB_LOCKREGION *lrp;
 	DB_LOCKREQ request;
 	DB_LOCKTAB *lt;
+	struct __db_lock *lp;
 	u_int32_t i;
-	int ret;
+	int released, ret;
 	char buf[DB_THREADID_STRLEN];
 
 	dbenv = env->dbenv;
@@ -62,6 +63,54 @@
 			    F_ISSET(lip, DB_LOCKER_HANDLE_LOCKER) ?
 			    DB_MUTEX_PROCESS_ONLY : 0))
 				continue;
+
+			/*
+			 * Does this locker hold anything THIS function can
+			 * actually release?  Only two things happen below:
+			 * the DB_LOCK_PUT_READ request, which releases just
+			 * DB_LOCK_READ and DB_LOCK_READ_UNCOMMITTED (the
+			 * writes==0 arm of __lock_vec), and __lock_freelocker,
+			 * which is reached only for a NON-transactional locker
+			 * (id < TXN_MINIMUM).
+			 *
+			 * So for a dead TRANSACTIONAL locker holding neither
+			 * of those modes every statement below is a no-op, and
+			 * the `goto retry' at the end of the body re-walks an
+			 * unchanged table -- forever.  That is not theoretical:
+			 * a read-only DB_TXN_SERIALIZABLE transaction whose
+			 * process is killed leaves exactly this shape, one
+			 * DB_LOCK_SIREAD marker on heldby with nlocks=1 and
+			 * nwrites=0, so the skip test above does not fire
+			 * either.  Measured before this guard: 45,284,819
+			 * identical BDB2053 lines (a 3.1 GB log) for a single
+			 * locker id, and DB_ENV->failchk never returned.
+			 * SIREAD is RETAINED by PUT_READ deliberately (see the
+			 * mode enumeration in __lock_vec, issue #140), so this
+			 * can never make progress here.
+			 *
+			 * Such a locker is not ours to clean: __txn_failchk
+			 * aborts the transaction, which releases the marker.
+			 * But __env_failchk_int calls __lock_failchk BEFORE
+			 * __txn_failchk (env_failchk.c), so spinning here waits
+			 * for a state only the later pass can produce.  Leave
+			 * it alone and carry on with the walk.
+			 *
+			 * NOTE this is not specific to lk_partitions=1; it
+			 * reproduces identically with the default partitioning.
+			 */
+			if (lip->id >= TXN_MINIMUM) {
+				released = 0;
+				SH_LIST_FOREACH(lp, &lip->heldby,
+				    locker_links, __db_lock)
+					if (lp->mode == DB_LOCK_READ ||
+					    lp->mode ==
+					    DB_LOCK_READ_UNCOMMITTED) {
+						released = 1;
+						break;
+					}
+				if (released == 0)
+					continue;
+			}
 
 			/*
 			 * We can only deal with read locks.  If a
```

## Result, with the partition A/B control

| build | `lk_partitions` | result |
|---|---|---|
| **before** | 1 | **FAIL** — `failchk` did not return within the 20 s alarm |
| **before** | 10 | **FAIL** — identical |
| **after** | 1 | **PASS** — `failchk RETURNED 0 (success)` |
| **after** | 10 | **PASS** — `failchk RETURNED 0 (success)` |

```
VERDICT s5_failchk_spin PASS failchk TERMINATED (ret=0) with 1 non-progress-shaped locker(s) present, lk_partitions=1
VERDICT s5_failchk_spin PASS failchk TERMINATED (ret=0) with 1 non-progress-shaped locker(s) present, lk_partitions=10
```

The `lk_partitions=10` row is the one that corrects the tracker: **this was
never a partition-specific bug**, so the v2026.09.6 latch-alias fix was never
going to address it, and neither will any further one-partition latch work.

## The must-fail arm

A fix whose test cannot fail is not verified. I reverted just the guard,
rebuilt, and re-ran:

```
=== SABOTAGED run (must FAIL) ===
  non_progress_shapes=1
  VERDICT s5_failchk_spin FAIL DB_ENV->failchk did NOT return within the alarm -- __lock_failchk is spinning (S5)
```

The test fails when the fix is removed and passes when it is present. The 3.1 GB
log also becomes **352 bytes**, and `failchk` returns **600 times** in the
multi-child driver where it previously never returned once.

Note the driver reports the hang **as a hang**, from a `SIGALRM` handler,
rather than letting an outer `timeout` turn it into `rc=124` — `rc` is not a
verdict.

## What remains

- The **multi-child** driver (`test/c/s5_locker_teardown.c`) still cannot
  complete its own staggered-exit schedule, for a reason that is **correct
  engine behaviour, not a bug**: a `SIGKILL`ed child holds a `DB_LOCK_WRITE` on
  the btree meta page, and all siblings then block in
  `__lock_get_internal` (measured: 8/8 children in
  `__db_hybrid_mutex_suspend` from `lock.c:1661`). `DB_ENV->failchk` is the
  documented remedy, and a lock timeout lets the siblings retry — but the shape
  is contended enough that children do not reliably reach their staggered exit
  points, so it reports `s5_kill FAIL ... the run proves nothing` rather than
  claiming a result. **`s5_proof.c` supersedes it**: one child, deterministic,
  ~1 s, and it isolates the defect exactly. I would land `s5_proof.c` as the
  regression test and keep the multi-child driver only as a soak.
- `ssi009.tcl` itself was **never run** — no TCL on the host, and no root to
  install it. The C driver was written per the brief's instruction. Running
  `ssi009` on a `--enable-tcl` build would be worth doing to confirm it is the
  same defect, though the mechanism above fully explains the reported symptom.

---

# Item 3 — G15: the six untested durability/IO flags

## Verdict: all six now have behaviour tests on FreeBSD; **all 8 verdicts PASS**

**Filesystem measured: ZFS** (`zroot/home`), stated because P2/P3 are known to
be filesystem-dependent and this result differs from the XFS one.

## Why the existing tier could not run, and why that mattered

`test/c/flag-run.sh` + `flag_behaviour.c` cannot run here for two reasons, both
harness-side:

1. `flag_behaviour.c` reads open flags from **`/proc/self/fdinfo/<fd>`**.
   FreeBSD mounts no procfs — `opendir("/proc/self/fd")` fails and the probe
   returns `-1`.
2. The two count modes shell out to **`strace`**, which FreeBSD does not have.

Left alone, FreeBSD would report "tier skipped" for all seven flags — which
reads as *"fine here"* and is **exactly G15's own failure mode one layer out**.
So I reimplemented the probe natively: `test/c/flag_behaviour_bsd.c` +
`test/c/flag-run-bsd.sh`, with `ktrace`/`kdump` replacing `strace`.

## The control that changed the answer — `kinfo_file` is the wrong interface

The obvious choice is `kinfo_getfile(3)`, which has `KF_FLAG_DIRECT` and
`KF_FLAG_FSYNC`. **It is wrong for `O_DSYNC`**, and using it would have made me
report a defect that does not exist. Plain `open(2)` control:

```
O_DSYNC=0x1000000 O_SYNC=0x80 O_DIRECT=0x10000 KF_FLAG_FSYNC=0x10 KF_FLAG_DIRECT=0x40
  plain      fd=3 kf_flags=0x00000003 DIRECT=0 FSYNC=0
  O_DSYNC    fd=3 kf_flags=0x00000003 DIRECT=0 FSYNC=0   <-- flag INVISIBLE
  O_SYNC     fd=3 kf_flags=0x00000013 DIRECT=0 FSYNC=1
  O_DIRECT   fd=3 kf_flags=0x00000043 DIRECT=1 FSYNC=0
```

`struct kinfo_file` has **no bit for `O_DSYNC`** — the kernel tracks it as
`FDSYNC` (`sys/fcntl.h:205`) and does not export it. My first run used
`kf_flags` and duly reported:

```
VERDICT dsync_db FAIL KF_FLAG_FSYNC NOT set ... -- the flag was accepted and silently ignored
```

**That was my probe's bug, not libdb's.** `F_GETFL` sees all four:

```
  plain   F_GETFL=0x00000002  O_DSYNC=0 O_SYNC=0 O_DIRECT=0
  dsync   F_GETFL=0x01000002  O_DSYNC=1 O_SYNC=0 O_DIRECT=0
  sync    F_GETFL=0x00000082  O_DSYNC=0 O_SYNC=1 O_DIRECT=0
  direct  F_GETFL=0x00010002  O_DSYNC=0 O_SYNC=0 O_DIRECT=1
```

The final probe uses `kinfo_getfile` **only** for the fd→path mapping (the job
`/proc/self/fd` does on Linux) and takes every flag from `F_GETFL`.

## `HAVE_O_DIRECT` is opt-in — a second trap

`os/os_open.c:72` only ORs in `O_DIRECT` under `#ifdef HAVE_O_DIRECT`, and
`--enable-o_direct` is **off by default**. On a default build the flag
*cannot* reach the fd, so "not set" says nothing about the flag. The runner
therefore **SKIPs those modes with that reason**, checked against the build's
own `db_config.h` rather than assumed. Both builds were measured.

## Results — `--enable-o_direct` build (`HAVE_O_DIRECT 1`)

| verdict | flag | assertion | result |
|---|---|---|---|
| `control` | — | neither `O_DIRECT` nor `O_DSYNC` by default | **PASS** `behaviour.db=0x2` |
| `direct_db` | `DB_DIRECT_DB` | `O_DIRECT` on the data file | **PASS** `0x10002/O_DIRECT` |
| `dsync_db` | `DB_DSYNC_DB` | `O_DSYNC` on the data file | **PASS** `0x1000002/O_DSYNC` |
| `direct_mpf` | `DB_DIRECT` | `O_DIRECT` on a `DB_MPOOLFILE` fd | **PASS** `0x10002/O_DIRECT` |
| `direct_log` | `DB_LOG_DIRECT` | `O_DIRECT` on a log file | **PASS** `0x10002/O_DIRECT` |
| `dsync_log` | `DB_LOG_DSYNC` | `O_DSYNC` on a log file | **PASS** `0x1000002/O_DSYNC` |
| `syncs@count` | `DB_LOG_WRNOSYNC` | log syncs drop, writes do **not** | **PASS** syncs **203 → 2**, writes **203 → 203** |
| `closesync@count` | `DB_NOSYNC` | `fsync`/`fdatasync` count drops | **PASS** **16 → 8** calls |

Default build (no `--enable-o_direct`): `control`, `dsync_db`, `dsync_log`,
`syncs@count`, `closesync@count` **PASS**; the three `O_DIRECT` modes **SKIP**
with the build reason. No mode silently passes.

### Finding: **P2 does not reproduce on FreeBSD/ZFS**

`direct_db` **PASSES** here. On XFS, `DB_DIRECT_DB` cannot open a database at
all (P2 — `__fop_read_meta` hands an unaligned buffer to an `O_DIRECT` read);
on ZFS the open succeeds and `O_DIRECT` really is on the descriptor. Same for
`direct_log`, which is XFAIL on Linux. This is consistent with P2/P3 being
filesystem-dependent, and it is why every verdict line prints the filesystem.
**It is not evidence that P2 is fixed** — the XFAIL arm is retained in the
driver so the mode still reports XFAIL (not FAIL) where the open does fail.

`DB_LOG_WRNOSYNC` (203→2) and `DB_NOSYNC` (16→8) **match the Linux numbers
exactly**, which is good cross-platform corroboration of both.

## Teeth, in both directions

An all-PASS table is worthless without showing the tests can fail.

**1. Anti-vacuity, built in.** `control` asserts the bits are *absent* by
default and passes, so the probe is not stuck-on-"present".

**2. The converse** — forced `direct_db` against a library that *cannot* set
`O_DIRECT`:

```
VERDICT direct_db FAIL O_DIRECT NOT set: and=0x2 or=0x2 [behaviour.db=0x2] on zfs -- the flag was accepted and silently ignored
```

**3. Sabotage A** — `DB_DSYNC_DB` made a no-op in `src/mp/mp_fopen.c`:

```
--- dsync_db: FAIL
flag-run-bsd.sh: FAIL
```

**4. Sabotage B** — `DB_NOSYNC` ignored at `src/db/db.c:889`:

```
    ktrace: 16 fsync/fdatasync syscall(s) in arm=sync
    ktrace: 16 fsync/fdatasync syscall(s) in arm=nosync
--- closesync@count: FAIL (DB_NOSYNC did not reduce fsync count: 16 -> 16)
```

Both sabotages were reverted and both modes returned to PASS. The runner also
FAILs on a missing `VERDICT` line and on a `sync`-arm count of **0** (a probe
that measured nothing cannot report a drop).

## Registered in `test/MANIFEST`

New `flagbsd` tier, 8 entries, with the `optional` split matching the `flag`
tier. `test/check_manifest.sh` accepts it:

```
RESULT flagbsd control pass
RESULT flagbsd direct_db pass
RESULT flagbsd dsync_db pass
RESULT flagbsd direct_mpf pass
RESULT flagbsd direct_log pass
RESULT flagbsd dsync_log pass
RESULT flagbsd syncs_count pass
RESULT flagbsd closesync_count pass
tiers with a results file: flagbsd
```

```diff
--- a/test/MANIFEST
+++ b/test/MANIFEST
@@ -243,6 +243,50 @@
 flag	closesync@count	optional
 
 # ---------------------------------------------------------------------------
+# Tier: flagbsd -- the SAME G15 flags on FreeBSD/BSD.  test/c/flag-run-bsd.sh
+#
+# The `flag` tier above cannot run on FreeBSD, for two reasons that are both
+# about the HARNESS and neither about libdb:
+#
+#   1. flag_behaviour.c reads the open flags from /proc/self/fdinfo/<fd>.
+#      FreeBSD mounts no procfs by default, so that probe returns -1 and every
+#      descriptor-flag mode is unmeasurable.
+#   2. the two count modes shell out to strace(1), which FreeBSD does not have.
+#
+# Left there, FreeBSD would report "tier skipped" for all seven flags -- which
+# reads as "fine here" and is exactly G15's own failure mode one layer out.  So
+# the probe is reimplemented against the platform's native interfaces:
+# kinfo_getfile(3) for the fd->path mapping and F_GETFL for the flags, with
+# ktrace/kdump in place of strace for the syscall counts.
+#
+# WHY F_GETFL AND NOT kinfo_file's kf_flags, which looks like the obvious
+# choice: struct kinfo_file has KF_FLAG_DIRECT and KF_FLAG_FSYNC but NO bit for
+# O_DSYNC (the kernel tracks it as FDSYNC, sys/fcntl.h, not exported there).
+# Measured with a plain open(2) control: open(O_DSYNC) gives kf_flags=0x03 with
+# FSYNC=0, so kf_flags reports DB_DSYNC_DB as silently ignored when the flag
+# did reach the descriptor -- a fabricated defect report.  F_GETFL sees all
+# four flags, so that is the authority.
+#
+# MANDATORY vs optional: same split as the `flag` tier.  The O_DIRECT modes
+# need --enable-o_direct (os/os_open.c only ORs in O_DIRECT under
+# #ifdef HAVE_O_DIRECT), and the runner SKIPs them with that reason rather than
+# reporting a flag as ignored on a build that cannot set it.
+#
+# Measured on FreeBSD 14.5-STABLE / ZFS, --enable-o_direct: all 8 PASS,
+# including direct_db -- so defect P2 does NOT reproduce here.  P2/P3 were
+# already known to be filesystem-dependent, so the filesystem is printed in
+# every verdict line and must be read with the result.
+# ---------------------------------------------------------------------------
+flagbsd	control
+flagbsd	dsync_db
+flagbsd	dsync_log
+flagbsd	syncs_count
+flagbsd	closesync_count
+flagbsd	direct_db	optional
+flagbsd	direct_log	optional
+flagbsd	direct_mpf	optional
+
+# ---------------------------------------------------------------------------
 # Tier: flagapi -- API FLAG BEHAVIOUR, second wave.  test/c/flagapi-run.sh
 #
 # The `flag` tier above covers the seven runtime I/O and durability flags.
```

## What remains

- **`cov_api_surface.c` was not touched.** The G15 entry's complaint — that it
  counts `DB_DIRECT_DB` as covered while only asserting the setter accepts it —
  is already addressed on master (`checks` is no longer incremented there). The
  behaviour assertions now exist for FreeBSD too.
- `p2_align`, `p3_align` and `direct_log@io` (the strace-based syscall-argument
  alignment checks) have **no FreeBSD equivalent yet**. `ktrace` can supply the
  same data — `kdump` prints `pread`/`pwrite` offsets and lengths — so a
  `direct_log@io` analogue is straightforward and is the obvious next step.
- Only **ZFS** was measured. The UFS arm matters, because P2/P3 are
  filesystem-dependent and ZFS is the most forgiving case: the host's root is
  ZFS and creating a UFS filesystem needs root.

---

# Files added (full sources are on the host under `/home/ec2-user/w/libdb-master/test/c/`)

| file | purpose |
|---|---|
| `test/c/flag_behaviour_bsd.c` | G15 descriptor-flag probe via `kinfo_getfile` + `F_GETFL` |
| `test/c/flag-run-bsd.sh` | the `flagbsd` tier runner (`ktrace` for the counts) |
| `test/c/s5_proof.c` | minimal deterministic S5 regression (1 child, ~1 s) |
| `test/c/s5_locker_teardown.c` | multi-process S5 soak (see caveat in Item 2) |
| `test/c/aio_kqueue_probe.c` | runtime proof the kqueue+aio backend works |

# Patches to apply (summary)

| # | file | what | confidence |
|---|---|---|---|
| **P-1** | `src/dbinc/queue.h` | rename `QUEUE_MACRO_DEBUG` → `DB_QUEUE_MACRO_DEBUG`; **unblocks all BSD builds** | high — build fails without it |
| **S5** | `src/lock/lock_failchk.c` | skip dead transactional lockers holding nothing releasable; **fixes an infinite loop** | high — A/B + must-fail arm |
| **P-2** | 9 × `test/db/run_*.sh` | `$HERE` instead of `../test/db/`; enables sibling build dirs | high — mechanical, matches existing runners |
| **G15** | `test/MANIFEST` | register the `flagbsd` tier | high |
| **P-3** | `test/db/run_qam_extent_vrfy.sh` | **not patched** — needs a tuning decision (timeout vs fixture size) | your call |
| **P-4** | `test/db/run_db_verify_multifile.sh` | **not patched** — suggest probing `python3.12`/`python` before failing | your call |
| **P-5** | `src/dbinc/os_aio.h`, `test/db/qam_extent_vrfy.c` | **not patched** — cosmetic `-Wcomment` noise | cosmetic |

**Sub-item 1b answer in one line: your include removal is safe on FreeBSD —
`os_aio_kqueue.c` and `os_aio_posix.c` both compile with `HAVE_AIO_KQUEUE=1`
confirmed present, the full library builds clean, the backend's 4 aio symbols
are really there, and it does 155 real `aio_write`s at runtime; `db_int.h`
supplies `errno.h`/`unistd.h`/`sys/types.h` unconditionally for every
non-Windows build via `HAVE_SYSTEM_INCLUDE_FILES`.**

---

# Appendix — the remaining 7 P-2 runner diffs, in full

Identical in shape to `run_hash_unsorted_cmp.sh` above; included so the whole
change set is applicable without reconstruction.

```diff
--- a/test/db/run_recd_compact.sh
+++ b/test/db/run_recd_compact.sh
@@ -18,8 +18,9 @@
 
 set -e
 
+HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
 BUILD=${BUILD:-.}
-SRC=${SRC:-../test/db/recd_compact.c}
+SRC=${SRC:-$HERE/recd_compact.c}
 HOME_DIR=${HOME_DIR:-RECD_COMPACT_TESTDIR}
 TIMEOUT=${TIMEOUT:-180}
 

--- a/test/db/run_recd_handlers.sh
+++ b/test/db/run_recd_handlers.sh
@@ -21,8 +21,9 @@
 
 set -e
 
+HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
 BUILD=${BUILD:-.}
-SRC=${SRC:-../test/db/recd_handlers.c}
+SRC=${SRC:-$HERE/recd_handlers.c}
 HOME_DIR=${HOME_DIR:-RECD_HANDLERS_TESTDIR}
 TIMEOUT=${TIMEOUT:-300}
 

--- a/test/db/run_lock_priority_nullderef.sh
+++ b/test/db/run_lock_priority_nullderef.sh
@@ -19,8 +19,9 @@
 
 set -e
 
+HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
 BUILD=${BUILD:-.}
-SRC=${SRC:-../test/db/lock_priority_nullderef.c}
+SRC=${SRC:-$HERE/lock_priority_nullderef.c}
 HOME_DIR=${HOME_DIR:-LOCK_PRIORITY_TESTDIR}
 TIMEOUT=${TIMEOUT:-180}
 

--- a/test/db/run_null_method_slots.sh
+++ b/test/db/run_null_method_slots.sh
@@ -16,8 +16,9 @@
 
 set -e
 
+HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
 BUILD=${BUILD:-.}
-SRC=${SRC:-../test/db/null_method_slots.c}
+SRC=${SRC:-$HERE/null_method_slots.c}
 HOME_DIR=${HOME_DIR:-NULL_METHOD_TESTDIR}
 TIMEOUT=${TIMEOUT:-180}
 

--- a/test/db/run_qam_extent_vrfy.sh
+++ b/test/db/run_qam_extent_vrfy.sh
@@ -19,8 +19,9 @@
 
 set -e
 
+HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
 BUILD=${BUILD:-.}
-SRC=${SRC:-../test/db/qam_extent_vrfy.c}
+SRC=${SRC:-$HERE/qam_extent_vrfy.c}
 HOME_DIR=${HOME_DIR:-QAM_EXTENT_VRFY_TESTDIR}
 TIMEOUT=${TIMEOUT:-180}
 

--- a/test/db/run_qam_readpath_bound.sh
+++ b/test/db/run_qam_readpath_bound.sh
@@ -17,8 +17,9 @@
 
 set -e
 
+HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
 BUILD=${BUILD:-.}
-SRC=${SRC:-../test/db/qam_readpath_bound.c}
+SRC=${SRC:-$HERE/qam_readpath_bound.c}
 HOME_DIR=${HOME_DIR:-QAM_READPATH_TESTDIR}
 TIMEOUT=${TIMEOUT:-180}
 

--- a/test/db/run_curadj_dup_partition.sh
+++ b/test/db/run_curadj_dup_partition.sh
@@ -23,8 +23,9 @@
 
 set -e
 
+HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
 BUILD=${BUILD:-.}
-SRC=${SRC:-../test/db/curadj_dup_partition.c}
+SRC=${SRC:-$HERE/curadj_dup_partition.c}
 HOME_DIR=${HOME_DIR:-CURADJ_DUP_TESTDIR}
 TIMEOUT=${TIMEOUT:-180}
 
```
