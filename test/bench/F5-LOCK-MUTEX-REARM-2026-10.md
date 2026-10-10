# F5 — `__lock_freelock` mutex re-arm: Linux arm, correctness gates, B3 classification

Patch under test: `the F5 change as merged in src/lock/lock.c (v2026.10.4)`, applied to `src/lock/lock.c` only.
Base commit: `b027cd36f` (`mutex: guard __mutex_free against a slot failchk already reclaimed (F6)`).
Patch is byte-identical on this host, the Linux boxes and the FreeBSD box: `md5 84a910d21416ddb1fb6a8895d4fb37ac` of `git diff src/lock/lock.c`.

Nothing was committed and nothing was pushed. The patch remains in `the F5 change as merged in src/lock/lock.c (v2026.10.4)` and as an uncommitted working-tree change.

---

## 0. The two facts I was asked to confirm rather than re-derive

Both verified by reading the code at the cited lines.

**`MUTEX_IS_OWNED` is hardcoded to `0` when `HAVE_MUTEX_SUPPORT` is undefined**, at `src/dbinc/mutex_int.h:978`:

```c
#ifdef HAVE_MUTEX_SUPPORT
#define MUTEX_IS_OWNED(env, mutex)                                      \
        (mutex == MUTEX_INVALID || !MUTEX_ON(env) ||                    \
        F_ISSET(env->dbenv, DB_ENV_NOLOCKING) ||                        \
        F_ISSET(MUTEXP_SET(env, mutex), DB_MUTEX_LOCKED))
#else
#define MUTEX_IS_OWNED(env, mutex)      0
#endif
```

A constant-`0` `MUTEX_IS_OWNED` makes `!MUTEX_IS_OWNED(...)` constantly true, so the new code always takes the `MUTEX_LOCK` arm in that configuration. **That is safe only because `MUTEX_LOCK` is itself a no-op in the same configuration** — `src/dbinc/mutex.h:323`:

```c
#define MUTEX_LOCK(env, mutex)          ((void)(mutex), 0)
```

So in a no-mutex-support build the new branch expands to "always do nothing", which is the correct behaviour there; the old branch expanded to `__mutex_refresh` + nothing. The two agree. The fix does not depend on `MUTEX_IS_OWNED` being meaningful in that configuration, which is the only reason the hardcoded `0` is not a bug here.

**`F_SET(mutexp, DB_MUTEX_LOCKED)` is outside `#ifdef DIAGNOSTIC`** — `src/mutex/mut_pthread.c:426` (the self-block/condwait path) and `:446` (the straight-line path). I read both; the `#ifdef DIAGNOSTIC` block at `:434-444` contains only the "lock currently in use" self-deadlock check, and the `F_SET` at `:446` sits *after* its `#endif`. This is the load-bearing fact for the whole fix: the flag the new conditional reads is maintained in production builds, not just diagnostic ones. Had it been diagnostic-only, the fix would have been a no-op in production and a correctness change in diagnostic builds — the opposite of what is wanted.

---

## 1. The Linux arm

**Box:** `c6id.8xlarge`, 32 vCPU, dedicated, Debian 12, kernel 6.1.0-53, `/nvme` XFS on the local NVMe, THP `[never]`, load average `0.00 0.00 0.00` at sweep start. This is the quiet box provisioned for this measurement, not the loaded shared host the previous attempt used.

**Method:** one binary, runtime switch only. Arms **alternated within each rep** (`ON` then `OFF` inside rep 1, then inside rep 2, …), **7 reps**, t = 1/2/4/8/16/32, 10 s per cell, 84 cells, zero timeouts. Driver `f5_bench` takes two `DB_LOCK_WRITE` locks per iteration on a 4-object shared set in a per-thread random order, holding the first while acquiring the second, with `DB_LOCK_YOUNGEST` detection on — so cycles form, victims are aborted, and locks reach `__lock_freelock` with `DB_LSTAT_ABORTED`, which is a status the F5 branch actually fires on.

Note on arm naming: the sweep script's `ON`/`OFF` labels are the opposite of the intuitive reading (`ON` = env var *absent* = F5 fix active). I relabel to NEW/OLD below so the direction cannot be misread.

### Per-cell results

| t | arm | n | mean ops/s | **cv%** | median | min | max | deadlocks/s | lock waits |
|---|-----|---|-----------:|--------:|-------:|----:|----:|------------:|-----------:|
| 1 | NEW | 7 | 2,548,664 | 0.18 | 2,550,217 | 2,538,511 | 2,552,275 | 0 | 0 |
| 1 | OLD | 7 | 2,551,973 | 0.11 | 2,551,749 | 2,547,475 | 2,556,395 | 0 | 0 |
| 2 | NEW | 7 | 376,514 | **2.45** | 376,582 | 364,005 | 393,898 | 7,956 | 2,107,167 |
| 2 | OLD | 7 | 382,897 | 2.32 | 383,153 | 370,534 | 392,659 | 8,142 | 2,115,064 |
| 4 | NEW | 7 | 179,420 | 1.46 | 179,966 | 174,071 | 182,364 | 23,877 | 2,367,448 |
| 4 | OLD | 7 | 179,528 | 1.35 | 178,658 | 176,459 | 182,641 | 23,875 | 2,366,929 |
| 8 | NEW | 7 | 52,704 | 0.66 | 52,686 | 52,126 | 53,167 | 33,937 | 1,384,496 |
| 8 | OLD | 7 | 52,568 | 0.51 | 52,502 | 52,191 | 53,040 | 33,819 | 1,379,729 |
| 16 | NEW | 7 | 22,302 | 0.53 | 22,312 | 22,084 | 22,414 | 41,160 | 1,163,003 |
| 16 | OLD | 7 | 22,281 | 0.53 | 22,275 | 22,158 | 22,472 | 41,157 | 1,162,688 |
| 32 | NEW | 7 | 9,769 | 0.83 | 9,742 | 9,654 | 9,901 | 40,618 | 968,858 |
| 32 | OLD | 7 | 9,734 | 0.39 | 9,734 | 9,667 | 9,784 | 40,457 | 965,073 |

**Worst per-cell cv = 2.45%.** Under the 10% bar, so deltas are quotable. (The previous attempt's 14–104% was the loaded shared host; its refusal to quote a number was the right call, and the fix for it was a quiet box, not more reps.)

### The delta

| t | NEW (fix) | OLD (refresh) | ratio | delta% | Welch t | verdict |
|---|----------:|--------------:|------:|-------:|--------:|---------|
| 1 | 2,548,664 | 2,551,973 | 0.999 | −0.13 | −1.59 | not significant |
| 2 | 376,514 | 382,897 | 0.983 | **−1.67** | −1.32 | not significant |
| 4 | 179,420 | 179,528 | 0.999 | −0.06 | −0.08 | not significant |
| 8 | 52,704 | 52,568 | 1.003 | +0.26 | +0.82 | not significant |
| 16 | 22,302 | 22,281 | 1.001 | +0.09 | +0.32 | not significant |
| 32 | 9,769 | 9,734 | 1.004 | +0.36 | +1.03 | not significant |

Welch's t-test, n=7 per arm; |t| > 2.18 would be p<0.05. **No cell reaches significance in either direction.**

**Verdict: no regression on Linux, and no measurable win either.** The honest statement is *flat*: every cell is within ±1.7%, every cell is statistically indistinguishable, and the largest nominal movement (−1.67% at t=2) is also the noisiest cell (cv 2.45%) and is not significant. This matches the stated expectation — the destroy+init round trip is wasted work on Linux too, but `pthread_mutex_destroy` on glibc is cheap enough that removing ~7,000 of them per second does not show up against a lock path doing millions of operations. I did not measure a Linux win; I measured the absence of a Linux cost.

### Vacuity check — the branch really executed on Linux

A flat result is ambiguous between "costs nothing" and "never ran". So I instrumented the branch with two counters and re-ran (`f5vac`, counters compiled into a dedicated copy of the tree):

| t | F5 branch executions | of which NOT already owned |
|---|---------------------:|---------------------------:|
| 2 | 65,000 | **0** |
| 4 | 195,447 | **0** |
| 8 | 333,572 | **0** |
| 16 | 328,963 | **0** |
| 32 | 323,012 | **0** |
| **total** | **1,245,994** | **0** |

The branch executed over 1.2 million times on Linux, and on **every single execution the mutex was already held by the calling thread** — `notowned=0 on 1,245,994 of 1,245,994`. This is the same result the FreeBSD arm recorded (0 of 737,833) and it is direct empirical confirmation of the analysis's central claim: the thread reaching `__lock_freelock`'s `DB_LOCK_FREE` arm already holds the locker mutex, so the old `destroy → init → lock` sequence was a round trip back to the state it started in. The `MUTEX_LOCK` in the new `else if` arm was never once needed in these runs; it is there for the states the enumeration cannot exclude by construction, not for a state that occurs in practice.

### FreeBSD re-confirmation (not asked for; done because it was cheap)

I re-ran the FreeBSD sweep independently (3 reps, arms alternated within reps, same binary + switch) to check the banked numbers reproduce:

| t | NEW | OLD | ratio | banked ratio |
|---|----:|----:|------:|-------------:|
| 1 | 2,419,344 | 2,416,197 | 1.001 | unchanged ✓ |
| 2 | 367,344 | 115,534 | 3.18× | 371,028 / 117,090 = 3.17× ✓ |
| 4 | 165,233 | 19,787 | 8.35× | 167,533 / 19,546 = 8.57× ✓ |
| 8 | 53,768 | 4,658 | 11.54× | 54,266 / 4,526 = 11.99× ✓ |
| 16 | 23,156 | 1,579 | 14.66× | 23,368 / 1,562 = 14.96× ✓ |
| 32 | 10,052 | 689 | **14.60×** | 10,025 / 683 = **14.68×** ✓ |

Worst per-cell cv 2.33%. The banked FreeBSD result reproduces within noise. **The win is a FreeBSD win; Linux is flat.**

---

## 2. Correctness gates

Two build configurations on a dedicated Debian 12 box, both with the build dir a **sibling** of the source tree (`/nvme/b_diag`, `/nvme/b_def`, source at `/nvme/src_f5`):

- `b_diag`: `../src_f5/dist/configure --enable-diagnostic --enable-test --with-tcl=/usr/lib/tcl8.6 --enable-stl=no` → `db_config.h: #define DIAGNOSTIC 1`, make rc=0
- `b_def`: same minus `--enable-diagnostic` → `db_config.h: /* #undef DIAGNOSTIC */`, make rc=0

Both have `HAVE_MUTEX_HYBRID 1` and `HAVE_MUTEX_PTHREADS 1`, so the `DB_MUTEX_LOCKED` assertions at `mut_pthread.c:513` and the self-deadlock check at `:434` are live in the diagnostic build.

Every tier was run **four times**: {diagnostic, default} × {NEW = fix active, OLD = `DB_LOCK_REFRESH_LOCK_MUTEX=1`}. Results dirs were separated per arm (`LIBDB_RESULTS_DIR`), because the harness's default `$SRC/test/.results` is one shared path and parallel streams would otherwise overwrite each other's verdict file.

### `test/db/run_all.sh`

| build | arm | pass | fail | hang |
|-------|-----|-----:|-----:|-----:|
| diagnostic | NEW | 20 | **0** | 0 |
| diagnostic | OLD | 20 | **0** | 0 |
| default | NEW | 20 | **0** | 0 |
| default | OLD | 20 | **0** | 0 |

The raw first pass reported `pass=16 fail=4` in **all four** arms, with the same four runners failing: `run_handle_sizes`, `run_s5_failchk_spin`, `run_opt_null_sample`, `run_mvcc_mtx_doublefree`. That is **not** an F5 failure and not a flake — it is a harness-path artifact, identical in both arms and both builds, and the error text says so:

```
run_handle_sizes.sh: FAIL no libdb library under /nvme/src_f5/build_unix
```

Those four runners take the build dir **positionally** (`BUILD=${1:-"$HERE/../../build_unix"}`) while the other sixteen read it from the environment (`BUILD=${BUILD:-.}`), and `run_all.sh` passes no argument to any runner. With a build dir that is a sibling of the source tree — the layout the brief specifies and `run_lock_priority_nullderef.sh`'s own comment documents — those four look for a library under `$SRC/build_unix`, find none, and fail before testing anything. Re-invoked with the build dir as `$1`, all four pass in all four arms:

```
POS b_diag_NEW run_handle_sizes         VERDICT handle_sizes PASS all 6 handle sizes match the recorded values
POS b_diag_NEW run_opt_null_sample      VERDICT opt_null_sample PASS __memp_fget_opt_valid(bhp=NULL) == 0 without faulting
POS b_diag_NEW run_mvcc_mtx_doublefree  VERDICT f6_mvcc_doublefree PASS failchk + env close survived a reclaimed mvcc_mtx
POS b_diag_NEW run_s5_failchk_spin      (see S5 below)
```
— and identically for `b_diag_OLD`, `b_def_NEW`, `b_def_OLD`. The table above reflects the corrected invocation. **This is a pre-existing test-harness inconsistency, unrelated to F5, and worth a separate fix** (either `run_all.sh` should pass `"$BUILD"` to every runner, or those four should read `$BUILD`).

`run_qam_extent_vrfy` passed with `TIMEOUT=900` as the brief anticipated.

**On the two expected SKIPs.** `run_mvcc_mtx_doublefree` SKIPped as predicted, with its own honest self-report: `SKIP failchk reclaimed no mutexes (0 BDB2017 lines), so the F6 scenario was never constructed -- this run proves nothing either way`. The JNI SKIP also appeared, but I found **three** environmental SKIPs, not two, and `run_all.sh` records an environmental SKIP as `pass` — so three tiers were being counted green without running. I installed the missing tools (`default-jdk-headless`, `meson`) and re-ran all three for real, both arms:

| runner | before | after (both arms) |
|--------|--------|-------------------|
| `run_u9_serializable` | SKIP no javac | **PASS** — `VERDICT u9 PASS all checks` |
| `run_meson_autoconf_parity` | SKIP no meson | **PASS** |
| `run_u8_backup_config` | SKIP no javac | still SKIP — `no JNI library in /nvme/b_diag (needs --enable-java)` |

So the genuine, irreducible SKIP set on these builds is `run_u8_backup_config` (needs `--enable-java`) and `run_mvcc_mtx_doublefree` (scenario not constructible on this host) — exactly the two the brief predicted. The other two were masking as skips only because of my host's missing tooling, and now actually run and pass.

### `test/lockmatrix/run.sh`

| arm | rc | result |
|-----|---:|--------|
| NEW | 0 | `0 check(s) failed` |
| OLD | 0 | `0 check(s) failed` |

Ran against an **ASan-instrumented libdb** (the tier auto-builds one under `build_asan_gate/`; log confirms `linking against .../build_asan_gate/libdb.a`), which is the configuration in which issue #140's out-of-bounds write inside `__lock_vec` is observable at all. No ASan report in either arm. This covers the ASan half of item 4.

### TCL `lock001`–`006` and `dead001`–`007`

The deadlock-detector tests — the ones most likely to catch a wrong mutex state.

| build | arm | pass | fail | `^FAIL` lines | rc |
|-------|-----|-----:|-----:|--------------:|---:|
| diagnostic | NEW | **13/13** | 0 | 0 | 0 |
| diagnostic | OLD | **13/13** | 0 | 0 | 0 |
| default | NEW | **13/13** | 0 | 0 | 0 |
| default | OLD | **13/13** | 0 | 0 | 0 |

All of `lock001 lock002 lock003 lock004 lock005 lock006 dead001 dead002 dead003 dead004 dead005 dead006 dead007` individually verdicted `pass` in every one of the four configurations. Each test is wrapped in its own `catch`, so a failure mid-suite produces a `fail` verdict rather than silently truncating the run — a missing verdict would have shown as `pass < 13`.

### `test/db/run_s5_failchk_spin.sh`, both `lk_partitions` arms

| build | arm | `lk_partitions=1` | `lk_partitions=10` |
|-------|-----|-------------------|--------------------|
| diagnostic | NEW | **PASS** | **PASS** |
| diagnostic | OLD | **PASS** | **PASS** |
| default | NEW | **PASS** | **PASS** |
| default | OLD | **PASS** | **PASS** |

Verbatim, all eight: `VERDICT s5_failchk_spin PASS failchk TERMINATED (ret=0) with 1 non-progress-shaped locker(s) present, lk_partitions=N`, followed by `s5_proof: 0 failure(s)`. The driver reports a hang from its own `SIGALRM` handler and emits a `VERDICT` line, so a spin would appear as an explicit FAIL verdict rather than as a timeout.

One self-correction worth recording: my first pass reported `firstverdictword=fail` for this tier in all four arms, which I nearly wrote up as a failure. It was my own grep matching the *string literal* `"VERDICT s5_failchk_spin FAIL ..."` inside a compiler warning quoting `s5_proof.c:54`. The actual `VERDICT` lines are the PASS ones above. The verdict word must be read from a line that starts with `VERDICT`, not from anywhere in the log.

### `dist/s_validate` and `dist/s_execbits`

| checker | rc | result |
|---------|---:|--------|
| `s_validate` | **0** | **22 checkers, all passed** |
| `s_execbits` | **0** | `every shebang-bearing test/*.sh is executable` |

Both are static source checkers — they read the tree and never execute the library, so the runtime arm cannot affect them. I ran them once each and say so rather than presenting a second identical run as independent evidence.

The 22: `s_chk_comma s_chk_copyright s_chk_defines s_chk_err s_chk_ext_method_calls s_chk_flags s_chk_inclconfig s_chk_include s_chk_javafiles s_chk_licence s_chk_message_id s_chk_mutex_print s_chk_newline s_chk_offt s_chk_osdir s_chk_proto s_chk_pubdef s_chk_runrecovery s_chk_spell s_chk_stats s_chk_tags s_chk_windef`, each `passed, 0`.

**A false red I had to chase down.** My first `s_validate` run gave rc=1, dying at the third checker with a flood of `FAIL: AI_PASSIVE: repmgr.h`, `FAIL: ALIGNP_INC: db_int.in`, … — 10+ apparently-unused macros. That was **my harness, not the patch**: I had shipped the tree with `git archive`, so there was no `.git`, and `s_chk_defines` scopes its input with `git ls-files` precisely to avoid phantom findings (`lock.c` comment: *"git ls-files makes the input set the repository"*). With no `.git` it silently fell back to `find`, which walked my build directories. Re-run from a real `git clone` of a bundle with F5 applied: **rc=0, 22/22**. Noted because "rc=1 from s_validate" would have been a wrong and alarming headline, and the cause was entirely in how I staged the source.

---

## 3. Multi-process check — **tested, and it passes**

The patch's comment claims two processes disagreeing about the switch is safe "because both means leave the same postcondition". I tested that claim directly rather than accepting it.

The existing `f5_mproc` **cannot** test it: it `fork()`s children, and `__lock_refresh_lock_mutex()` caches its `getenv` in a process-local `static` that `fork` copies — so every child necessarily agrees with the parent. I wrote `f5_mpmix` (`test/bench/f5_mpmix.c`), which `fork` + **`execv`**s each worker so `getenv` is read fresh in a fresh address space, and the worker's index parity decides its arm. Progress lives in a **file-backed** `MAP_SHARED` mmap, since an exec'd child does not inherit anonymous mappings. Workers attach to the parent's region with `DB_INIT_LOCK` and **without** `DB_PRIVATE`/`DB_CREATE` — a `DB_PRIVATE` child would get its own region and the test would be single-process N times over, a vacuous pass.

| configuration | processes | result |
|---------------|-----------|--------|
| **mixed switch** (the claim under test) | 4 NEW + 4 OLD, one region | **PASS** — progress 2,183,424, aborts **855,504** |
| homogeneous NEW | 8 NEW | **PASS** — progress 1,290,496, aborts 504,631 |
| homogeneous OLD | 8 OLD | **PASS** — progress 1,304,320, aborts 509,241 |

The mixed run verbatim:

```
# f5_mpmix nprocs=8 secs=25 home=/nvme/mixd
# even idx -> NEW (conditional lock); odd idx -> OLD (DB_LOCK_REFRESH_LOCK_MUTEX=1)
  workers on NEW path: 4 (progress 1070336)
  workers on OLD path: 4 (progress 1113088)
  VERDICT f5_mpmix PASS 4 NEW-path + 4 OLD-path processes shared ONE region for 25 s,
  contending on the same objects, with no worker idle for 5 consecutive seconds;
  progress=2183424 aborts=855504 (aborts>0 => cross-process block/grant/abort DID occur;
  0 transient idle second(s))
```

The judgement is on **forward progress per worker**, not on exit status: each worker publishes a counter the parent samples every second, and a worker that stops advancing for 5 consecutive seconds is a failure even though its process is still alive and `rc` would eventually be 0. A lost cross-process wake — the failure mode that matters here, since a waiter in process A blocks on the same `DB_MUTEX` a granter in process B unlocks — would present exactly as that wedge. The gate also fails as `VACUOUS` if aborts are 0 (the F5 branch never ran) or if all workers landed on one arm (not actually mixed); **855,504 cross-process aborts with a 4/4 split** means neither escape applies.

So the comment's claim is **supported by measurement** on Linux, not merely by argument. Two caveats I will not paper over: this was run on Linux only (the FreeBSD box was busy with the B3 investigation and the sweep re-confirmation), and 25 s × 8 processes is a soak, not a proof — it cannot exclude a rare interleaving, only the systematic wedge that the pre-F5 FreeBSD arm exhibits plainly.

---

## 4. Sanitizers

**ASan: done, clean.** The `lockmatrix` tier built and ran against an ASan-instrumented libdb in both arms, `0 check(s) failed`, no ASan report. (Relevant detail from a previous cycle: `__os_malloc` only returns an offset pointer under `DIAGNOSTIC`, so the allocator/deallocator mismatch class of bug is build-configuration-dependent — this tier's ASan libdb is `--enable-debug`, not `DIAGNOSTIC`.)

### TSan: done, with a control — **F5 introduces no new races**

TSan build per `ci.yml`'s tsan job (`CFLAGS=-fsanitize=thread -fno-omit-frame-pointer -g`, `LDFLAGS=-fsanitize=thread`, `TSAN_OPTIONS=...:second_deadlock_stack=1`); make rc=0, `-fsanitize=thread` confirmed in the generated Makefile.

The TCL route turned out to be unusable under TSan for harness reasons, and in a way that **fabricated a lock-manager failure** — see the trap below. So I drove the F5 branch directly, in-process, with a purpose-built TSan driver (`f5_tsan`: N threads, two conflicting `DB_LOCK_WRITE` locks in random order, `DB_LOCK_YOUNGEST` on, so victims abort and locks reach `__lock_freelock`'s `DB_LOCK_FREE` arm as `DB_LSTAT_ABORTED`). Single-process, no `tclsh`, every thread instrumented — so a race on the `DB_MUTEX` flags word, the field the fix now *reads* where the old code rewrote it via destroy+init, is precisely what TSan would catch.

**Crucially I also built a pristine, unpatched TSan library as the control**, so the warning count has something to be compared against:

| build | t=4 warnings | t=16 warnings | `__mutex_refresh` frames |
|-------|-------------:|--------------:|-------------------------:|
| **PRISTINE (no F5)** | 306 | 399 | 10 |
| **F5 NEW (fix active)** | 315 | 404 | **0** |
| **F5 OLD (`DB_LOCK_REFRESH_LOCK_MUTEX=1`)** | 278 | 416 | 9 |

20 s per cell, all runs non-vacuous (`aborts` 9,667–17,320, so the branch under test executed; `__lock_freelock` appears in 67–75 reported frames per arm).

**Reading:** the counts are statistically indistinguishable across all three builds, and the unpatched library produces them too. These races are **pre-existing and F5-independent** — the top racing frames are the same in every arm (`pthread_create`, `__lock_get_internal`, `__lock_getobj`, `__dd_build`, `__db_tas_mutex_lock_int`, `__db_tas_mutex_unlock`), i.e. libdb's TAS/hybrid mutex internals, which do their own atomics in ways TSan cannot see as synchronisation. Had I reported the patched numbers alone, "315 data races with F5" would have looked damning and been meaningless.

The one arm-dependent difference is in the fix's favour: `__mutex_refresh` appears in reported race frames **10x on pristine and 9x on the OLD arm, and 0x on the NEW arm** — the destroy+init the patch removes was itself participating in reported races, and removing it removes them.

**Verdict: TSan shows no new race attributable to F5, with a pristine control establishing the baseline.** What this does *not* do is clear libdb's pre-existing mutex-internals races, which are outside F5's scope.

### The TSan harness trap — it manufactured a false failure, and `ci.yml` may share it

Worth recording in full, because it produced a convincing wrong answer twice.

**Stage 1 — vacuous near-green.** My first TSan run exited rc=1 having run **zero** tests: `pass=0 fail=0`, `0 ThreadSanitizer warnings`. Read carelessly, especially the "0 warnings", that looks clean. Cause:

```
couldn't load file ".libs/libdb_tcl-2026.0.so": undefined symbol: __tsan_atomic32_exchange
```

`tclsh` is not TSan-linked, so the instrumented `libdb_tcl.so` cannot resolve its `__tsan_*` imports and `test.tcl` dies on line 18 before sourcing a single test.

**Stage 2 — a fabricated lock-manager failure.** `LD_PRELOAD=libtsan.so.2` fixes the load, and the suite then ran: `mut001`, `lock001`–`006`, `dead002`, `dead003` all **pass**, 0 warnings — but `dead001` **failed**:

```
VERDICT dead001 fail (FAIL: ring:2:deadlocks: expected 1, got 0)
```

That is exactly what a broken deadlock detector looks like, and `dead001` passed in all four non-TSan arms. It was my harness. `dead001` execs `db_deadlock` and N `ddscript.tcl` workers as **separate processes**, and an exported `LD_PRELOAD` reaches all of them. The libdb **utilities are already TSan-instrumented** (`nm .libs/db_deadlock` finds 9 `__tsan` symbols), so preloading libtsan into them **segfaults** them:

```
$ LD_PRELOAD=.../libtsan.so.2 ./db_deadlock -V   -> exit 139 (Segmentation fault)
$ ./db_deadlock -V                               -> libdb 2026.10.3, exit 0
```

So `db_deadlock` died instantly, `dd.out` was **empty** (verified) and both `dead001.log.*` were **empty** — no detector sweep ever happened, hence "expected 1, got 0". The harness had shot the detector and the test faithfully reported the consequence.

Scoping the preload to only the parent `tclsh` does not rescue it either: the `ddscript` children are spawned as a bare `/usr/bin/tclsh8.6` (`include.tcl: set tclsh_path /usr/bin/tclsh8.6`), so they then cannot load the library and **hang** instead (rc=124 at 900 s, twice). Multi-process TCL under a TSan libdb is simply not runnable without reworking how the children are launched — which is why I moved to the in-process driver.

**`ci.yml`'s tsan job deserves a look.** (Fixed in v2026.10.4: the step now requires `tclsh` rc=0 via `PIPESTATUS` and exactly six `PASS` lines.) Its gate is `! grep -qE "^FAIL|data race|WARNING: ThreadSanitizer" /tmp/tsan.out`, which a zero-test run satisfies trivially, and that job emits no manifest verdicts. If it is hitting stage 1, it has been passing while testing nothing — the exact "an exit status cannot distinguish passed from never ran" failure `test/harness.sh` exists to prevent. I did not confirm this against CI; I only observed that the configuration it specifies reproduces stage 1 locally.

---

## 5. B3 — classification: **shipping defect in libdb, but NOT the defect that was filed**

Filed as: `test/bench/tproc_c` panics at t≥2 with `BDB0102 "previous transaction deadlock return not resolved"` → region PANIC, with the question being harness (mishandling `DB_LOCK_DEADLOCK`, then reusing the txn) vs libdb lock manager.

**Reproduced** on a pristine clone of `e4281be0c` on the FreeBSD box, as filed:

```
$ ./tproc_pristine -i -h /nvme/b3p -S 2        # load
$ ./tproc_pristine -h /nvme/b3p -S 2 -t $T -s 10
t=2 panics=3 bdb0102=0 txn_s=192
t=4 panics=4 bdb0102=0 txn_s=NONE
t=8 panics=7 bdb0102=0 txn_s=NONE
```

But the filed **cause does not survive contact with evidence**, on five counts.

**(1) `BDB0102` never appears.** Five reps at t=4: `bdb0102=0` in every single one. The actual failure chain is always:

```
tproc: pthread suspend failed: Invalid argument
tproc: BDB0061 PANIC: Invalid argument
tproc: BDB0060 PANIC: fatal region error detected; run recovery
txn error: BDB0087 DB_RUNRECOVERY: Fatal error, run database recovery
```

`BDB0102` is `__db_txn_deadlock_err` (`src/common/db_err.c:923`), reached from `__txn_commit`/`__txn_prepare` when `TXN_DEADLOCK` is set. It is not in this failure's path at all. The tracker row's error string appears to have been mis-transcribed from a different observation.

**(2) It is not a transaction-handling bug, because it reproduces with transactions switched off.** `-X txn` clears `use_txn`, so `bb_begin`/`bb_commit`/`bb_abort` become no-ops and `DB_INIT_TXN` is never requested. With the environment *initialised and run* that way — no transactions in existence, therefore no mishandled `DB_LOCK_DEADLOCK` return and no txn reuse possible:

```
rep1 panics_or_suspendfail=6
rep2 panics_or_suspendfail=6
rep3 panics_or_suspendfail=6
```

A hypothesis about txn misuse cannot explain a failure that persists when there are no transactions. This is the decisive control.

**(3) Fixing the harness does not cure it.** I fixed all five `DEADLOCK`/`NOTGRANTED` sites in `tproc_c.c` — including `tproc_c.c:395`, which the previous agent's partial fix missed — and linked against **pristine** libdb. Still panics: `t=2 panics=3`, `t=4 panics=3`, `t=8 panics=8`.

**(4) It is FreeBSD-only and completely F5-independent.** On Linux, 0 panics in both arms (`t=2/4/8`, `panics=0 bdb0102=0`, 3,420–5,277 txn/s). On FreeBSD it fires identically with F5 active (`t=2/4/8 → 4/7/14` panics) and with `DB_LOCK_REFRESH_LOCK_MUTEX=1` (`4/7/11`). F5 neither causes nor cures it.

**(5) Probe-instrumented libdb pins the `EINVAL`.** I added instrumentation at the error site and rebuilt (note: a `cp -a` reset mtimes and make skipped the rebuild, so the first "probe" run silently measured an unprobed library — I caught that by checking `strings libdb.a | grep -c B3PROBE` returned 0, and forced the rebuild):

```
B3SITE  prep ret=22 alloc_id=8 flags=0x33
B3PROBE suspend err ret=22 mutex=33121 alloc_id=8 flags=0x31 wait=0 pid=99030 excl=1
```

`ret=22` is `EINVAL`, returned by the `pthread_mutex_lock` inside `__db_pthread_mutex_prep` (`mut_pthread.c:275`), called from `__db_hybrid_mutex_suspend` (`:563`). `flags=0x33` decodes as `DB_MUTEX_ALLOCATED | DB_MUTEX_LOCKED | DB_MUTEX_SELF_BLOCK | DB_MUTEX_SHARED`, and `alloc_id=8` is `MTX_LOCK_REGION`. A `MTX_LOCK_REGION` mutex carrying `DB_MUTEX_SHARED` is one of the locker-hash **stripe shared latches** allocated at `src/lock/lock_region.c:267` with `DB_MUTEX_SHARED` — and under `HAVE_MUTEX_HYBRID`, `__db_tas_mutex_init` (`mut_tas.c:60`) hands every mutex to `__db_pthread_mutex_init` with `flags | DB_MUTEX_SELF_BLOCK`, which is how a `SHARED` latch ends up with a self-block pthread mutex underneath it and can reach `__db_hybrid_mutex_suspend` at all. FreeBSD's libthr returning `EINVAL` from `pthread_mutex_lock` on that object is the trigger; the region panic is downstream fallout.

**Control against "any FreeBSD concurrency dies":** `scale_bench wrand`, an independent transactional workload on the same box and the same pristine library, runs clean — 0 panics in 3 reps at t=4, plus a t=4/t=8 sweep. So the failure is specific, not ambient.

### Classification

**Shipping defect, in libdb, not a test bug** — but a *different* defect from the one filed, in a different subsystem (hybrid mutex / shared-latch interaction with FreeBSD libthr, on the locker-hash stripe latches), not in the deadlock-detector or transaction paths. The filed mechanism (harness mishandling `DB_LOCK_DEADLOCK`, then reusing the txn) is **disproven**: the failure reproduces with transactions entirely absent, and survives a correct harness.

Evidence summary: reproduces on pristine `e4281be0c` ✓; `BDB0102` absent in 5/5 reps ✓; persists under `-X txn` in 3/3 reps ✓; persists with harness fixed ✓; absent on Linux in both arms ✓; identical with F5 on and off ✓; `EINVAL` pinned to a specific pthread call on a specific mutex class ✓; independent workload clean on the same box ✓.

**Separately and genuinely: `tproc_c.c:395` is a real harness defect**, and should be fixed on its own merits even though it is not B3's cause:

```c
if (get_rec(g_ord, txn, fk->a, fk->b, fk->c, &o, sizeof(o)) == 0) {
```

Every other call site in this file routes a `DB_LOCK_DEADLOCK` through `RETRY(...)` or an explicit `if (ret == DB_LOCK_DEADLOCK) { abort; goto again; }`. This one compares only against `0`, so a `DB_LOCK_DEADLOCK` here is discarded and the transaction — now flagged `TXN_DEADLOCK` by `__db_lget` (`db_meta.c:1314`) — is used for the following `put_rec` and then committed. That *is* the code path that would produce the filed `BDB0102` text, via `__txn_commit`'s check at `txn.c:818`. It is latent on both platforms in my runs (my probe at that site recorded 0 hits in the runs I instrumented), which is consistent with it being a real but rarely-taken path rather than B3's mechanism. **Not fixed, per instructions — classified only.**

---

## 6. What I did NOT verify

1. **TSan via the TCL suite.** TSan coverage came from the in-process driver plus a pristine control, not from `dead001`–`007` under TSan — that path is not runnable (see the trap above: the utilities segfault under a blanket preload, the `ddscript` children hang under a scoped one). The TCL tests *were* run under TSan up to the point the harness broke: `mut001`, `lock001`–`006`, `dead002`, `dead003` passed with 0 warnings, then `dead004` hit the 7000 s wall-clock limit and is recorded as `VERDICT=HANG rc=124`, not as a pass. So **`dead004`–`dead007` are unverified under TSan.**
2. **The pre-existing TSan races in libdb's mutex internals are not explained.** I established they are F5-independent (same rate on an unpatched build) and stopped. Whether they are genuine or artefacts of TSan not understanding libdb's hand-rolled TAS atomics is open, and outside F5's scope.
3. **Multi-process on FreeBSD.** The mixed-switch test passed on Linux only. FreeBSD is where the destroy-of-a-pshared-object cost lives, so a FreeBSD mixed-arm run would be the stronger evidence for the cross-process claim.
4. **The multi-process result is a soak, not a proof.** 25 s × 8 processes with 855k cross-process aborts excludes a systematic lost wake; it cannot exclude a rare interleaving.
5. **The full TCL suite (`run_std`).** Only `lock001`–`006` and `dead001`–`007` (plus `mut001` in the TSan list) were run, per the brief. The remaining several hundred TCL tests are unexercised against F5.
6. **`run_u8_backup_config`** remains SKIP on both builds — needs `--enable-java`, which I did not build.
7. **`run_mvcc_mtx_doublefree`** self-reports SKIP (`0 BDB2017 lines` — the F6 scenario was not constructed), so it proves nothing either way about F5 here. It was *also* re-run via the positional path and returned a PASS verdict line, but its own SKIP disclaimer governs.
8. **No non-pthread mutex backend was tested.** Both builds are `HAVE_MUTEX_HYBRID` + `HAVE_MUTEX_PTHREADS`. The `!HAVE_MUTEX_SUPPORT` configuration (where `MUTEX_IS_OWNED` is constant `0`) was verified **by reading the macros**, not by building it. A TAS-only or `HAVE_MUTEX_FCNTL` build is unverified.
9. **No `--enable-java`/JNI, no Windows, no 32-bit, no big-endian** build of any kind.
10. **Replication paths** (`MTX_REP_*`) untouched by any test here.
11. **B3's own root cause is classified, not diagnosed to a fix.** I established what it is *not* (not txn handling, not the harness, not F5, not Linux) and pinned the failing call and mutex class. I did not determine *why* FreeBSD's libthr returns `EINVAL` on that object — whether the pthread mutex is uninitialised, destroyed, or mismatched in type — which is what a fix would need.
12. **`tproc_c`'s t≥2 numbers remain unusable on FreeBSD** in both arms, so `tproc_c` cannot serve as an F5 benchmark there regardless of harness state.
13. **The four positionally-invoked runners** are a pre-existing `run_all.sh` inconsistency I worked around, not fixed.

---

## 7. Bottom line

- **Linux: no regression, no measurable win.** cv ≤ 2.45% across 84 cells; every thread count within ±1.7% and not statistically significant. The branch provably executed (1,245,994 times) and the mutex was already held on **every** execution, so the removed `destroy → init → lock` was a round trip to the state it started in — just a cheap one on glibc.
- **FreeBSD: the banked ~14.7× at t=32 reproduces independently** (14.60× vs 14.68×, worst cv 2.33%).
- **Correctness: clean in all four configurations** ({diagnostic, default} × {new path, old path}) across `run_all.sh` (20 pass / 0 fail), `lock001`–`006` + `dead001`–`007` (13/13 ×4), `lockmatrix` under ASan (both arms), S5 failchk both `lk_partitions` arms (8/8 PASS), `s_validate` (22/22, rc=0) and `s_execbits` (rc=0).
- **Multi-process: the patch comment's "both means leave the same postcondition" claim is measured and holds** — 4 NEW + 4 OLD processes on one region, 855,504 cross-process aborts, no wedge. Linux only.
- **TSan: no new race attributable to F5**, established against a pristine control — 306/399 warnings unpatched vs 315/404 with the fix, indistinguishable and with the same top frames — and `__mutex_refresh` drops out of reported race frames entirely with the fix active (10 pristine / 9 old-path / **0** new-path).
- **B3 is a real libdb defect but was filed with the wrong cause and wrong error string**; the harness hypothesis is disproven by a run with no transactions at all. `tproc_c.c:395` is a separate, genuine, still-unfixed harness bug.
- **Two harness defects found along the way**, both capable of producing a wrong verdict: `run_all.sh` not passing the build dir to four of its runners (false red, 4 tests × 4 arms), and the TSan/`LD_PRELOAD` interaction that segfaults `db_deadlock` and manufactures a `dead001` "deadlocks: expected 1, got 0" — a false red that reads exactly like a lock-manager regression. `ci.yml`'s tsan gate may also be satisfiable by a zero-test run.

**On the patch itself I found nothing against it.** Every correctness gate passes in both arms and both build types; the multi-process claim in its own comment holds under test; TSan is clean relative to a pristine control; the FreeBSD win reproduces; and Linux is flat with the branch provably executing 1.2 M times with the mutex already held on every one of them.
