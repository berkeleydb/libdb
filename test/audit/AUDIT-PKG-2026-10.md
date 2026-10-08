<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.

SPDX-License-Identifier: Sleepycat

See the file LICENSE for redistribution information.
-->

# libdb SHIPPING PACKAGE audit — baseline `6710649f0` vs `origin/master` (`f677e28ed`)

> **Preserved from `.agent/`, which is gitignored.** This audit was produced
> as an agent working note and existed only on one developer's machine. Its
> findings drove real decisions -- the A1 handle-size gate, the packaging
> questions, and the correction of several claims the project had been
> repeating -- so the evidence belongs in the repository rather than in an
> untracked directory. Read-only inspection; it changes nothing by itself.


**Scope:** what `make install` / a release tarball actually delivers, not what the source tree contains.

| | commit | date | subject |
|---|---|---|---|
| baseline | `6710649f09b169e46549673053ab64178a0db1e1` | 2026-04-21 | Update autoconf, fix atomics and tests. |
| current | `f677e28ed40cd6c75944237505eb9c4d1fcb8885` | — | Merge PR #209 (fix/u8-u9-backup-csharp) |

783 commits separate them (`git rev-list --count 6710649f0..origin/master`).

## Method — commands that produced the key numbers

Everything was measured, not read off. Both points were exported with
`git archive`, configured, built and **actually installed** into separate
prefixes inside `/tmp`; the repo was never written to.

```
git -C /home/gburd/ws/libdb archive <rev> | tar -x -C /tmp/pkgaudit/src_<v>
nix develop /home/gburd/ws/libdb --command bash -c \
  'cd /tmp/pkgaudit/b_<v> && ../src_<v>/dist/configure --prefix=/tmp/pkgaudit/i_<v> \
   && make -j8 && make install'
find i_<v> -not -path './docs/*'          # installed artifact lists
readelf -d i_<v>/lib/libdb-2026.0.so      # soname + NEEDED
nm -D --defined-only ...                  # exported-symbol sets
cc -I i_<v>/include probe.c && ./probe    # struct sizes + db_version()
diff <(sed -n '/^Optional Features:/,/^Report bugs/p' conf_base.sh) <(... conf_master.sh)
cd src_now/docs_src && python3 build.py --no-pdf   # docs render
meson setup/ninja/meson install            # the second build system
```

Two caveats recorded up front:

- **The working tree is dirty** (182 modified files per `git status --porcelain`).
  All source numbers come from `git show <rev>:<path>` or `git archive`. The six
  non-source files quoted verbatim (`meson.build`, `meson_options.txt`,
  `VERSIONING.md`, `LICENSES/README.md`, `dist/meson/db_subs.json`, `README.md`)
  were each confirmed identical to `origin/master` with
  `git diff --quiet origin/master -- <file>`.
- **`abidiff` (libabigail) is not in the nix dev shell** (`which abidiff` →
  not found). ABI claims below are from `nm`, `readelf` and compiled
  `sizeof()` probes. A libabigail-level verdict is **unverified here**; CI runs
  one in `.github/workflows/cocci.yml`.

---

## 1. INSTALLED ARTIFACTS

### 1a. Default `make install` (no flags) — complete measured tree

| path | baseline | now |
|---|---|---|
| `lib/libdb-2026.0.so` | ✅ 1,853,992 B | ✅ 13,129,392 B |
| `lib/libdb-2026.so` → `libdb-2026.0.so` | ✅ | ✅ |
| `lib/libdb.so` → `libdb-2026.0.so` | ✅ | ✅ |
| `lib/libdb-2026.0.a` | ✅ 2,892,678 B | ✅ 29,389,272 B |
| `lib/libdb.a` | ✅ | ✅ |
| `lib/libdb-2026.0.la` | ✅ | ✅ |
| `include/db.h` | ✅ 3,111 lines | ✅ 3,169 lines |
| `include/db_cxx.h` | ✅ 1,523 lines | ✅ 1,523 lines (byte-identical) |
| `bin/` utilities | ✅ 14 | ✅ 14 |
| `docs/` | ✅ 5,245 files, 93 MB | ⚠️ **0 files** (see §5) |

The filename, symlink chain and layout are **unchanged**. The size growth
(7.1× for the `.so`) is partly real code and partly build flags: baseline
configures `-O3` with no `-g`, now configures `-g -O2 -std=gnu99`
(`grep '^CFLAGS' b_*/Makefile`). `file` reports baseline "not stripped" and
now "with debug_info, not stripped" — **a packager that was not stripping
before now ships debug info.**

### 1b. Utilities — diff of the installed list

`diff <(ls i_base/bin) <(ls i_now/bin)` → **empty. All 14 identical, none added, none dropped.**

```
db_archive  db_checkpoint  db_deadlock  db_dump    db_hotbackup
db_load     db_log_verify  db_printlog  db_recover db_replicate
db_stat     db_tuner       db_upgrade   db_verify
```

`UTIL_PROGS` in `dist/Makefile.in` is textually identical at both points.
The four conditional utilities (`db_dump185`, `dbsql`, `sqlite3`,
`db_sql_codegen`, all via `@ADDITIONAL_PROGS@`) are absent from both default
installs, as expected.

`db_dump185` is the one *regression-shaped* finding, but it is **not a
regression**: `--enable-dump185` fails to build at **both** points with the
same error, `util/db_dump185.c:26:10: fatal error: 'db.h' file not found`.
Pre-existing, unchanged.

### 1c. Headers

`INCDOT = db.h db_cxx.h @ADDITIONAL_INCS@` — identical line in both
`dist/Makefile.in`. The `@ADDITIONAL_INCS@` contributors are the same five
`configure.ac` sites at both points (`db_185.h`, `dbsql.h`,
`lang/sql/generated/sqlite3.h`, `dbstl_common.h`, the STL headers), so the
installed header set is driven entirely by flags and is **unchanged**.

`db.h` grew 58 lines. Measured delta: **9 added `#define DB_*`, 0 removed, 0
value reassigned** (`grep -oE '#define[[:space:]]+DB_[A-Za-z0-9_]+' | comm`):

```
DB_CALVER  DB_CURSOR_NPART  DB_ENV_MPOOL_AIO  DB_ENV_NOIO
DB_ENV_TXN_SERIALIZABLE  DB_MPOOL_AIO  DB_SNAPSHOT_CONFLICT
DB_SNAPSHOT_UNSAFE  DB_TXN_SERIALIZABLE
```

`DB_SNAPSHOT_CONFLICT (-30968)` / `DB_SNAPSHOT_UNSAFE (-30967)` are new error
codes; no pre-existing negative error code changed value.

### 1d. Language bindings

| binding | baseline buildable | now buildable | note |
|---|---|---|---|
| C | ✅ default | ✅ default | — |
| C++ (`--enable-cxx`) | ✅ built + installed | ✅ built + installed | installed trees byte-for-byte equal in name set |
| STL (`--enable-stl`) | ❌ fails | ❌ fails | identical cause both points: `"No appropriate TLS modifier defined."` / `"...requires thread local storage. None is configured."` — **pre-existing, not a fork regression** |
| Java (`--enable-java`) | ❌ fails | ❌ fails | identical cause both points: bundled `com.sleepycat.asm` ClassEnhancer crashes under JDK 21 at `make db.jar`. **Pre-existing.** |
| Tcl (`--enable-tcl`) | configures | configures | not separately install-verified |
| **C# (`lang/csharp`)** | **present in tree** | **REMOVED** | see below |

**C# removal — verified and quantified.** `git log -1 a77b2119d` →
`a77b2119d46a6bee6fd46e873633e1f904f77a98`, 2026-08-03, *"chore(repo): remove
C# binding, dead README, and public ROADMAP"*. `git show --numstat`:

| path | files | text lines deleted | binary files |
|---|---|---|---|
| `lang/csharp/` | 125 | 59,864 | 0 |
| `docs/csharp/` | 2,457 | 87,677 | 136 |
| `examples/csharp/` | 37 | 5,187 | 0 |
| build glue (`dist/s_csharp*`, `dist/win_projects/*csharp*`, `dist/winmsi/fixupCsharp.xq`) | 10 | 919 | 0 |
| other (`README`, `README.md`, `ROADMAP.md`, `.gitignore`, `dist/Makefile.in`, `dist/s_all`) | 6 | 254 | — |
| **commit total** | **2,634** | **153,906** | 136 |

`git ls-tree origin/master:lang/` confirms `csharp` gone; `cxx db185 dbm
hsearch java perl php_db4 sql tcl` remain. There was never a
`--enable-csharp` flag (`grep -i csharp` on both `configure.ac` → 0 hits), so
**no packager-visible configure option changed**, and C# was never in a
`make install`. Impact is source-tarball-only.

> **Live contradiction (pre-existing, still open).**
> `git show origin/master:dist/pubdef.in` still has **311 rows claiming a `C`
> (C#) column**, and `dist/validate/s_chk_pubdef` still reads that field
> (`grep -c iscsharp` → 4) without validating it. The binding is gone; its
> metadata is not. This is a source/validation inconsistency, not a shipped
> artifact difference.

---

## 2. VERSION IDENTITY

| field | baseline | now | changed? |
|---|---|---|---|
| `DB_VERSION_FAMILY` | 11 | 11 | no |
| `DB_VERSION_RELEASE` | 2 | 2 | no |
| `DB_VERSION_MAJOR` | **2026** | **2026** | **no** |
| `DB_VERSION_MINOR` | **0** | **0** | **no** |
| `DB_VERSION_PATCH` | 1 | 9 | yes |
| `DB_CALVER` | `2026.04` | `2026.10.1` | yes |
| `DB_VERSION_STRING` | `libdb 2026.04 (April 22, 2026)` | `libdb 2026.10.1 (October 6, 2026)` | yes |
| `DB_RELEASE_DATE` | April 22, 2026 | October 6, 2026 | yes |
| `PACKAGE_VERSION` (configure) | `5.3.29` | `2026.0.9` | **yes** |
| `PACKAGE_STRING` | `Berkeley DB 5.3.29` | `Berkeley DB 2026.0.9` | yes |
| `DB_VERSION_UNIQUE_NAME` | `_2026000` (unset by default) | `_2026000` (unset by default) | no |
| **soname** (`readelf -d`) | **`libdb-2026.0.so`** | **`libdb-2026.0.so`** | **no** |
| `LIBVERSION` (`Makefile.in:60`) | `@DB_VERSION_MAJOR@.@DB_VERSION_MINOR@` | same | no |
| `db_version()` at runtime | `2026, 0, 1` | `2026, 0, 9` | patch only |

**ABI triplet: did NOT change at MAJOR/MINOR.** Only `PATCH` moved 1 → 9.
**Soname: did NOT change** — `libdb-2026.0.so` at both points, measured on the
installed `.so`.

**The CalVer move predates the baseline.** `git show e66a5e66d:dist/RELEASE`
(the baseline's parent) has `MAJOR=5 MINOR=3 PATCH=28`. `6710649f0` itself is
the commit that set `2026/0/1` + `DB_CALVER="2026.04"`
(`git log -S'DB_VERSION_MAJOR=2026' --reverse -- dist/RELEASE` → `6710649f0`,
the only hit). So the audited baseline **already shipped
`libdb-2026.0.so`**; the fork's `5.3.x` → CalVer soname break happened *at or
before* the baseline, not in the window under audit.

What *did* change in `dist/RELEASE` is 21 lines of added commentary plus the
`DB_CALVER` / `PATCH` / date values. One of those comment blocks is factually
wrong about the shipped artifact — it claims the frozen level drives "the
shared-object soname (**libdb-5.3.so** via LIBVERSION)" and
"`DB_VERSION_UNIQUE_NAME` symbol mangling (**_5003**)", while the measured
values are `libdb-2026.0.so` and `_2026000`. `VERSIONING.md` carries an
explicit dated correction for exactly this ("Correction, 2026-09-18 … had
already set the triplet to 2026.0.9"), so the stale text survives only in the
`dist/RELEASE` comment.

### ⚠️ ABI/compat finding the soname does not express

The soname is unchanged, yet the two libraries are **mutually incompatible at
runtime for shared environments**, and the C ABI did move. Measured:

**(a) `sizeof(DB)` changed: 1456 → 1744 bytes** (compiled probe against each
installed `db.h`). 22 other public structs were probed
(`DB_ENV DBC DBT DB_TXN DB_LOCK DB_LSN DB_SEQUENCE DB_MPOOLFILE DB_LOGC` +
all the `*_STAT` types + `DB_COMPACT DB_PREPLIST`) and **every one is
unchanged**. Cause, from the `db.h` diff: `struct __db`'s single
`free_queue`/`active_queue` pair became `struct __cq_part
cq_parts[DB_CURSOR_NPART]` with `DB_CURSOR_NPART = 8`, and `struct __dbc`
gained `u_int32_t part`.

**(b) Existing environments cannot be attached across the two builds.**
Run with controls:

| experiment | result |
|---|---|
| base creates env, base re-attaches (control) | `rc=0` |
| now creates env, now re-attaches (control) | `rc=0` |
| base creates env → **now attaches** | `BDB1539 Build signature doesn't match environment`, `rc=-30969 DB_VERSION_MISMATCH` |
| now creates env → **base attaches** (reverse) | same failure |

The `majver`/`minver` gate at `src/env/env_region.c:253` passes (both 2026/0);
the failure is the *next* check, `renv->signature != signature` at line 265,
fed by `__env_struct_sig()` over 145 structures. This is not reported as a
regression — it is the documented consequence of changing shared-region
structs, and `dist/RELEASE`'s own note says the triplet is what guards it. The
packaging consequence is that **soname equality is not a safe upgrade signal
for this pair**; co-installing is impossible and any live environment must be
recreated.

**(c) Exported symbols** (`nm -D --defined-only`, T/B/D/R):

| | baseline | now |
|---|---|---|
| exported | 1,795 | 1,833 |

- **3 removed**: `atomic_compare_exchange`, `__atomic_dec`, `__atomic_inc`
- **41 added**, of which exactly **one is public API**: `db_get_multiple`
  (declared `db.h:3106`). The other 40 are `__`-prefixed internals
  (`__os_aio_*`, `__memp_*`, `__lock_si*`, `__bam_opt_*`, `__os_csprng`, …).

The three removals are the only candidate for breaking an existing dynamic
link.

---

## 3. TARBALL CONTENTS — **the mechanism is broken at both points, and this is the headline finding**

### What exists

- `dist/s_dist`, `dist/s_tar`, `dist/s_release`: **do not exist at either
  point** (`git ls-tree <rev>:dist/ | grep -E 's_dist|s_tar|s_release'` → empty
  both).
- `make dist` / `make rpm` / `make rpmbuild`: present at both points as a
  **deliberate stub**, identical text:
  ```make
  dist rpm rpmbuild:
          @echo "make: $@ target not available" && exit 1
  ```
- `dist/buildpkg` (244 lines): the only tarball assembler, at both points.

### `dist/buildpkg` cannot run in this repository

It is Oracle-era Mercurial tooling. Measured facts about the
`origin/master` copy:

| requirement | status |
|---|---|
| `hg archive $R`, `hg diff`, `hg pull -u`, `hg tag`, `hg up -r` | repo is **git** |
| `../../docs_books-5.3` sibling hg repo | not present; its absence `die`s the script |
| `../../db_addons-5.3` sibling hg repo | not present; explicit `exit 1` at line ~65 |
| `find . -name '.hg*' \| xargs rm -f` | no-op in git |
| `sh s_docs db-$VERSION $DOCS` | `dist/s_docs` is **unchanged from baseline** and still requires an hg `docs_books` repo |
| `-csharp_doc_src` / `-csharp_doc_url` flags, `rm -rf $R/docs/csharp` | the C# docs they fetch were deleted in `a77b2119d` |
| output names | `db-$VERSION.tar.gz`, `db-$VERSION.zip`, plus `.NC` non-crypto variants — i.e. `db-2026.0.9.tar.gz`, *not* a CalVer name |

**The entire diff to `buildpkg` across 783 commits is one line**
(`git diff 6710649f0 origin/master -- dist/buildpkg`): `test/vxworks` dropped
from the "source directories we don't distribute" list, because the directory
no longer exists. The C#-doc plumbing was left in place after the binding was
deleted.

So: **there is no working release-tarball mechanism at either point, and it
decayed further in the window** (it now references two deleted trees). This is
not a before/after regression in artifact *content* — it is the finding that
*the content is undefined*, because nothing assembles it.

### What a release actually is now, measured

`grep -rn 'upload-artifact|tar -c|release' .github/workflows/` across 15
workflow files: **no workflow builds a source tarball or a release archive.**
The only `tar czf` is in `docs.yml:246`, producing
`libdb-man-$full.tar.gz` (man3 pages) pushed to `gh-pages` by a manual
`workflow_dispatch` job. `VERSIONING.md` §"Cutting a release" documents the
process as: bump `DB_CALVER`, regenerate headers, "tag `vYYYY.0M[.MICRO]` and
create the GitHub release with that title" — i.e. **the release artifact is
GitHub's auto-generated git snapshot**, governed by `.gitattributes`
(new in this window; no `export-ignore` lines, so nothing is excluded).

Consequence for a packager: the tarball is now a plain source snapshot that
**omits the 5,245 pre-built HTML doc files** the baseline tarball would have
carried, and **includes the full test infrastructure** the old `buildpkg`
pruned (`test/perf`, `test/repmgr`, `test/server`, `test/stl`, `test/upgrade`,
`test/erlang`, `test/scr036`, `test/tcl/TODO`).

### Tree size and shape, for reference

| | baseline | now |
|---|---|---|
| tracked files (`git ls-tree -r \| wc -l`) | 9,231 | 5,921 |
| `git archive` extracted size | 158 MB | 85 MB |
| `dist/srcfiles.in` entries | 328 | 302 |

`srcfiles.in` delta: **35 removed** (30 × `build_vxworks/*`, 5 ×
`src/os_vxworks/*`), **9 added** (`src/log/log_handoff_trace.c`,
`src/mutex/mut_order.c`, `src/os/os_aio{,_iocp,_kqueue,_pool,_posix,_uring}.c`,
`src/os/os_csprng.c`).

Removed top-level platform trees: `build_vxworks` (79 files, commit
`022a557f9`), `build_wince` (11 files, commit `a6e91d033`), `docs` (5,245
files, commits `88cbdbe33` and `b686cf3be`).

New top-level entries a packager will see: `.github/`, `LICENSES/`,
`README.md`, `VERSIONING.md`, `docs_src/`, `flake.nix`, `flake.lock`,
`meson.build`, `meson_options.txt`, `rfc/`, `.editorconfig`,
`.gitattributes`, `.git-blame-ignore-revs`. Gone: `README` (plain),
`docs/`, `build_vxworks/`, `build_wince/`.

### Second build system — new, and it installs something different

`meson.build` + `meson_options.txt` are new in this window. Measured
`meson setup && ninja && meson install` of `origin/master`:

```
mi_now/
  lib/libdb.so        # soname: libdb.so      (autoconf: libdb-2026.0.so)
```

**That is the whole install.** No headers, no utilities, no static library, no
versioned soname, no `.la`, no docs. The library is correct otherwise
(`strings` → `libdb 2026.10.1 (October 6, 2026)`, 12,996,192 B). A packager who
follows the README's lead (`meson setup build && ninja -C build`) gets an
unusable install; the autoconf path is the only complete one. The meson
`project()` declares `license: 'AGPL-3.0-or-later OR Sleepycat'`.

---

## 4. BUILD OPTIONS a packager sees

Measured by diffing the `--help` text of the committed `dist/configure` at each
point (`sed -n '/^Optional Features:/,/^Report bugs/p'`), which is the ground
truth a packager reads.

| | baseline | now |
|---|---|---|
| total options in `--help` | **63** | **66** |
| `--enable-*` / `--disable-*` | 51 | 54 |
| `--with-*` / `--without-*` | 12 | 12 |
| `AC_ARG_ENABLE`/`AC_ARG_WITH` declared in `configure.ac` + `aclocal/options.m4` | 51 | 54 |

**Removed: 0.** **Changed default: 0.** **Added: 3** — all `--enable-*`,
all defaulting to `no` (`db_cv_dst="no"`, `db_cv_faultinject="no"`,
`db_cv_handoff_trace="no"`):

| new option | help text |
|---|---|
| `--enable-dst` | Build with Deterministic Simulation Testing (DST) fault-injection hooks. |
| `--enable-faultinject` | Build with SQLite-style malloc-failure injection hooks at the `__os_*` allocation seam. |
| `--enable-handoff-trace` | Instrument the group-commit handoff in `__log_flush_int` … *"Changes the log region layout and signature; for measurement only, never for production."* |

The complete `--help` diff is 9 added lines and nothing else. **No existing
option was removed, renamed, or had its default flipped** — every baseline
`configure` line a distro has in its spec/rules file still works.

Meson contributes exactly one option, `-Dhegel=disabled` (default `disabled`).

### Undeclared new dependency — the one thing a packager *must* react to

| | baseline | now |
|---|---|---|
| `readelf -d … \| grep NEEDED` | `libpthread.so.0`, `libc.so.6` | **`liburing.so.2`**, `libpthread.so.0`, `libc.so.6` |
| `.la` `dependency_libs` | `-lpthread` | **`-luring**` `-lpthread` |

`liburing` is auto-detected with **no configure flag to control it**
(`grep -ci uring` on the `--help` option list → 0). From
`configure.ac:220-230`: a bare `AC_CHECK_HEADER(liburing.h)` +
`AC_CHECK_LIB(uring, io_uring_queue_init)` appends `-luring` unconditionally
on success. A build host with `liburing-dev` installed silently produces a
library with a hard `DT_NEEDED` on `liburing.so.2`; a build host without it
silently does not. **There is no `--without-uring` escape hatch, so the
resulting package's dependency set is a property of the build machine, not of
the build invocation.**

New `HAVE_*` defines in the generated `db_config.h` (10 added, 0 removed):
`HAVE_IO_URING`, `HAVE_AIO_POSIX`, `HAVE_AIO_THREADPOOL`, `HAVE_GETRANDOM`,
`HAVE_SYS_RANDOM_H`, `HAVE_ARC4RANDOM_BUF`, `HAVE_ATOMIC_64BIT`,
`HAVE_ATOMIC_BUILTINS`, `HAVE_ATOMIC_GCC_BUILTIN`, `HAVE_ATOMIC_SUPPORT`.

---

## 5. DOCUMENTATION shipped

| | baseline | now |
|---|---|---|
| source of truth | `docs/` — 5,245 committed files, pre-rendered Oracle output | `docs_src/` — 1,762 files, **1,697 Markdown** |
| format mix | 2,709 `.html`, 2,300 `.htm`, 106 `.gif`, 27 `.bin`, 26 `.css`, 24 `.pdf`, 23 `.jpg`, 9 `.js`, 1 `.chm`, 2 `.cs` | 1,697 `.md`, 22 `.toml`, 21 `.jpg`, 10 `.py`, 4 `.gif`, 3 `.tmpl`, 2 `.txt`, 2 `.css` |
| committed pre-rendered output | **yes** | **no** |
| **installed by a plain `make install`** | **5,245 files, 93 MB** | **0 files** |
| installed after `make docs` first | n/a | **2,009 files, 28 MB** |

`docs/` was deleted in two commits: `88cbdbe339` (2026-08-03, *"remove old
docs/ archive; add doc/bench/compdb build targets"*) and `b686cf3be4`
(2026-09-21, *"retire docs/, move design notes to rfc/"*).

`install_docs` changed from an unconditional copy of a 14-entry `DOCLIST` to a
guarded copy of `$(topdir)/docs-build/html/`. With no `make docs` run, the
install prints and skips:

```
No docs-build/html found; run 'make docs' first to install docs (skipping).
```

**So out of the box, `make install` now ships zero documentation.** That is a
measured behaviour change, not a configuration subtlety.

Rendering works. `cd docs_src && python3 build.py --no-pdf` succeeded in the
nix shell and produced:

| output | files |
|---|---|
| `docs-build/html/` | 2,009 (1,963 `.html` + assets) |
| `docs-build/man/man3/` | **788 `.3` man pages** |
| `docs-build/site/` | 3 |
| total | 2,800 files, 32 MB |

Then `make install` installed 2,009 files / 28 MB into `$prefix/docs`.

**The 788 man pages are built and then not installed.** `install_docs` copies
only `docs-build/html/.`; `find i_now_docs -name '*.3'` → 0, and there is no
`man*` directory anywhere in the prefix. For a distro this is the most
actionable docs finding: `libdb` now generates proper `man3` pages — the thing
a `-doc`/`-man` subpackage most wants — and the install target drops them on
the floor. They are only reachable via the manual `docs.yml` `publish` job,
which tars them to `gh-pages` as `libdb-man-$full.tar.gz`.

Toolchain required for docs, now vs then: `python3` + `pandoc` (and
`weasyprint` for PDF, `mandoc` for the lint gate). The baseline required
nothing — the HTML was committed. `make docs` and `make docs-check` both
degrade to a `SKIP` message rather than failing when the toolchain is absent,
so this does not break a docs-less build.

---

## 6. LICENSE / legal files

| file | baseline | now | measured verdict |
|---|---|---|---|
| `LICENSE` | 130 lines | 130 lines | **byte-identical.** `git diff 6710649f0 origin/master -- LICENSE` → empty. Still the Sleepycat/Oracle text + Regents of UC BSD + Harvard notices, including the `berkeleydb-info_us@oracle.com` contact line. |
| `README` (plain) | 5 lines, *"Berkeley DB 11g Release 2, library version 11.2.5.3.28"* | **deleted** (`080217acd`) | — |
| `README.md` | **absent** | 273 lines (added `f15717021`) | new |
| `LICENSES/` | **absent** | 7 files | new |
| `COPYRIGHT` / `COPYING` | absent | absent | neither point ships one |

`LICENSES/` contents: `ASM.txt`, `BSD.txt`, `CDDL.txt`, `HARVARD.txt`,
`SPL.txt`, `license_db.html`, `README.md`. Its README says these were
*"Relocated from the old `docs/legal/` and `docs/license/` when the scraped
Oracle DocBook archive was removed"*.

### The AGPL question — verified, and the answer is mixed

`git grep -l -i AGPL origin/master` → **3 files only**:
`README.md`, `meson.build`, `test/bench/TIDESDB-COMPARISON.md`.

| claim | evidence |
|---|---|
| `meson.build:16` declares `license: 'AGPL-3.0-or-later OR Sleepycat'` | ✅ confirmed verbatim |
| 12 source files carry `SPDX-License-Identifier: AGPL-3.0-or-later OR Sleepycat-OSL` | ⚠️ **only in the dirty working tree.** `git grep -l SPDX-License-Identifier origin/master` → **0 files.** `git show origin/master:src/env/env_sig.c` has no SPDX line. The dual-license SPDX headers are **uncommitted work in progress**, not shipped. |
| `LICENSE` states dual AGPL/Sleepycat terms | ❌ **No.** `LICENSE` is unchanged from Oracle's and never mentions AGPL or Affero. |
| `LICENSES/` contains an AGPL text | ❌ **No.** `grep -ci affero` over all 7 files → 0 each. There is no AGPL-3.0 licence text anywhere in the shipped tree. |
| `README.md` mentions AGPL | ✅ but **in the opposite sense**: lines 78-82 explain that Oracle's 6.x+ releases are *"licensed by Oracle under the AGPLv3, which is **incompatible** with redistributing them under this project's Sleepycat-license terms"*. The README's own §License (267-273) says *"Berkeley DB is distributed under **its original license**; see `LICENSE`"* — i.e. Sleepycat only. |

**Measured conclusion.** The "AGPL-3.0-or-later OR Sleepycat" dual licence is
asserted in **exactly one shipped file**, `meson.build`, where it is build
metadata rather than a grant. Every legal file that actually ships
(`LICENSE`, all of `LICENSES/`, the README's own License section) says
**Sleepycat/Oracle, unchanged**, and no AGPL-3.0 text is distributed. The
SPDX headers that would make the dual licence real are **uncommitted**. A
packager reading `meson.build` and a packager reading `LICENSE` get different
answers, and the one that ships to users is `LICENSE`. **This needs a
maintainer decision before it reaches a distro's legal review** — I am
reporting the discrepancy, not resolving it.

---

## 7. A PACKAGER'S DIFF SUMMARY

What a distro packaging `libdb` must change between these two points.

| # | area | baseline | now | action required | severity |
|---|---|---|---|---|---|
| 1 | soname | `libdb-2026.0.so` | `libdb-2026.0.so` | **none** — unchanged | — |
| 2 | ABI triplet | `2026.0.1` | `2026.0.9` | none (MAJOR/MINOR frozen) | — |
| 3 | release version | `2026.04` | `2026.10.1` | bump `Version:` to the CalVer; note `PACKAGE_VERSION` is `2026.0.9`, a *third* string | low |
| 4 | **`liburing` dep** | none | hard `DT_NEEDED liburing.so.2` | **add `liburing` BuildRequires + Requires**, or build in a `liburing`-free root; **there is no `--without-uring`** | **high** |
| 5 | **docs in `make install`** | 5,245 files / 93 MB automatic | **0 files** unless `make docs` runs first | insert `make docs` before `make install`, and add `python3` + `pandoc` (+ `weasyprint` for PDF) BuildRequires | **high** |
| 6 | **`man3` pages** | none | 788 generated, **0 installed** | install `docs-build/man/man3/*.3` by hand into `%{_mandir}/man3`; `install_docs` will not do it | **high** |
| 7 | **env compatibility** | — | **cross-attach fails** both directions (`BDB1539` / `DB_VERSION_MISMATCH`) despite equal soname | treat as a non-co-installable, non-in-place upgrade; document that live environments must be recreated; do not trust soname equality | **high** |
| 8 | **`sizeof(DB)`** | 1456 | 1744 | rebuild every reverse-dependency that allocates or embeds a `DB`; 22 other public structs unchanged | **high** |
| 9 | exported symbols | 1,795 | 1,833 | 3 removed (`atomic_compare_exchange`, `__atomic_dec`, `__atomic_inc`); 41 added, 1 public (`db_get_multiple`) | medium |
| 10 | **release tarball** | `dist/buildpkg` (hg, already non-functional) | same script, now also referencing 2 deleted trees | **stop expecting an upstream tarball**; package from a git tag; `make dist` exits 1 by design | **high** |
| 11 | tarball contents | would have excluded `test/{perf,repmgr,server,stl,upgrade,erlang}` and shipped pre-built docs | git snapshot: ships all test infra, no pre-built docs | expect a larger, docs-free source archive; prune in `%prep` if desired | medium |
| 12 | C# subpackage | `lang/csharp` present (125 files) | **removed** (2,634 files / 153,906 lines in `a77b2119d`) | drop any C#/ADO.NET subpackage; **no configure flag changes** (none ever existed) | medium (none if unpackaged) |
| 13 | platform trees | `build_vxworks` (79), `build_wince` (11) | removed | drop if referenced | low |
| 14 | configure options | 63 | 66 | **none** — 0 removed, 0 defaults changed, 3 added (all default `no`, all dev-only) | — |
| 15 | utilities | 14 | 14 | **none** — identical list | — |
| 16 | headers | `db.h`, `db_cxx.h` | same two | none; `db.h` +9 `#define`s, `db_cxx.h` byte-identical | — |
| 17 | debug info / size | `-O3`, `.so` 1.85 MB | `-g -O2`, `.so` 13.1 MB | verify the `-debuginfo` split still fires; `.a` grew 2.9 MB → 29.4 MB | medium |
| 18 | **licence metadata** | Sleepycat everywhere | `meson.build` says `AGPL-3.0-or-later OR Sleepycat`; `LICENSE`/`LICENSES/` still Sleepycat-only; **no AGPL text ships**; SPDX headers uncommitted | **get a maintainer ruling before legal review**; `LICENSE` is unchanged | **high** |
| 19 | second build system | autoconf only | meson added, but `meson install` ships **only `lib/libdb.so`** (soname `libdb.so`, no headers/utils/static) | **keep using autoconf**; meson is not install-complete | medium |
| 20 | Java / STL / `dump185` | all three fail to build | all three fail identically | none — **pre-existing, not regressions** | — |

### Items explicitly *not* verified

- libabigail `abidiff` verdict — tool absent from the dev shell. Symbol-level
  and `sizeof()` evidence given instead.
- Tcl binding install contents — configures at both points, not install-diffed.
- Windows (`build_windows/`) and Android (`build_android/`) packaging outputs.
- Whether any published GitHub release asset exists for either version
  (no network access used).
