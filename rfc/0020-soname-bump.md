# RFC 0020: Bump the soname to libdb-2026.1 for the handle-layout change (A1)

- **Status:** Implemented (branch build/soname-bump)
- **Type:** Normative
- **Author:** libdb maintainers
- **Date:** 2026-11-02
- **Tracking:** review decision Q6 (`REVIEW-2026-11.md` Part 3); KNOWN-ISSUES A1
- **Prototype:** none

---

## Summary

Move the compatibility triplet from `2026.0.9` to **`2026.1.0`** on `master`. The shared library becomes
**`libdb-2026.1.so`**, the install prefix `BerkeleyDB.2026.1`, the symbol suffix `_2026001`, and a region
stamped `2026.1`.

This states plainly that `sizeof(DB)` grew from 1456 to 1744 bytes (and `DBC` gained a field) between the
fork baseline and today. Until now that growth was hidden behind an unchanged soname. Databases and log
files do not change: they have their own version numbers, which this RFC does not touch. The LTS line
keeps `libdb-2026.0.so` for its whole life.

## Motivation

KNOWN-ISSUES **A1**: commit `f9937fab5` (cursor sharding, #178) replaced `struct __db`'s two cursor-queue
heads with eight partitions and added `part` to `struct __dbc`. The soname stayed `libdb-2026.0`. The
abidiff gate compares each PR with the previous release tag, so a struct that grows in one commit never
shows up in a later comparison.

The project had taken option (b): declare handle structs internal, let the soname track only the exported
functions, and pin the sizes with `test/db/handle_sizes.c`. The 2026-11 review (Anderson, Bostic) asked
for the choice to be stated, either way. The maintainer chose **(a), bump the soname** (decision Q6).

The reasons:

- **A packager has one number to trust.** Two libraries with the same soname must be interchangeable for a
  dynamically linked program. Today they are, as far as exported functions go. But a program that loads
  two libdb builds, through a plugin and a host for example, can pass a `DB *` between them, and that is
  silent memory corruption.
- **It is the honest signal for what the next releases do.** The master work queued after this review
  changes formats on purpose (decision Q1): RFC 0019 changes the region, RFC 0022 changes the log, and
  RFC 0018 adds API. A new soname now means those land under a name that already says "not
  interchangeable with 2026.0".

## North-star check

| Property | Effect |
|---|---|
| embedded, no server | none |
| ACID, recovery | none |
| access methods | none |
| multi-process regions | A 2026.1 process refuses a 2026.0 region with `DB_VERSION_MISMATCH` (`env_region.c:253-262`). That is the intended, clean failure, the same one the build signature already gives. |
| on-disk format | **none.** `DB_BTREEVERSION` 9, `DB_HASHVERSION` 9, `DB_QAMVERSION` 4 and `DB_HEAPVERSION` 1 are unchanged. A 2026.0 database opens in 2026.1 and the other way round. |
| log format | **none.** `DB_LOGVERSION` 19 is unchanged. A 2026.1 process recovers a 2026.0 log. |
| public ABI | **this is the ABI break**, declared with `ABI-BREAK-INTENDED` |

## Design

### What moves

| Item | 2026.0 (LTS and earlier master) | 2026.1 (master) | Where |
|---|---|---|---|
| `DB_VERSION_MINOR` | 0 | **1** | `dist/RELEASE` |
| `DB_VERSION_PATCH` | 9 | **0** | `dist/RELEASE` |
| `db_version(&maj,&min,&pat)` | 2026, 0, 9 | 2026, 1, 0 | derived |
| shared library | `libdb-2026.0.so` | `libdb-2026.1.so` | `dist/Makefile.in` `LIBVERSION` (derived) |
| C++/Java/Tcl/SQL libraries | `libdb_cxx-2026.0.so` and so on | `-2026.1` | derived |
| symbol suffix | `_2026000` | `_2026001` | `DB_VERSION_UNIQUE_NAME` (derived, `--with-uniquename` only) |
| install prefix | `BerkeleyDB.2026.0` | `BerkeleyDB.2026.1` | `configure.ac` (derived) |
| region stamp | majver 2026, minver 0 | 2026, 1 | `env_region.c:435-437` (derived) |
| CalVer (`DB_CALVER`) | unaffected; `2026.10.8` and later | unaffected | independent of the triplet |

Every derived item follows from `dist/RELEASE` through `dist/s_config`, `s_include`, `s_windows` and
`s_android`. The meson build reads `dist/RELEASE` through `dist/meson/gen_header.py`. Because the change is
made once, at the source, the two build systems cannot disagree. The parity test
`run_meson_autoconf_parity.sh` checks that.

### What must be edited by hand (not derived)

- `lang/rust/libdb-sys/build.rs` and `tests/drift_gate.rs`: the probed library name and prefix
  (`libdb-2026.0` becomes a probe for either version, preferring the newer).
- `lang/zig/build.zig` and the `src/libdb.zig` doc comment.
- `test/bench/*.sh` scripts that hard-code `-ldb-2026.0`: change them to discover the library, as the test
  runners already do.
- `VERSIONING.md`: rewrite the "frozen triplet" section into the rule in the next subsection.
- `test/db/handle_sizes.c`: keep it. It now records the level the sizes belong to, and FAILs on a
  size change at an unchanged level.
- `meson.build`: the meson build named its library plain `libdb.so`, with no soname. It now builds
  `libdb-<MAJOR>.<MINOR>.so` from `dist/RELEASE`, like autoconf. That needs `fs.read`, so the minimum
  meson moves from 0.56 to 0.57.

### The rule after this RFC (VERSIONING.md)

1. **Within an LTS line, the triplet never moves.** `lts/2026.10.7` stays `2026.0.9` and `libdb-2026.0.so`.
2. **On master, the triplet's minor moves when, and only when, the public ABI breaks.** That means:
   - a public function signature changes or a function is removed;
   - a public struct's layout changes, whether or not the struct is allocated by the library, which covers
     `DB`, `DBC`, `DB_ENV` and `DB_TXN` (`handle_sizes.c` enforces this);
   - the meaning of an existing flag changes.
3. **A region-only change does not move the triplet.** The build signature gates it, as today.
4. **An on-disk or log-format change does not move the triplet.** Those have their own versions and
   upgrade paths (`DB->upgrade`, log version checks).
5. More than one break between two releases moves the minor once.

So 2026.1 is the next soname; the next ABI break after it on master moves it to 2026.2.

### Gates

- `test/db/run_handle_sizes.sh` records the sizes together with the level they belong to
  (`RECORDED_MAJOR`/`RECORDED_MINOR`). A size change without a level change FAILs. That closes the gap
  that let A1 through: the handle sizes are now pinned to the soname, rather than being compared only
  with the previous release tag.
- The abidiff job (`.github/workflows/cocci.yml`) requires `ABI-BREAK-INTENDED` when the exported
  symbol set changes; it is unchanged.

## Alternatives

- **(b) Declare handles opaque and keep the soname.** This was the previous choice. It is cheaper, but it
  leaves the mixed-build corruption case and asks packagers to trust a prose rule. The maintainer rejected
  it (Q6).
- **A major bump, `2027.0`.** That would tie the soname to the calendar, the way CalVer does, and would
  muddle the separation the fork made deliberately between the release name and the compatibility level.
- **Bump at every release.** That turns every release into a rebuild for packagers. It is not what a
  stable embedded library does.

## Risks

- **Every dynamically linked consumer must relink against the new soname.** That is the purpose of the
  change, and the release notes and the upgrade note must say so in the first paragraph.
- **A running 2026.0 environment cannot be attached by a 2026.1 process.** This is the standard procedure
  the 2026.10.5 and 2026.10.6 region changes already used: recover with the old version, remove the
  regions (`__db.0*`), and open with the new one. Databases and logs are kept.
- **Downstream build scripts that hard-code `-ldb-2026.0` will fail to link.** That is loud, not silent.

## Test plan

- Build with autoconf and meson; parity test.
- `db_version()` returns 2026.1.0.
- `libdb-2026.1.so` exists, and its `SONAME` (readelf -d) is `libdb-2026.1.so`.
- A 2026.0 region is refused with `DB_VERSION_MISMATCH`, and a 2026.0 database and log open and recover
  under 2026.1: `test/db/run_soname_compat.sh` (OLD_BUILD = a build of the previous level).
- The Rust and Zig bindings build and link against the new name.
- Full `test/db` tier on Linux and FreeBSD.

## Decision

Accepted by the maintainer as review decision Q6 (2026-11-02): bump the soname.
