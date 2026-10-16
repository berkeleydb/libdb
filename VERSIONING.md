# Versioning

This fork uses **[CalVer](https://calver.org/)** for its release identity,
deliberately breaking from the SemVer-style `5.3.x` / `11.2.5.x` / `18.1.x`
numbering that Sleepycat and then Oracle used. The point is to make it
unmistakable which releases are the community continuation and which are the
historical upstream product.

## Release version: `YYYY.0M[.MICRO]`

- `YYYY` — four-digit year of the release.
- `0M`   — zero-padded month (`01`–`12`).
- `.MICRO` — optional, only for a second (or later) release in the **same
  month** (`2026.09`, then `2026.09.1`, `2026.09.2`, …).

Set in `dist/RELEASE` as `DB_CALVER`. Git tags are `vYYYY.0M[.MICRO]`
(e.g. `v2026.09`). This is the number in `db_version()` output, the README, and
the GitHub release title.

## Compatibility level: FROZEN at `2026.0.9`

`DB_VERSION_MAJOR.MINOR.PATCH` in `dist/RELEASE` is **not** the release version.
It is the on-disk / log / region / ABI **compatibility level**, and it is frozen
at `2026.0.9`. Do not bump it to "do a release" — bump `DB_CALVER` instead.

**Correction, 2026-09-18.** Earlier revisions of this document said the level was
frozen at `5.3.37`. That was never true of the shipped artifacts: commit
`6710649f0` (2026-04-21, five months *before* this document was written) had
already set the triplet to `2026.0.9`, so **every** fork release from `v2026.04`
onward has shipped `libdb-2026.0.so` and stamped regions `majver=2026 minver=0`.
The document described an intention, not the code, and nothing checked it. The
freeze is real and still matters for exactly the reasons below — it is simply
frozen at a different value than this file claimed. `dist/meson/db_subs.json` had
kept the stale `5.3.37` copy, which made the meson build emit a `db.h` claiming
`5.3.29` and produce a library that could not attach an autoconf-built
environment; `dist/meson/gen_header.py` now derives the triplet from
`dist/RELEASE` so the two build systems cannot disagree again.

It stays frozen because several load-bearing things key off it, and changing it
breaks them for anyone upgrading within the fork:

- **Multi-process shared regions.** `env_region.c` refuses to attach a region
  whose stamped `majver`/`minver` don't match `DB_VERSION_MAJOR`/`MINOR`. A
  running environment created by an older build must still attach.
- **Shared-object soname.** The library is `libdb-5.3.so` (`LIBVERSION =
  MAJOR.MINOR`). Changing it breaks every dynamically-linked consumer.
- **Symbol mangling.** `DB_VERSION_UNIQUE_NAME` (`_5003`) is derived from
  MAJOR/MINOR and mangles the public symbols.
- **Downstream packaging** parses `5.3.x`.

Change the compatibility triplet **only** for a real format/ABI break, and then
through the `ABI-BREAK-INTENDED` process (see `.github/workflows` abi-drift
gate) — never merely to cut a release.

### Region-only format changes do not move the triplet

A change to a *shared region* struct (one hashed by `src/env/env_sig.c`) that
touches no public ABI, no log record and no database page is gated by the
**build signature**, not by the triplet. `env_region.c` compares the region's
stamped signature against `__env_struct_sig()` and refuses to attach on a
mismatch (`BDB1539` → `DB_VERSION_MISMATCH`), so a mixed-version attach fails
cleanly without reading region bytes. Bumping MAJOR.MINOR for such a change would
additionally change the soname and symbol mangling, and so break every
dynamically-linked consumer, to signal something the signature already enforces.

Such a change must still:

1. carry `REGION-SIG-CHANGE-INTENDED` in its commit message (the `region-sig`
   job in `.github/workflows/cocci.yml` fails without it);
2. ship an upgrade note under `docs_src/guides/upgrading/` giving the
   recover → remove regions → restart procedure;
3. be called out as a **breaking change** in the release notes.

The first such release was **v2026.10.5** (P12, per-stripe locker allocation,
`0xb86f77f0` → `0x0b67cec8`). It was made deliberately: no release had been
designated LTS, and the measured gain (+12% to +29% at 16–64 threads)
outweighed a one-time region recreation. Once a release is designated LTS,
region changes within that line are not allowed.

The second was **v2026.10.6** (RFC 0015 S4 retaining the TXN_DETAIL MVCC mutex
across recycling, plus the AArch64 hybrid mutex that fixes B3 on ARM). The
signature moved `0xe7904e69` → `0x56a06689` on both LP64 x86_64 and aarch64
(the aarch64 mutex struct also grew, but the dominant txn-region additions land
the hash at the same value on both). Still pre-LTS; the gain was correctness
(B3 on ARM) plus scalability overhead reductions, against a one-time region
recreation.

The third was **v2026.10.7**, on **macOS arm64 only**: the AArch64 hybrid mutex
is selected there again by default (F7), moving that platform's signature
`0xe0ad2911` → `0x2c8d3688`. Every other platform's build output is
byte-identical to v2026.10.6 (same `db_config.h`, same `.text`, same signature,
verified on Linux x86_64, FreeBSD x86_64 and FreeBSD aarch64), which is why the
v2026.10.6 soak qualifies it there. This is the last region change before the
first LTS; from the LTS tag on, the format is frozen within the line.

## LTS lines

An **LTS (long-term support)** release is a regular release that has also
cleared the LTS bar in
[`test/audit/QUALIFICATION-MATRIX.md`](test/audit/QUALIFICATION-MATRIX.md),
which adds a 72-hour resource-accounting soak on Linux x86_64, FreeBSD x86_64,
FreeBSD aarch64 and Windows/MSVC x86_64 to the normal release gates. Declaring
a release LTS is a promise about the **line** that begins at it:

- The on-disk, log and region formats, the soname and the public ABI are
  **frozen** for the whole line. Every later release in the line can open an
  environment created by any earlier one, and processes from different releases
  in the line can share one environment. A region-format change like the three
  above cannot happen inside an LTS line; it starts a new line.
- Releases in the line carry **fixes only** — bug, security and portability
  fixes. Features and performance work go to regular (non-LTS) releases.
- The line is **supported until the next LTS, plus a 12-month overlap**, so
  applications have time to migrate.

**`v2026.10.7` is the first LTS release.** It was qualified on the full matrix,
including the 72-hour soak on all four platforms above (Linux: 8,913 passes;
Windows: 4,600 passes over 4,320 minutes; both FreeBSD arms clean), with zero
resource-accounting failures. Its signatures per platform are the
`2026.10.7`/`2026.10.6` column of the table above (macOS arm64 `0x2c8d3688`;
Linux/FreeBSD x86_64 `0x56a06689` LP64; FreeBSD/Linux aarch64 `0x56a06689`),
and those are frozen for the `2026.10.x` LTS line from this tag forward.

## Consuming code

- Human / release identity → `DB_CALVER` (or the leading token of
  `db_version()`).
- Format / ABI / compatibility decisions → the `DB_VERSION_MAJOR/MINOR/PATCH`
  triplet (unchanged API: `db_version(&maj,&min,&pat)` returns `2026,0,9` --
  verified against a built library, not assumed).

`db_version()` returns, for example:

    libdb 2026.09.2 (September 10, 2026)

(That is the real string from a built library. Earlier revisions of this file
showed a "Berkeley DB compat 5.3.37:" form that the code has never produced.)

## Cutting a release

1. Bump `DB_CALVER` in `dist/RELEASE` (and `DB_RELEASE_DATE`).
2. Regenerate the generated version headers so the committed platform copies
   match: `cd dist && sh ./s_config && sh ./s_include && sh ./s_windows &&
   sh ./s_android`. (POSIX `db.h` regenerates at `configure` time; the
   `build_windows/` and `build_android/` copies are committed and must be
   regenerated by hand — mind the `build_windows/db.h` skip-worktree trap.)
3. Qualify from a pristine clone, then tag `vYYYY.0M[.MICRO]` and create the
   GitHub release with that title.
