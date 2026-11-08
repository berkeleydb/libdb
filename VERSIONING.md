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
- `.N` — a fourth component, used **only** for a patch release of an LTS line:
  `<base>.N`, where `<base>` is the LTS release (`2026.10.7.1` is the first
  patch of the `2026.10.7` LTS). The extra component keeps an LTS fix from
  taking the number of the next regular release from `master` (`2026.10.8`).
  See [`RELEASING.md`](RELEASING.md).

Set in `dist/RELEASE` as `DB_CALVER`. Git tags are `vYYYY.0M[.MICRO]`
(e.g. `v2026.09`). This is the number in `db_version()` output, the README, and
the GitHub release title.

## Compatibility level: `2026.1.0` on master, `2026.0.9` on the 2026.10.7 LTS line

`DB_VERSION_MAJOR.MINOR.PATCH` in `dist/RELEASE` is **not** the release version.
It is the **ABI compatibility level**. Do not bump it to "do a release"; bump
`DB_CALVER` instead. It drives:

- **The soname.** The library is `libdb-MAJOR.MINOR.so` (`LIBVERSION` in
  `dist/Makefile.in`; the meson build reads the same two numbers from
  `dist/RELEASE`). Two libraries with the same soname are interchangeable for a
  dynamically linked program.
- **Symbol mangling.** With `--with-uniquename`, `DB_VERSION_UNIQUE_NAME`
  (`_2026001` at 2026.1) suffixes the internal symbols, so two levels can be
  linked into one process.
- **The region attach check.** `env_region.c` stamps `majver`/`minver` into a
  new region and refuses to attach a region of another level
  (`BDB1538`, `DB_VERSION_MISMATCH`). Nothing is read in that case.
- **The install prefix** (`/usr/local/BerkeleyDB.MAJOR.MINOR`) and downstream
  packaging.

It does **not** version the database or log formats. Those have their own
numbers (`DB_BTREEVERSION`, `DB_HASHVERSION`, `DB_QAMVERSION`, `DB_HEAPVERSION`,
`DB_LOGVERSION` in `db.in`), and a database or log written at one level opens
and recovers at the next.

### When the level moves (rfc/0020)

1. **On an LTS line, never.** `lts/2026.10.7` is `2026.0.9` and ships
   `libdb-2026.0.so` for its whole life.
2. **On master, the MINOR moves when, and only when, the public ABI breaks:**
   a public function signature changes or a function is removed; a public
   struct layout changes, including the library-allocated handles `DB`, `DBC`,
   `DB_ENV` and `DB_TXN` (`test/db/handle_sizes.c` records their sizes for one
   level and FAILs if they change without a level change); or the meaning of an
   existing flag changes. The commit carries `ABI-BREAK-INTENDED`.
3. More than one break between two releases moves the MINOR once.
4. **A region-only change does not move it**, and neither does an on-disk or
   log-format change. The build signature and the per-format version numbers
   gate those (below).

**History.** From `v2026.04` through `v2026.10.8` the level was `2026.0.9`
(`libdb-2026.0.so`). Commit `6710649f0` set it; an earlier revision of this file
said it was frozen at `5.3.37`, which was never true of the shipped artifacts,
and `dist/meson/db_subs.json` carried that stale copy until
`dist/meson/gen_header.py` began deriving the triplet from `dist/RELEASE`. In
2026.1 (rfc/0020) the level moved because `sizeof(DB)` had grown from 1456 to
1744 bytes and `DBC` had gained a field (cursor sharding, `f9937fab5`;
KNOWN-ISSUES A1) under the unchanged `libdb-2026.0` soname.

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

The line lives on the branch `lts/<base>` and ships `v<base>.N` patch releases
(`RELEASING.md`). Its first patch release is **`v2026.10.7.1`**.

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
  triplet. `db_version(&maj,&min,&pat)` returns `2026,1,0` on master and
  `2026,0,9` on the 2026.10.7 LTS line (verified against built libraries).

`db_version()` returns, for example:

    libdb 2026.09.2 (September 10, 2026)

(That is the real string from a built library. Earlier revisions of this file
showed a "Berkeley DB compat 5.3.37:" form that the code has never produced.)

## Cutting a release

The procedure, for both `master` releases and LTS patch releases, is in
[`RELEASING.md`](RELEASING.md).
