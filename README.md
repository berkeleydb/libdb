# Berkeley DB — Historical Archive (`historical` branch)

This branch is a **linear, chronological reconstruction of every Berkeley DB
release and official patch** that could be recovered from the public archive at
`https://ftpmirror.your.org/pub/misc/Berkeley-DB/`. It exists as a historical
reference: a single place to read, browse, and `git diff` the entire published
lineage of Berkeley DB from **1.85 (1992)** through **5.1.29 (2011)**.

> This branch is independent of `master`. `master` carries the maintained
> 5.3.x line and ongoing bug fixes; `historical` is an archival record and is
> **not** merged into `master`.

## What's here

Each upstream release is imported verbatim as one commit (the whole source tree
is replaced), and **every official patch is its own commit** on top of its base
release. Commits are dated with each release's real publication date so that
`git log` reads as a true timeline.

### Tag scheme

| Tag form | Meaning | Example |
|----------|---------|---------|
| `vX.Y.Z` | Base upstream release (source tarball, imported verbatim) | `v4.6.21` |
| `vX.Y.Z.N` | The Nth official upstream patch applied on top of `vX.Y.Z` | `v4.6.21.2` |
| `vX.Y.Z-NC` | "No-Crypto" export variant, branched from the `vX.Y.Z` base | `v4.2.52-NC` |

The releases, in chain order:

```
1.85 (+patches 1–4)  1.86  2.7.7
3.0.55 (+1)  3.1.17  3.2.9 (+1,2)  3.3.11 (+1,2)
4.0.14  4.1.25 (+1–3, NC)  4.2.52 (+1–5, NC)  4.3.29 (+1)
4.4.20 (+1–4)  4.5.20 (+1,2)  4.6.21 (+1–4)  4.7.25 (+1–4)
4.8.30  5.0.32  5.1.29
```

## How to diff versions

```sh
# What changed between two releases (file summary, then full diff):
git diff --stat v4.5.20 v4.6.21
git diff        v4.5.20 v4.6.21

# Exactly what a single official patch changed:
git diff v4.6.21 v4.6.21.2          # base -> patch 2
git diff v4.6.21.1 v4.6.21.2        # patch 1 -> patch 2

# What the No-Crypto export variant strips out:
git diff v4.2.52 v4.2.52-NC

# Walk the whole timeline:
git log --oneline --reverse historical

# List every archived version:
git tag -l | sort -V
```

## Provenance & integrity

- Source: `https://ftpmirror.your.org/pub/misc/Berkeley-DB/` (tarballs and
  `patch.*` files).
- SHA-256 checksums of every imported artifact are recorded in the matching
  GitHub/Codeberg **release** for each tag, and collectively in `SHA256SUMS`
  alongside the import tooling.
- Tarballs are extracted and committed unmodified. Patches are applied with GNU
  `patch -p0` from the source root, matching upstream's documented convention.

## Caveats and interpretation notes

- **Dates:** release dates are taken from each distribution's own `README`
  banner. Berkeley DB **1.85** and **1.86** predate that convention, so their
  dates are approximate (year-accurate).
- **`v1.85.1`:** upstream `patch.1.1` targets a bare `Makefile` (a library build
  rule fix). The 1.85 distribution has no top-level `Makefile`; that rule lives
  in `PORT/<system>/Makefile`. The patch was therefore applied to every
  `PORT/*/Makefile` whose context matched it (11 of 20; the others carry a
  slightly different object list).
- **No-Crypto (`-NC`) variants** are tagged off their base release rather than
  placed inline in the linear chain, so that `git diff vX.Y.Z vX.Y.Z-NC` shows
  exactly the crypto-related differences.
- **Not present:** `5.2.42` and `5.3.21` exist on the mirror only as ~2 KB
  placeholder stubs (Oracle withdrew the downloads), so they could not be
  imported here. `5.3.21`, `5.3.28`, and the maintained `5.3.29` live on
  `master`.
- A handful of very old patches apply with small context offsets or fuzz; this
  is expected for diffs reconstructed decades later and does not change the
  resulting source.

## Rebuilding

The import is fully scripted and idempotent (`build_historical.py` in the import
tooling). It rebuilds this branch and all tags from the mirrored artifacts
without touching `master` or the `v5.3.x` tags.
