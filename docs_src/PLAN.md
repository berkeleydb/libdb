# libdb Documentation Modernization — Plan

## Problem

`./docs/` is **93 MB of DocBook-XSL-generated HTML** (2703 files, 17 trees) scraped
from the old Oracle Berkeley DB site. The DocBook XML *source is lost* — only
rendered HTML survives. Consequences:

- **Unmaintainable**: every page hard-codes the same boilerplate — `Library
  Version 11.2.5.3`, the copyright, the `apiReference.css` link, the DocBook
  navheader (Prev/Next tables), the generator meta. Changing the version means
  editing 2703 files.
- **Single output format** (HTML only). Targets needed: **HTML, Markdown, PDF**.
- **No man pages** despite ~393 public methods + 28 functions in `db.h`.
- **Duplicated** headers/footers/nav/version/copyright on every page.

## Insight: the content is structured, we just have to recover the structure

The HTML is DocBook `refentry`/`chapter` output with a **consistent semantic
schema**. API pages have: Title → Description → Parameters (per-param blocks) →
Errors → Class → See Also. That maps 1:1 to Markdown front-matter + sections AND
to man-page sections (NAME/SYNOPSIS/DESCRIPTION/PARAMETERS/ERRORS/SEE ALSO). So
we **reverse the DocBook rendering**: extract clean structured content once, then
render to all three formats from a single source.

## Chosen approach: Markdown-as-source + a small static generator

Rejected alternatives (with why):
- *Re-DocBook it* — heavyweight XML toolchain, XSLT, the exact thing that rotted;
  nobody will hand-edit DocBook XML.
- *Keep HTML, template the boilerplate* — solves duplication but not
  multi-format or maintainability; still HTML-only and verbose to edit.
- *Sphinx/reStructuredText* — capable but heavier than needed and a less
  familiar authoring format than Markdown.

**Markdown is the source of truth.** It's the most maintainable authoring format,
diffs cleanly, and both PDF (via pandoc) and man pages (via pandoc or a small
converter) and HTML (via a template) render from it. Common pieces live in ONE
place (a `docs_src/_data/` config + shared templates/partials), injected at build.

### Source layout (new — `docs_src/`, the committed source of truth)
```
docs_src/
  _data/
    site.toml           # version, copyright, project name, base URL — ONE place
                        #   (version derived from dist/RELEASE at build time)
  _templates/
    page.html.tmpl      # HTML shell: header, nav, footer, version/copyright
    man.tmpl            # man-page (mdoc/man) skeleton
    pdf-header.tex      # PDF/LaTeX title + running header (pandoc)
  api/
    c/                  # one .md per public API (dbget.md, dbput.md, ...)
      _meta.toml        #   ordering/grouping for nav + the index
    cxx/  stl/
  guides/               # gsg, gsg_txn, gsg_db_rep, programmer_reference,
                        #   collections, bdb-sql, upgrading, installation,
                        #   porting, articles — each a chaptered .md set
  design/               # already-Markdown design notes move here as-is
  index.md              # site landing content
```

### The generator (`docs_src/build.py` — stdlib + pandoc, no framework)
One small Python script:
1. loads `site.toml` (version pulled live from `dist/RELEASE`),
2. walks `docs_src/**.md`,
3. renders each to **HTML** (Markdown + the shared template → boilerplate injected
   once), **PDF** (pandoc per book), and **man pages** (per API .md → section 3),
4. builds nav/index/TOC from `_meta.toml`,
5. emits to `docs-build/{html,pdf,man}/`.

Man pages: every public API .md → a `*.3` page (section 3 = library calls); plus
**one `libdb.3` overview** man page for the library as a whole, generated from the
API index + the programmer's-reference intro.

## Migration (content-preserving — nothing is lost)

A **one-time extraction** script `docs_src/_migrate/extract.py`:
1. Parse each existing `docs/**.html` with Python's `html.parser`/`lxml`.
2. Strip the repeated boilerplate (navheader, libver, generator meta, css link,
   Prev/Next) — recognized by the stable DocBook classes (`navheader`, `libver`,
   `titlepage`).
3. Convert the semantic body (sect1/sect2/refsect, programlisting, tables,
   variablelist for parameters) to Markdown (pandoc `html→gfm`, then a cleanup
   pass that restores the API section schema as headings + a front-matter block).
4. Preserve images (copy to `docs_src/**/img/`), internal cross-links (rewrite
   `.html` → the new scheme), and code samples verbatim.
5. Emit the `docs_src/` tree. **Diff-verify**: a checksum/word-count report per
   page old-vs-new so we prove no content dropped (completeness gate).

The old `docs/` HTML stays in git history; once `docs_src/` + `docs-build/` are
validated, `docs/` is replaced by the generated `docs-build/html` for publishing
(or kept until the new tree is signed off).

## CI validation (new `.github/workflows/docs.yml`)

Runs on PRs touching `docs_src/**` + on push + schedule:
- **Build**: the generator produces HTML/PDF/man with 0 errors.
- **Spelling**: `codespell` (+ a project wordlist for BDB terms: mpool, DBT,
  txnid, lsn, ...).
- **Grammar/prose**: `vale` with a light style (or `write-good`) — advisory.
- **Link check**: `lychee` (or `linkchecker`) over generated HTML — internal links
  must resolve; external links advisory.
- **Man-page lint**: `mandoc -Tlint` on every generated `*.3`.
- **Completeness gate**: assert every public API in `db.h` (the ~393 methods +
  28 functions) has a corresponding `docs_src/api/**.md` — fail if an API is
  undocumented (this is the "completeness" check + catches drift as APIs change).
- **No-orphan check**: every .md is reachable from an index/nav.

## Publish (GitHub Pages / libdb.org)

`docs.yml` (or the existing pages flow) publishes `docs-build/html` to the
`gh-pages` branch. The utilitarian `index.html` we already built stays as the
landing page; its "Docs" section points at the regenerated, versioned tree
(replacing the archived 5.3.21/5.3.28 links). PDFs + a man-page tarball attach to
the site (and can attach to GitHub releases).

## Execution phases

1. **Scaffold** (DONE, PR #119): `docs_src/` skeleton, `site.toml` (version from
   `dist/RELEASE`), templates, `build.py` (HTML first).
2. **Extractor + trees** (DONE): `extract.py` + `verify.py` no-loss gate on the
   C API tree (100.00%), then STL (100.00%) and every guide tree. See the
   retention table below. `migrate_tree.py` drives chaptered guides (order from
   the index TOC, image copy); `fix_xrefs.py` remaps cross-tree links to the new
   `docs_src/` layout.
3. **Man pages** (DONE): `build_man()` -> 787 `*.3` (785 C+STL refentry pages +
   `libdb.3` overview + utilities), `mandoc -Tlint` = 0 errors. `man_coverage.py`
   reports API coverage (measure-only; the CI gate is phase 5).
4. **PDF** (DONE): `build_pdf()` renders ONE PDF per book (13 books: 2 API refs
   + 9 guides + articles' 2 sub-books) via pandoc(html)->**weasyprint** -- no TeX
   toolchain, deterministic, ~3.5 min for all 13. Title page (project + live
   version + copyright) + running header/footer via `_templates/pdf-print.css`
   (CSS paged-media). Output `docs-build/pdf/<book>.pdf`; `validate_pdf.py`
   asserts non-empty + sane page count + version on the title page. Page counts:
   api_c 655, programmer_reference 370, api_stl 257, installation 168,
   upgrading 164, gsg_txn 119, gsg 96, collections 93, gsg_db_rep 66, bdb-sql 47,
   mssgtxt 41, inmemory 20, porting 16.
5. **CI** (DONE): `.github/workflows/docs.yml` (nix devShell for tool parity).
   HARD gates: build (html+man, 0 errors), no-loss (`verify_all.py`, all 13
   trees), completeness (`man_coverage.py --ci`: 28/28 functions + allowlisted
   methods), spelling (`spellcheck.py`, codespell baselined to legacy typos),
   internal link-check (`lychee` + `lychee.toml`), man-lint (mandoc, 0 ERRORS).
   ADVISORY: prose (write-good). BEST-EFFORT/scheduled: PDF build+validate,
   external link-check. The 2 genuinely-undocumented APIs
   (`db_env_set_func_assert`, `db_env_set_win_security`) got real stub pages, so
   the function gate is a hard 100%.
6. **Publish** (TODO): wire gh-pages to the generated HTML; update the landing
   page. Needs: a `pages` job (or step) that runs `build.py`, uploads
   `docs-build/html` via `actions/upload-pages-artifact` + `deploy-pages`
   (permissions: `pages: write`, `id-token: write`); a top-level `index.md`
   linking the 12 book landing pages + the man/PDF outputs; and a decision on
   whether PDFs/man are published alongside HTML. The deferred CXX/TCL/java/
   csharp trees stay out until regenerated from their native doc tools (their
   inbound links are excluded in `lychee.toml`).

### Phase 2 retention (verify.py, mean word retention; 0 hard drops on all)

| tree                    | pages | retention |
|-------------------------|-------|-----------|
| api/c                   | 470   | 100.00%   |
| api/stl                 | 322   | 100.00%   |
| guides/programmer_reference | 203 | 99.98%  |
| guides/upgrading        | 180   | 100.00%   |
| guides/installation     | 101   | 99.99%    |
| guides/porting          | 16    | 100.00%   |
| guides/gsg (C)          | 37    | 100.00%   |
| guides/gsg_txn (C)      | 38    | 100.00%   |
| guides/gsg_db_rep (C)   | 26    | 100.00%   |
| guides/collections      | 37    | 99.98%    |
| guides/bdb-sql          | 30    | 100.00%   |
| guides/articles         | 2     | 100.00%   |

### Deferred: docs/csharp (38 MB) and docs/java (12 MB)

These are LANGUAGE-BINDING docs and are NOT DocBook, so the reverse-DocBook
extractor does not apply:
- **csharp** — a compiled Sandcastle/MS-Help-Viewer tree (`.chm` + `.aspx` +
  JS/PNG, 2457 files, only 1 real `.html`). Would need a bespoke extractor.
- **java** — standard Javadoc HTML (525 files: allclasses-frame, package-frame,
  index-all). Different structure; the `refentry`/`chapter` isolation is moot.

Both are enormous and lower-value for the core C engine. Deferred to a future
phase (regenerate from the C#/Java sources with their native doc tools, or write
a per-format extractor) rather than sink budget reverse-engineering rendered
help output. The gsg/gsg_txn/gsg_db_rep CXX/JAVA sub-variants are likewise
deferred; the C variants are migrated.

## Non-negotiables

- **No content lost** — the diff-verify report is a hard gate.
- Version/copyright/header in **exactly one place** (`site.toml` + templates).
- Three formats (HTML/MD/PDF) + man pages, all from the one Markdown source.
- Generator uses stdlib + pandoc (already common); no heavyweight doc framework.
