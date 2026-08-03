# One-time migration: reverse-DocBook extractor

The DocBook XML source for `docs/` is lost — only DocBook-XSL-rendered HTML
survives. These scripts recover clean, maintainable Markdown from that HTML.

## Scripts

- **`extract.py`** — parse `docs/api_reference/C/*.html`, strip the repeated
  boilerplate (navheader, libver `Library Version 11.2.5.3`, generator meta,
  stylesheet link, Prev/Next tables) by their stable DocBook classes, and
  convert the semantic body to Markdown via `pandoc html→gfm` + a cleanup pass
  (front-matter, `.html`→`.md` cross-link rewrite, `programlisting`→```` ```c ````
  fences). Emits `docs_src/api/c/*.md`.

- **`verify.py`** — the no-loss gate. Per page, compares OLD visible prose vs
  NEW Markdown prose (normalized word multiset) and separately asserts no code
  block or parameter/error sub-section was dropped. Reports mean retention and
  any outlier; exits non-zero on a hard structural drop (for CI).

## Run (needs pandoc — use the dev shell)

```sh
nix develop --command bash -c 'python3 docs_src/_migrate/extract.py'
python3 docs_src/_migrate/verify.py            # --threshold 0.97 default
nix develop --command bash -c 'python3 docs_src/build.py'
```

## Status (Phase 1)

Proven on the **C API tree** (470 pages): mean word retention **99.98%**,
0 outliers, 0 hard drops. C++/STL and the guide trees are follow-up phases;
`extract.py` takes `[SRC_HTML_DIR] [OUT_MD_DIR]` args so it retargets to any
tree once its per-tree cleanup is tuned.
