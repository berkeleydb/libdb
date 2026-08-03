#!/usr/bin/env python3
"""Rewrite cross-tree links from the OLD docs/ layout to the NEW docs_src/ one.

The extractor rewrites *same-tree* links (`foo.html` -> `foo.md`). Cross-tree
links keep their old relative shape, e.g. from an API page:
    ../../programmer_reference/env_db_config.html#frag
    ../api_reference/C/dbget.html
Those still address the OLD `docs/` tree. This pass maps the old tree segment to
the new `docs_src/` location and re-computes the relative prefix from each page's
own depth, emitting `.md` targets (build.py turns `.md` -> `.html`).

Trees not migrated yet (CXX, TCL, java, csharp) are left as-is: their links stay
`.html` pointing at the archived `docs/` tree, which still resolves for readers.

Idempotent: only rewrites links whose old tree segment is in OLD_TO_NEW.
Usage:  fix_xrefs.py            # rewrite under docs_src/api + docs_src/guides
"""
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
DOCS_SRC = HERE.parent            # docs_src/
ROOTS = [DOCS_SRC / "api", DOCS_SRC / "guides"]

# Old docs/ tree segment  ->  new docs_src/ tree dir (relative to docs_src root).
OLD_TO_NEW = {
    "api_reference/C": "api/c",
    "api_reference/STL": "api/stl",
    "programmer_reference": "guides/programmer_reference",
    "upgrading": "guides/upgrading",
    "installation": "guides/installation",
    "porting": "guides/porting",
    "gsg/C": "guides/gsg",
    "gsg_txn/C": "guides/gsg_txn",
    "gsg_db_rep/C": "guides/gsg_db_rep",
    "collections/tutorial": "guides/collections",
    "bdb-sql": "guides/bdb-sql",
}
# Longest keys first so "api_reference/C" wins over a bare "C" etc.
OLD_KEYS = sorted(OLD_TO_NEW, key=len, reverse=True)

# href="<any ../ prefix><rest>.html<#frag>"  (cross-tree = has a "/" in rest)
HREF = re.compile(r'href="((?:\.\./)+)([A-Za-z0-9_./\-]+)\.html(#[^"]*)?"')


def new_root_prefix(page):
    """`../` * (depth of page below docs_src) -> reach docs_src root."""
    depth = len(page.relative_to(DOCS_SRC).parts) - 1
    return "../" * depth


def rewrite_one(page):
    text = page.read_text(encoding="utf-8")
    root = new_root_prefix(page)

    def sub(m):
        rest = m.group(2)            # e.g. programmer_reference/env_db_config
        frag = m.group(3) or ""
        for key in OLD_KEYS:
            if rest == key or rest.startswith(key + "/"):
                tail = rest[len(key):].lstrip("/")   # page stem within the tree
                if not tail:
                    tail = "index"
                return f'href="{root}{OLD_TO_NEW[key]}/{tail}.md{frag}"'
        return m.group(0)            # untouched (unmigrated tree)

    new = HREF.sub(sub, text)
    if new != text:
        page.write_text(new, encoding="utf-8")
        return True
    return False


def main():
    changed = 0
    for root in ROOTS:
        for p in root.rglob("*.md"):
            if rewrite_one(p):
                changed += 1
    print(f"cross-tree xrefs rewritten in {changed} pages")


def _selfcheck():
    # from a depth-2 page (api/c/x.md, root=../../), an old ../../programmer_reference
    # link becomes ../../guides/programmer_reference/*.md
    import tempfile
    d = Path(tempfile.mkdtemp())
    (d / "api/c").mkdir(parents=True)
    global DOCS_SRC, ROOTS
    DOCS_SRC, ROOTS = d, [d / "api"]
    pg = d / "api/c/x.md"
    pg.write_text('a <a href="../../programmer_reference/env.html#f">E</a> '
                  'b <a href="../api_reference/C/dbget.html">G</a> '
                  'c <a href="../api_reference/CXX/foo.html">X</a>')
    assert rewrite_one(pg)
    out = pg.read_text()
    assert 'href="../../guides/programmer_reference/env.md#f"' in out, out
    assert 'href="../../api/c/dbget.md"' in out, out
    assert '../api_reference/CXX/foo.html' in out, out   # unmigrated: untouched
    print("selfcheck ok")


if __name__ == "__main__":
    if "--selfcheck" in sys.argv:
        _selfcheck()
    else:
        main()
