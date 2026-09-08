#!/usr/bin/env python3
"""Rebuild docs_src/api/stl/index.md from the STL tree's own converted pages.

The upstream DocBook STL index.html (recovered from the gh-pages branch at
docs/db/5.3.28/api_reference/STL/index.html, 3698 bytes) is titlepage-only: it
carries the legal notice and nothing else. Unlike the C tree -- whose
index.html is 107 KB and holds the full method table -- STL navigation lived in
the frameset sidebar frame_index.html. So the chapter ORDER comes from
frame_index.md, and each chapter's member rows come from that chapter page's
own "Public Members" table. Nothing here is invented.
"""
import re
import sys
from pathlib import Path

STL = Path(sys.argv[1] if len(sys.argv) > 1 and not sys.argv[1].startswith("-")
           else "docs_src/api/stl")

LEGAL = """**Legal Notice**

This documentation is distributed under an open source license. You may review the terms of this license at: <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html" class="ulink" target="_top">http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html</a>

Oracle, Berkeley DB, and Sleepycat are trademarks or registered trademarks of Oracle. All rights to these marks are reserved. No third-party use is permitted without the express prior written consent of Oracle.

Other names may be trademarks of their respective owners.

To obtain a copy of this document's original source code, please submit a request to the Oracle Technology Network forum at: <a href="http://forums.oracle.com/forums/forum.jspa?forumID=271" class="ulink" target="_top">http://forums.oracle.com/forums/forum.jspa?forumID=271</a>

9/9/2013

------------------------------------------------------------------------"""


def front_matter_title(p):
    m = re.search(r'^title:\s*"(.*)"', p.read_text(encoding="utf-8"), re.M)
    return m.group(1) if m else p.stem


def chapter_order():
    """Chapter stems in upstream sidebar order (frame_index.md)."""
    txt = (STL / "frame_index.md").read_text(encoding="utf-8")
    stems = []
    for m in re.finditer(r'<span class="chapter">\s*<a href="([A-Za-z0-9_]+)\.md"', txt):
        if m.group(1) not in stems:
            stems.append(m.group(1))
    return stems


def members(stem):
    """Verbatim `| member | description |` rows of the chapter page's
    `Public Members` table. Copied as-is rather than re-assembled from parsed
    cells: some titles contain a literal `|` (ElementHolder's `operator|=`),
    which any naive cell split would mangle and silently drop."""
    txt = (STL / f"{stem}.md").read_text(encoding="utf-8")
    m = re.search(r"^#### Public Members\s*$(.*?)^#### ", txt, re.M | re.S)
    if not m:
        return []
    rows = []
    for line in m.group(1).splitlines():
        line = line.strip()
        if not line.startswith("|"):
            continue
        if set(line) <= set("|- "):           # header underline
            continue
        if line.startswith("| Member |"):      # header row
            continue
        rows.append(line)
    return rows


def clean_title(t):
    """`Chapter 4.  Db_vector` -> `Db_vector`; keep descriptive tails."""
    return re.sub(r"^Chapter\s+\d+\.\s*", "", t).strip()


def selfcheck():
    """The two things that would silently gut the index if they broke: the
    verbatim row copy (a naive cell split drops `operator|=`, whose title
    contains a literal `|`), and every link resolving to a real page."""
    assert clean_title("Chapter 4.  Db_vector") == "Db_vector"
    assert clean_title("ElementHolder") == "ElementHolder"
    rows = members("ElementHolder")
    assert any("operator_oa" in r for r in rows), "operator|= row dropped"
    assert all(r.startswith("|") for r in rows)
    pages = {p.stem for p in STL.glob("*.md")}
    txt = (STL / "index.md").read_text(encoding="utf-8")
    targets = {m.group(1) for m in re.finditer(r'href="([A-Za-z0-9_]+)\.md', txt)}
    assert not targets - pages, f"dangling: {sorted(targets - pages)}"
    # Only the frameset stubs + front matter may sit outside the TOC.
    assert pages - targets == {"frame_index", "frame_main", "index",
                               "moreinfo", "preface"}, sorted(pages - targets)
    print("gen_stl_index selfcheck ok")


def main():
    order = chapter_order()
    missing = [s for s in order if not (STL / f"{s}.md").exists()]
    if missing:
        sys.exit(f"sidebar names pages that do not exist: {missing}")

    out = [
        "---",
        'title: "Berkeley DB C++ Standard Template Library API Reference"',
        'api-name: "Berkeley DB C++ Standard Template Library API Reference"',
        "source: docs/api_reference/STL/index.html",
        "---",
        "# Berkeley DB C++ Standard Template Library API Reference",
        "",
        "| DB C++ STL API | Description |",
        "|----|----|",
    ]
    # Top table: one row per chapter, in upstream sidebar order.
    for stem in order:
        label = clean_title(front_matter_title(STL / f"{stem}.md"))
        out.append(f'| <a href="{stem}.md" class="xref" title="{label}">{label}</a> | '
                   f"{len(members(stem))} members |")
    out.append("")

    # Then one section per chapter carrying that chapter's own member rows,
    # so every method page is reachable in one hop from the landing page.
    for stem in order:
        label = clean_title(front_matter_title(STL / f"{stem}.md"))
        rows = members(stem)
        out.append(f'## <a href="{stem}.md" class="xref" title="{label}">{label}</a>')
        out.append("")
        if not rows:
            out.append(f'See <a href="{stem}.md" class="xref" title="{label}">{label}</a>.')
            out.append("")
            continue
        out.append("| Member | Description |")
        out.append("|----|----|")
        out += rows
        out.append("")

    out.append(LEGAL)
    (STL / "index.md").write_text("\n".join(out) + "\n", encoding="utf-8")
    print(f"wrote index.md: {len(order)} chapters")


if __name__ == "__main__":
    if "--selfcheck" in sys.argv:
        selfcheck()
    else:
        main()
