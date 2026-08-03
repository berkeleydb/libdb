#!/usr/bin/env python3
"""libdb docs generator: Markdown source -> HTML (and, later, PDF + man).

- Version is read LIVE from dist/RELEASE (never hard-coded).
- Boilerplate (header/footer/version/copyright) is injected ONCE from the
  shared template + site.toml, replacing the old per-page duplication.
- Nav/index come from per-directory _meta.toml (falls back to a flat listing).

PDF and man outputs are Phase 3/4 — man is implemented; build_pdf renders
one PDF per book via pandoc(html)->weasyprint (no TeX toolchain).

Usage:  build.py            # build HTML + man + PDF into docs-build/
        build.py --no-pdf   # skip PDF (e.g. weasyprint not installed)
Requires: pandoc on PATH; PDF also needs weasyprint
(run under `nix shell nixpkgs#pandoc nixpkgs#python3Packages.weasyprint`).
"""
import html
import re
import subprocess
import sys
import tomllib
from pathlib import Path

HERE = Path(__file__).resolve().parent          # docs_src/
REPO = HERE.parent
SRC = HERE
OUT = REPO / "docs-build/html"
TEMPLATE = HERE / "_templates/page.html.tmpl"
MAN_TEMPLATE = HERE / "_templates/man.tmpl"
SITE_TOML = HERE / "_data/site.toml"
RELEASE = REPO / "dist/RELEASE"
MAN_OUT = REPO / "docs-build/man/man3"
PDF_OUT = REPO / "docs-build/pdf"
PDF_CSS = HERE / "_templates/pdf-print.css"
# API .md trees whose refentry pages become section-3 man pages.
API_DIRS = [HERE / "api/c", HERE / "api/stl"]

# Directories under docs_src/ that are machinery/data, not content.
SKIP_DIRS = {"_data", "_templates", "_migrate"}


def load_version():
    """DB_VERSION from dist/RELEASE -> e.g. '5.3.33'. Single source of truth."""
    txt = RELEASE.read_text()
    def g(k):
        m = re.search(rf"^{k}=(\d+)", txt, re.M)
        if not m:
            sys.exit(f"cannot read {k} from {RELEASE}")
        return m.group(1)
    return f"{g('DB_VERSION_MAJOR')}.{g('DB_VERSION_MINOR')}.{g('DB_VERSION_PATCH')}"


def load_site():
    with SITE_TOML.open("rb") as f:
        return tomllib.load(f)


def load_meta(d):
    """Optional per-dir _meta.toml -> {title, order:[stem,...]}. May be empty."""
    p = d / "_meta.toml"
    if not p.exists():
        return {}
    with p.open("rb") as f:
        return tomllib.load(f)


def md_files():
    for p in sorted(SRC.rglob("*.md")):
        rel = p.relative_to(SRC)
        if rel.parts and rel.parts[0] in SKIP_DIRS:
            continue
        if p.name == "PLAN.md":
            continue
        yield p


def strip_front_matter(md):
    """Pull the leading `--- ... ---` YAML block; return (meta_dict, body)."""
    if not md.startswith("---"):
        return {}, md
    end = md.find("\n---", 3)
    if end == -1:
        return {}, md
    block = md[3:end].strip()
    body = md[end + 4:].lstrip("\n")
    meta = {}
    for line in block.splitlines():
        if ":" in line:
            k, v = line.split(":", 1)
            meta[k.strip()] = v.strip().strip('"')
    return meta, body


def pandoc_md_to_html(body):
    p = subprocess.run(
        ["pandoc", "-f", "gfm", "-t", "html", "--wrap=none"],
        input=body, capture_output=True, text=True,
    )
    if p.returncode != 0:
        raise RuntimeError(f"pandoc md->html failed: {p.stderr[:500]}")
    # `.md` links point at source; the built site is HTML. Rewrite BOTH
    # same-dir (`foo.md`) and cross-tree (`../../api/c/foo.md`) targets, so the
    # path charset includes `/`. Skip absolute URLs (http:, //) -- only local
    # relative .md links become .html.
    return re.sub(r'(href="(?!\w+:|//)[A-Za-z0-9_./\-]+)\.md(#[^"]*)?"',
                  lambda m: f'{m.group(1)}.html{m.group(2) or ""}"', p.stdout)


def render_page(tmpl, ctx):
    out = tmpl
    for k, v in ctx.items():
        out = out.replace("{{" + k + "}}", v)
    return out


def crumbs_for(rel, site):
    """Simple breadcrumb: Home / <section>."""
    parts = rel.parts[:-1]
    links = ['<a href="{root}index.html">Home</a>'.format(root="../" * len(rel.parts[:-1]) or "")]
    for i, seg in enumerate(parts):
        links.append(html.escape(seg))
    return " / ".join(links) if len(links) > 1 else '<a href="index.html">Home</a>'


def build_html(version, site, tmpl):
    OUT.mkdir(parents=True, exist_ok=True)
    n = 0
    for p in md_files():
        rel = p.relative_to(SRC)
        meta, body = strip_front_matter(p.read_text(encoding="utf-8"))
        body_html = pandoc_md_to_html(body)
        depth = len(rel.parts) - 1
        root = "../" * depth
        ctx = {
            "title": html.escape(meta.get("title", rel.stem)),
            "project": html.escape(site["project"]),
            "version": version,
            "copyright": html.escape(site["copyright"]),
            "root": root,
            "crumbs": crumbs_for(rel, site),
            "content": body_html,
        }
        dest = OUT / rel.with_suffix(".html")
        dest.parent.mkdir(parents=True, exist_ok=True)
        dest.write_text(render_page(tmpl, ctx), encoding="utf-8")
        n += 1
    _copy_assets()
    return n


def _copy_assets():
    """Copy tree images to the built site. Pages reference figures bare
    (`![](deadlock.jpg)`) but migrate stored them under `<tree>/img/`, so
    flatten each `img/` into the page dir (docs-build/html/<tree>/deadlock.jpg).
    Without this the <img>/asset links dangle -- the link-check gate catches it."""
    import shutil
    for img_dir in SRC.rglob("img"):
        if not img_dir.is_dir():
            continue
        rel = img_dir.parent.relative_to(SRC)
        if rel.parts and rel.parts[0] in SKIP_DIRS:
            continue
        out_dir = OUT / rel
        out_dir.mkdir(parents=True, exist_ok=True)
        for asset in img_dir.iterdir():
            if asset.is_file():
                shutil.copy2(asset, out_dir / asset.name)


# --- Phase 3 seam: man pages (implemented). Phase 4 (PDF) stays stubbed. ---

# Man pages skip the sidebar/frameset stub pages and the tree index pages
# (those are nav, not an API entry).
MAN_SKIP_STEMS = {"frame_index", "frame_main", "index"}

_HEADING = re.compile(r"^(#{2,4}) +(.*)$", re.M)
_FIRST_SENTENCE = re.compile(r"(.+?[.!?])(?:\s|$)", re.S)


def _man_escape_name(s):
    """Plain text for a NAME/.TH field: strip md/link noise, collapse spaces."""
    s = re.sub(r"</?[a-zA-Z][^>]*>", "", s)      # raw HTML tags
    s = re.sub(r"\\([<>*_`~\[\]()])", r"\1", s)    # gfm punct escapes
    s = re.sub(r"`([^`]*)`", r"\1", s)             # inline code
    s = re.sub(r"\s+", " ", s).strip()
    return s


def _to_man_markdown(body, title):
    """Reshape an API refentry .md into man sections.

    The schema is fixed: a single `##` title heading, then the SYNOPSIS code
    block + DESCRIPTION prose, then `###` sections (Parameters/Errors/Class/
    See Also/...) with `####` sub-items. Man wants top-level `.SH`, so:
      - synthesize NAME + SYNOPSIS + DESCRIPTION from the title block,
      - promote `###` -> `#` (.SH) and `####` -> `##` (.SS).
    Returns (man_markdown, name_line_desc).
    """
    # Everything before the first `##` is discarded (there is none but the
    # title); split the title heading off.
    m = re.search(r"^## +(.*)$", body, re.M)
    intro = body[m.end():] if m else body
    # Split intro into [synopsis code + description]  vs  the first `###`.
    nxt = re.search(r"^### ", intro, re.M)
    head = intro[: nxt.start()] if nxt else intro
    rest = intro[nxt.start():] if nxt else ""

    # SYNOPSIS = the first fenced code block in head; DESCRIPTION = the prose.
    syn = ""
    cm = re.search(r"^``` ?[a-zA-Z]*\n.*?^```\s*$", head, re.S | re.M)
    if cm:
        syn = cm.group(0)
        desc = (head[: cm.start()] + head[cm.end():]).strip()
    else:
        desc = head.strip()

    # NAME one-liner: first sentence of the description, else the title.
    fs = _FIRST_SENTENCE.match(desc.lstrip())
    purpose = _man_escape_name(fs.group(1)) if fs else _man_escape_name(title)
    # Trim the boilerplate "The `X()` method " lead-in so NAME reads as a
    # purpose phrase (man convention), not a repeat of the signature.
    purpose = re.sub(r"^The\s+.+?\s+(?:method|function|class)\s+", "", purpose)
    purpose = purpose[:1].upper() + purpose[1:] if purpose else _man_escape_name(title)
    purpose = purpose[:200]

    # Promote the trailing `###`/`####` sections one level (-> .SH/.SS). But a
    # heading INDENTED inside a list item (`  ### Note`) would emit an .SS in an
    # open .RS block and unbalance mandoc's blocks — turn those into a bold
    # label paragraph so they stay inline in the list.
    rest = re.sub(r"^([ \t]+)#{2,4} +(.*)$", r"\1**\2**", rest, flags=re.M)
    rest = _HEADING.sub(lambda h: ("#" * (len(h.group(1)) - 2)) + " " + h.group(2)
                        if len(h.group(1)) >= 3 else h.group(0), rest)

    parts = [f"# NAME", f"{title} \\- {purpose}", ""]
    if syn:
        parts += ["# SYNOPSIS", syn, ""]
    if desc:
        parts += ["# DESCRIPTION", desc, ""]
    parts.append(rest)
    return "\n".join(parts).strip() + "\n", purpose


def _pandoc_md_to_man(man_md, meta):
    # Let pandoc wrap prose (default ~72 cols) so mandoc -Tlint stays quiet on
    # long-line STYLE warnings; man readers reflow anyway.
    cmd = ["pandoc", "-f", "gfm", "-t", "man",
           "--template", str(MAN_TEMPLATE)]
    for k, v in meta.items():
        cmd += ["-M", f"{k}={v}"]
    p = subprocess.run(cmd, input=man_md, capture_output=True, text=True)
    if p.returncode != 0:
        raise RuntimeError(f"pandoc md->man failed: {p.stderr[:500]}")
    return _tidy_roff(p.stdout)


def _tidy_roff(man):
    """Drop a `.PP` that immediately follows a section/subsection heading —
    pandoc emits it before tables/content and mandoc flags it ("skipping
    paragraph macro: PP after SS"). Safe: an empty paragraph after a heading is
    always droppable."""
    return re.sub(r"^(\.S[HS] [^\n]*\n)\.PP\n", r"\1", man, flags=re.M)


def _iter_api_pages():
    for d in API_DIRS:
        if not d.exists():
            continue
        for p in sorted(d.glob("*.md")):
            if p.stem in MAN_SKIP_STEMS:
                continue
            yield p


def build_man(version, site):
    """Every public-API refentry .md -> a section-3 man page, plus one
    libdb.3 overview. Output: docs-build/man/man3/. Returns the page count."""
    if not MAN_TEMPLATE.exists():
        sys.exit(f"missing man template {MAN_TEMPLATE}")
    MAN_OUT.mkdir(parents=True, exist_ok=True)
    # .TH fields: date (field 3), OS/source (field 4), manual title (field 5).
    date = site.get("man_date", "")
    source = f"{site['project']} {version}"
    n = 0
    names = []
    for p in _iter_api_pages():
        meta_fm, body = strip_front_matter(p.read_text(encoding="utf-8"))
        title = meta_fm.get("title", p.stem)
        man_md, _purpose = _to_man_markdown(body, title)
        manual = "Berkeley DB STL API" if p.parent.name == "stl" else "Berkeley DB C API"
        man = _pandoc_md_to_man(man_md, {
            "title": p.stem, "section": "3", "date": date,
            "footer": source, "header": manual,
        })
        (MAN_OUT / f"{p.stem}.3").write_text(man, encoding="utf-8")
        names.append((p.stem, title))
        n += 1
    _build_overview(version, site, names, date, source)
    n += 1
    return n


def _build_overview(version, site, names, date, source):
    """Synthesize libdb.3 from the API index + programmer_reference intro.

    NAME + a short DESCRIPTION of the library, and a SEE ALSO listing the major
    API groups (from api/c/_meta.toml's index db.md grouping) and every
    generated page. The one-line summary is pulled from existing content."""
    groups = _api_groups()
    desc = ("Berkeley DB is an embedded, transactional database library that "
            "stores key/data pairs in one of four access methods (Btree, Hash, "
            "Heap, Queue/Recno). It provides ACID transactions with "
            "write-ahead logging, fine-grained locking, and safe concurrent "
            "access from multiple threads and processes, all as a library "
            "linked directly into the application — there is no separate "
            "server process.")
    lines = [
        "# NAME",
        "libdb \\- Berkeley DB embedded database library",
        "",
        "# DESCRIPTION",
        desc,
        "",
        "# API GROUPS",
    ]
    for label, stem in groups:
        lines.append(f"**{label}** ({stem}(3))")
        lines.append("")
    lines += ["# SEE ALSO", ""]
    lines.append(", ".join(f"{s}(3)" for s, _ in sorted(names)) + ".")
    man = _pandoc_md_to_man("\n".join(lines) + "\n", {
        "title": "libdb", "section": "3", "date": date,
        "footer": source, "header": "Berkeley DB",
    })
    (MAN_OUT / "libdb.3").write_text(man, encoding="utf-8")


def _api_groups():
    """Major API handle groups, from the C API index sidebar (frame_index.md).
    Falls back to a fixed list if the sidebar is absent."""
    fb = [("Databases", "db"), ("Cursors", "dbc"), ("Key/Data Pairs", "dbt"),
          ("Environments", "env"), ("Locking", "lock"), ("Logging", "lsn"),
          ("Memory Pool", "memp"), ("Mutexes", "mutex"),
          ("Replication", "rep"), ("Sequences", "seq"),
          ("Transactions", "txn")]
    idx = HERE / "api/c/frame_index.md"
    if not idx.exists():
        return fb
    # frame_index sidebar lists each group as <a href="stem.md">Label</a>.
    groups = []
    for stem, label in re.findall(
            r'<a href="([A-Za-z0-9_]+)\.md"[^>]*>([^<]+)</a>',
            idx.read_text(encoding="utf-8")):
        if stem in {s for _, s in fb}:
            groups.append((label.strip(), stem))
    return groups or fb


# --- Phase 4 seam: per-book PDF (weasyprint HTML->PDF; no TeX toolchain). ---
#
# Each top-level "book" (the two API refs + each guide) becomes ONE PDF, its
# chapters concatenated in _meta.toml `order` (API refs have no order -> index
# first, then the rest alphabetically). We render via the SAME pandoc html we
# already produce and hand it to weasyprint, which needs no LaTeX. The title
# page + running header/footer come from _templates/pdf-print.css (CSS paged
# media), which is what a LaTeX header would otherwise supply.
#
# `articles/` has no top-level _meta.toml (it is two independent sub-books,
# inmemory + mssgtxt), so book discovery walks every dir with a _meta.toml
# that names a title -- that yields one PDF per real book, not one monster.

PDF_SKIP_STEMS = {"frame_index", "frame_main"}


def _find_books():
    """Every content dir with a _meta.toml -> (dir, meta). One book per dir."""
    books = []
    for meta_path in sorted(SRC.rglob("_meta.toml")):
        d = meta_path.parent
        rel = d.relative_to(SRC)
        if rel.parts and rel.parts[0] in SKIP_DIRS:
            continue
        meta = load_meta(d)
        if not meta.get("title"):
            continue
        books.append((d, meta))
    return books


def _book_chapters(d, meta):
    """Ordered chapter .md paths for a book. `order` pins the reading order;
    absent (API refs) -> landing/index first, then the rest alphabetically,
    minus the frameset nav stubs."""
    order = meta.get("order") or []
    if order:
        paths = [d / f"{s}.md" for s in order]
        return [p for p in paths if p.exists()]
    landing = meta.get("landing", "index.md")
    first = d / landing
    rest = [p for p in sorted(d.glob("*.md"))
            if p.name != landing and p.stem not in PDF_SKIP_STEMS]
    return ([first] if first.exists() else []) + rest


def _book_name(d):
    """Slug for the output file: guides/gsg_txn -> gsg_txn, api/c -> api_c,
    guides/articles/inmemory -> articles_inmemory."""
    return "_".join(d.relative_to(SRC).parts)


def _concat_book_md(chapters):
    body = []
    for p in chapters:
        _meta, txt = strip_front_matter(p.read_text(encoding="utf-8"))
        body.append(txt.strip())
    return "\n\n".join(body)


def _pdf_css(version, book_title):
    """Fill the {{version}}/{{book}} placeholders in the print stylesheet."""
    css = PDF_CSS.read_text(encoding="utf-8")
    return (css.replace("{{version}}", version)
               .replace("{{book}}", book_title.replace('"', "'")))


def _title_page_html(project, book_title, version, copyright_):
    return (
        '<div class="pdf-title-page">'
        f'<p class="project">{html.escape(project)}</p>'
        f'<p class="book">{html.escape(book_title)}</p>'
        f'<p class="version">Version {html.escape(version)}</p>'
        f'<p class="copyright">{html.escape(copyright_)}</p>'
        "</div>\n"
    )


def _book_html(body_md, project, book_title, version, copyright_):
    """Standalone HTML for one book: title page + pandoc-rendered chapters,
    with the print CSS inlined so weasyprint needs no external files."""
    p = subprocess.run(
        ["pandoc", "-f", "gfm", "-t", "html", "--wrap=none"],
        input=body_md, capture_output=True, text=True,
    )
    if p.returncode != 0:
        raise RuntimeError(f"pandoc md->html (pdf) failed: {p.stderr[:500]}")
    css = _pdf_css(version, book_title)
    title = _title_page_html(project, book_title, version, copyright_)
    return (
        "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">"
        f"<title>{html.escape(book_title)}</title>"
        f"<style>{css}</style></head><body>\n{title}{p.stdout}\n</body></html>\n"
    )


def _weasyprint(html_text, dest):
    """HTML string -> PDF file via the weasyprint CLI (stdin '-')."""
    p = subprocess.run(
        ["weasyprint", "-q", "-", str(dest)],
        input=html_text, capture_output=True, text=True,
    )
    if p.returncode != 0:
        raise RuntimeError(f"weasyprint failed for {dest.name}: {p.stderr[:500]}")


def build_pdf(version, site):
    """Render every book (2 API refs + each guide) to docs-build/pdf/<book>.pdf.
    Returns a list of (name, dest) for the caller to report / page-count.
    Requires `pandoc` and `weasyprint` on PATH."""
    if not PDF_CSS.exists():
        sys.exit(f"missing pdf css {PDF_CSS}")
    PDF_OUT.mkdir(parents=True, exist_ok=True)
    project, copyright_ = site["project"], site["copyright"]
    built = []
    for d, meta in _find_books():
        chapters = _book_chapters(d, meta)
        if not chapters:
            continue
        book_title = meta["title"]
        body_md = _concat_book_md(chapters)
        html_text = _book_html(body_md, project, book_title, version, copyright_)
        dest = PDF_OUT / f"{_book_name(d)}.pdf"
        _weasyprint(html_text, dest)
        built.append((_book_name(d), dest, len(chapters)))
    return built


def _selfcheck():
    """Guard the md->man reshape: NAME/SYNOPSIS/DESCRIPTION split, heading
    promotion, and in-list heading demotion."""
    body = ("## DB->foo()\n\n``` c\nint DB->foo(void);\n```\n\n"
            "The DB->foo() method does a thing. More prose.\n\n"
            "### Parameters\n\n#### bar\n\nThe bar param.\n\n"
            "  ### Note\n\n  An in-list note.\n\n### See Also\n\nx\n")
    mm, purpose = _to_man_markdown(body, "DB->foo()")
    assert purpose.lower().startswith("does a thing"), purpose
    assert "# NAME" in mm and "# SYNOPSIS" in mm and "# DESCRIPTION" in mm
    assert "# Parameters" in mm and "## bar" in mm      # ### -> #, #### -> ##
    assert "# See Also" in mm
    assert "**Note**" in mm and "### Note" not in mm    # in-list heading demoted
    assert "int DB->foo(void);" in mm                   # synopsis kept
    # roff tidy drops PP right after a heading
    assert _tidy_roff(".SS X\n.PP\n.TS\n") == ".SS X\n.TS\n"
    # PDF book discovery: every book has a title + at least one chapter, and
    # articles is TWO sub-books (not one), so no monster merge.
    books = _find_books()
    names = {_book_name(d) for d, _ in books}
    assert "api_c" in names and "api_stl" in names, names
    assert "guides_articles_inmemory" in names, names
    assert "guides_articles_mssgtxt" in names, names
    assert "guides_articles" not in names, "articles must not be one merged book"
    for d, meta in books:
        assert meta.get("title"), d
        assert _book_chapters(d, meta), f"no chapters for {d}"
    # ordered book uses `order`; unordered (api) puts landing first.
    prd = SRC / "guides/programmer_reference"
    ch = _book_chapters(prd, load_meta(prd))
    assert ch[0].stem == "preface", ch[0]
    apic = SRC / "api/c"
    assert _book_chapters(apic, load_meta(apic))[0].stem == "index"
    # title page HTML carries project + version.
    tp = _title_page_html("Berkeley DB", "C API", "5.3.33", "(c) X")
    assert "Version 5.3.33" in tp and "Berkeley DB" in tp and "C API" in tp
    # cross-tree .md links rewrite to .html; absolute URLs are left alone.
    got = re.sub(r'(href="(?!\w+:|//)[A-Za-z0-9_./\-]+)\.md(#[^"]*)?"',
                 lambda m: f'{m.group(1)}.html{m.group(2) or ""}"',
                 'a href="../../api/c/env.md#x" b href="foo.md" c href="http://x/y.md"')
    assert '../../api/c/env.html#x' in got and 'foo.html' in got
    assert 'http://x/y.md' in got, "absolute .md URL must not be rewritten"
    print("selfcheck ok")


def main(build_pdf_too=True):
    if not TEMPLATE.exists():
        sys.exit(f"missing template {TEMPLATE}")
    version = load_version()
    site = load_site()
    tmpl = TEMPLATE.read_text()
    n = build_html(version, site, tmpl)
    print(f"built {n} HTML pages -> {OUT}  (version {version})")
    m = build_man(version, site)
    print(f"built {m} man pages -> {MAN_OUT}  (version {version})")
    if build_pdf_too:
        books = build_pdf(version, site)
        print(f"built {len(books)} PDF books -> {PDF_OUT}  (version {version})")
        for name, dest, nch in books:
            print(f"  {name:24s} {nch:4d} chapters  {dest.stat().st_size:>9d} bytes")


if __name__ == "__main__":
    if "--selfcheck" in sys.argv:
        _selfcheck()
    else:
        main(build_pdf_too="--no-pdf" not in sys.argv)
