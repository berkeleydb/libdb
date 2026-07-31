#!/usr/bin/env python3
"""One-time reverse-DocBook extractor for the C API reference tree.

The DocBook XML source is lost; only DocBook-XSL-rendered HTML survives. Every
page has the SAME stable shape (verified across the tree):

    <body>
      <div class="navheader"> ... Library Version 11.2.5.3, Prev/Next ... </div>
      <div class="sect1|chapter|book|preface|appendix"> ... real content ... </div>
      <div class="navfooter"> ... Prev/Up/Next/Home ... </div>
    </body>

So recovering clean content = keep the middle div, drop the boilerplate
siblings (recognized by their stable DocBook classes), hand the inner HTML to
pandoc (html -> gfm), then a small cleanup pass that:
  - writes a YAML front-matter block (title, api-name, source),
  - rewrites internal `foo.html` cross-links to the new `foo.md`/page scheme,
  - keeps programlisting code fences and parameter/errors sections verbatim.

Usage:  extract.py [SRC_HTML_DIR] [OUT_MD_DIR]
Defaults: docs/api_reference/C  ->  docs-src/api/c
Requires: pandoc on PATH (run under `nix shell nixpkgs#pandoc`).
"""
import html.parser
import re
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
SRC = Path(sys.argv[1]) if len(sys.argv) > 1 else REPO / "docs/api_reference/C"
OUT = Path(sys.argv[2]) if len(sys.argv) > 2 else REPO / "docs-src/api/c"

# DocBook classes whose top-level <div> IS the real content.
CONTENT_CLASSES = {"sect1", "chapter", "book", "preface", "appendix",
                   "article", "part", "refentry", "index"}


class BodyExtractor(html.parser.HTMLParser):
    """Capture the inner HTML of the single content <div> under <body>,
    skipping the navheader/navfooter boilerplate siblings entirely."""

    def __init__(self):
        super().__init__(convert_charrefs=False)
        self.in_body = False
        self.depth = 0            # nesting depth inside <body>
        self.capture = False      # currently inside the content div
        self.cap_depth = 0        # depth at which capture started
        self.buf = []
        self.title = None
        self._in_title = False

    def handle_starttag(self, tag, attrs):
        ad = dict(attrs)
        if tag == "title":
            self._in_title = True
            return
        if tag == "body":
            self.in_body = True
            self.depth = 0
            return
        if not self.in_body:
            return
        if not self.capture and self.depth == 0 and tag == "div":
            cls = (ad.get("class") or "").split()
            if any(c in CONTENT_CLASSES for c in cls):
                self.capture = True
                self.cap_depth = self.depth
                self.depth += 1
                return  # drop the wrapper div itself; keep its children
        if self.capture:
            self.buf.append(self._fmt_start(tag, attrs))
        self.depth += 1

    def handle_startendtag(self, tag, attrs):
        if self.capture:
            self.buf.append(self._fmt_start(tag, attrs, self_closing=True))

    def handle_endtag(self, tag):
        if tag == "title":
            self._in_title = False
            return
        if tag == "body":
            self.in_body = False
            return
        if not self.in_body:
            return
        self.depth -= 1
        if self.capture and self.depth == self.cap_depth:
            self.capture = False  # closed the content div
            return
        if self.capture:
            self.buf.append(f"</{tag}>")

    def handle_data(self, data):
        if self._in_title:
            self.title = (self.title or "") + data
        elif self.capture:
            self.buf.append(data)

    def handle_entityref(self, name):
        if self._in_title:
            self.title = (self.title or "") + f"&{name};"
        elif self.capture:
            self.buf.append(f"&{name};")

    def handle_charref(self, name):
        if self._in_title:
            self.title = (self.title or "") + f"&#{name};"
        elif self.capture:
            self.buf.append(f"&#{name};")

    @staticmethod
    def _fmt_start(tag, attrs, self_closing=False):
        a = "".join(f' {k}="{v}"' if v is not None else f" {k}" for k, v in attrs)
        return f"<{tag}{a}{' /' if self_closing else ''}>"

    def inner_html(self):
        return "".join(self.buf)


# Wrapper <div>s that carry no content — just DocBook layout scaffolding. We
# unwrap them (drop the tags, keep children) so pandoc doesn't emit a wall of
# empty `<div>` noise. Headings inside survive and carry the structure.
UNWRAP_CLASSES = ("titlepage", "sect1", "sect2", "sect3", "itemizedlist",
                  "note", "navtable", "variablelist", "informaltable", "book",
                  "chapter", "preface", "appendix", "article")


def preprocess_html(inner):
    """Drop layout-only wrapper divs and normalize inline spans BEFORE pandoc,
    using the stable DocBook classes. Content and headings are untouched."""
    # Remove opening tags of layout wrapper divs (any attr order); their
    # matching </div> we remove blindly below by re-balancing is overkill —
    # instead just strip the class so pandoc treats them as plain divs, which
    # gfm output renders as nothing. Simpler: delete the whole opening div tag
    # for these classes and delete a matching number of </div>. DocBook nests
    # titlepage 3 divs deep, so we take the robust route: strip *all* <div ...>
    # and </div> tags — the API schema is fully carried by <h2..h4>, <p>, <pre>,
    # <ul>, <table>, so divs contribute no semantics here.
    inner = re.sub(r"<div\b[^>]*>", "", inner)
    inner = inner.replace("</div>", "")
    # Bold spans -> <strong>; drop redundant nested <strong> pandoc would double.
    inner = re.sub(r'<span class="bold">\s*<strong>(.*?)</strong>\s*</span>',
                   r"<strong>\1</strong>", inner, flags=re.S)
    inner = re.sub(r'<span class="bold">(.*?)</span>', r"<strong>\1</strong>",
                   inner, flags=re.S)
    return inner


def pandoc_html_to_gfm(inner_html):
    p = subprocess.run(
        ["pandoc", "-f", "html", "-t", "gfm", "--wrap=none"],
        input=inner_html, capture_output=True, text=True,
    )
    if p.returncode != 0:
        raise RuntimeError(f"pandoc failed: {p.stderr[:500]}")
    return p.stdout


# Rewrite internal cross-links: `dbget.html` -> `dbget.md`, `dbget.html#frag`
# -> `dbget.md#frag`. Leave ../-relative links to OTHER trees as-is for now
# (those trees are follow-up phases); they still resolve against docs/ layout.
LINK_RE = re.compile(r"(\]\()([A-Za-z0-9_.\-]+)\.html(#[^)]*)?\)")


def rewrite_links(md):
    return LINK_RE.sub(lambda m: f"{m.group(1)}{m.group(2)}.md{m.group(3) or ''})", md)


# Any remaining raw <a href="foo.html..."> that pandoc emitted (olinks/xrefs it
# couldn't turn into Markdown) — rewrite the local .html to .md too.
HTML_LINK_RE = re.compile(r'(<a\b[^>]*href=")([A-Za-z0-9_.\-]+)\.html(#[^"]*)?"')


def cleanup(md, title, api_name, source_rel):
    md = rewrite_links(md)
    md = HTML_LINK_RE.sub(
        lambda m: f'{m.group(1)}{m.group(2)}.md{m.group(3) or ""}"', md)
    # Tag DocBook programlisting fences as C for highlighting.
    md = md.replace("``` programlisting", "``` c")
    # DocBook leaves empty anchor targets like `<span id="idp...">`; drop them
    # and any stray empty spans/divs pandoc preserved.
    md = re.sub(r"<span id=\"[^\"]*\"></span>\s*", "", md)
    md = re.sub(r"^#+\s*$", "", md, flags=re.M)  # empty headings
    md = re.sub(r"\n{3,}", "\n\n", md).strip()
    fm = [
        "---",
        f'title: "{title}"',
        f'api-name: "{api_name}"',
        f"source: {source_rel}",
        "---",
        "",
    ]
    return "\n".join(fm) + md + "\n"


def clean_title(t):
    """Unescape the HTML title. DocBook titles look like `DB-&gt;get()`."""
    import html as _h
    return _h.unescape(t).strip()


def extract_one(path):
    raw = path.read_text(encoding="utf-8", errors="replace")
    ex = BodyExtractor()
    ex.feed(raw)
    inner = preprocess_html(ex.inner_html())
    title = clean_title(ex.title or path.stem)
    md = pandoc_html_to_gfm(inner)
    md = cleanup(md, title, title, f"docs/api_reference/C/{path.name}")
    return md, title


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    files = sorted(SRC.glob("*.html"))
    if not files:
        sys.exit(f"no .html under {SRC}")
    ok = 0
    for f in files:
        try:
            md, title = extract_one(f)
        except Exception as e:  # noqa: BLE001 - report and continue
            print(f"FAIL {f.name}: {e}", file=sys.stderr)
            continue
        (OUT / f"{f.stem}.md").write_text(md, encoding="utf-8")
        ok += 1
    print(f"extracted {ok}/{len(files)} pages -> {OUT}")


if __name__ == "__main__":
    main()
