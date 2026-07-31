#!/usr/bin/env python3
"""libdb docs generator: Markdown source -> HTML (and, later, PDF + man).

- Version is read LIVE from dist/RELEASE (never hard-coded).
- Boilerplate (header/footer/version/copyright) is injected ONCE from the
  shared template + site.toml, replacing the old per-page duplication.
- Nav/index come from per-directory _meta.toml (falls back to a flat listing).

PDF and man outputs are Phase 3/4 — the seams are stubbed below (build_pdf,
build_man) so a follow-up wires pandoc without reshaping this file.

Usage:  build.py            # build HTML into docs-build/html
        build.py --serve    # (not implemented) placeholder for a preview seam
Requires: pandoc on PATH (run under `nix shell nixpkgs#pandoc`).
"""
import html
import re
import subprocess
import sys
import tomllib
from pathlib import Path

HERE = Path(__file__).resolve().parent          # docs-src/
REPO = HERE.parent
SRC = HERE
OUT = REPO / "docs-build/html"
TEMPLATE = HERE / "_templates/page.html.tmpl"
SITE_TOML = HERE / "_data/site.toml"
RELEASE = REPO / "dist/RELEASE"

# Directories under docs-src/ that are machinery/data, not content.
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
    # `.md` links point at source; the built site is HTML.
    return re.sub(r'(href="[A-Za-z0-9_.\-]+)\.md(#[^"]*)?"',
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
    return n


# --- Phase 3/4 seams: implemented later, kept here so the shape is fixed. ---
def build_man(version, site):
    """TODO(phase-3): per-API .md -> section-3 man page via pandoc -t man,
    plus one libdb.3 overview from the API index. Not built this phase."""
    return 0


def build_pdf(version, site):
    """TODO(phase-4): pandoc per book (api_reference, GSGs, programmer_reference)
    with a shared LaTeX header. Not built this phase."""
    return 0


def main():
    if not TEMPLATE.exists():
        sys.exit(f"missing template {TEMPLATE}")
    version = load_version()
    site = load_site()
    tmpl = TEMPLATE.read_text()
    n = build_html(version, site, tmpl)
    print(f"built {n} HTML pages -> {OUT}  (version {version})")


if __name__ == "__main__":
    main()
