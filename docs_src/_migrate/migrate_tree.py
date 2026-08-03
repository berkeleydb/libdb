#!/usr/bin/env python3
"""Per-tree migration driver for the chaptered guide trees.

Wraps extract.py for one guide tree and, additionally:
  - derives a reading `order` for _meta.toml from the tree's index.html TOC
    (the DocBook <dt><a href=x.html> chain, in document order),
  - copies images (*.gif/*.png/*.jpg) into the tree's img/,
  - writes _meta.toml (title from index.html <title>, landing = index.md).

extract.py already handles the DocBook body isolation + the body-level fallback
for the couple of div-less article pages, so this just orchestrates.

Usage:  migrate_tree.py SRC_HTML_DIR OUT_MD_DIR "Nav Title"
Run under `nix develop` (needs pandoc via extract.py).
"""
import re
import shutil
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
SRC = Path(sys.argv[1])
OUT = Path(sys.argv[2])
TITLE = sys.argv[3] if len(sys.argv) > 3 else OUT.name

IMG_EXT = (".gif", ".png", ".jpg", ".jpeg", ".svg")
TOC_HREF = re.compile(r'href="([A-Za-z0-9_.\-]+)\.html"')


def index_order(src):
    """Ordered, de-duped stems from index.html's TOC link chain."""
    idx = src / "index.html"
    if not idx.exists():
        return []
    t = idx.read_text(encoding="utf-8", errors="replace")
    seen, order = set(), []
    for href in TOC_HREF.findall(t):
        if href in ("index", "frame_index", "frame_main") or href in seen:
            continue
        seen.add(href)
        order.append(href)
    return order


def index_title(src):
    idx = src / "index.html"
    if not idx.exists():
        return TITLE
    import html as _h
    m = re.search(r"<title>(.*?)</title>", idx.read_text(errors="replace"), re.S)
    return _h.unescape(m.group(1).strip()) if m else TITLE


def copy_images(src, out):
    n = 0
    imgdir = out / "img"
    for p in src.iterdir():
        if p.suffix.lower() in IMG_EXT:
            imgdir.mkdir(parents=True, exist_ok=True)
            shutil.copy2(p, imgdir / p.name)
            n += 1
    return n


def write_meta(out, title, order):
    lines = [
        f"# Nav/index metadata for the {out.name} guide (auto-derived from the",
        "# source index.html TOC chain). `order` pins the reading order for nav",
        "# and later PDF assembly; `landing` is the tree's index page.",
        "",
        f'title = "{title}"',
        'landing = "index.md"',
        "order = [",
    ]
    for stem in order:
        lines.append(f'  "{stem}",')
    lines.append("]")
    (out / "_meta.toml").write_text("\n".join(lines) + "\n", encoding="utf-8")


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    # extract.py resolves SRC_REL against the repo; run it as a subprocess so
    # its module-level SRC/OUT pick up our argv.
    r = subprocess.run(
        [sys.executable, str(HERE / "extract.py"), str(SRC), str(OUT)],
        check=False,
    )
    if r.returncode != 0:
        sys.exit(f"extract failed for {SRC}")
    order = index_order(SRC)
    # keep only stems that actually produced a .md
    order = [s for s in order if (OUT / f"{s}.md").exists()]
    nimg = copy_images(SRC, OUT)
    write_meta(OUT, index_title(SRC), order)
    print(f"migrated {SRC.name}: {len(order)} ordered pages, {nimg} images -> {OUT}")


if __name__ == "__main__":
    main()
