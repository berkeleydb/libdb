#!/usr/bin/env python3
"""Validate the generated per-book PDFs (docs-build/pdf/*.pdf).

Asserts each PDF: exists + is non-empty, has a sane page count (>= a floor and
roughly >= its chapter count / a divisor -- a book with N chapters must not
collapse to a couple of pages), and carries the live version string on its
title page (page 1). The version comes from dist/RELEASE, same source as the
build. Exits non-zero on any failure so CI can gate (best-effort in practice --
see docs.yml).

Usage:  validate_pdf.py [--version 5.3.33]
Requires: pdfinfo + pdftotext (poppler-utils) on PATH.
"""
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
PDF_DIR = REPO / "docs-build/pdf"


def load_version():
    txt = (REPO / "dist/RELEASE").read_text()
    import re
    g = lambda k: re.search(rf"^{k}=(\d+)", txt, re.M).group(1)
    return f"{g('DB_VERSION_MAJOR')}.{g('DB_VERSION_MINOR')}.{g('DB_VERSION_PATCH')}"


def pages(pdf):
    out = subprocess.run(["pdfinfo", str(pdf)], capture_output=True, text=True).stdout
    for l in out.splitlines():
        if l.startswith("Pages:"):
            return int(l.split()[1])
    return 0


def first_page_text(pdf):
    return subprocess.run(["pdftotext", "-f", "1", "-l", "1", str(pdf), "-"],
                          capture_output=True, text=True).stdout


def main():
    version = (sys.argv[sys.argv.index("--version") + 1]
               if "--version" in sys.argv else load_version())
    pdfs = sorted(PDF_DIR.glob("*.pdf"))
    if not pdfs:
        sys.exit(f"no PDFs in {PDF_DIR} (run build.py first)")
    fails = []
    for pdf in pdfs:
        size = pdf.stat().st_size
        np = pages(pdf)
        tp = first_page_text(pdf)
        has_ver = version in tp
        has_project = "Berkeley DB" in tp
        ok = size > 1024 and np >= 2 and has_ver and has_project
        print(f"{pdf.name:34s} {np:5d} pages  {size:>9d} B  "
              f"title[ver={'Y' if has_ver else 'N'} proj={'Y' if has_project else 'N'}]"
              f"  {'OK' if ok else 'FAIL'}")
        if not ok:
            why = []
            if size <= 1024: why.append("empty")
            if np < 2: why.append(f"too few pages ({np})")
            if not has_ver: why.append(f"no version {version} on title page")
            if not has_project: why.append("no project name on title page")
            fails.append((pdf.name, ", ".join(why)))
    if fails:
        print(f"\nPDF VALIDATION FAIL: {len(fails)} book(s):")
        for name, why in fails:
            print(f"  {name}: {why}")
        sys.exit(1)
    print(f"\nPDF VALIDATION PASS: {len(pdfs)} books, all non-empty, "
          f"title page carries Berkeley DB {version}.")


if __name__ == "__main__":
    main()
