#!/usr/bin/env python3
"""ninja docs / ninja docs-check: drive docs_src/build.py (and, for check, the
docs.yml gates) from the Meson build. Mirrors the Autoconf `make docs` /
`make docs-check` targets. Guards on pandoc: skips with a note if absent.

Usage: run_docs.py build|check <project_source_root>
"""
import shutil
import subprocess
import sys
from pathlib import Path

mode = sys.argv[1]
root = Path(sys.argv[2])
docs_src = root / "docs_src"
migrate = docs_src / "_migrate"


def have(tool):
    return shutil.which(tool) is not None


if not have("pandoc"):
    print("SKIP docs: need pandoc on PATH (nix develop provides it).")
    sys.exit(0)


def run(args, **kw):
    print("  " + " ".join(str(a) for a in args))
    return subprocess.run(args, **kw)


py = sys.executable
if mode == "build":
    extra = [] if have("weasyprint") else ["--no-pdf"]
    if not have("weasyprint"):
        print("(weasyprint not found: HTML + man only, no PDF)")
    sys.exit(run([py, "build.py", *extra], cwd=docs_src).returncode)

# check: build + self-check + completeness + spelling + man-lint
steps = [
    ([py, "build.py", "--selfcheck"], docs_src),
    ([py, "build.py", "--no-pdf"], docs_src),
    ([py, str(migrate / "man_coverage.py"), "--ci"], root),
    ([py, str(migrate / "spellcheck.py")], root),
]
for args, cwd in steps:
    if run(args, cwd=cwd).returncode != 0:
        sys.exit(1)

if have("mandoc"):
    bad = 0
    for f in sorted((root / "docs-build/man/man3").glob("*.3")):
        out = subprocess.run(["mandoc", "-Tlint", str(f)],
                             capture_output=True, text=True)
        if "ERROR" in (out.stdout + out.stderr):
            print(f"ERRORS in {f}")
            bad += 1
    if bad:
        sys.exit(1)
else:
    print("(mandoc not found: skipping man-lint)")
print("docs-check: PASS")
