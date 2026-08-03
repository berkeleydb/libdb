#!/usr/bin/env python3
"""Run the no-loss gate (verify.py) over EVERY migrated tree.

The source(HTML)->dest(MD) pairs are the authoritative migration map (the same
ones phase 2 migrated); keeping them here means CI calls one command instead of
duplicating the list in YAML. Exits non-zero if ANY tree drops content (a hard
code/sub-section drop, or word retention below --threshold), so CI gates on it.

Usage:  verify_all.py [--threshold 0.97]
"""
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
VERIFY = HERE / "verify.py"

# (old HTML dir, new MD dir) relative to REPO. Guides that migrated the C
# variant point at the C/ subdir; articles is two independent sub-books.
PAIRS = [
    ("docs/api_reference/C", "docs_src/api/c"),
    ("docs/api_reference/STL", "docs_src/api/stl"),
    ("docs/programmer_reference", "docs_src/guides/programmer_reference"),
    ("docs/upgrading", "docs_src/guides/upgrading"),
    ("docs/installation", "docs_src/guides/installation"),
    ("docs/porting", "docs_src/guides/porting"),
    ("docs/gsg/C", "docs_src/guides/gsg"),
    ("docs/gsg_txn/C", "docs_src/guides/gsg_txn"),
    ("docs/gsg_db_rep/C", "docs_src/guides/gsg_db_rep"),
    ("docs/collections/tutorial", "docs_src/guides/collections"),
    ("docs/bdb-sql", "docs_src/guides/bdb-sql"),
    ("docs/articles/inmemory/C", "docs_src/guides/articles/inmemory"),
    ("docs/articles/mssgtxt", "docs_src/guides/articles/mssgtxt"),
]


def main():
    thr = ["--threshold", sys.argv[sys.argv.index("--threshold") + 1]] \
        if "--threshold" in sys.argv else []
    failed = []
    for old, new in PAIRS:
        p = subprocess.run(
            [sys.executable, str(VERIFY), str(REPO / old), str(REPO / new), *thr],
            capture_output=True, text=True,
        )
        ret = next((l for l in p.stdout.splitlines()
                    if "mean word retention" in l), "?")
        status = "OK" if p.returncode == 0 else "FAIL"
        print(f"{old:38s} {ret.replace('mean word retention: ', ''):>8s}  {status}")
        if p.returncode != 0:
            failed.append(old)
            # surface the hard-drop detail so CI logs show WHAT was lost.
            for l in p.stdout.splitlines():
                if "HARD DROP" in l or l.strip().startswith(("code", "sub-section")):
                    print(f"    {l.strip()}")
    if failed:
        print(f"\nNO-LOSS GATE FAIL: {len(failed)} tree(s) dropped content: "
              f"{', '.join(failed)}")
        sys.exit(1)
    print(f"\nNO-LOSS GATE PASS: all {len(PAIRS)} trees retained (0 hard drops).")


if __name__ == "__main__":
    main()
