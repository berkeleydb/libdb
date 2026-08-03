#!/usr/bin/env python3
"""ninja bench: build the test/bench microbenchmark drivers against the Meson
libdb. The bench Makefile targets the Autoconf layout (.libs/libdb-5.3), so
Meson can't reuse it -- compile each driver here against the Meson build dir
(generated db.h + libdb.so both live in builddir/dist/).

Usage: run_bench.py <project_source_root> <dist_build_dir>
"""
import os
import subprocess
import sys
from pathlib import Path

src_root = Path(sys.argv[1])
build_dir = Path(sys.argv[2])          # builddir/dist -- holds db.h + libdb.so
bench = src_root / "test" / "bench"
cc = os.environ.get("CC", "cc")

drivers = ["scale_bench", "lock_bench", "ssi_abort_bench",
           "tproc_c", "tproc_b", "tproc_h"]
rc = 0
for d in drivers:
    out = bench / d
    cmd = [cc, "-O2", "-pthread", f"-I{build_dir}", f"-I{bench}",
           str(bench / f"{d}.c"),
           f"-L{build_dir}", "-ldb", f"-Wl,-rpath,{build_dir}",
           "-o", str(out)]
    print("  " + " ".join(cmd))
    p = subprocess.run(cmd)
    if p.returncode != 0:
        rc = p.returncode
    else:
        print(f"  built {out}")
sys.exit(rc)
