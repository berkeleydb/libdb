#!/usr/bin/env python3
"""
xe_parse_logs.py -- build the results TSV from the per-run LOG FILES rather
than from a shell-emitted TSV.

WHY THIS EXISTS.  The driver also streams a TSV, and on one run that TSV came
out with literal "t" and "n" where the tabs and newlines should have been: the
printf format strings lost their backslashes passing through an ssh command
line.  The data was still recoverable, but a results file whose field separator
is the letter "t" is one bad character away from silently mis-parsing values
that contain a "t".

The per-run logs are the primary artifact and always were: each one holds the
full VERDICT line, the per-transaction latency table, the IOSTAT line, the
steady-state determination and the OPTREAD counters.  Parsing those directly
removes the shell from the data path entirely, so quoting bugs in a driver
cannot corrupt results.  The driver's TSV is now a convenience; this is the
source of truth.

usage: xe_parse_logs.py /nvme/results '*.log' > results.tsv
"""
import glob
import os
import re
import sys


def parse(path):
    """Return a list of (workload, arm, engine, am, aio, threads, rep, metric, value)."""
    base = os.path.basename(path)
    # c_libdb-sync-btree_t8_r3.log
    m = re.match(r"^([ch])_(.+)_t(\d+)_r(\d+)\.log$", base)
    if not m:
        return []
    wl, arm, threads, rep = m.group(1), m.group(2), m.group(3), m.group(4)
    engine = "wt" if arm.startswith("wt") else "libdb"
    am = arm.rsplit("-", 1)[-1]
    aio = "1" if "uring" in arm else "0"
    out = []

    def row(metric, value):
        out.append((wl, arm, engine, am, aio, threads, rep, metric, str(value)))

    txt = open(path, errors="replace").read()

    # ---- provenance guard ----------------------------------------------
    # Record the scale/pad/cache the run ACTUALLY used, so a log from a
    # different experiment cannot be silently averaged into this one.
    #
    # Not hypothetical: three in-cache smoke logs (scale=5, pad=200, 1 GiB
    # cache) sat in the results directory beside the campaign's out-of-cache
    # logs (scale=10481, pad=1024, 8 GiB cache) and produced a phantom "t=2"
    # row reporting 17,308 txn/s next to the real 248 txn/s -- a 70x gap that
    # read as a thread-count effect and was actually a different experiment.
    cm = re.search(r"^# arm=\S+ .*?scale=(\d+) pad=(\d+) cache=(\d+)MB",
                   txt, re.M)
    if cm:
        row("cfg_scale", cm.group(1))
        row("cfg_pad", cm.group(2))
        row("cfg_cache_mb", cm.group(3))

    # ---- the verdict line: the throughput numbers -----------------------
    vm = re.search(r"^VERDICT (?:tproc-c|tproc-h) (.+)$", txt, re.M)
    if vm:
        for k, v in re.findall(r"(\w+)=([-\d.]+)", vm.group(1)):
            if k in ("txn_per_sec", "tpmC_like", "committed", "data_gib",
                     "elapsed", "queries_per_sec", "rows_per_sec",
                     "updates_per_sec", "ops"):
                row(k, v)
    else:
        # A run with no VERDICT line is a FAILED run, recorded as such.  Never
        # counted as a pass: rc=0 with no throughput number is the vacuous-green
        # pattern this project has nine recorded instances of.
        row("NO_VERDICT", 0)

    # ---- per-transaction latency (TPROC-C: TXN, TPROC-H: OP) ------------
    for mm in re.finditer(
            r"^TXN\s+(\S+)\s+(\d+)\s+(\d+)\s+(\d+)\s+([\d.]+)\s+([\d.]+)\s+([\d.]+)",
            txt, re.M):
        name = mm.group(1)
        row(f"committed_{name}", mm.group(2))
        row(f"retry_{name}", mm.group(3))
        row(f"err_{name}", mm.group(4))
        row(f"p50_{name}", mm.group(5))
        row(f"p99_{name}", mm.group(6))
        row(f"p999_{name}", mm.group(7))
    for mm in re.finditer(
            r"^OP\s+(\S+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+([\d.]+)\s+([\d.]+)\s+([\d.]+)",
            txt, re.M):
        name = mm.group(1)
        row(f"completed_{name}", mm.group(2))
        row(f"na_{name}", mm.group(5))
        row(f"p50_{name}", mm.group(6))
        row(f"p99_{name}", mm.group(7))
        row(f"p999_{name}", mm.group(8))
    # N/A rows print dashes; record the N/A count so the report can state it.
    for mm in re.finditer(r"^OP\s+(\S+)\s+-\s+-\s+-\s+(\d+)", txt, re.M):
        row(f"na_{mm.group(1)}", mm.group(2))

    # ---- out-of-cache evidence -----------------------------------------
    im = re.search(r"^IOSTAT \S+ (.+)$", txt, re.M)
    if im:
        for k, v in re.findall(r"(\w+)=([\d.]+)", im.group(1)):
            row(f"io_{k}", v)

    # ---- steady state, optread engagement, hash-delivery honesty -------
    sm = re.search(r"^# steady_state=(\S+)", txt, re.M)
    row("steady_state", sm.group(1) if sm else "unknown")

    om = re.search(r"^OPTREAD \S+ (.+)$", txt, re.M)
    if om:
        for k, v in re.findall(r"(\w+)=(\w+)", om.group(1)):
            row(f"optread_{k}", v)

    hm = re.search(r"^HASHDELIV \S+ (.+)$", txt, re.M)
    if hm:
        for k, v in re.findall(r"(\w+)=([\d.]+)", hm.group(1)):
            row(f"hashdeliv_{k}", v)

    return out


def main():
    d = sys.argv[1] if len(sys.argv) > 1 else "."
    pat = sys.argv[2] if len(sys.argv) > 2 else "*.log"
    files = sorted(glob.glob(os.path.join(d, pat)))
    print("workload\tarm\tengine\tam\taio\tthreads\trep\tmetric\tvalue")
    n = 0
    for f in files:
        for r in parse(f):
            print("\t".join(r))
            n += 1
    print(f"# parsed {len(files)} logs -> {n} rows", file=sys.stderr)


if __name__ == "__main__":
    main()
