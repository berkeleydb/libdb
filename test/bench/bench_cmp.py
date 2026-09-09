#!/usr/bin/env python3
"""bench_cmp.py -- noise-floor report and regression gate for test/bench.

Two modes, both fed by run_bench.sh output (TSV, one row per
benchmark/config/threads/rep):

  bench_cmp.py --noise results.tsv
        Per-case min/median/max and coefficient of variation across the reps
        in ONE file, plus the tolerance that spread implies and whether the
        case is usable in the gate at all.  Run this first on any new machine.

  bench_cmp.py baseline.tsv candidate.tsv
        Per-case delta of candidate vs baseline medians with a pass/fail
        verdict.  Exit status 0 = no regression, 1 = regression, 2 = usage or
        input error.  This is the gate.

WHY THE TOLERANCE IS DERIVED, NOT TYPED IN
------------------------------------------
Every tolerance here comes from the baseline file's own rep-to-rep spread:

    tolerance_pct = clip(TOL_SIGMA * CV_baseline, TOL_FLOOR_PCT, ...)

so it cannot be re-tightened by guess -- tightening it requires producing a
quieter baseline, which is the only honest way to tighten a threshold.  A case
whose own CV exceeds CV_EXCLUDE_PCT is EXCLUDED from the verdict rather than
gated with a tolerance so wide it would never fire: a check that cannot
distinguish signal from its own noise is worse than no check, because a gate
that cries wolf gets switched off.  Excluded cases are still printed.

Only throughput metrics (higher is better) exist today, so a regression is
always "median fell by more than the tolerance".
"""

import argparse
import statistics
import sys

# TOL_SIGMA: how many "CVs" of headroom the gate allows.  3 is the usual
# 3-sigma convention; with >= 5 reps the median's own spread is well under one
# CV, so 3*CV leaves room for a slower/quieter day without hiding a real
# double-digit regression.
TOL_SIGMA = 3.0

# TOL_FLOOR_PCT: minimum tolerance regardless of how quiet the baseline was.
# A perfectly quiet baseline (CV ~ 0.5%) would otherwise imply a 1.5% gate,
# which no cross-build comparison survives -- recompiling the same source with
# a different linker order moves these numbers by a few percent.
TOL_FLOOR_PCT = 5.0

# CV_EXCLUDE_PCT: above this, a case is reported but not gated.  Set from the
# measured spread on the baseline hardware (see NOISE.md): at 10% the widest
# surviving tolerance is ~30%, which still catches a halving; the three cases
# above it would have been gated at 45-60%, which is not a gate at all.
CV_EXCLUDE_PCT = 10.0

# MIN_MEDIAN_GATE: a metric whose median is below this is excluded regardless
# of its CV, because its resolution -- not its stability -- limits it.  Under a
# 10 s window a value of, say, 0.5 queries/s is 5 completed queries: one more
# or fewer moves it 20%, and five identical reps report CV 0.00% purely because
# the count quantised the same way each time.  A tolerance derived from that CV
# would be far tighter than the metric can actually resolve.
MIN_MEDIAN_GATE = 100.0

KEY_COLS = ("benchmark", "config", "threads")


def parse(path):
    """Return (meta, samples) where samples maps key -> {rep: value}."""
    meta, samples, units, hdr, errs = {}, {}, {}, None, []
    with open(path) as fh:
        for line in fh:
            line = line.rstrip("\n")
            if not line:
                continue
            if line.startswith("#"):
                bits = line.lstrip("# ").split("\t", 1)
                if len(bits) == 2:
                    if bits[0].strip() == "driver_stderr":
                        errs.append(bits[1].strip())
                    else:
                        meta[bits[0].strip()] = bits[1].strip()
                continue
            cols = line.split("\t")
            if hdr is None:
                hdr = cols
                missing = [c for c in KEY_COLS + ("value", "rep") if c not in hdr]
                if missing:
                    sys.exit("%s: header lacks %s" % (path, ",".join(missing)))
                continue
            if len(cols) != len(hdr):
                continue
            r = dict(zip(hdr, cols))
            key = (r["benchmark"], r["config"], int(r["threads"]), r["metric"])
            try:
                samples.setdefault(key, {})[int(r["rep"])] = float(r["value"])
            except ValueError:
                continue
            units[key] = r.get("unit", "")
    if not samples:
        sys.exit("%s: no data rows" % path)
    meta["_driver_stderr"] = errs
    return meta, samples, units


def warn_stderr(path, meta):
    """A driver that logged errors may have retired workers mid-run, in which
    case its throughput describes a shrinking thread pool rather than the
    workload.  Never let that pass silently."""
    errs = meta.get("_driver_stderr") or []
    if not errs:
        return
    print("# WARNING: %s contains %d driver stderr report(s) -- affected "
          "cases may be measuring a degraded run:" % (path, len(errs)))
    for e in errs[:10]:
        print("#   %s" % e)
    if len(errs) > 10:
        print("#   ... %d more" % (len(errs) - 10))


def stats(vals):
    vals = sorted(vals)
    med = statistics.median(vals)
    # Population stdev over the reps; a single rep has no spread to report.
    sd = statistics.pstdev(vals) if len(vals) > 1 else 0.0
    cv = 100.0 * sd / med if med else 0.0
    return min(vals), med, max(vals), cv


def tolerance_for(cv_pct):
    """Tolerance in percent implied by a case's own coefficient of variation."""
    return max(TOL_FLOOR_PCT, TOL_SIGMA * cv_pct)


def num(v):
    """Format a metric value without discarding sub-unit resolution: a
    queries/s median of 0.5 must not print as 0."""
    return "%.2f" % v if abs(v) < 1000 else "%.0f" % v


def fmt_key(key):
    return "%s/%s/t%d" % (key[0], key[1], key[2])


def cmd_noise(path, tol_floor, cv_exclude):
    meta, samples, units = parse(path)
    warn_stderr(path, meta)
    nreps = max(len(v) for v in samples.values())
    print("# noise floor from %s (%d reps, hardware=%s)"
          % (path, nreps, meta.get("hardware", "?")))
    if nreps < 5:
        print("# WARNING: %d reps is too few to characterise noise; use >= 5"
              % nreps)
    print("\t".join(("case", "metric", "unit", "n", "min", "median", "max",
                     "cv_pct", "tolerance_pct", "gated")))
    rows = []
    for key, reps in samples.items():
        lo, med, hi, cv = stats(list(reps.values()))
        rows.append((key, len(reps), lo, med, hi, cv))
    rows.sort(key=lambda r: -r[5])
    excluded = 0
    for key, n, lo, med, hi, cv in rows:
        gated = cv <= cv_exclude and med >= MIN_MEDIAN_GATE
        excluded += 0 if gated else 1
        print("%s\t%s\t%s\t%d\t%s\t%s\t%s\t%.2f\t%.1f\t%s"
              % (fmt_key(key), key[3], units.get(key, ""), n, num(lo),
                 num(med), num(hi), cv,
                 max(tol_floor, TOL_SIGMA * cv),
                 "yes" if gated else
                 "NO(res)" if med < MIN_MEDIAN_GATE else "NO(cv)"))
    print("# %d cases, %d excluded (cv > %.1f%% or median < %.0f)"
          % (len(rows), excluded, cv_exclude, MIN_MEDIAN_GATE))
    return 0


def cmd_compare(base_path, new_path, tol_floor, cv_exclude, tol_override,
                exclude_pats, quiet):
    bmeta, bsamp, units = parse(base_path)
    nmeta, nsamp, _ = parse(new_path)
    warn_stderr(base_path, bmeta)
    warn_stderr(new_path, nmeta)

    print("# baseline   %s  (%s, git %s)"
          % (base_path, bmeta.get("hardware", "?"), bmeta.get("git_rev", "?")))
    print("# candidate  %s  (%s, git %s)"
          % (new_path, nmeta.get("hardware", "?"), nmeta.get("git_rev", "?")))
    if bmeta.get("hardware") != nmeta.get("hardware"):
        print("# WARNING: hardware differs between the two files -- a delta "
              "across machines is not a regression signal")
    for k in ("secs", "nkeys", "seed", "tproc_scale"):
        if bmeta.get(k) != nmeta.get(k):
            print("# WARNING: %s differs (%s vs %s): configurations are not "
                  "comparable" % (k, bmeta.get(k), nmeta.get(k)))

    print("\t".join(("case", "metric", "base_median", "base_spread",
                     "new_median", "new_spread", "delta_pct", "tol_pct",
                     "verdict")))
    fails, gated, skipped, missing = [], 0, [], []
    for key in sorted(bsamp, key=lambda k: (k[0], k[1], k[2])):
        blo, bmed, bhi, bcv = stats(list(bsamp[key].values()))
        if key not in nsamp:
            missing.append(key)
            print("%s\t%s\t%s\t%s..%s\t-\t-\t-\t-\tMISSING"
                  % (fmt_key(key), key[3], num(bmed), num(blo), num(bhi)))
            continue
        nlo, nmed, nhi, ncv = stats(list(nsamp[key].values()))
        delta = 100.0 * (nmed - bmed) / bmed if bmed else 0.0
        tol = tol_override if tol_override is not None else \
            max(tol_floor, TOL_SIGMA * bcv)
        name = fmt_key(key)
        excluded = bcv > cv_exclude or bmed < MIN_MEDIAN_GATE or \
            any(p in name or p == key[0] for p in exclude_pats)
        if excluded:
            verdict = "SKIP"
            skipped.append((name, bcv))
        elif delta < -tol:
            verdict = "FAIL"
            fails.append((name, delta, tol))
            gated += 1
        else:
            verdict = "pass"
            gated += 1
        if not quiet or verdict != "pass":
            print("%s\t%s\t%s\t%s..%s\t%s\t%s..%s\t%+.1f\t%.1f\t%s"
                  % (name, key[3], num(bmed), num(blo), num(bhi), num(nmed),
                     num(nlo), num(nhi), delta, tol, verdict))
    for key in sorted(set(nsamp) - set(bsamp), key=lambda k: (k[0], k[1], k[2])):
        nlo, nmed, nhi, _ = stats(list(nsamp[key].values()))
        print("%s\t%s\t-\t-\t%s\t%s..%s\t-\t-\tNEW"
              % (fmt_key(key), key[3], num(nmed), num(nlo), num(nhi)))

    print("# %d cases gated, %d skipped (noisy/excluded), %d missing"
          % (gated, len(skipped), len(missing)))
    if skipped:
        print("# skipped: " + ", ".join("%s (cv %.1f%%)" % s for s in skipped))
    if fails:
        for name, delta, tol in fails:
            print("# REGRESSION %s: %+.1f%% (tolerance %.1f%%)"
                  % (name, delta, tol))
        print("# VERDICT: FAIL (%d regression%s)"
              % (len(fails), "" if len(fails) == 1 else "s"))
        return 1
    print("# VERDICT: PASS")
    return 0


def main():
    p = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    p.add_argument("files", nargs="+", metavar="FILE",
                   help="one file with --noise, else BASELINE CANDIDATE")
    p.add_argument("--noise", action="store_true",
                   help="report the noise floor of a single results file")
    p.add_argument("--tolerance", type=float, default=None, metavar="PCT",
                   help="override the derived tolerance for every case "
                        "(use only to reproduce a specific verdict)")
    p.add_argument("--tolerance-floor", type=float, default=TOL_FLOOR_PCT,
                   metavar="PCT", help="minimum tolerance (default %.1f)"
                                       % TOL_FLOOR_PCT)
    p.add_argument("--cv-exclude", type=float, default=CV_EXCLUDE_PCT,
                   metavar="PCT", help="exclude cases noisier than this "
                                       "(default %.1f)" % CV_EXCLUDE_PCT)
    p.add_argument("--exclude", action="append", default=[], metavar="MATCH",
                   help="also exclude cases whose name contains MATCH")
    p.add_argument("--quiet", action="store_true",
                   help="print only non-passing cases")
    a = p.parse_args()

    if a.noise:
        if len(a.files) != 1:
            p.error("--noise takes exactly one file")
        return cmd_noise(a.files[0], a.tolerance_floor, a.cv_exclude)
    if len(a.files) != 2:
        p.error("compare mode takes exactly two files (baseline, candidate)")
    return cmd_compare(a.files[0], a.files[1], a.tolerance_floor,
                       a.cv_exclude, a.tolerance, a.exclude, a.quiet)


if __name__ == "__main__":
    sys.exit(main())
