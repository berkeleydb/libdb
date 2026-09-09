#!/usr/bin/env python3
"""Self-check for bench_cmp.py -- the gate must not fire on identical input
and must fire on a synthetic slowdown.  Run: python3 test_bench_cmp.py
"""
import os
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
CMP = os.path.join(HERE, "bench_cmp.py")

HDR = ("# libdb-bench-results\tv1\n"
       "# hardware\ttestbox\n# secs\t10\n# nkeys\t100000\n# seed\t42\n"
       "# tproc_scale\t1\n"
       "benchmark\tconfig\tthreads\tmetric\tunit\trep\tvalue\n")


def write(rows):
    fh = tempfile.NamedTemporaryFile("w", suffix=".tsv", delete=False)
    fh.write(HDR)
    for r in rows:
        fh.write("\t".join(str(c) for c in r) + "\n")
    fh.close()
    return fh.name


def case(bench, vals, cfg="c", threads=8):
    return [(bench, cfg, threads, "ops_per_sec", "ops/s", i + 1, v)
            for i, v in enumerate(vals)]


def run(*args):
    p = subprocess.run([sys.executable, CMP] + list(args),
                       capture_output=True, text=True)
    return p.returncode, p.stdout


def main():
    # A quiet 5-rep case: CV ~1%, so the derived tolerance is the 5% floor.
    quiet = [100000, 101000, 99000, 100500, 99500]
    base = write(case("lock_bench", quiet))

    # 1. identical data -> PASS (no false positive).
    rc, out = run(base, base)
    assert rc == 0 and "VERDICT: PASS" in out, out

    # 2. a second sample of the same distribution -> still PASS.
    same = write(case("lock_bench", [99800, 100200, 100700, 99300, 100000]))
    rc, out = run(base, same)
    assert rc == 0 and "VERDICT: PASS" in out, out

    # 3. a 25% slowdown -> FAIL, and the reported delta is right.
    slow = write(case("lock_bench", [v * 0.75 for v in quiet]))
    rc, out = run(base, slow)
    assert rc == 1 and "VERDICT: FAIL" in out and "REGRESSION" in out, out
    assert "-25.0" in out, out

    # 4. a speedup is never a failure.
    fast = write(case("lock_bench", [v * 1.4 for v in quiet]))
    rc, out = run(base, fast)
    assert rc == 0 and "VERDICT: PASS" in out, out

    # 5. a case noisier than --cv-exclude is skipped, not gated: even a 40%
    #    drop must not fail, because the baseline cannot resolve it.
    nbase = write(case("noisy", [100000, 200000, 50000, 300000, 150000]))
    nslow = write(case("noisy", [60000, 120000, 30000, 180000, 90000]))
    rc, out = run(nbase, nslow)
    assert rc == 0 and "SKIP" in out and "skipped" in out, out

    # 5b. a resolution-limited case (median below MIN_MEDIAN_GATE) is skipped
    #     even at CV 0: 5 identical low counts mean the metric quantised the
    #     same way, not that it is stable.  A halving must still not FAIL.
    qbase = write(case("tproc_h", [0.5] * 5))
    qslow = write(case("tproc_h", [0.25] * 5))
    rc, out = run(qbase, qslow)
    assert rc == 0 and "SKIP" in out, out
    # ... and the small value must not print as 0.
    assert "0.50" in out, out
    rc, out = run("--noise", qbase)
    assert "NO(res)" in out, out

    # 6. explicit --exclude also removes a case from the verdict.
    rc, out = run("--exclude", "lock_bench", base, slow)
    assert rc == 0 and "SKIP" in out, out

    # 7. --tolerance override is honoured both ways: a 25% drop passes under a
    #    50% tolerance, and a 2% drop fails under a 1% tolerance.
    rc, out = run("--tolerance", "50", base, slow)
    assert rc == 0, out
    small = write(case("lock_bench", [v * 0.98 for v in quiet]))
    rc, out = run("--tolerance", "1", base, small)
    assert rc == 1, out

    # 8. a case present in the baseline but absent from the candidate is
    #    reported, not silently dropped.
    rc, out = run(write(case("lock_bench", quiet) + case("gone", quiet)), base)
    assert "MISSING" in out, out

    # 9. --noise reports median and CV, and flags too-few-reps.
    rc, out = run("--noise", base)
    assert rc == 0 and "100000" in out and "cv_pct" in out, out
    rc, out = run("--noise", write(case("lock_bench", [100000])))
    assert "too few" in out, out

    # 10. a driver_stderr provenance line must surface as a warning in both
    #     modes -- a driver that logged errors may have retired workers, and
    #     its throughput then describes a shrinking thread pool.
    dirty = tempfile.NamedTemporaryFile("w", suffix=".tsv", delete=False)
    dirty.write(HDR.replace("benchmark\t",
                            "# driver_stderr\tscale_iso/snap rep 1: 9 line(s)"
                            ", first: BDB2055 Lock table\nbenchmark\t"))
    for r in case("lock_bench", quiet):
        dirty.write("\t".join(str(c) for c in r) + "\n")
    dirty.close()
    rc, out = run("--noise", dirty.name)
    assert "WARNING" in out and "BDB2055" in out, out
    rc, out = run(dirty.name, base)
    assert "WARNING" in out, out
    os.unlink(dirty.name)

    for f in (base, same, slow, fast, small, nbase, nslow, qbase, qslow):
        os.unlink(f)
    print("test_bench_cmp: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
