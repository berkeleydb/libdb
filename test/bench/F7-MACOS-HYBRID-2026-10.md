# F7 — the AArch64 hybrid mutex on macOS (Apple Silicon)

**Decision:** macOS/arm64 selects the hybrid (TAS spin + pthread block) mutex
by default again, as FreeBSD/arm64 and Linux/arm64 do. It was excluded for one
release (`681026b49`) on a misdiagnosis.

## What F7 actually was

`run_mutex_stress` printed no verdict on every macOS CI job. That was read as a
mutex defect, so `681026b49` stopped selecting the hybrid on Darwin and
`f3bef0ab5` then skipped the test. PR #211 found the real cause: the **runner**
never started its driver on macOS. The dylib's baked-in install name took
precedence over `-rpath`, so dyld aborted the binary before `main()` (rc=134).
Stock macOS also has no GNU `timeout` (rc=127). `2>&1 | grep` threw away both
messages. Neither mutex was at fault.

## Evidence

Run [38646734711](https://github.com/berkeleydb/libdb/actions/runs/38646734711)
(`.github/workflows/f7-macos-ab.yml`, dispatch-only) built master twice on each
of `macos-14` and `macos-latest` (both "Apple M1 (Virtual)"): the pthreads mutex
Darwin selected, and the hybrid forced with `--with-mutex`. The workflow asserts
the hybrid arm really defines `HAVE_MUTEX_HYBRID` + `HAVE_MUTEX_ARM64_GCC_ASSEMBLY`
and the pthreads arm does not.

### Correctness: identical on both arms, both runners

| | pthreads | hybrid |
|---|---|---|
| `run_mutex_stress` (counter ×2, handoff ×2) | 4/4 PASS | 4/4 PASS |
| lockmatrix (modes, conflicts, list: 55 checks) | 3/3, 0 failed | 3/3, 0 failed |
| `test/db/run_all.sh` | 33 PASS / 0 FAIL | 33 PASS / 0 FAIL |

The stress test's teeth hold on Apple Silicon (PR #211): with a sabotaged
`src/dbinc/mutex_int.h`, a non-atomic `MUTEX_SET` and RELAXED set/unset each fail
counter mode 2/2, and `MUTEX_MEMBAR` reduced to a compiler barrier hangs handoff
mode 2/2.

### Performance: hybrid / pthreads, median of 7 alternated reps

Ratio of medians with a bootstrap 95% CI. "faster" means the CI's lower bound is
above 1.0. Hosted runners are small VMs (3 vCPU), so a cell with cv ≥ 10% is
marked noisy and should be read as direction only.

```
macos-14
  bench          thr     pthread      hybrid  ratio         95% CI  cv_p  cv_h  verdict
  lock_distinct    1    10798623    11677948   1.08  [1.01,1.21]   4.3   7.7  faster
  lock_distinct    2     6871813    13838829   2.01  [1.29,3.04]  29.6  13.2  faster (noisy)
  lock_distinct    4     9616393    16667177   1.73  [1.17,1.89]  16.7  14.1  faster (noisy)
  lock_distinct    8    11175841    16820999   1.51  [1.01,2.05]  15.7  20.3  faster (noisy)
  lock_shared      1    10815503    11843273   1.10  [1.01,1.16]   4.3   7.2  faster
  lock_shared      2     6786825    11323694   1.67  [1.25,2.88]  29.8  11.3  faster (noisy)
  lock_shared      4     8422738    12078849   1.43  [1.25,2.31]  22.8  14.8  faster (noisy)
  lock_shared      8     9374983    10956987   1.17  [0.94,1.76]  14.5  25.7  ~ (noisy)
  scale_rhot       1     2108572     2201129   1.04  [0.96,1.11]   4.7   6.7  ~
  scale_rhot       2     1461011     1895637   1.30  [0.99,1.42]  13.8  13.1  ~ (noisy)
  scale_rhot       4     1627468     2349861   1.44  [1.06,3.71]  43.4  13.6  faster (noisy)
  scale_rhot       8     1374087     2538714   1.85  [1.04,2.05]  27.8  15.9  faster (noisy)
  scale_wrand      1        1965        2237   1.14  [0.87,1.29]  14.2  13.2  ~ (noisy)
  scale_wrand      2        2866        3399   1.19  [0.98,1.32]  10.7  10.3  ~ (noisy)
  scale_wrand      4        4558        4580   1.00  [0.66,1.34]  16.7  20.7  ~ (noisy)
  scale_wrand      8        7727        9051   1.17  [0.88,1.40]  12.0  17.5  ~ (noisy)

macos-latest
  bench          thr     pthread      hybrid  ratio         95% CI  cv_p  cv_h  verdict
  lock_distinct    1    10771051    11680979   1.08  [0.98,1.12]   8.8   6.8  ~
  lock_distinct    2     8412044    12555868   1.49  [1.44,1.77]   6.9   8.6  faster
  lock_distinct    4    10385990    13312235   1.28  [0.97,1.45]  12.2  13.4  ~ (noisy)
  lock_distinct    8    10838935    15369611   1.42  [1.26,1.74]  11.6  11.3  faster (noisy)
  lock_shared      1    10491468    11657149   1.11  [1.01,1.18]   7.9   4.6  faster
  lock_shared      2     7802181    11247456   1.44  [1.25,1.63]   7.7   7.0  faster
  lock_shared      4     9988546    11132727   1.11  [0.91,1.30]  14.0  15.7  ~ (noisy)
  lock_shared      8    10663050    11121170   1.04  [0.88,1.42]  17.3  19.9  ~ (noisy)
  scale_rhot       1     2136089     2282619   1.07  [0.96,1.10]   9.0   4.2  ~
  scale_rhot       2     1879061     1961766   1.04  [1.00,1.20]  13.8   3.3  ~ (noisy)
  scale_rhot       4     1458100     2183452   1.50  [1.02,1.93]  17.2  21.5  faster (noisy)
  scale_rhot       8     1774082     2039497   1.15  [0.80,1.51]  11.8  22.4  ~ (noisy)
  scale_wrand      1        2381        2264   0.95  [0.82,1.18]  10.5  12.1  ~ (noisy)
  scale_wrand      2        3081        3634   1.18  [0.91,1.36]  16.7   8.0  ~ (noisy)
  scale_wrand      4        5348        5701   1.07  [0.96,1.42]  13.7   7.2  ~ (noisy)
  scale_wrand      8        9000       10017   1.11  [0.98,1.27]  14.8   6.9  ~ (noisy)
```

- **Lock manager** (`lock_bench`, no access methods, no buffer pool): faster or
  neutral in every cell. At 2 threads the hybrid ran 1.44–2.0× the pthreads arm
  on both runners. No cell is significantly slower.
- **Hot-key reads** (`scale_bench rhot`): faster at 4 threads on both runners
  (1.44×, 1.50×). Other cells are neutral.
- **Writes** (`scale_bench wrand`): neutral everywhere. Expected, since this path
  is bound by log flush, not mutex hand-off.

The harness's own noise floor, measured by pointing both arms at the **same**
library on Linux, is 0.94–1.08×.

### What this does not measure

It was not run on Apple Silicon metal or at more than 8 threads: AWS mac2 hosts
need a dedicated-host quota, and the request for it is still pending. Re-measure
on metal when the quota arrives.

## Region signature

The hybrid makes `DB_MUTEX` larger, so the macOS/arm64 environment signature
changes: `0xe0ad2911` (pthreads) → `0x2c8d3688` (hybrid), measured on both
runners. No other platform's build changes. A macOS/arm64 process from
2026.10.6 and one from 2026.10.7 cannot share an environment, and the attempt
fails cleanly with `DB_VERSION_MISMATCH`.
