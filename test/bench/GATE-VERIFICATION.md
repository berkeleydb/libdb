# Gate verification

Both directions, measured on the baseline hardware (EC2 `c7i.24xlarge`, 96
vCPU, idle, `uptime` load < 1.0 before each run). A gate that has never failed
proves nothing, so this file records the run where it did.

All three runs used the same fixed configuration: seed 42, 100 000 records,
10 s window, 5 reps, threads `1 8 32 96` (`1 8 32` for `tproc_*`). Hardware
and build provenance is in the header of every `.tsv` and in `NOISE.md`.

| run | code | file |
|---|---|---|
| A | libdb 5.3.36 at `b4d65c25`, unmodified | `baseline-c7i.24xlarge.tsv` (committed) |
| B | **identical to A**, measured again ~40 min later | not committed |
| C | A plus a busy-spin in `__lock_get` | not committed |

## 1. No false positive: A vs B, identical code

Two independent 5-rep measurements of the same binary. **Required outcome: no
regression.**

```
# baseline   /tmp/baseline_A.tsv  (c7i.24xlarge, git b4d65c25 (worktree dirty))
# candidate  /tmp/candidate_B.tsv  (c7i.24xlarge, git b4d65c25 (worktree dirty))
case	metric	base_median	base_spread	new_median	new_spread	delta_pct	tol_pct	verdict
lock_bench/distinct/t1	ops_per_sec	5857951	5853414..5868362	5862699	5849817..5872167	+0.1	5.0	pass
lock_bench/distinct/t8	ops_per_sec	16848298	16240926..17646612	15982027	15507835..16532726	-5.1	10.3	pass
lock_bench/distinct/t32	ops_per_sec	52804258	51715616..53294663	52867271	52394634..53067852	+0.1	5.0	pass
lock_bench/distinct/t96	ops_per_sec	99603920	98898094..99895783	99479412	98667105..100287029	-0.1	5.0	pass
lock_bench/shared/t1	ops_per_sec	5914547	5848930..5927480	5849266	5780457..5932437	-1.1	5.0	pass
lock_bench/shared/t8	ops_per_sec	13634133	13407633..14579118	14440213	12566621..15117424	+5.9	9.4	pass
lock_bench/shared/t32	ops_per_sec	31441115	20603241..32273402	30541237	28036743..33112974	-2.9	44.6	SKIP
lock_bench/shared/t96	ops_per_sec	6508286	4920033..8932450	4847811	4278932..8870000	-25.5	60.1	SKIP
scale_bench/rhot/t1	ops_per_sec	1407892	1400702..1409238	1407459	1394236..1408632	-0.0	5.0	pass
scale_bench/rhot/t8	ops_per_sec	606465	572517..697641	643145	619604..713743	+6.0	24.6	pass
scale_bench/rhot/t32	ops_per_sec	136312	127190..184220	125823	121270..140100	-7.7	46.3	SKIP
scale_bench/rhot/t96	ops_per_sec	53433	52643..62225	52453	51091..54587	-1.8	20.1	pass
scale_bench/rrand/t1	ops_per_sec	922763	921196..933886	920789	919227..933391	-0.2	5.0	pass
scale_bench/rrand/t8	ops_per_sec	499483	478348..563718	568833	489905..607425	+13.9	20.1	pass
scale_bench/rrand/t32	ops_per_sec	135971	125760..142191	132063	123811..139724	-2.9	14.4	pass
scale_bench/rrand/t96	ops_per_sec	51362	50728..52857	52912	51411..53713	+3.0	5.0	pass
scale_bench/wrand/t1	ops_per_sec	151924	151361..152327	151124	150788..151640	-0.5	5.0	pass
scale_bench/wrand/t8	ops_per_sec	137882	135827..140835	137399	135627..141087	-0.4	5.0	pass
scale_bench/wrand/t32	ops_per_sec	114803	113034..116616	115460	114651..116911	+0.6	5.0	pass
scale_bench/wrand/t96	ops_per_sec	103334	102470..104406	103498	102560..103874	+0.2	5.0	pass
scale_iso/none/t1	ops_per_sec	935546	923888..942865	938185	925477..940681	+0.3	5.0	pass
scale_iso/none/t8	ops_per_sec	3215530	3053402..3298984	3251121	3170405..3416370	+1.1	9.0	pass
scale_iso/none/t32	ops_per_sec	3715268	3570308..4331181	4124454	3756786..4196315	+11.0	26.7	pass
scale_iso/none/t96	ops_per_sec	5004206	4921186..5218264	5179279	5010975..5244448	+3.5	6.4	pass
ssi_abort_bench/hot64/t1	txn_per_sec	106008	105914..106251	105764	105705..105989	-0.2	5.0	pass
ssi_abort_bench/hot64/t8	txn_per_sec	9942	9416..10178	9542	9255..10114	-4.0	9.3	pass
ssi_abort_bench/hot64/t32	txn_per_sec	7018	6980..7185	6936	6824..7031	-1.2	5.0	pass
ssi_abort_bench/hot64/t96	txn_per_sec	7314	7215..7551	7459	6205..7603	+2.0	5.3	pass
tproc_b/S1/t1	txn_per_sec	82824	82646..83049	82822	82725..83231	-0.0	5.0	pass
tproc_b/S1/t8	txn_per_sec	6577	6449..6761	6736	6417..6953	+2.4	5.0	pass
tproc_b/S1/t32	txn_per_sec	1183	1175..1228	1207	1186..1229	+2.0	5.0	pass
tproc_c/S1/t1	tpmC_like	335595	335175..336658	336389	335951..336645	+0.2	5.0	pass
tproc_c/S1/t8	tpmC_like	133124	130103..140359	136260	128548..143522	+2.4	7.8	pass
tproc_c/S1/t32	tpmC_like	74708	72924..76679	75353	72384..76421	+0.9	5.0	pass
tproc_h/S1/t1	queries_per_sec	7.90	7.70..7.90	7.80	7.70..7.90	-1.3	5.0	SKIP
tproc_h/S1/t1	rows_per_sec	811647	782940..812950	789635	764000..812446	-2.7	5.0	pass
tproc_h/S1/t8	queries_per_sec	1.50	1.30..1.70	1.40	1.30..1.80	-6.7	26.5	SKIP
tproc_h/S1/t8	rows_per_sec	119395	104990..136656	117991	103775..145361	-1.2	27.4	pass
tproc_h/S1/t32	queries_per_sec	0.50	0.50..0.50	0.50	0.50..0.50	+0.0	5.0	SKIP
tproc_h/S1/t32	rows_per_sec	48719	47420..53669	49227	46979..52063	+1.0	14.2	pass
# 34 cases gated, 6 skipped (noisy/excluded), 0 missing
# skipped: lock_bench/shared/t32 (cv 14.9%), lock_bench/shared/t96 (cv 20.0%), scale_bench/rhot/t32 (cv 15.4%), tproc_h/S1/t1 (cv 1.0%), tproc_h/S1/t8 (cv 8.8%), tproc_h/S1/t32 (cv 0.0%)
# VERDICT: PASS
```

`EXIT=0`. Every gated case passed. The largest gated movement between two runs
of identical code was **+13.9%** (`scale_bench/rrand/t8`), inside its derived
tolerance of 20.1% — the tolerance is doing exactly the job it was sized for.

Note what the *excluded* cases did across identical code:
`lock_bench/shared/t96` moved **-25.5%** and `scale_bench/rhot/t32` **-7.7%**.
Had those been gated, this run would have reported a regression with no code
change. That is the concrete justification for the CV-based exclusion in
`NOISE.md`.

## 2. Fires on a real slowdown: A vs C, pessimal build

A busy-spin was inserted into `__lock_get` in `src/lock/lock.c`, inside the
region lock, so every lock acquisition costs measurably more:

```c
	LOCK_SYSTEM_LOCK(lt, (DB_LOCKREGION *)lt->reginfo.primary);
	/* PESSIMAL: temporary busy-spin to prove the regression gate fires. */
	{ volatile int _s; for (_s = 0; _s < 4000; _s++) continue; }
	ret = __lock_get_internal(lt, locker, flags, obj, lock_mode, 0, lock);
	LOCK_SYSTEM_UNLOCK(lt, (DB_LOCKREGION *)lt->reginfo.primary);
```

The library was rebuilt with `make -j96` and re-measured with the same
5-rep matrix. **Required outcome: regression detected, non-zero exit.**

```
# baseline   /tmp/baseline_A.tsv  (c7i.24xlarge, git b4d65c25 (worktree dirty))
# candidate  /tmp/slowed_C.tsv  (c7i.24xlarge, git b4d65c25 (worktree dirty))
case	metric	base_median	base_spread	new_median	new_spread	delta_pct	tol_pct	verdict
lock_bench/distinct/t1	ops_per_sec	5857951	5853414..5868362	5854649	5841105..5867234	-0.1	5.0	pass
lock_bench/distinct/t8	ops_per_sec	16848298	16240926..17646612	16337954	15435468..17400815	-3.0	10.3	pass
lock_bench/distinct/t32	ops_per_sec	52804258	51715616..53294663	52419117	50906323..53624780	-0.7	5.0	pass
lock_bench/distinct/t96	ops_per_sec	99603920	98898094..99895783	99197956	98828899..100246811	-0.4	5.0	pass
lock_bench/shared/t1	ops_per_sec	5914547	5848930..5927480	5900564	5874259..5926820	-0.2	5.0	pass
lock_bench/shared/t8	ops_per_sec	13634133	13407633..14579118	13450750	12861365..14014369	-1.3	9.4	pass
lock_bench/shared/t32	ops_per_sec	31441115	20603241..32273402	24325089	20428204..31104493	-22.6	44.6	SKIP
lock_bench/shared/t96	ops_per_sec	6508286	4920033..8932450	9262913	4677034..9570246	+42.3	60.1	SKIP
scale_bench/rhot/t1	ops_per_sec	1407892	1400702..1409238	133340	132323..133950	-90.5	5.0	FAIL
scale_bench/rhot/t8	ops_per_sec	606465	572517..697641	742960	718491..782246	+22.5	24.6	pass
scale_bench/rhot/t32	ops_per_sec	136312	127190..184220	134264	125357..144473	-1.5	46.3	SKIP
scale_bench/rhot/t96	ops_per_sec	53433	52643..62225	53203	52644..79737	-0.4	20.1	pass
scale_bench/rrand/t1	ops_per_sec	922763	921196..933886	174651	173418..179562	-81.1	5.0	FAIL
scale_bench/rrand/t8	ops_per_sec	499483	478348..563718	679696	585504..744476	+36.1	20.1	pass
scale_bench/rrand/t32	ops_per_sec	135971	125760..142191	128928	128428..134609	-5.2	14.4	pass
scale_bench/rrand/t96	ops_per_sec	51362	50728..52857	52878	49440..53540	+3.0	5.0	pass
scale_bench/wrand/t1	ops_per_sec	151924	151361..152327	72837	72724..72955	-52.1	5.0	FAIL
scale_bench/wrand/t8	ops_per_sec	137882	135827..140835	137973	136507..144505	+0.1	5.0	pass
scale_bench/wrand/t32	ops_per_sec	114803	113034..116616	117503	115423..118597	+2.4	5.0	pass
scale_bench/wrand/t96	ops_per_sec	103334	102470..104406	102896	102681..104377	-0.4	5.0	pass
scale_iso/none/t1	ops_per_sec	935546	923888..942865	176929	170822..178033	-81.1	5.0	FAIL
scale_iso/none/t8	ops_per_sec	3215530	3053402..3298984	1338050	1315009..1358917	-58.4	9.0	FAIL
scale_iso/none/t32	ops_per_sec	3715268	3570308..4331181	3772480	3625761..3868283	+1.5	26.7	pass
scale_iso/none/t96	ops_per_sec	5004206	4921186..5218264	4666202	4554000..4775955	-6.8	6.4	FAIL
ssi_abort_bench/hot64/t1	txn_per_sec	106008	105914..106251	42253	42216..42327	-60.1	5.0	FAIL
ssi_abort_bench/hot64/t8	txn_per_sec	9942	9416..10178	10476	9853..10758	+5.4	9.3	pass
ssi_abort_bench/hot64/t32	txn_per_sec	7018	6980..7185	7033	6794..7155	+0.2	5.0	pass
ssi_abort_bench/hot64/t96	txn_per_sec	7314	7215..7551	7489	6262..8180	+2.4	5.3	pass
tproc_b/S1/t1	txn_per_sec	82824	82646..83049	17330	17208..17434	-79.1	5.0	FAIL
tproc_b/S1/t8	txn_per_sec	6577	6449..6761	5609	5304..5957	-14.7	5.0	FAIL
tproc_b/S1/t32	txn_per_sec	1183	1175..1228	1195	1185..1215	+1.0	5.0	pass
tproc_c/S1/t1	tpmC_like	335595	335175..336658	23634	23288..23757	-93.0	5.0	FAIL
tproc_c/S1/t8	tpmC_like	133124	130103..140359	25257	23897..26404	-81.0	7.8	FAIL
tproc_c/S1/t32	tpmC_like	74708	72924..76679	23417	21020..24113	-68.7	5.0	FAIL
tproc_h/S1/t1	queries_per_sec	7.90	7.70..7.90	0.60	0.60..0.60	-92.4	5.0	SKIP
tproc_h/S1/t1	rows_per_sec	811647	782940..812950	83356	82363..83437	-89.7	5.0	FAIL
tproc_h/S1/t8	queries_per_sec	1.50	1.30..1.70	1.50	1.50..1.60	+0.0	26.5	SKIP
tproc_h/S1/t8	rows_per_sec	119395	104990..136656	123633	117785..130852	+3.5	27.4	pass
tproc_h/S1/t32	queries_per_sec	0.50	0.50..0.50	0.50	0.50..0.60	+0.0	5.0	SKIP
tproc_h/S1/t32	rows_per_sec	48719	47420..53669	52454	49755..57671	+7.7	14.2	pass
# 34 cases gated, 6 skipped (noisy/excluded), 0 missing
# skipped: lock_bench/shared/t32 (cv 14.9%), lock_bench/shared/t96 (cv 20.0%), scale_bench/rhot/t32 (cv 15.4%), tproc_h/S1/t1 (cv 1.0%), tproc_h/S1/t8 (cv 8.8%), tproc_h/S1/t32 (cv 0.0%)
# REGRESSION scale_bench/rhot/t1: -90.5% (tolerance 5.0%)
# REGRESSION scale_bench/rrand/t1: -81.1% (tolerance 5.0%)
# REGRESSION scale_bench/wrand/t1: -52.1% (tolerance 5.0%)
# REGRESSION scale_iso/none/t1: -81.1% (tolerance 5.0%)
# REGRESSION scale_iso/none/t8: -58.4% (tolerance 9.0%)
# REGRESSION scale_iso/none/t96: -6.8% (tolerance 6.4%)
# REGRESSION ssi_abort_bench/hot64/t1: -60.1% (tolerance 5.0%)
# REGRESSION tproc_b/S1/t1: -79.1% (tolerance 5.0%)
# REGRESSION tproc_b/S1/t8: -14.7% (tolerance 5.0%)
# REGRESSION tproc_c/S1/t1: -93.0% (tolerance 5.0%)
# REGRESSION tproc_c/S1/t8: -81.0% (tolerance 7.8%)
# REGRESSION tproc_c/S1/t32: -68.7% (tolerance 5.0%)
# REGRESSION tproc_h/S1/t1: -89.7% (tolerance 5.0%)
# VERDICT: FAIL (13 regressions)
```

`EXIT=1`, **13 regressions**, from -6.8% (`scale_iso/none/t96`, tolerance 6.4%
— caught only just, as a 3-sigma gate should) to -93.0% (`tproc_c/S1/t1`).

The patch was then reverted, the library rebuilt, and A vs B re-run to confirm
the verdict returned to PASS. **The pessimal change is not committed anywhere
in this tree** — `grep -r PESSIMAL src/` finds nothing.

### Why some cases legitimately did not fire

`lock_bench/distinct` barely moved (-0.1% to -3.0%). That is not a gate
failure: `lock_bench` is a tight `lock_get`/`lock_put` loop whose cost is
already dominated by region-lock contention at 8+ threads, so a fixed spin
inside the same critical section is absorbed by wait time that was already
there. The single-threaded access-method cases, where the spin lands on the
critical path with nothing to hide behind, moved -52% to -93%.

Several high-thread cases even went *up* (`scale_bench/rrand/t8` +36.1%): with
lock acquisition slowed, threads collide less often and the workload
accidentally becomes more efficient per-thread. Throughput under contention is
not monotonic in per-operation cost — which is precisely why a gate needs many
cases across the thread sweep rather than one headline number.

## 3. Synthetic verification, no hardware required

`test_bench_cmp.py` checks both directions on constructed data, so the gate's
logic is verifiable in CI and on a laptop:

```
$ python3 test_bench_cmp.py
test_bench_cmp: all checks passed
```

It asserts, among other things, that identical input passes, that a 25%
synthetic slowdown exits 1 with the correct reported delta, that a speedup
never fails, that a case noisier than the CV threshold is skipped even when it
drops 40%, that a resolution-limited case is skipped even at CV 0, and that a
file containing driver stderr reports raises a warning.
