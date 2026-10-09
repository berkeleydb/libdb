<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.

SPDX-License-Identifier: Sleepycat

See the file LICENSE for redistribution information.
-->

# RFC 0013 — Cross-engine benchmark campaign: plan for review

| | |
|---|---|
| Status | **Draft — for maintainer review. Nothing provisioned, nothing spent.** |
| Supersedes | extends `test/bench/CROSS-ENGINE-2026-09.md` (libdb vs WiredTiger, i4i.metal) |
| Blocked by | **F5**, **P13**, and the four-arm matrix in `test/bench/P13-FREEBSD-MUTEX-GC-2026-10.md` |

## 0. Read this first: the plan as asked cannot be run yet

The request is a FreeBSD metal comparison of current libdb against five other
engines plus two historical libdb points. Two things make that premature, and
both are measurements this project already has rather than opinions.

**F5 caps libdb on FreeBSD at one thread.** With P13's fix in place FreeBSD does
**117,124 commits at t=1 and 1,644 at t=2**. A multi-threaded FreeBSD
comparison today would therefore report libdb losing to every competitor by
~70×. That number would be *true* and would answer the wrong question: it
measures a platform defect in one call site (`__mutex_refresh` ←
`__lock_freelock`), not the engine. Publishing it would be the
vacuous-green failure mode inverted — a real number used as an answer to a
question it does not address.

**P13 is unmerged and its Linux value is unknown.** The four-arm matrix was not
measured: 61–79% cv against a 10% ceiling. So we cannot currently say whether
the release being compared is the right one.

**Therefore this RFC proposes gating the campaign**, with the cheap
prerequisite work stated as explicit entry criteria (§2). The alternative —
running it now on Linux instead — is offered in §9 as a smaller, deliverable
option, because a plan whose only outcome is "wait" is not useful.

## 1. What the campaign is for

Two questions, which need different designs and should not be conflated:

**Q1 — Is current libdb stable under pressure?** A durability and robustness
question. Answered by sustained, destructive, long-running work, not by
throughput. Any crash, corruption, assert or leak found here is a **defect to
fix and re-run**, per the request.

**Q2 — Where does libdb stand against its competitors?** A positioning
question. Answered by narrow, fair, statistically defensible comparisons on
workloads those competitors are themselves measured on.

Q1 does not need competitors. Q2 does not need 72 hours. Running them as one
campaign is how benchmark campaigns become unfalsifiable, so they are phased.

## 2. Entry criteria — none of this starts until these hold

| # | criterion | why | status |
|---|---|---|---|
| E1 | **F5 fixed or quantified-and-excluded** | otherwise every FreeBSD multi-thread number measures F5 | **open** |
| E2 | **P13 merged or rejected on measured Linux evidence** | we must know which libdb is "latest" | **open** |
| E3 | **F6 fixed** | a double-free that silently corrupts the mutex free list in production builds will be hit by a 72-hour soak | **open** |
| E4 | **T8 closed** | P13 ships a locker-mutex lifetime change with no test that has teeth for it | **open** |
| E5 | A **release tagged** from the resulting tree | the comparison must name a version, not a commit | — |

E1–E4 are all already-filed rows. E3 in particular is not optional: a soak that
runs `failchk` and closes environments repeatedly is precisely the shape that
trips F6.

## 3. Engines, and an honest cost for each

The harness (`test/bench/xe_engine.h`, 1,578 lines) dispatches on
`cfg.engine` at **18 sites** with `if/else`, and an engine must implement
~25 operations (`xe_open`, `xe_txn_begin/commit/abort`, `xe_get/put/del`,
`xe_cursor_*`, `xe_stats_*`, …). It supports **libdb** and **WiredTiger**
today. So "add four engines" is not configuration — it is four ports.

| engine | integration | honest effort | risk |
|---|---|---|---|
| **libdb (latest)** | native | — | — |
| **libdb v5.3.28** (pre-fork, in-repo) | native, same API | ~0 | builds with a 2013-era toolchain; may need `-std=gnu89` |
| **WiredTiger** | already ported | — | — |
| **Oracle BDB 18.1** | same API as libdb | low | **licence, see §4** |
| **LMDB** | no txn-abort-heavy API fit; single-writer | medium | single-writer design makes TPROC-C write arms structurally unfair |
| **RocksDB** | C++; needs `rocksdb::OptimisticTransactionDB` for TPROC-C | **high** | tuning surface is enormous; an untuned RocksDB result is worthless and will be read as dishonest |
| **InnoDB** | **not embeddable** | **very high** | InnoDB is not a library. It ships inside MySQL/MariaDB. Comparing it means benchmarking a *server* through a client protocol, which measures the server, the network stack and the client driver as much as the storage engine |

**Recommendation on InnoDB:** exclude it, and say why in the write-up. If a
MySQL comparison is wanted it is a *different* study — a server-vs-embedded
comparison with its own fairness argument — and mixing it in would undermine
the credibility of everything else. I would rather publish five defensible
engines than six where one is indefensible.

**Recommendation on RocksDB:** include it, but budget the tuning honestly
(§5) and have a RocksDB-experienced reviewer check the options before the run.
The published record is littered with RocksDB benchmarks that measure a default
configuration; we should not add one.

## 4. The Oracle licence question

Oracle BDB ≥ 6.0 is AGPLv3, which is why this archive deliberately stops at
5.3.28 (`README.md:76-82`). AGPL restricts **distribution** and network-service
use, not private measurement. So:

- **Permitted:** fetch Oracle 18.1 onto the bench host, build it, measure it,
  publish the *numbers*.
- **Not permitted, and not proposed:** vendoring it into this repo, committing
  it, or publishing a harness binary linked against it.

Mechanically: the provisioning script fetches it at bench time into
`/nvme/vendor/` (outside the repo); `.gitignore` gets an entry; the harness
links it only in a build directory that is never archived. The write-up states
this explicitly so no reader infers we redistribute it.

## 5. Fairness — the part that decides whether anyone believes the result

The prior campaign already fixed several methodology bugs
(`CROSS-ENGINE-2026-09.md:115`) and one result file is retained but marked
`SUSPECT-warmtrend` and explicitly excluded. That precedent continues.

**Per-engine tuning is mandatory, not optional.** An untuned competitor is not
a fair comparison; it is a straw man that discredits the whole exercise. Each
engine gets:

- a tuning pass by someone who will argue *for* that engine, with the
  configuration committed alongside the results
- cache sized identically in **bytes**, not in engine-specific units
- durability mode matched semantically (libdb `DB_TXN_NOSYNC` ↔ WiredTiger
  `transaction_sync=none` ↔ RocksDB `WriteOptions::sync=false`), with a
  separate fully-durable arm
- its own warmup to a declared steady state, not a fixed sleep

**Identical OS and filesystem for every engine:**

| knob | setting | why |
|---|---|---|
| filesystem | one choice, all engines, stated (ZFS *or* UFS on FreeBSD; XFS on Linux) | ZFS ARC double-caching would flatter engines with small caches |
| THP | off | the project has measured THP skew before |
| NUMA balancing | off | prior run found NUMA to be the axis metal restored |
| CPU governor | performance / no C-state drift | metal exposes this |
| dataset | local NVMe only, never EBS | EBS burst credits silently change mid-run |
| background work | none; cron and updates disabled | |

**Statistical credibility, inherited from the existing gate:**

- ≥9 reps per arm; the scaling gate **refused to report at 5 reps** and needed 9
- arms **alternated within** each rep, never all-of-A-then-all-of-B
- **cv under 10%** before any delta is quoted; above that the result is
  unreportable, exactly as happened to P13's matrix
- one binary with a runtime switch where possible (the `DB_PRIVATE` layout
  warning in `run_bench.sh` applies)
- report medians with min/max and cv, never a single number
- a **base-vs-base control arm** to establish the noise floor, as the prior run did

**Two harness traps already paid for, documented so they are not re-paid:**
engine configuration read at region/database *create* and silently ignored on
*attach* (this fabricated a 13× regression once), and build-loop ordering that
produced four identically-compiled "arms" (caught only because the `.o` files
were byte-identical). Every arm must recreate its dataset, and distinct object
sizes are a pre-run check.

## 6. Workloads

Beyond TPROC-C and TPROC-H, the workloads the competitors are actually measured
on:

| workload | why it is in | engines |
|---|---|---|
| **TPROC-C** (TPC-C-like) | the existing harness; write-heavy OLTP | all |
| **TPROC-H** (TPC-H-like) | analytic scans; the existing harness | all |
| **YCSB A/B/C/D/E/F** | the lingua franca of KV comparisons; RocksDB and WiredTiger are routinely cited on it | all |
| **db_bench-style microbenchmarks** | `fillseq`, `fillrandom`, `readrandom`, `readseq`, `overwrite`, `seekrandom` — RocksDB's own vocabulary, so its users can read our numbers | all |
| **Point-read scaling sweep** | t=1…128 on metal; the dimension libdb has been losing on | all |
| **Range-scan sweep** | selectivity 0.01%…10%; LMDB's strength, so it must be present |all |
| **Large-value / overflow** | 8 B…1 MB values; exposes B-tree vs LSM differences honestly | all |
| **Working-set sweep** | 0.5×…4× RAM; the axis that separates cache-resident from I/O-bound, and where LSM engines earn their keep | all |

## 7. Q1 — stability under pressure (separate phase, no competitors)

Per the request, anything that breaks gets fixed and that benchmark re-run.

- **72-hour sustained TPROC-C** at the highest stable thread count, with
  `db_verify` at the end and `failchk` + env close every hour (this is the shape
  that trips F6)
- **Kill-9 recovery loop** — SIGKILL under load, recover, verify, repeat;
  assert zero data loss for committed transactions
- **Disk-full and ENOSPC** injection against the existing fault-injection tier
- **Sanitizer arms** — ASan/UBSan and TSan under concurrent load, which the
  existing tiers already support
- **Resource exhaustion** — the mutex pool is bounded by `mutex_max`, and P13
  changes mutex lifetime, so this is directly relevant
- **Multi-process** arms throughout; libdb's north star is multi-process shared
  regions and a thread-only soak would miss its distinguishing feature

## 8. Host, cost, and what I would actually spend

`i4i.metal` — 128 vCPU, 30 TB NVMe, 1 TiB RAM — matching the prior
cross-engine run so results are comparable. At **$10.98/hr**:

| phase | duration | cost |
|---|---|---|
| Port + tune (RocksDB, LMDB, Oracle, v5.3.28) | — | engineer time; use a **small** instance, not metal |
| Q1 stability soak | 72 h | **~$790** |
| Q2 comparison matrix | 24–36 h | **~$265–$400** |
| Re-runs after fixes (assume ≥1) | 24 h | **~$265** |
| | **total** | **~$1,300–1,500** |

The porting and tuning work must happen on a cheap box. Doing it on metal would
multiply the cost several times over for no measurement benefit — the single
largest avoidable expense in this plan.

## 9. If you want a result sooner: the Linux-first option

FreeBSD is where the request points, and it is also where libdb is currently
worst for reasons we have already diagnosed. A credible, smaller campaign is
available now:

1. **Finish P13's four-arm matrix on a dedicated Linux metal box** (~$265,
   24 h). This answers the open question blocking a release and is needed
   regardless.
2. **Run Q2 on Linux** with libdb (latest), libdb v5.3.28, WiredTiger and
   Oracle 18.1 — the three engines needing no new port plus the in-repo
   historical point. Defer RocksDB and LMDB to a second pass.
3. **Treat FreeBSD as its own deliverable**: fix F5, then publish a
   *FreeBSD-specific* report, where the headline is honest and interesting —
   "libdb was unusable above one thread on FreeBSD for *N* releases, here is the
   mechanism and the fix."

That last point is worth more to users than a six-engine table, and it is a
story only this project can tell.

## 10. What this plan deliberately does not promise

- **Not "best on all dimensions."** On the prior campaign's own evidence
  (`CROSS-ENGINE-2026-09.md:149`, "Where libdb still loses, plainly") the
  WiredTiger gap is real and grows with threads. A campaign designed to show
  libdb winning everywhere would be designed to mislead. This one is designed to
  find out, and to publish losses with the same prominence as wins.
- **No single summary number.** Per workload, per thread count, per durability
  mode, with cv.
- **No result from an arm whose cv exceeds 10%.** P13's matrix is already
  unreported for exactly this reason, and that precedent must hold even when the
  answer is inconvenient.
