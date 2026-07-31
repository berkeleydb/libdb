# LSM design study: adaptive compaction, Bitcask, index-in-WAL, and Amethyst

Status: design note (informs ROADMAP items #9 LSM, #13 HASH, #14 index-in-WAL).

This note compares three log-structured storage models against the
adaptive-compaction ideas in *Amethyst* and proposes a synthesized LSM design
for libdb.

## Models studied

### Adaptive LSM (HanoiDB-style, structure-adaptive)
- **Merge structure:** HanoiDB "Towers of Hanoi" — level *k* holds up to
  `2^(k-1)` runs; overflow merges a level's runs and promotes one run up.
  Bounded write amplification `O(log n)`.
- **Generic backing index:** the per-run/level index is generic (B+tree,
  skiplist, RAX).
- **Read filters:** Bloom + **SuRF** (succinct range filter), with adaptive
  filter selection by workload.
- **Structure-level adaptation:** switch the *whole structure* between three
  modes — `SingleIndex` → `Hybrid` (memtable + 1 index) → `MultiLevel` (full
  LSM) — driven by rolling-window metrics (writes/reads/flushes per minute,
  compaction backlog) with **hysteresis** (a cooldown) to prevent flapping. It
  "spawns" complexity under write pressure and "collapses" back when idle.
- **Merge scheduling:** a merge-strategy knob (Fast / Predictable /
  HanoiIncremental) controls the per-level *work budget* and run selection
  (latency vs smooth throughput). This is **work scheduling, not layout
  policy**.
- **Gap:** no leveled-vs-tiered choice; the layout is fixed (Hanoi).

### Bitcask (log-structured hash)
- Append-only data file + **in-memory hash index** (`key -> offset,len,ts`);
  put = append + index update, delete = tombstone append, get = index + one
  `pread`. Index rebuilt by scanning the log on recovery. A background **merge**
  reclaims dead space from superseded entries.
- This is exactly a **log-structured hash** — the `LSM-HASH ≈ Bitcask` model.

### JE-style index-in-WAL + cleaner
- A **B+tree whose nodes live in the WAL**; a **cleaner** is a log-GC that
  reclaims obsolete log segments (the Berkeley DB Java Edition / Oracle NoSQL
  model); a separate evictor handles cache management; recovery does
  checkpoint/replay.
- Replication in this model rides on a **VLSN** (versioned LSN); consensus can
  use quorum systems + Fast Paxos (ROADMAP #15).
- This is the **index-in-WAL + cleaner** durability model — the cleaner plays
  the same role a compactor does in an LSM.

### Amethyst (paper, Shankar & Rose) — segment-level adaptive compaction
- Each SSTable **segment** carries a compaction identity + lightweight
  read/write counters. A finite-state controller rewrites segments between
  **leveled** and **tiered** layouts, with **cooldowns** to avoid oscillation,
  so the tree converges to the better policy as the workload shifts between
  read- and write-heavy phases (≈2.2× lower runtime than static tiered while
  matching leveled read amplification).
- This is **per-segment layout policy adaptation** — orthogonal to the
  structure-adaptive axis above.

## The key insight: two orthogonal adaptation axes, one mechanism

The structure-adaptive LSM and Amethyst adapt *different things* with the
*same machinery* (rolling counters + cooldown/hysteresis):

| Axis | What it decides | Source | Signal |
|------|-----------------|--------|--------|
| **Structure** (vertical) | SingleIndex vs Hybrid vs MultiLevel — *how much LSM* | structure-adaptive LSM | env-wide write/read/flush rate |
| **Policy** (horizontal) | leveled vs tiered *per segment/level* — *how to compact* | Amethyst | per-segment read/write counters |

Neither model has both. Combining them yields an LSM that (a) only pays LSM
cost when the workload warrants it, and (b) within the LSM, compacts each
segment with the locally-best policy.

## Proposed libdb LSM design (ROADMAP #9)

1. **Base structure:** HanoiDB Towers-of-Hanoi levels for bounded `O(log n)`
   write amplification, with incremental merge scheduling for smooth
   foreground latency.
2. **Unified adaptive controller** with one metrics/cooldown core driving two
   axes:
   - *Structure axis*: SingleIndex ⇄ Hybrid ⇄ MultiLevel by env-wide write
     pressure, with hysteresis.
   - *Policy axis* (Amethyst): each segment/level tagged leveled or tiered from
     per-segment read/write counters, rewritten with cooldowns.
   Both reuse the same rolling-window + cooldown primitive (one implementation,
   two decision sites) — and the same primitive already used by the SSI marker
   GC, keeping the engine's "adaptive" machinery uniform.
3. **Read acceleration:** Bloom + SuRF per run, adaptive selection.
4. **Generic over the access method's page/index** so the LSM composes with
   libdb's existing B-tree/Hash rather than replacing them.

## Mapping to durability configs (ROADMAP #14)

The "index-in-WAL with a cleaner" option is the same idea as LSM compaction,
specialized by access method:

- **HASH → `LSM-HASH` = Bitcask**: append-only log + in-memory hash directory;
  the LSM "compaction" is Bitcask's dead-space merge. Natural for write-heavy
  point-lookup workloads.
- **B-tree → JE-style index-in-WAL + cleaner**: B+tree nodes in the log, a
  cleaner reclaims obsolete segments. The LSM merger and the JE cleaner are the
  same mechanism viewed from two access methods.

So #9 (adaptive LSM) and #14 (index-in-WAL) share one log-structured core; the
HASH review (#13) should evaluate the Bitcask directory against Ctrie/HAMT for
the in-memory index.

## Concrete next steps
- Prototype the unified adaptive controller (structure + segment-policy axes)
  as a standalone module, validated with the Amethyst-style phase-shifting
  workload before wiring it under an access method.
- Port the HanoiDB level math + SuRF as the reference run/level layout.
- Reuse the index-in-WAL cleaner / VLSN learnings for the index-in-WAL config
  and the replication path (#15).
