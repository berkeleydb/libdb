# Feature comparison against TidesDB, and what it implies for libdb

Prompted by a comparison against [TidesDB](https://tidesdb.com/) v10.0.1
(`d62d694`), a 17-month-old LSM engine in C. This records which of its features
libdb already has, which are genuinely absent, and which are worth an RFC — so the
list is not re-litigated from a README next time.

**Method note.** Every "libdb already has this" below was checked by grepping for
the *mechanism* and confirming a public entry point in `src/dbinc/db.in`, not by
grepping for a feature name. That distinction matters here: a previous session
claimed libdb lacked group commit on the basis of a name grep, and had to retract
it — the mechanism had been present since Sleepycat.

## Already in libdb — no RFC needed

| feature | libdb entry point |
|---|---|
| 2PC / XA | `DB_TXN->prepare()` (`db.in`), `__txn_prepare` |
| Hot backup | `DB_ENV->backup()` / `DB->backup()`, `src/db/db_backup.c` |
| `compact_range` | `DB->compact()` already takes **start and stop `DBT`s** — it is a range compaction, not whole-db only |
| Compression | `DB->set_bt_compress()`, `src/btree/bt_compress.c` (custom callback pair) |
| Partitioning / "partition range filters" | `DB->set_partition()`, `set_partition_dirs()`, `src/db/partition.c` |
| Column-family-like grouping | subdatabases (multiple named DBs in one file, `__db_master_open`) |
| Rich runtime statistics | **219 `st_*` fields** across the `db.in` stat structs |

Two of these deserve a caveat rather than a flat "we have it":

- **Column families are not the same as subdatabases.** TidesDB's column families
  carry *per-family configuration* (compression, memtable sizing, comparator,
  TTL) and per-family stats. libdb subdatabases share the environment's settings.
  The gap is configurability, not namespacing.
- **"Richer statistics" was a misreading on my part.** libdb exposes more stat
  fields; TidesDB *presents* them better (typed getters per subsystem —
  `get_io_stats`, `get_cache_stats`, `get_stall_stats` — versus libdb's
  print-oriented `*_stat()` calls). That is an ergonomics gap, and it is largely
  what the seven operator health signals added in v2026.09.5 were addressing.

## Genuinely absent from libdb

| feature | verdict |
|---|---|
| **TTL / row expiry** | Absent. Cheap to emulate in the application; as an engine feature it needs a reaper and interacts with recovery and MVCC visibility. **Not RFC-worthy on its own.** |
| **Range deletes** | Absent as a single operation. Today: cursor loop. An engine-level range delete is mostly a write-amplification optimisation, which matters far more for an LSM (tombstone over a range) than for a B-tree that must touch each leaf anyway. **Low value here.** |
| **Key-value separation (blob/vlog)** | Absent. This is the one with a real performance argument — it shrinks the B-tree for large values. Oracle BDB 6.x added blob support under AGPL, so it cannot be imported; a clean implementation would be a genuine RFC. **RFC-worthy, and the strongest candidate on this list.** |
| **Commit hooks** | Absent. Small, but touches the commit path, which is the most safety-critical code in the engine. Would need to state exactly where in the commit sequence the hook fires and what it may legally do (the answer for a durable engine is: very little). **Not worth it without a concrete user.** |
| **Configurable compression *pipeline*** | libdb has a compression *hook*, not a pluggable pipeline of named codecs (Snappy/LZ4/Zstd). The difference is packaging, not capability. **No RFC; possibly a contrib example.** |

## Recommendation

**One RFC, not eleven: key-value separation.** It is the only item on the list
with a mechanism libdb lacks *and* a measurable performance thesis (B-tree fanout
improves when large values move out of leaves), and it is architecturally
interesting rather than merely absent.

Everything else is already shipped, is application-level, or is ergonomics. Note
what the audit actually found: **6 of 11 features were already present.** The
lesson generalises — a feature-list comparison against a younger project mostly
measures *documentation and API ergonomics*, not capability.

## The SSI finding, which is the substantive one

TidesDB implements the same Cahill dangerous-structure rule as libdb, but with
key-precise read/write sets — exactly what **RFC 0005** proposed. Reading its
source rather than its README:

- `src/txn/readset.h` records `(cf_index, key, key_size, seq)` — points only, with
  no interval or gap representation.
- `tidesdb_readset_record` is called from one function, `txn_get_impl`
  (`src/txn/txn.c:339,347`) — point gets only. **Iterators and range scans record
  nothing.**
- It handles the absent-read case correctly (a miss is recorded at the snapshot
  seq, so a later insert of that key is caught).
- But with no predicate for a scan, a **scan-then-write phantom can commit under
  its SERIALIZABLE level**, and its public header does not document the limit.

That is the precise failure RFC 0005 predicted in its phantom section: *"every
option below can be implemented in a way that looks correct, passes
`ssi001`–`ssi011`, lowers the abort rate, and is wrong."* An independent team
built the design competently and shipped the over-approximation loss.

Consequences, recorded in RFC 0005's Decision block:

1. It **raises** the cost of doing key-precise SSI correctly (predicates for every
   scan, plus next-key/gap handling), while the Step-1 measurement says the payoff
   is a knob users already have (`set_pagesize`).
2. RFC 0005 is therefore moved from Draft to **Rejected**, with a concrete
   reopening bar.
3. It is also a **caution about libdb's own advantage**: page granularity is what
   causes ~100% of libdb's false aborts *and* what provides phantom prevention
   (invariant **D10**). They are the same mechanism. TidesDB is genuinely better
   on false aborts and genuinely weaker on range-scan serializability, and those
   two facts have one cause.

## Measured comparison

See `test/bench/TIDESDB-2026-09.md`. Summary: on a dedicated quiet box, TidesDB is
**3.71× on single-threaded transactional writes** (LSM memtable insert versus
B-tree page update) and **1.14× on warm point reads** — near-parity on reads. The
defining structural difference is not performance: TidesDB is **single-process**
(a second `tidesdb_open` on the same directory returns `-12`), where libdb's
multi-process shared-region architecture is most of its complexity and its main
reason to exist.
