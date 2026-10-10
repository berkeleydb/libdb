# RFC 0014: Typed data and constraints — a codec socket and a constraint layer

- **Status:** Draft
- **Type:** Prospective
- **Author:** libdb maintainers (draft prepared for review)
- **Date:** 2026-10-10
- **Tracking:** none yet
- **Prototype:** none. This is a design study. No code was written.
- **Review note (2026-10-10):** the two questions this draft marked as
  gating were checked against the source before it was placed here, and both
  resolved favourably: recovery calls no comparator (so `db_recover` needs no
  codecs), and the UNIQUE probe is a dirty fetch (so it is race-free under
  plain SI). See §Recovery and §Concurrency. Nine lesser TODOs remain, all
  citations to prior art rather than claims about libdb.

---

## Reading guide

Each claim about existing code is marked as one of these:

- **[FOUND]** I read it in the code. A `file:line` citation follows.
- **[PROPOSED]** This RFC's design. It does not exist yet.
- **[UNSURE]** I believe it but did not confirm it, or it depends on
  measurement that has not been done.

Paths are relative to each repository's root. The repositories are
`~/ws/libdb` (this tree), `~/ws/postgres/master`, `~/ws/dbsql`, `~/src/je`
and `~/ws/noxu`.

---

## Summary

This RFC proposes two optional subsystems, both selected at configure time
(`--enable-typed`).

1. **A codec socket (the "modem").** An application registers named
   *codecs*. A codec is a type: how to find a value's length, compare two
   values, test equality and hash, and optionally produce an
   order-preserving byte image. The application also registers a *record
   codec*, which finds field N inside a DBT. libdb ships no encodings: no
   ICU, no iconv, no EBCDIC tables. It defines the contract and checks that
   every process registered the same thing.
2. **A small constraint layer built on that socket.** NOT NULL, length and
   shape checks, and UNIQUE on a field inside a DBT come first. Foreign keys
   come later, finishing the `associate_foreign` machinery that libdb
   already has.

The central design choice is that the layer is a **generator, not a new
engine**. It turns a declarative schema into the callbacks libdb already
accepts: `bt_compare`, `dup_compare`, `h_hash`, `h_compare`, `associate`
and `associate_foreign`. It adds as few new enforcement points as it can.
UNIQUE on a field turns out to need almost no new engine code, because
libdb already enforces uniqueness through a secondary index that has no
duplicates ([FOUND] `src/db/db_cam.c:1679-1698`).

It also says plainly what libdb should **not** do: row-level security,
general CHECK assertions across tables, defaults, triggers, and anything
resembling a planner.

---

## Motivation

libdb is the storage layer under SQL engines, document stores and object
layers. Each of those rebuilds the same machinery above it: an encoding for
typed fields, a comparator that knows the encoding, secondary-key
extraction, and uniqueness and referential checks. Each one gets the hard
parts wrong in the same ways:

- **The comparator is not total.** RFC 0007 changed the `bt_compare`
  contract. Under `DB_OPTREAD` the comparator is called on unvalidated, torn
  page bytes and must be total over arbitrary bytes ([FOUND]
  `docs_src/api/c/dbset_bt_compare.md`, section "Page bytes passed to the
  comparison function"; `rfc/0007-optimistic-read-validation.md:358-375`).
  A comparator that decodes a length or type tag from the key and then
  indexes with it, which is exactly what a typed record comparator does,
  can fault. dbsql had to hand-write a bounded pre-validator for this
  reason ([FOUND] `~/ws/dbsql/src/sm/sm_cmp.c:77-121`, `:190-216`).
- **Processes disagree about the comparator.** Comparators are function
  pointers in process memory. libdb's only defence is a sentence in the
  documentation: the comparator "must be the same as that historically used
  to create the database or corruption can occur" ([FOUND]
  `docs_src/api/c/dbset_bt_compare.md`, the paragraph after the
  description). Nothing checks it. JE and noxu both persist the comparator's
  identity and fail the open when it does not match (see §Prior art).
- **The foreign keys that exist are not persisted.** libdb has foreign keys
  ([FOUND] `src/db/db_am.c:1143-1188`, `src/db/db_cam.c:3085-3256`), but
  the relationship lives only in the handles of the process that called
  `associate_foreign`. Another process that opens the same files without
  associating them can insert orphan rows.
- **Collation and encoding.** The maintainer's example is a SQL row that
  mixes types and encodings: one UTF-8 field compared with an ICU collator,
  one EBCDIC field, one custom type. Today the application must write one
  monolithic comparator that knows every field's encoding. The codec socket
  lets the application register each encoding once and compose them per
  field.

Who benefits, and how much, is covered honestly in §dbsql: what it would use
and what it would refuse. In short, dbsql has already built most of this
itself and would take only part of it. A new consumer would get far more.

---

## North-star check

| North star | Status under this proposal |
|---|---|
| Embedded, no server | Unchanged. Everything runs in the calling process. |
| ACID, WAL, recovery | Unchanged. Constraint checks are reads made before a write. The log records the outcome, and recovery never re-runs a check (§Recovery). Recovery never calls codec code. |
| All 4 access methods (+Heap) | Unchanged for databases that do not use the feature. Phase 1 supports **typed Btree and Hash only**, because those are the two access methods whose open path already *fails closed* on an unknown meta flag (§On-disk format). Queue, Recno and Heap need a different marker, deferred to a later phase. |
| Multi-process shared regions | No new shared-region state. All codec and schema state is process-local. Agreement *between* processes is enforced at `DB->open` by checking a persisted fingerprint (§Multi-process). |
| On-disk format | **No change for databases that do not use it.** A typed database carries one new meta flag bit and a catalog record (§On-disk format). An older libdb **refuses** to open a typed Btree or Hash database. |
| Log format | **No new log record types.** Catalog writes are ordinary btree puts. |
| Region format and signature | **No change when the feature is not configured.** When it is, `sizeof(struct __db)` grows by one pointer under `#ifdef HAVE_TYPED`, which changes the build signature. This is the same precedent `HAVE_COMPRESSION` already sets for `struct __btree` and `struct __cursor` (§Region signature). A zero-change alternative exists but costs a lookup on every put. |
| ABI | Additive only: new functions and new flag and error constants, all present only in a `--enable-typed` build. No existing signature changes. |
| Footprint, no mandatory dependencies | Off by default. When it is on, the estimate is roughly 2–3k lines of C in Phase 1 [UNSURE]. No ICU and no iconv: the application links those itself. |

---

## Prior art: what exists, with citations

### libdb today: the hooks this must build on

**[FOUND] The callback surface.** The relevant `DB` methods are
`associate` (`src/dbinc/db.in:1640`), `associate_foreign` (`:1642`),
`set_bt_compare` (`:1724`), `set_bt_compress` (`:1726`), `set_bt_prefix`
(`:1730`), `set_dup_compare` (`:1734`), `set_h_compare` (`:1743`),
`set_h_hash` (`:1746`) and `set_partition` (`:1757`). The btree comparator
and prefix pointers live in the process-local `struct __btree`
(`src/dbinc/btree.h:487-490`). The secondary callback is
`DB.s_callback` in `struct __db` (`src/dbinc/db.in`, the "Secondary
callback" field in the secondary-index block).

**[FOUND] DBT has an application pointer that libdb never reads.**
`struct __db_dbt` carries `void *app_data` (`src/dbinc/db.in:234`). dbsql
uses it to pass an unpacked probe record to its comparator, so that a seek
never re-decodes the probe ([FOUND] `~/ws/dbsql/src/sm/sm_cursor.c:704-720`,
`~/ws/dbsql/src/sm/sm_cmp.c:197-209`). This is the "abbreviated / pre-decoded
probe" trick, and the codec layer should keep it available.

**[FOUND] Secondary indexes already enforce uniqueness.** When a secondary
is opened *without* `DB_DUP`, `__dbc_put_secondaries` looks up the new
secondary key with `DB_SET | rmw`. If the key is present with a different
primary key, it fails the put with "Put results in a non-unique secondary
key in an index not configured to support duplicates" and returns
**`EINVAL`** (`src/db/db_cam.c:1679-1698`). This is UNIQUE on a derived
field. Two things are wrong with it as a constraint mechanism:

1. The error is `EINVAL`, which the caller cannot tell apart from a
   programming error.
2. It compares the old primary key with `__bam_defcmp` (`:1686`), which is
   bytewise. That is correct only while primary keys are compared bytewise.

**[FOUND] The callback can veto the put.** If `s_callback` returns any
error other than `DB_DONOTINDEX`, the put fails (`src/db/db_cam.c:1522-1531`).
`DB_DONOTINDEX` means "no secondary key", which is exactly the
`UNIQUE NULLS DISTINCT` behaviour: NULLs are not indexed, so they never
collide.

**[FOUND] Foreign keys exist, and they are incomplete.**
`DB->associate_foreign(foreign, secondary, callback, flags)`
(`docs_src/api/c/dbassociate_foreign.md:11-16`):

- *Insert side:* after the secondary callback produces a key, the put looks
  that key up in the foreign database with `DB_SET | rmw`. A miss returns
  `DB_FOREIGN_CONFLICT` (`src/db/db_cam.c:1533-1586`; the error is defined
  at `src/dbinc/db.in:1381`).
- *Delete side:* `__dbc_del` calls `__dbc_del_foreign` *before* deleting
  (`src/db/db_cam.c:446-453`). It applies one of three actions
  (`src/db/db_cam.c:3166-3236`):
  - `DB_FOREIGN_ABORT` returns `DB_FOREIGN_CONFLICT` (`:3196-3200`). This
    is SQL `RESTRICT`.
  - `DB_FOREIGN_CASCADE` deletes the referencing primaries (`:3208-3218`).
  - `DB_FOREIGN_NULLIFY` calls the application's nullify callback and
    rewrites the primary (`:3219-3233`). This covers both `SET NULL` and
    `SET DEFAULT`, because the callback may write any value.
- *Restrictions* (`src/db/db_iface.c:2174-2213`): the foreign database may
  not be a secondary, may not allow duplicates, and may not be a
  renumbering Recno. The associating database must be a secondary.
  NULLIFY requires a callback.
- *State:* everything is process-local. The `__db_foreign_info` record is
  allocated with `__os_malloc` (`src/db/db_am.c:1155`) and linked into
  `fdbp->f_primaries` (`:1174-1176`). The struct is defined at
  `src/dbinc/db_am.h:63-79`.
- **Small defect noticed while reading:** `__db_associate_foreign` inserts
  `f_info` into the foreign database's list *before* it checks
  `pdbp->s_foreign != NULL` and returns `EINVAL` (`src/db/db_am.c:1174-1186`).
  A failed second association therefore leaves a dangling entry. This is
  unrelated to the RFC and worth its own fix.

**How complete libdb's foreign keys are** [FOUND for what exists; the
judgement is mine]:

| SQL feature | libdb |
|---|---|
| Insert into child must match parent | yes |
| ON DELETE RESTRICT | yes (`DB_FOREIGN_ABORT`) |
| ON DELETE NO ACTION (check deferred to end of statement or commit) | **no**. There is no deferral mechanism. |
| ON DELETE CASCADE | yes |
| ON DELETE SET NULL / SET DEFAULT | yes, through the NULLIFY callback |
| ON UPDATE of the parent key | n/a: a libdb key is never updated in place; a key change is a delete plus a put, so DELETE rules apply |
| Composite FK, MATCH SIMPLE / FULL / PARTIAL | only by encoding a composite key in the callback. MATCH SIMPLE is achievable by returning `DB_DONOTINDEX` when any part is NULL. MATCH FULL is not expressible. MATCH PARTIAL is not implemented in PostgreSQL either (see below). |
| Child needs an index on the FK column | **required** (the child side must be a secondary). PostgreSQL does not require one. |
| Persisted, so every process enforces it | **no**. This is the largest gap. |
| Self-referencing FK | no: the foreign database may not be the secondary's own primary through this API [UNSURE: not tested]. |

**[FOUND] Configure-time precedent.** Optional subsystems are
`AC_ARG_ENABLE` blocks in `dist/aclocal/options.m4`, for example
compression at `:35-43` and partitioning at `:90-99`. Each defaults to
`$db_cv_build_full` and turns into an `AC_DEFINE(HAVE_*)` plus a
conditional object list in `dist/configure.ac:1092-1130`. A disabled
access method links a stub object instead (`hash_stub`, `heap_stub`,
`qam_stub`, in `dist/configure.ac:1104-1130`). The most recent precedent,
`--with-uring`, follows the same pattern: detect by default, with an
explicit `--without-` and a hard error when it was explicitly requested
but is missing (`dist/configure.ac:220-261`).

**[FOUND] Compression is the template for "a meta flag that a build without
the feature refuses".** A btree created with compression sets
`BTM_COMPRESS` (`src/dbinc/db_page.h:107`). A library built without
`HAVE_COMPRESSION` refuses it with "compression support has not been
compiled in" (`src/btree/bt_open.c:261-267`). The compression fields in the
process-local `struct __btree` are themselves `#ifdef HAVE_COMPRESSION`
(`src/dbinc/btree.h:491-499`).

### PostgreSQL: the constraint taxonomy and the type/operator system

**[FOUND] Constraint kinds.** `pg_constraint.contype` takes the values
`c` CHECK, `f` FOREIGN, `n` NOT NULL, `p` PRIMARY, `u` UNIQUE, `t`
constraint trigger and `x` EXCLUSION
(`src/include/catalog/pg_constraint.h:197-204`). Every constraint carries
`condeferrable`, `condeferred`, `conenforced` and `convalidated`
(`:53-57`). `conperiod` marks `WITHOUT OVERLAPS` / `PERIOD` keys
(`:113-117`). The FK action and match types are in `confupdtype`,
`confdeltype` and `confmatchtype` (`:96-98`). A CHECK expression is stored
as a serialized node tree, `conbin` (`:184-186`). **SQL ASSERTION is
reserved and not implemented:** "For SQL-style global ASSERTIONs, both
conrelid and contypid would be zero. This is not presently supported"
(`:70-73`), and `CONSTRAINT_ASSERTION` is marked "for future expansion"
(`:221`).

**[FOUND] Where checks run.**

- NOT NULL and CHECK run per row in `ExecConstraints`
  (`src/backend/executor/execMain.c:2039`) and `ExecRelCheck` (`:1837`).
  A NULL result from a CHECK is not a failure, by SQL rule, so the code
  uses `ExecCheck` rather than `ExecQual` (`:1891-1897`).
- RLS `WITH CHECK` runs in `ExecWithCheckOptions` (`:2287`).
- The partition constraint runs in `ExecPartitionCheck` (`:1915`).
- Unique and exclusion constraints run during index insertion
  (`src/backend/executor/execIndexing.c`, file header).
- Foreign keys run as AFTER triggers (`src/backend/utils/adt/ri_triggers.c`).

**[FOUND] UNIQUE is atomic inside the index access method.** The nbtree
comment states the protocol. `_bt_check_unique` can only see keys already
in the index, so concurrent inserts are serialized by holding a write lock
"on the first page the value could be on". Any other inserter of the same
key must take the same page lock (`src/backend/access/nbtree/nbtinsert.c:190-200`).
If the conflicting tuple's transaction is still in progress, the inserter
releases the lock, waits for that transaction (`XactLockTableWait`, or
`SpeculativeInsertionWait`), and searches again (`:213-232`). The check
reads with a *dirty* snapshot, not the transaction's MVCC snapshot
(`:433`). Before it reports a violation, it calls
`CheckForSerializableConflictIn`, so that SSI can surface a serialization
failure in place of a unique violation (`:632-640`). NULL keys skip the
check entirely (`:120-140`), unless the index is `NULLS NOT DISTINCT`
(`src/backend/access/nbtree/nbtutils.c:136-144`).

**[FOUND] Exclusion constraints scan after inserting and accept a
deadlock risk.** The file header of `execIndexing.c` explains it. After
inserting the index tuple, a separate scan looks for conflicts. Two
concurrent inserters can each see the other's tuple and wait on each other;
the deadlock detector aborts one of them. The comment calls this "fairly
harmless, as one of them was bound to abort ... anyway". For speculative
insertion, which must not abort, the rule is that "the transaction with the
higher XID backs out". The implementation is
`check_exclusion_or_unique_constraint` (`execIndexing.c:705`), which uses
a dirty snapshot (`:792`), waits and restarts (`:823`, `:883-900`), and
re-reads a conflicting tuple under the MVCC snapshot for SSI (`:907-925`).

**[FOUND] Foreign keys are triggers, and they take a row lock on the
parent.** The insert-side check queries the parent with
`SELECT ... FOR KEY SHARE OF x` (`ri_triggers.c:521`). Under REPEATABLE
READ or SERIALIZABLE, a check that must "detect new rows" runs against the
*latest* snapshot and errors if it finds rows that are not visible to the
transaction snapshot: this is the "crosscheck snapshot" (`:2685-2704`). The
check runs as the table owner with `SECURITY_NOFORCE_RLS`, so RLS cannot
hide a parent row from the RI check (`:2713-2717`). MATCH PARTIAL is
`#ifdef NOT_USED`: "not implemented" (`:402-411`).
TODO: cite the archive thread on *why* FKs are triggers.

**[FOUND] Error messages are an inference channel, and PostgreSQL closes
part of it.** `ExecBuildSlotValueDescription` returns NULL, meaning "print
no row values", when RLS is active on the table (`execMain.c:2466-2471`).
Otherwise it prints only the columns the user may SELECT (`:2478-2484`).
The *existence* of the conflict still leaks, as the error itself.
TODO: archive thread on RLS and unique violations.

**[FOUND] B-tree support functions, the closest analogue to a codec.**
An operator class supplies `BTORDER_PROC` (1, a 3-way compare),
`BTSORTSUPPORT_PROC` (2), `BTINRANGE_PROC` (3), `BTEQUALIMAGE_PROC` (4),
`BTOPTIONS_PROC` (5) and `BTSKIPSUPPORT_PROC` (6)
(`src/include/access/nbtree.h:690-723`). **`equalimage`** is static
information meaning "`order` returns 0 only when A and B are interchangeable
without any loss of semantic information". Deduplication is enabled only
when *every* key column's opclass says so (`doc/src/sgml/btree.sgml:460-548`).
Text opclasses register `btvarstrequalimage`, which returns
`locale->deterministic` (`src/backend/utils/adt/varlena.c:2311-2323`), so
deduplication is off under a nondeterministic collation. Most other
opclasses register the unconditional `btequalimage`
(`src/backend/utils/adt/datum.c:433`).
TODO: `pg_type` I/O functions; `sortsupport.c` abbreviated keys;
`pg_locale.c` deterministic vs nondeterministic; hashing under
nondeterministic collations; collation version mismatch.

TODO: deferrable constraints (after-trigger queue, `SET CONSTRAINTS`);
`NOT VALID` / `VALIDATE CONSTRAINT` in `tablecmds.c`; RLS in
`rowsecurity.c` and `pg_policy`; domains; generated columns.

### dbsql: the real customer

**[FOUND] Rowid tables.** The key is an 8-byte big-endian integer with the
sign bit flipped, so `memcmp` order equals integer order. These tables use
libdb's default comparator (`~/ws/dbsql/src/sm/sm_cmp.c:218-236`;
`~/ws/dbsql/.agent/steering/STORAGE.md:66-68`). This is a hand-rolled
order-preserving codec.

**[FOUND] Indexes and WITHOUT ROWID tables.** The key is the whole SQLite
record, the data is empty, and `DB_DUP` is not used. A custom comparator
`__sm_rec_cmp` is installed with `set_bt_compare`
(`~/ws/dbsql/src/sm/sm.c:1015-1017`). The per-index `key_info_t`, which
holds the collations and sort flags, is attached through `app_private`
(`sm.c:1001`; the struct is defined at
`~/ws/dbsql/src/inc/dbsql_int.h:2674-2682`).

**[FOUND] dbsql met the OPTREAD totality contract on its own.** Its
NORTH_STAR invariant 6 reads: "The B-tree comparison function is invoked by
libdb's lock-free descent on unvalidated page bytes. It must never read
outside its two DBTs, allocate, lock, or fault on arbitrary input"
(`~/ws/dbsql/.agent/NORTH_STAR.md:40-43`). The implementation:

- `__sm_varint_bounded` never reads past its end pointer
  (`sm_cmp.c:41-61`).
- `__sm_rec_valid` walks the record header with explicit bounds and checks
  that every serial-type length fits (`:87-121`).
- A malformed record "sorts after every well-formed one and reads nothing
  outside its DBT" (`:185-186`, `:210-213`).

This is the pattern the codec layer should generalise.

**[FOUND] Collations come from the application.** The ICU extension
registers collations with `ucol_open` / `ucol_strcoll`
(`~/ws/dbsql/src/ext/icu/icu.c:430-445`, `:483`). The comparator reaches
them through `key_info_t.colls[]` and `__mem_compare`
(`sm_cmp.c:159-160`). The comparator "never calls libdb, never takes a
libdb lock, and does not allocate. The one exception is a user collation
registered for a different text encoding, which upstream also converts"
(`STORAGE.md:87-89`). In other words, under OPTREAD a UTF-16 collation can
allocate.

**[FOUND] Constraints are enforced in the VDBE, above libdb.**

- UNIQUE is "enforced by the VDBE (`OP_NoConflict`), exactly as upstream"
  (`STORAGE.md:72-73`; code generation at
  `~/ws/dbsql/src/cg_insert.c:2512-2515`).
- NOT NULL is `OP_HaltIfNull` (`cg_insert.c:2026`).
- CHECK is generated expression code (`cg_insert.c:2061-2096`).
- Each constraint has an SQL conflict-resolution action (ROLLBACK, ABORT,
  FAIL, IGNORE, REPLACE) with semantics specific to its kind
  (`cg_insert.c:1860-1893`).
- Foreign keys are SQLite's counter scheme. Deferred violations are counted
  per connection, and "when a commit fails due to a deferred foreign key
  constraint, there is no way to tell which foreign constraint is not
  satisfied, or which row" (`~/ws/dbsql/src/cg_fkey.c:20-48`). The opcodes
  are `OP_FkCounter` and `OP_FkIfZero` (`~/ws/dbsql/src/vdbe/vdbe.c:7633-7664`).

**[FOUND] dbsql serialises writers itself.** Each database has one writer,
enforced by a `DB_LOCK_WRITE` lock taken with `DB_LOCK_NOWAIT` on a
per-database lock object (`~/ws/dbsql/src/sm/sm.c:565-590`;
`STORAGE.md:99`). Its check-then-insert UNIQUE is therefore race-free by
construction, and none of the snapshot-isolation traps in §Concurrency
apply to it.

**[FOUND] dbsql uses only the public API** ("No libdb-internal symbols,
headers, or struct layouts", `~/ws/dbsql/.agent/NORTH_STAR.md:19-20`). It
must match SQLite 3.53.4 semantics exactly (`:31-35`).

See §dbsql: what it would use and what it would refuse.

### Berkeley DB Java Edition DPL

**[FOUND] Declarative keys and relationships.**

- `@SecondaryKey(relate=..., relatedEntity=..., onRelatedEntityDelete=...)`:
  `relate()` is required and fixes cardinality ONE_TO_ONE, MANY_TO_ONE,
  ONE_TO_MANY or MANY_TO_MANY, which in turn fixes unique versus duplicate
  secondary keys (`~/src/je/src/com/sleepycat/persist/model/SecondaryKey.java:74-134`).
- `relatedEntity` makes the field a foreign key (`:137-170`).
- `onRelatedEntityDelete` defaults to ABORT and also offers CASCADE and
  NULLIFY (`:173-203`; `DeleteAction.java:35-51`). These are libdb's three
  `DB_FOREIGN_*` actions, by the same authors.
- A null secondary key field is simply not indexed (`SecondaryKey.java:38-40`).
  That is NULLS DISTINCT.

**[FOUND] Ordering comes from a sort-preserving encoding, not a callback.**
"This sort order is based on a storage encoding that allows a fast
byte-by-byte comparison" (`persist/model/PrimaryKey.java:57-61`). The tuple
package says it directly: "custom comparators often reduce performance
because comparators are called very frequently during Btree operations",
and a format must be "designed so that a byte-by-byte unsigned comparison
results in the natural sort order" (`bind/tuple/package.html:9-25`).
Examples are `writeSortedFloat`, `writeSortedDouble`,
`writeSortedPackedInt` and `writeSortedBigDecimal`
(`bind/tuple/TupleOutput.java:273`, `:292`, `:495`, `:638`). Composite
keys order their fields by `@KeyField(n)` (`persist/model/KeyField.java:25-67`).

**[FOUND] The comparator rules JE wrote down are the totality and
recovery rules.** A custom `Comparable` key class must:

- always return the same result for the same inputs;
- not depend on state that may change, such as the default locale;
- not assume the store is open, because "the comparison method is called
  during database recovery";
- not assume it sees only present keys, because it "will occasionally be
  called with deleted keys or with keys for records that were not part of a
  committed transaction".

(`persist/model/KeyField.java:105-125`.)

**[FOUND] Comparator identity is persisted.** JE stores the comparator
(serialized, or by class name) in the database record
(`je/dbi/DatabaseImpl.java:162`) and rebuilds it at open, including during
recovery (`:441-463`). The DPL's `PersistComparator` rebuilds its binding
"without access to the stored catalog since recovery is not complete"
(`persist/impl/PersistComparator.java:68-106`).

**[FOUND] JE has `equalimage` under another name.** A
`BinaryEqualityComparator` "considers two keys to be equal if and only if
they have the same length and they are equal byte-per-byte", and that
property enables internal optimizations ("blind puts" with bloom filters)
(`je/BinaryEqualityComparator.java`, class comment;
`je/DatabaseConfig.java:539-546`).

**[FOUND] Class evolution.** Key data is **not** versioned: "the physical
key format for an index is fixed once the index has been opened", and
"Changing the behavior of a Comparable key class is likely to make the
index unusable" (`persist/evolve/package.html:21-45`). Entity (data)
records **are** versioned, through a per-record format id
(`persist/impl/PersistEntityBinding.java:137-140`) and a catalog database
(`persist/impl/Store.java:93`, `com.sleepycat.persist.formats`;
`persist/impl/PersistCatalog.java:62-110`). Incompatible changes need
version-specific `Renamer`, `Deleter` or `Converter` mutations
(`persist/evolve/package.html:106-210`).

**What the DPL could do only because Java has reflection** [my judgement]:
discover the fields, read and write them without generated code
(`ReflectionAccessor`), serialize a comparator object into the database
record, and instantiate a class by name during recovery. C has none of
these. The codec socket replaces each of them with an explicit,
application-supplied table, and replaces "instantiate by name" with "every
process must register the named codec, and libdb verifies it".

### noxu (Rust, the JE lineage): the no-reflection analogue

**[FOUND] Derive macros replace reflection.** `#[derive(Entity)]`,
`#[derive(PrimaryKey)]` and `#[derive(SecondaryKey)]` mirror JE's three
annotations (`~/ws/noxu/crates/noxu-persist-derive/src/lib.rs:14-52`). The
`SecondaryKey` derive emits an extractor closure per field
(`:597-601`). This is the C equivalent of a generated `associate` callback.

**[FOUND] Order-preserving encodings are the default.**
`PrimaryKey::to_sortable_bytes` is documented as "order-preserving,
self-delimiting ... the same approach as JE's tuple format"
(`crates/noxu-persist/src/entity.rs:87-118`). The table of encodings is in
`crates/noxu-bind/src/tuple/sort_key.rs:9-35`: sign-flipped big-endian
integers, IEEE floats with a sign-conditional flip, and strings
null-escaped with a `00 00` terminator.

**[FOUND] Comparator identity replaces class names.** "A Rust `Fn` has no
portable name and cannot be reconstructed from a string, so Noxu's
faithful adaptation asks the application to supply that name itself"
(`crates/noxu-db/src/database_config.rs:39-53`). The identity is persisted,
and a mismatch at open fails with `ComparatorMismatch` unless an override
flag is set (`crates/noxu-dbi/src/environment_impl.rs:2152-2175`;
`docs/src/maintainer/design-decisions.md:381-410`). **Recovery has no
closure, so noxu redoes in byte order and re-sorts the tree once the real
comparator is attached** (`design-decisions.md:405-410`). libdb does not
need this, because its recovery is physical (§Recovery).

**[FOUND] Foreign-key actions are metadata only in the DPL layer:**
"the DPL `open_secondary_index` path does not yet wire this
`on_related_entity_delete` attribute ... the field is metadata only"
(`crates/noxu-persist/src/secondary_spec.rs:70-77`). This is a cautionary
data point: declaring a constraint is easy, and wiring enforcement is the
actual work.

**[FOUND] Per-record version envelope.** Each entity record carries
`[2-byte class_version][tag len][tag]` ahead of the payload, and adding it
was a breaking change for existing stores
(`crates/noxu-persist/src/evolve/envelope.rs:1-32`).

---

## The breadth pass: every constraint option

Legend.

- **Class:**
  - **S** structural: needs only the DBT bytes, length and key/data role.
  - **C** codec: needs a field located and decoded, then compared, hashed
    or tested.
  - **P** predicate: arbitrary application code.
  - **X** cross-record: reads other records, so it interacts with locking
    and isolation.
- **When:** W at write, T at commit, R at read, V at validation time.

| # | Constraint | PostgreSQL / Oracle / SQLite / JE DPL | When | Class | Cost | libdb verdict |
|---|---|---|---|---|---|---|
| 1 | NOT NULL | PG `contype='n'`, checked in `ExecConstraints` (`execMain.c:2039`). SQLite `OP_HaltIfNull` (`cg_insert.c:2026`). DPL: primitive fields cannot be null; key fields are non-null | W | C (the record codec reports `isnull`) | one field walk per put | **Phase 1** |
| 2 | Max length, fixed width | SQL `varchar(n)`, `char(n)`. libdb Recno/Queue already have `re_len`, and Queue records are fixed-length by construction | W | S for a whole-DBT bound; C for a per-field bound | trivial | **Phase 1** (whole-DBT bound and per-field bound) |
| 3 | Record conforms to schema (well-formed) | implicit in every RDBMS (the input functions reject malformed text) | W | C (record codec `validate`) | one walk per put | **Phase 1**. This is also what makes generated comparators safe (§Totality). |
| 4 | Enum membership, range on one field (simple domain CHECK) | PG domain CHECK; SQL `CHECK (x BETWEEN ..)` | W | C with declarative bounds; P in general | small | Phase 2, declarative forms only |
| 5 | CHECK on one row, across fields | PG `contype='c'`, `conbin` expression (`pg_constraint.h:184-186`), with NULL meaning pass (`execMain.c:1891-1897`). SQLite generated VDBE code (`cg_insert.c:2061-2096`). Oracle CHECK | W | P | the predicate's cost | Phase 2 as a **named, registered predicate**. Never as an expression language. |
| 6 | UNIQUE on a field inside the DBT (the maintainer's example) | PG unique index, atomic in the AM (`nbtinsert.c:190-232`). SQLite `OP_NoConflict` in the VDBE (`cg_insert.c:2514`). DPL `relate=ONE_TO_*` | W | C + X | one secondary lookup and put per write | **Phase 1**, reduced to a non-DUP secondary (§Phase 1) |
| 7 | UNIQUE NULLS DISTINCT (SQL default) | PG skips the check for NULL keys (`nbtinsert.c:120-140`). DPL does not index null keys (`SecondaryKey.java:38-40`) | W | C | none (NULL is not indexed) | **Phase 1**: the generated callback returns `DB_DONOTINDEX` |
| 8 | UNIQUE NULLS NOT DISTINCT | PG `indnullsnotdistinct` (`nbtutils.c:143-144`) | W | C | as #6 | **Phase 1**: index NULL as a reserved byte image |
| 9 | PRIMARY KEY | PG `contype='p'` = UNIQUE + NOT NULL. libdb: the key of a non-DUP database is unique by construction | W | S, plus C for NOT NULL parts | none extra | already exists. `DB_NOOVERWRITE` gives INSERT rather than UPSERT. |
| 10 | FOREIGN KEY, insert side | PG RI trigger with `FOR KEY SHARE` (`ri_triggers.c:521`). libdb `associate_foreign` (`db_cam.c:1533-1586`). DPL `relatedEntity` | W (immediate) or T (deferred) | X | one parent lookup per child put | exists. Phase 2 adds persistence. |
| 11 | FK ON DELETE NO ACTION | PG: checked at end of statement, deferrable | T | X | per-txn work list | Phase 3, needs a pre-commit hook |
| 12 | FK ON DELETE RESTRICT | PG: immediate. libdb `DB_FOREIGN_ABORT` (`db_cam.c:3196-3200`) | W | X | one secondary probe | exists |
| 13 | FK ON DELETE CASCADE | libdb `DB_FOREIGN_CASCADE` (`db_cam.c:3208-3218`); DPL CASCADE | W | X | per child | exists |
| 14 | FK ON DELETE SET NULL / SET DEFAULT | libdb `DB_FOREIGN_NULLIFY` plus a callback (`db_cam.c:3219-3233`) | W | X + C | per child | exists. Phase 2 *generates* the nullify callback from the codec (write the NULL or default image of field k). |
| 15 | FK ON UPDATE (any action) | PG `confupdtype` | W | X | — | n/a for libdb primary keys (a key change is delete + put). Above libdb otherwise. |
| 16 | MATCH SIMPLE / FULL / PARTIAL | PG implements SIMPLE and FULL (`ri_triggers.c:380-400`); PARTIAL is not implemented (`:402-411`) | W | C + X | — | SIMPLE in Phase 2 (`DB_DONOTINDEX` when any part is NULL). FULL in Phase 2 (reject mixed NULLs at write). PARTIAL: **never**. |
| 17 | Exclusion constraint (general) | PG `EXCLUDE USING gist`: post-insert scan, deadlock risk accepted (`execIndexing.c` header; `:705-952`) | W | C + X + a non-btree index | index-dependent | **Never in general.** libdb has no GiST. |
| 18 | 1-D non-overlap (`WITHOUT OVERLAPS`, period keys) | PG `conperiod` (`pg_constraint.h:113-117`) | W | C + X | predecessor and successor probe | Phase 3 [UNSURE about worth]. On a btree keyed `(group, start)`, the intervals in a group are non-overlapping if and only if each new interval clears its predecessor's end and its successor's start. Two neighbour probes suffice. Safe under 2PL and SSI. **Not safe under plain SI** (§Concurrency). |
| 19 | DEFERRABLE INITIALLY DEFERRED / IMMEDIATE | PG `condeferrable`/`condeferred` (`pg_constraint.h:54-55`). Deferred unique uses `UNIQUE_CHECK_PARTIAL`, then a later `UNIQUE_CHECK_EXISTING` (`execIndexing.c` header). SQLite FK counters (`cg_fkey.c:20-48`) | T | X | per-txn list plus a commit-time pass | Phase 3. Requires a pre-commit hook in `__txn_commit`, and nested transactions must merge their lists into the parent. |
| 20 | NOT VALID + VALIDATE | PG `convalidated` (`pg_constraint.h:57`). Enforced for new writes; a later scan validates the old rows | V | as the underlying constraint | one full scan | Phase 2. A catalog flag plus a validate pass that scans under `DB_TXN_SNAPSHOT`. |
| 21 | NOT ENFORCED | PG `conenforced` (`pg_constraint.h:56`) | — | — | none | Phase 2 (a catalog flag; the planner-hint use does not apply to libdb) |
| 22 | Domain (type + constraints) | PG `contypid` (`pg_constraint.h:66-74`), `CONSTRAINT_DOMAIN` (`:220`) | W | C (+P) | as its checks | Phase 2: a codec may carry declarative bounds |
| 23 | Generated column, stored | PG `attgenerated` | W | P | per write | **Never**. The application computes it when it builds the record. |
| 24 | Generated column, virtual | PG virtual generated (`execMain.c:2058-2071`) | R | P | per read | **Already exists in another form**: a secondary key *is* a virtual generated column, computed by the `associate` callback. Nothing to add. |
| 25 | DEFAULT | everywhere | W (record construction) | P | — | **Never**. It is record construction, which belongs to the application. |
| 26 | Row-level security: read filter (`USING`) | PG `pg_policy` and `rowsecurity.c`. Oracle VPD (predicate injection), Oracle Label Security | R | P + an identity model | per row read | **Never** (§What libdb must not do) |
| 27 | RLS write check (`WITH CHECK`) | PG `ExecWithCheckOptions` (`execMain.c:2287`) | W | P + identity | per write | **Never** as security. A registered predicate (#5) can express "a writer may only write rows tagged X" as a correctness check. |
| 28 | Column-level privileges | PG column ACL (`execMain.c:2478-2484`) | R/W | identity | — | **Never** |
| 29 | Inference channels (unique-violation leaks a hidden row; FK check sees hidden parents) | PG suppresses row values in errors under RLS (`execMain.c:2466-2471`), but the error itself still leaks. RI checks bypass RLS on purpose (`ri_triggers.c:2713-2717`) | — | — | — | Recorded as the reason #26–#28 are never. |
| 30 | Append-only / immutable rows | Oracle blockchain/immutable tables; PG via triggers | W | **S** | none | Phase 2. Force `DB_NOOVERWRITE`, reject `del` and `DB_CURRENT` puts. Needs no codec. |
| 31 | Write-once field | trigger in PG | W | C (old field image equals new) | old-record fetch, which `__dbc_put_primary` already does when secondaries exist | Phase 2 |
| 32 | System-versioned temporal table (SQL:2011) | MariaDB, SQL Server, Db2; Oracle Flashback | W | X + P | a history write per update | **Never**. It is a second table maintained by policy. (libdb MVCC is not a history store.) |
| 33 | Cardinality ("at most N children") | trigger or ASSERTION | W | X + count | count of duplicates | **Never as a guarantee.** It is a write-skew shape: two inserters each count N-1. Correct only under SSI or 2PL. A per-parent counter record (a single row both writers must update) is the application pattern that works under SI. |
| 34 | Aggregate invariant (sum over a group), `CREATE ASSERTION` | SQL standard; implemented by almost no RDBMS. PG reserves it (`pg_constraint.h:70-73`, `:221`) | T | X + arbitrary query | unbounded | **Never.** General assertions need either re-evaluating a query on every write or incremental view maintenance, and under SI they are write-skew-prone. That is why nobody ships them. |
| 35 | Monotonic key (new key > every existing key) | sequences; Queue/Recno `DB_APPEND` | W | S (compare with `DB_LAST`) | a hot last-page lock | exists for Recno/Queue. For Btree, Phase 2 [UNSURE about worth]. |
| 36 | Gap-free sequence | invoice numbers. PG explicitly does not provide one | W/T | X (serialises every allocator) | a single-writer bottleneck | **Never** in libdb. `DB_SEQUENCE` is gap-ful by design. A gap-free counter is one record updated under a write lock, which an application can already do. |
| 37 | Queue: fixed-length, consume in order | libdb Queue `re_len`, `DB_CONSUME`, `DB_INORDER` | W/R | S | — | exists |
| 38 | Heap: data constraints | libdb Heap assigns RIDs and has no key semantics | W | C | — | via secondaries (#6), once Heap gets a fail-closed marker (§On-disk) |
| 39 | Partition constraint (a row belongs to its partition) | PG `ExecPartitionCheck` (`execMain.c:1915`) | W | C | — | n/a: libdb's `set_partition` *routes* rows rather than rejecting them. A typed range partition is a generated `set_partition` callback, Phase 2. |
| 40 | Global UNIQUE across partitions | PG cannot, unless the key includes the partition key | W | C + X | — | **Comes free**: a libdb secondary is one database, independent of how the primary is partitioned [UNSURE: associate on a partitioned primary not tested]. |
| 41 | Constraint triggers (`contype='t'`) and triggers generally | PG; noxu runtime triggers (`noxu-db/src/database_config.rs:9-20`) | W/T | P | — | **Never**. libdb's callbacks already are its triggers. |

---

## Classification, and the concurrency question

### By what libdb must understand

- **Purely structural (no codec):** #2 (whole-DBT bound), #9, #30, #35,
  #37. libdb can enforce these without understanding the data.
- **Codec only (locate a field, decode it, compare or test it):** #1, #3,
  #4, #7, #8, #22, #31. Each is a pure function of the one record being
  written.
- **Arbitrary predicate:** #5, #23, #25, #27. These are the application's
  code on the application's data. libdb can *call* them; it cannot reason
  about them.
- **Cross-record or cross-database:** #6, #10–#20, #33, #34, #36. These are
  the only ones where correctness depends on isolation.

### UNIQUE without a check-then-insert race

**[FOUND] Under locking (default, `DB_READ_COMMITTED`, 2PL):** the
uniqueness probe on the secondary uses `DB_SET | rmw` whenever
`STD_LOCKING` (`src/db/db_cam.c:1496`, `:1679-1683`, `:1858-1863`). It
takes a write lock on the leaf where the key belongs, and that lock is held
until commit. A second inserter of the same key blocks on that lock, then
finds the key. This is PostgreSQL's "lock the first page the value could be
on" (`nbtinsert.c:190-200`), with libdb's ordinary page lock in place of
the buffer lock.

**[PROPOSED analysis; partly FOUND] Under `DB_TXN_SNAPSHOT` (plain SI):**
the probe reads the transaction's snapshot, so it can miss a duplicate that
a concurrent transaction committed after the snapshot was taken. The
*insert* still has to modify the same leaf page. In libdb MVCC, fetching a
page dirty when a newer version exists than the one this transaction can
see returns `DB_LOCK_DEADLOCK` (`src/mp/mp_fget.c:455-467`). dbsql measured
exactly this: "A stale snapshot txn that writes a page updated after its
snapshot gets `DB_LOCK_DEADLOCK`" (`~/ws/dbsql/.agent/steering/STORAGE.md:154-155`).
So a concurrent duplicate surfaces as an update conflict rather than as a
uniqueness error. The retry then sees the committed duplicate and gets the
uniqueness error. **Correctness holds, because no duplicate commits, on one
condition: the probe and the insert must target the same page.** The probe
is a lookup of the exact key and the insert is at that key's position, so
they do. A split between them changes which page holds the key, but the
splitter wrote the page, so the version check still fires.
**[FOUND — verified by the maintainer's reviewer]** The probe *is* a dirty
fetch, so the conflict is detected **at the probe**, before the insert:
`rmw = STD_LOCKING(dbc) ? DB_RMW : 0` (`db_cam.c:1861-1863`), and
`STD_LOCKING` is true under `DB_TXN_SNAPSHOT` because it tests only
`LOCKING_ON` and the absence of CDB (`db_int.in:589-591`). A `DBC_RMW`
cursor searches with `SR_FIND_WR` (`bt_cursor.c:2390`, `:2610`, `:2617`),
which carries `SR_WRITE` (`btree.h:150`). `SR_WRITE` makes the descent
take `DB_LOCK_WRITE` at the leaf (`bt_search.c:659-660`) and sets
`get_mode = DB_MPOOL_DIRTY` (`:938`, `:1269`, `:1355`), which is passed to
the leaf `__memp_fget` (`:1497-1498`). `__memp_fget` with `dirty` set on a
buffer whose version chain has a newer entry returns `DB_LOCK_DEADLOCK`
(`mp_fget.c:466-467`). So under plain SI, a concurrent committed duplicate
on the same leaf fails the probe with an update conflict, and the retry
sees it. One caveat this does not cover: the probe-to-insert window on the
secondary is safe because both touch the same leaf, but the **primary**
write and the secondary probe are on different files. The primary's own
write lock and dirty fetch protect only the primary key, and the secondary
leaf is protected only by the probe above. That is sufficient for UNIQUE,
whose whole content is the secondary leaf.

**Under `DB_TXN_SERIALIZABLE` (SSI):** SI plus SIREAD rw-conflict
detection (RFC 0003). The page-level write conflict above still applies, so
UNIQUE needs nothing extra.

**What is *not* safe under plain SI:** any check whose read and write touch
*different* pages. These include:

- 1-D non-overlap (#18), where the neighbour probe can read a page the
  insert does not write;
- cardinality limits (#33);
- the FK insert-side check when the parent is deleted concurrently.

For the FK case, PostgreSQL uses the crosscheck snapshot plus
`FOR KEY SHARE` (`ri_triggers.c:2685-2704`, `:521`). libdb's insert-side
probe uses `DB_SET | rmw` on the parent (`db_cam.c:1556-1580`), which takes
a write lock under 2PL. Under MVCC it should behave as SELECT FOR UPDATE
does, through the same dirty-fetch version check (verified above for the
secondary probe; the FK parent probe is the same `DB_SET | rmw` shape). So the
parent leaf gets copied on every FK check, which is a real
cost under MVCC [UNSURE: not measured].

**[PROPOSED] Rule:** each constraint kind declares which isolation levels
it is correct under. A constrained database used from a transaction whose
isolation cannot guarantee the constraint fails the operation with `EINVAL`
at the first write. It does not silently weaken the guarantee. UNIQUE, FK
and PK are correct at all levels. #18 and #33 require 2PL or SSI.

---

## The codec ("modem") subsystem — design

### What a type is

**[PROPOSED]**

```c
/* Codec function results. */
#define	DB_CODEC_MALFORMED	(-30780)	/* value bytes are not well formed (TODO: pick a free number) */

typedef struct __db_codec DB_CODEC;
struct __db_codec {
	const char *name;		/* Stable identity, e.g. "app.text.icu.de-u-ks-level2". */
	u_int32_t   version;		/* Bump whenever ordering/equality/hash semantics change. */

#define	DB_CODEC_TOTAL		0x0001	/* Every function is total over arbitrary bytes. */
#define	DB_CODEC_MEMCMP		0x0002	/* memcmp(a,b) has the same sign as compare(a,b). */
#define	DB_CODEC_EQUALIMAGE	0x0004	/* compare(a,b)==0 iff a and b are byte-identical. */
#define	DB_CODEC_FIXED		0x0008	/* Every value is exactly fixed_len bytes. */
	u_int32_t   flags;
	u_int32_t   fixed_len;

	/* All functions: reads bounded by [p, p+len); must not allocate,
	 * lock, call libdb, or fault; must be deterministic. */
	int	  (*compare)(const DB_CODEC *, const u_int8_t *a, u_int32_t alen,
		      const u_int8_t *b, u_int32_t blen);
	int	  (*equal)(const DB_CODEC *, const u_int8_t *a, u_int32_t alen,
		      const u_int8_t *b, u_int32_t blen);	/* NULL: compare()==0 */
	u_int32_t (*hash)(const DB_CODEC *, const u_int8_t *, u_int32_t);
						/* NULL only if EQUALIMAGE (then bytes are hashed) */
	int	  (*validate)(const DB_CODEC *, const u_int8_t *, u_int32_t);
						/* 0 or DB_CODEC_MALFORMED */
	int	  (*sortkey)(const DB_CODEC *, const u_int8_t *, u_int32_t,
		      u_int8_t *out, u_int32_t outlen, u_int32_t *needed);
						/* Optional: memcmp-ordered image (ucol_getSortKey, strxfrm, JE sorted tuple) */
	void	   *app_private;		/* e.g. the UCollator * */

	/* Golden vectors: an ordered list the codec must reproduce. */
	const DBT  *vectors;
	u_int32_t   nvectors;
};
```

- **compare / equal / hash.** `hash` must agree with `equal`. That means
  equal values hash equal, which is the rule PostgreSQL relies on when it
  hashes text under a nondeterministic collation (TODO: cite `hashtext`).
  When `equal` is coarser than byte equality (case-insensitive ICU, say),
  `hash` must hash a normalised form. The usual choice is the collation's
  sort key.
- **`DB_CODEC_EQUALIMAGE`** is PostgreSQL's `equalimage`
  (`btree.sgml:460-548`) and JE's `BinaryEqualityComparator`. When every
  field in a key is EQUALIMAGE, libdb may use bytewise equality for the
  whole key: in the UNIQUE probe's "same primary?" test (today hard-coded
  `__bam_defcmp`, `db_cam.c:1686`), in hash bucket equality, and in a future
  btree deduplication. A deterministic ICU collation may set it. A
  nondeterministic one must not. This is the line that broke deduplication
  in PostgreSQL (`varlena.c:2311-2323`), and the flag is how the codec says
  which side of it it is on.
- **`DB_CODEC_MEMCMP`** is the performance flag. When every field of a key
  is MEMCMP (JE's sorted tuple formats, noxu's `SortKey`, dbsql's rowid
  key), the generated database uses **libdb's default comparator**. That
  means no callback per comparison, `set_bt_prefix` and prefix compression
  stay valid, and `DB_OPTREAD` is safe by construction, because the
  default `__bam_defcmp` is bytewise and bounded.
- **`sortkey`** is the bridge from a non-MEMCMP type to a MEMCMP *index*.
  A secondary built on `sortkey(field)` stores the order-preserving image as
  its key. That secondary needs no comparator. Under a case-insensitive
  collation, uniqueness also becomes `memcmp` on sort keys, because equal
  strings produce equal sort keys at the chosen strength. This is the
  recommended path for UNIQUE on a collated text field. A comparator
  callback is then needed only where the key must stay decodable *and*
  cannot be order-preserving, which is dbsql's SQLite-record-as-key case.
- **Abbreviated keys** (PostgreSQL sortsupport; TODO cite `sortsupport.c`).
  For libdb these matter only inside the comparator. `DBT.app_data` already
  lets a caller pass a pre-decoded probe (`db.in:234`), and dbsql uses it
  (`sm_cursor.c:704-720`). The codec layer keeps that channel. It adds no
  abbreviated-key API in Phase 1. Skipped: add it when a profile shows
  decode cost inside `bt_compare` dominating.
- **Golden vectors** feed the multi-process fingerprint (below).

### What a record schema is

**[PROPOSED]** libdb does not own the record format. dbsql must keep
SQLite's record format bit for bit (`NORTH_STAR.md:31-35`), a JE-style
application has its tuple format, and a C application has structs. So the
schema is:

```c
typedef struct __db_record_codec DB_RECORD_CODEC;
struct __db_record_codec {
	const char *name;
	u_int32_t   version;
	u_int32_t   nfields;
	/* Locate field i of rec.  Total: bounded by rec->size; a malformed
	 * record returns DB_CODEC_MALFORMED, never faults. */
	int (*field)(const DB_RECORD_CODEC *, const DBT *rec, u_int32_t i,
	    const u_int8_t **p, u_int32_t *len, int *isnull);
	/* Optional: walk every field once (bounded), the __sm_rec_valid pattern. */
	int (*validate)(const DB_RECORD_CODEC *, const DBT *rec);
	const DB_CODEC **types;		/* per-field codec, nfields entries */
	void *app_private;
};
```

- **Fixed or variable layout.** The application's `field` function decides.
  libdb provides one built-in helper, `db_record_fixed(offsets[], lens[])`,
  for C-struct records. It is about 30 lines and saves every C user writing
  the same thing. Nothing else is built in.
- **NULL representation** belongs to the record codec (`*isnull`). libdb
  does not invent one.
- **Key versus data.** A schema binds a record codec to the key, to the
  data, or to both. A field reference is `(DB_KEY | DB_DATA, field_no)`.
- **Per-field collation and encoding: the maintainer's mixed-encodings
  example.** `types[3]` is the application's EBCDIC codec. `types[1]` is
  its ICU UTF-8 codec built over a `UCollator` held in `app_private`.
  `types[0]` is a sign-flipped integer marked MEMCMP. The application
  registers each codec once, by name, and refers to it from as many fields
  as it likes:

  ```c
  db_env->codec_register(db_env, &app_ebcdic_cp037);
  db_env->codec_register(db_env, &app_icu_de_level2);
  ...
  static const DB_CODEC *row_types[] = { &app_i64_be, &app_icu_de_level2,
      &app_blob, &app_ebcdic_cp037 };
  ```

- **Schema evolution.** **Keys are not versioned.** JE's conclusion,
  that "the physical key format for an index is fixed"
  (`evolve/package.html:21-45`), is the right one. Changing a key codec
  means building a new index. Data records *may* be versioned, but only by
  the application's record codec, for example a version byte it reads
  inside `field()`. libdb adds no per-record envelope. noxu's envelope was a
  breaking format change (`envelope.rs:20-25`). What the catalog records is
  which record-codec `(name, version)` *pairs* a database has been written
  with (`[v1, v2]`), so an open with only `v2` registered can refuse rather
  than misread. The renamer/deleter/converter machinery stays above libdb.

### How it composes with the existing hooks: a generator

**[PROPOSED]** `DB->set_schema(DB *, const DB_SCHEMA *)` is called before
`DB->open`. At open it installs:

| Schema says | libdb installs (existing hook) |
|---|---|
| key fields all MEMCMP | nothing: default comparator, default prefix |
| key fields not all MEMCMP | generated `bt_compare` = bounded walk + per-field `compare`; **no** `bt_prefix` |
| Hash DB | generated `h_hash` (per-field `hash`, combined) and `h_compare` (per-field `equal`) |
| sorted dups | generated `dup_compare` |
| UNIQUE(field k) | a secondary DB (non-`DB_DUP`) plus a generated `associate` callback that emits `sortkey(field k)` (or the raw field if MEMCMP), and `DB_DONOTINDEX` for NULL (NULLS DISTINCT) |
| FK(field k) → P | generated `associate` on a DUP secondary, plus `associate_foreign(P, secondary, generated_nullify or NULL, action)` |
| NOT NULL, length, validate | **the one new enforcement point**: a pre-put validator (below) |

**The one new hook [PROPOSED].** `__dbc_put` gets a call, under
`#ifdef HAVE_TYPED`, to `__typed_put_check(dbc, key, data, flags)`, made
before the secondaries and the primary are written. It runs NOT NULL,
length, record validation and registered predicates on the *full* new
record. Partial puts are materialised first, which
`__dbc_put_primary` already does for secondaries (the `DBC_PUT_HAVEREC`
state, `db_cam.c:1199`, `:1596-1603`). TODO: confirm that every put path
(`DB->put`, `DBC->put`, `DB_MULTIPLE` / bulk, `DB_APPEND`, compression)
funnels through one function, and name it.

### The `DB_OPTREAD` totality contract

**[FOUND]** Under `DB_OPTREAD` the comparator, prefix, dup-compare and
compression callbacks may see torn bytes. `dbt2.size` may belong to a
different record from `dbt2.data`, and the callback must be total
(`docs_src/api/c/dbset_bt_compare.md`, section "Page bytes passed to the
comparison function"). The 2026-10 amendment to RFC 0007 says "Containment
is not consistency" (`rfc/0007-optimistic-read-validation.md:358-375`).

**[PROPOSED] How the codec layer guarantees it:**

1. **A generated comparator never hands a length it has not checked to
   user code.** It first runs a bounded walk over the record (the record
   codec's `field` / `validate`, the `__sm_rec_valid` pattern,
   `sm_cmp.c:87-121`). Each field slice `(p, len)` passed to a codec lies
   inside the DBT, because the walk proved it.
2. **Malformed input orders deterministically.** If either side fails the
   walk, the generated comparator does not call any codec. A malformed
   record sorts after every well-formed one, two malformed records compare
   by `(memcmp, then length)`, and the result is a total order over all
   byte strings. (dbsql: `sm_cmp.c:210-213`.)
3. **Codec functions must still be total within their slice.** A
   well-formed slice of torn bytes is still arbitrary bytes to the codec.
   A codec that does not set `DB_CODEC_TOTAL` makes `DB->open` fail with
   `EINVAL` when the environment has `DB_OPTREAD` set. Totality is a
   property libdb cannot verify (the documentation says so), so it must be
   declared, and an undeclared codec is refused instead of trusted.
4. **MEMCMP keys need no callback**, so they are safe whatever the
   application code does.
5. **A conformance harness, not a proof.** `test/typed/codec_fuzz` feeds
   every registered codec random, truncated and torn inputs under ASan, with
   a per-input timeout. It is the same shape as `test/fuzz`. It can catch a
   violation. It cannot certify totality.

The alternative RFC 0007 itself names, copying the candidate key before
validating and comparing afterwards (`rfc/0007-...md:380-385`), would make
point 3 unnecessary at the cost of a copy per interior level. That is the
core's decision to make, not this layer's.

### Multi-process agreement (the hardest part)

**The problem.** Codecs are function pointers. They cannot live in the
shared region, so every process must register the same codecs. If two
processes disagree, one inserts keys in an order the other cannot find.
Results are silent misses, duplicate keys that defeat UNIQUE, and trees
that `db_verify` reports as out of order. **libdb's existing comparators
already have this problem.** The only mitigation is the documentation
sentence quoted in §Motivation. JE persists the comparator
(`DatabaseImpl.java:162`, `:441-463`). noxu persists an identity and fails
the open on a mismatch (`environment_impl.rs:2152-2175`).

**[PROPOSED] Persisted identity plus a behavioural fingerprint, checked at
every open:**

1. The catalog (§On-disk) stores, for each typed database, every codec
   `(name, version, flags)` it uses and a **fingerprint**: a hash of the
   results of running each codec's `compare` over its own golden vectors,
   plus `hash` on each vector.
2. At `DB->open`, a process that has not registered every named codec at
   the stored version gets `EINVAL` ("typed database requires codec X v3").
   **This turns "forgot to register" into a refusal to open instead of
   corruption.**
3. At open, libdb recomputes the fingerprint from the registered functions.
   A mismatch means same name, same version, different behaviour, and it
   gets `DB_CODEC_MISMATCH` (a new error). The motivating case is an ICU
   upgrade. PostgreSQL's collation version tracking addresses the same
   problem (TODO: cite `pg_locale.c` collversion check). An ICU library
   upgrade changes collation without anyone bumping a version, and the
   golden vectors catch it whenever the vectors exercise the change.
4. **Stated limit.** A fingerprint can only show disagreement on the
   vectors. Two codecs that agree on every vector and differ elsewhere pass
   the check. The check converts the common failures (missing
   registration, version skew, a library upgrade that changes common
   strings) into errors. It cannot convert all of them. The golden vectors
   are the application's responsibility, and the documentation must say
   what makes a good set.
5. **Untyped databases are untouched.** Today's raw `set_bt_compare` keeps
   working exactly as before, with no checking. The fingerprint is a
   property of typed databases only.

### Recovery

**[FOUND / argued]** PostgreSQL never re-checks constraints during WAL
replay. (The pgsql-hackers citation was not located in this pass; the
argument below stands on the structure of redo, not on an archive quote.)
The reason is that redo is
physical: a WAL record says "put these bytes at this place", and the
constraint decision was made before the record was written. Replay must be
deterministic, and it must not depend on catalog state or user code that
may differ at replay time.

For libdb the same argument holds if libdb recovery never calls
`bt_compare`, `dup_compare`, `h_hash` or `s_callback`. **[FOUND — verified
by the maintainer's reviewer, not by the drafting agent]** It does not. The
ten recovery files (`src/{btree/bt,db/crdel,db/db,dbreg/dbreg,fileops/fop,
hash/hash,heap/heap,qam/qam,repmgr/repmgr,txn/txn}_rec.c`) contain **zero**
references to `bt_compare`, `dup_compare`, `h_compare`, `h_hash`,
`s_callback`, `__bam_cmp`, `__bam_search`, `__ham_lookup`,
`__ham_call_hash` or `__dbt_defcmp`. The two paths this draft flagged as
risks reduce to cursor-adjustment helpers in `src/btree/bt_curadj.c`
(`__bam_ca_delete`, `__bam_ca_di`, `__bam_ca_undodup`, `__bam_ca_rsplit`,
`__bam_ca_undosplit`, called from `bt_rec.c:1309` and `:1612-1629`), and
each of those bodies has **zero** comparator calls: they move cursors by
page number and index, never by key. Recovery is physical page redo/undo
keyed by LSN and (pgno, indx). **So `db_recover` can stay usable with no
application codecs**, and the JE fallback below is not needed. (Caveat: this
is a grep over today's tree; a future recovery path that searches by key
would reopen the question, which is why Phase 1's test list includes "run
standalone `db_recover` against a typed environment with no codecs
registered".) Secondaries are separate databases whose writes are
logged as their own operations. Recovery replays those page operations; it
does not re-derive secondary keys from primaries [FOUND by structure: the
secondary put is an ordinary `__dbc_put` on `sdbc`, `db_cam.c:1703-1704`,
which logs normally].

**[PROPOSED]** `db_recover` stays usable with no application codecs. It
never opens the catalog and never checks fingerprints. That check is a
`DB->open` concern of a *non-recovery* open (`DB_AM_RECOVER` set means
skip it). If verification shows that some recovery path does call the
comparator, then the honest fallback is JE's ("the comparison method is
called during database recovery", `KeyField.java:119-121`): typed
databases require recovery from inside the application, with codecs
registered, and a standalone `db_recover` refuses a typed environment.
That would be a usability regression worth knowing about before
acceptance.

---

## On-disk format and compatibility

**[FOUND] The meta-page facts that decide this.**

- Btree: `__bam_metachk` rejects any meta flag outside `BTM_MASK = 0x0ff`
  through `__db_fchk` (`src/btree/bt_open.c:156-158`;
  `src/dbinc/db_page.h:100-108`). All eight bits are already used.
- Hash: `__ham_metachk` rejects anything outside
  `DB_HASH_DUP | DB_HASH_SUBDB | DB_HASH_DUPSORT` (`src/hash/hash_open.c:194-197`;
  `src/dbinc/db_page.h:131-133`).
- **Heap and Queue: no flag check at all** (`src/heap/heap_open.c:80-121`,
  `src/qam/qam_open.c:180-232`). They check the version and nothing else.
- The generic `metaflags` byte (`db_page.h:81-84`) is tested for specific
  bits only (`src/db/db_open.c:457`, `:671-674`). Unknown bits are
  **ignored**.
- Every access method rejects an unknown *version*
  (`bt_open.c:130-144`, `heap_open.c:93-99`, `qam_open.c:194-210`).

**[PROPOSED] A typed database carries:**

1. **A meta flag bit:** `BTM_TYPED 0x100` for Btree and
   `DB_HASH_TYPED 0x08` for Hash. These are added to the accepted mask
   **only under `#ifdef HAVE_TYPED`**.
   - *An old libdb* (any release before this one) opening a typed Btree or
     Hash fails at `__db_fchk` with `EINVAL`. **It fails closed.** It
     cannot write rows that skip UNIQUE.
   - *A new libdb built without `--enable-typed`* fails the same way,
     through the same code. No new code is needed in the disabled build, so
     it stays byte-identical. An optional `#else` branch could give a
     clearer message, as compression does at `bt_open.c:261-267`, but that
     would be a change to the disabled build and is left to the maintainer.
2. **A catalog**, as ordinary records in an ordinary btree named
   `__db_typed`. It holds the schema (codec names, versions, flags,
   fingerprints, field references, constraints and their
   `validated`/`enforced` state) and the list of record-codec versions
   written. Location is decision D3:
   - (a) a subdatabase in the same file, which works only for files opened
     with subdatabases, and which is what dbsql's one-file layout would
     want (`STORAGE.md:47-55`);
   - (b) one per-environment file, as JE does (`Store.java:93`);
   - (c) both, (a) when possible.
   Catalog writes are ordinary transactional puts, so there are **no new
   log record types**.
3. **Nothing else.** The meta page's unused space (`unused2[92]` in
   `db_page.h:116`) is not touched. A schema digest there would be
   convenient, but it would need its own logging and verification story,
   and the catalog already holds the digest.

**Heap, Queue, Recno.** Heap and Queue have no flag check, so an old libdb
would open a flagged file and silently skip enforcement. **It would fail
open.** The fail-closed alternatives are a version bump, which conflates
"typed" with "format version" and drags in `DB->upgrade` and `db_upgrade`,
or a typed marker in a place both old open paths already check, and there
is none. **[PROPOSED] Phase 1 refuses `set_schema` on Heap, Queue and
Recno.** Recno shares the Btree meta page, so it could carry `BTM_TYPED`;
it is deferred only because a record-number key gives a codec little to do.
Decision D4.

**Utilities.** These tools have no codecs: `db_dump`, `db_load`,
`db_verify`, `db_stat`, `db_hotbackup`, `db_recover`.

- `db_dump`, `db_stat` and `db_hotbackup` read bytes and are unaffected.
- `db_load` into a typed database must refuse, because it cannot enforce
  constraints. The meta flag does this for it.
- `db_verify` cannot check the order of a non-MEMCMP typed tree, which is
  already true today for any custom comparator. It should report "ordering
  not checked: typed database, codecs not registered" rather than claim
  success or failure. [UNSURE what `db_verify` does today with a
  custom-comparator tree and no comparator.]
- A **codec plugin** (`DB_CODEC_PATH`, `dlopen` of an application `.so`
  that registers codecs) would make every utility work. It is deferred to
  Phase 3 and is decision D8.

---

## Region signature and handle size

**[FOUND] The signature hashes more than shared structs.**
`__env_struct_sig` hashes `sizeof` of every struct marked `SHARED` and,
unless `HAVE_MIXED_SIZE_ADDRESSING`, of every struct declared in
`src/dbinc/db.in`, `db_int.in` and `src/dbinc/*.h`. That includes
process-local handles: `__db` (`src/env/env_sig.c:120`), `__btree`
(`:144`), `__db_foreign_info` (`:146`) and `__db_env` (`:128`). The list is
generated by `dist/s_sig`, which scans exactly those headers. A mismatch
fails `DB_ENV->open` with "Build signature doesn't match environment"
(`src/env/env_region.c:51`, `:265-267`).

**[FOUND] A configure option already changes it.** `struct __btree` and
`struct __cursor` have `#ifdef HAVE_COMPRESSION` members
(`src/dbinc/btree.h:491-499`, `:312`). A `--disable-compression` build
already cannot attach to an environment created by a default build, and the
reverse holds too.

**[FOUND] Handle sizes are pinned.** `test/db/handle_sizes.c:51` asserts
`sizeof(DB) == 1744` on LP64. Its header records the decision that handle
structs are internal and that a size change must be deliberate
(`:20-28`).

**[PROPOSED] Choice (decision D2):**

- **(a) One pointer, `void *typed_internal`, in `struct __db` under
  `#ifdef HAVE_TYPED`.** This is the boring choice and follows the
  compression precedent. A disabled build is unchanged: same signature,
  same size, and `handle_sizes` passes untouched. An enabled build has a
  different signature and is therefore incompatible *with a disabled build
  on the same environment*. That is the same as today's
  compression-on/compression-off situation. `handle_sizes.c` gains one
  `#ifdef HAVE_TYPED` expected value. All other new structs (`DB_CODEC`,
  schema, catalog cache) go in a header *outside* `src/dbinc/`, say
  `src/typed/typed_int.h`, so `s_sig` does not scan them. Only the one
  pointer moves the signature.
- **(b) Zero struct change:** a process-local hash from `DB *` to typed
  state, looked up on each put. The signature is identical even when the
  feature is enabled, so enabled and disabled builds can share an
  environment. The cost is a lookup per put on *every* database, typed or
  not, plus lifetime coupling to `DB->close`. [UNSURE: the cost is
  probably small but unmeasured.]

**No new shared-region state either way.** No constraint state lives in
the region. Deferred constraints (Phase 3) would keep their per-transaction
work list in the process-local `DB_TXN`, not in `TXN_DETAIL`. If that ever
needs to be shared, as for a prepared transaction that must survive
failover, it would be a region change and needs its own RFC.

---

## dbsql: what it would use and what it would refuse

**What it would refuse, and why** [my judgement, from the code above]:

- **libdb-enforced UNIQUE / NOT NULL / CHECK.** SQLite's conflict
  resolution (ROLLBACK, ABORT, FAIL, IGNORE, REPLACE, UPSERT;
  `cg_insert.c:1860-1893`), statement-level rollback of partial work, and
  the exact error text and timing are part of the specification dbsql must
  match (`NORTH_STAR.md:31-35`). A libdb error raised at `put` time cannot
  express `OR REPLACE` ("the other row that conflicts ... is removed",
  `cg_insert.c:1883-1884`) or `OR IGNORE`. dbsql will keep `OP_NoConflict`.
- **libdb foreign keys.** SQLite's FKs are a per-connection counter with
  immediate and deferred variants and statement-level semantics
  (`cg_fkey.c:20-80`). libdb's FKs fire per put and cannot be deferred.
  The mismatch is semantic, not a missing feature.
- **A libdb record format.** dbsql must store SQLite records byte for byte,
  which is why the design lets the application supply the record codec.
- **Anything that needs libdb internals** (`NORTH_STAR.md:19-20`).

**What it would use:**

- **The totality and fingerprint machinery.** dbsql's comparator is exactly
  the risky shape: decode varints and serial types, then compare with
  application collations. It would gain:
  1. open-time refusal when a second process opens with a different
     collation set, or a different ICU, which today corrupts silently;
  2. `codec_fuzz` coverage of `__sm_rec_cmp`;
  3. the OPTREAD gate on `DB_CODEC_TOTAL`.
- **MEMCMP and EQUALIMAGE flags.** These would let libdb know that rowid
  tables are bytewise. They already are, using the default comparator, so
  dbsql gains nothing there. They matter more for `DB_CODEC_EQUALIMAGE`
  on index keys with BINARY collation [UNSURE about value].
- **A fix it needs regardless:** libdb bug R1 ("an equal put revives a
  deleted item with its old key bytes", `~/ws/dbsql/.agent/libdb-bugs/README.md`
  R1; `STORAGE.md:227-232`). R1 is an *equality-is-not-image* bug: libdb
  assumes that compare()==0 means the key bytes may be kept. With
  `DB_CODEC_EQUALIMAGE` known per database, libdb would know when keeping
  the old key bytes is legitimate. **This is the strongest link between the
  RFC and a real defect, and the fix should not wait for the RFC.**

**What dbsql builds that this would have given it for free**, if it had
existed first: the bounded record walk (`sm_cmp.c:41-121`), the
malformed-sorts-last rule, and the sign-flipped rowid encoding. That is
roughly 250 lines, and it is the hardest 250 lines to get right.
Everything else dbsql built is SQL semantics and stays in dbsql.

**Who gains more:** a C application or a non-SQL layer (document store,
object store, Java DPL over libdb, Tcl test suite) that has no constraint
engine of its own. For those, UNIQUE-on-a-field plus persisted FKs plus
safe comparators is most of what they need.

---

## Phased plan

### Phase 0 — fixes that do not need the subsystem

These are worth doing whether or not the RFC is accepted. Each is a
`test/db/` runner with a teeth arm.

1. Uniqueness through a non-DUP secondary returns `EINVAL`
   (`db_cam.c:1697-1698`). Return `DB_KEYEXIST`, or a new
   `DB_CONSTRAINT_UNIQUE`, so callers can tell a constraint violation from
   misuse. Note that this changes behaviour for existing callers that test
   for `EINVAL`, so it may need a flag. Decision D6.
2. `__db_associate_foreign` leaks a list entry on its `EINVAL` path
   (`db_am.c:1174-1186`).
3. R1 (equal-key put revives deleted key bytes), already reported by
   dbsql.

### Phase 1 — codec socket, UNIQUE on a field, NOT NULL, shape

**API [PROPOSED]**

```c
int DB_ENV->codec_register(DB_ENV *, const DB_CODEC *);
int DB_ENV->record_codec_register(DB_ENV *, const DB_RECORD_CODEC *);
int DB->set_schema(DB *, const DB_SCHEMA *);	/* before DB->open */
int DB->get_schema(DB *, const DB_SCHEMA **);
int db_record_fixed(DB_RECORD_CODEC *, u_int32_t n,
    const u_int32_t *off, const u_int32_t *len, const DB_CODEC **types);

/* DB_SCHEMA: record codec for key and data, plus constraints[] of
 * { kind (NOT_NULL|MAXLEN|VALID|UNIQUE), field ref, nulls_distinct,
 *   name, unique_db (the secondary handle, opened by the caller) }. */

/* New errors (additive): DB_CONSTRAINT_VIOLATION, DB_CODEC_MISMATCH. */
```

The UNIQUE secondary is a database the application opens and passes in,
exactly as with `associate` today. The layer does not create hidden
databases behind the application's back, so there are no handle-lifetime
surprises of the kind dbsql measured (`STORAGE.md:167-194`).

**Changes:** configure option; `struct __db` pointer (D2); BTM/HASH flag bit;
catalog records; `__typed_put_check` call in the put path; generated
callbacks; open-time verification; OPTREAD gate. No log, no region
(beyond D2).

**Tests (each must fail when its fix is reverted):**

- `typed_unique`. Two transactions insert the same field value. Exactly one
  commits, under each of default locking, `DB_TXN_SNAPSHOT` and
  `DB_TXN_SERIALIZABLE`. *Sabotage arm:* make the generated callback emit
  a constant secondary key per call (salted) → duplicates commit → test
  must FAIL.
- `typed_unique_mp`. The same insert from two **processes**.
- `typed_notnull` and `typed_maxlen`, each with a sabotage arm that removes
  the `__typed_put_check` call.
- `typed_codec_mismatch`. Process A creates the database with codec v1.
  Process B opens with v2 and must get `EINVAL`. Process C registers v1
  with different behaviour and must get `DB_CODEC_MISMATCH`. *Sabotage:*
  skip the fingerprint check, then show that C's inserts make `db_verify`
  (run with the right codec) report misordering. That proves the check is
  what prevents it.
- `typed_old_reader`. Open a typed database with a library built without
  `--enable-typed` and expect `EINVAL`. Also expect `EINVAL` from the
  `v2026.10.3` release library, if CI can fetch it.
- `typed_recover`. Kill -9 mid-transaction, run `db_recover` with **no**
  codecs, then reopen with codecs and check the constraints (full scan
  through a reference implementation).
- `typed_optread_totality`. Run under `DB_OPTREAD` with a codec that lacks
  `DB_CODEC_TOTAL`; the open must refuse. Then fuzz the generated
  comparator with torn inputs under ASan.
- Footprint gate: a default build's `libdb.so` text size and
  `__env_struct_sig()` are unchanged from master (recorded numbers).

**What could go wrong.**

- The put-path hook misses a path. Bulk, `DB_APPEND`, compressed btree and
  partial puts are the suspects.
- Golden vectors give false confidence.
- Applications set `DB_CODEC_TOTAL` without meaning it.
- The catalog drifts from the files after a `DB->rename` or `DB->remove`
  of one database.

**Effort:** 4–6 engineer-weeks including tests [UNSURE]. **Risk:** medium.
The engine changes are small. The volume is in tests and in the catalog's
lifecycle (rename, remove, truncate, upgrade, backup).

### Phase 2 — persisted foreign keys, registered predicates, validation

- FK definitions in the catalog. Opening a child database that has an FK
  in the catalog, without an `associate_foreign` to a parent opened in the
  same environment, fails the open. This closes the largest gap.
- Generated nullify callback (SET NULL / SET DEFAULT).
- Composite FK with MATCH SIMPLE and MATCH FULL.
- Named predicates (`DB_ENV->predicate_register(name, version, fn)`) for
  CHECK. They fall under the same fingerprint rule, except that a predicate
  has no golden vectors. The name and version are the only identity, and
  the documentation must say so.
- `NOT VALID` plus `DB->validate_constraint(name)` (a scan).
- Append-only, write-once, declarative domain bounds.

**Effort:** 4–5 weeks [UNSURE]. **Risk:** medium. The FK-under-MVCC cost
needs measuring (the parent-leaf copy, §Concurrency).

### Phase 3 — only if a consumer asks

- Deferred constraints through a pre-commit hook in `__txn_commit`, with
  child-to-parent list merge. **High risk**, because it touches commit.
- 1-D non-overlap.
- Codec plugin for utilities (`dlopen`).
- Typed Heap and Queue (needs D4).

### Never (in libdb)

- **Row-level security, VPD, label security, column privileges, any notion
  of a principal.** libdb is a library in the caller's address space, and
  every process attached to the environment maps the whole buffer pool. A
  visibility filter enforced inside libdb protects nothing from the code
  it would be filtering for. Isolation between principals is a **process
  and file boundary**: separate databases or environments, OS
  permissions, and `set_encrypt` per database. An RLS-like filter would
  also leak through counts (`DB->stat`), `DB->key_range`, record numbers,
  `DB_GET_RECNO`, join cursors and every uniqueness or FK error (see #29,
  and PostgreSQL's partial mitigation at `execMain.c:2466-2471`). Getting
  that right is a research project. Getting it wrong while calling it
  security is worse than not offering it.
- `CREATE ASSERTION` and cardinality guarantees (#33, #34).
- Defaults, stored generated columns, general triggers, system-versioned
  history.
- An expression language for CHECK. Predicates are C functions.
- Shipped encodings or collations (ICU, iconv, EBCDIC tables).
- Query planning, statistics for planning, cost models. **The line:**
  libdb may evaluate a constraint on *one record being written* and do
  *keyed lookups* the schema names explicitly (the UNIQUE secondary, the FK
  parent). It never chooses an access path, never scans to answer a
  question (except the explicit validate pass), and never joins.

---

## Alternatives considered

1. **Do nothing: documentation plus a comparator-fuzz harness.** This
   delivers about half the value (totality coverage) for about 5% of the
   cost. It leaves the multi-process mismatch and non-persisted FKs as they
   are. It is a reasonable answer if no consumer besides dbsql is expected.
2. **A separate library over the public API (`libdb_typed`).** This needs
   no engine change and no signature change. It cannot do the meta flag
   bit (so old readers fail open), it cannot hook every put path without
   wrapping every handle method, and it would need `app_private`, which
   belongs to the application (dbsql uses it, `sm.c:1001`). It is viable
   for Phase 1 minus the fail-closed marker [UNSURE whether that is
   acceptable].
3. **Ship a libdb record format (a JE-style tuple) and build constraints on
   it.** This is simpler internally, and dbsql would refuse it. It
   contradicts "libdb does not ship encodings".
4. **Store codec code in the database (bytecode, WASM).** This would solve
   multi-process agreement completely. It adds a VM to the footprint and is
   rejected for the north star.

---

## Risks & open questions

- ~~Whether libdb recovery ever calls a comparator~~ — **resolved: it does
  not** (§Recovery; zero comparator/hash/search references across all ten
  `*_rec.c` files). `db_recover` stays usable without codecs.
- ~~`DB_RMW` under MVCC: does the probe fetch the leaf dirty?~~ —
  **resolved: yes** (§Concurrency; `SR_FIND_WR` → `DB_MPOOL_DIRTY` →
  `DB_LOCK_DEADLOCK` on a newer version). UNIQUE is race-free under SI.
- [UNSURE] The cost of the put-path hook on *untyped* databases in an
  enabled build. It should be one pointer test. It must be measured with
  `test/bench`, not asserted.
- Fingerprints prove less than they appear to. The documentation must not
  oversell them.
- The catalog lifecycle across `rename`, `remove`, `truncate`, `upgrade`,
  hot backup and replication (the catalog replicates because it is
  ordinary data; the clients must register the same codecs, which is noxu's
  stated bound, `design-decisions.md:412-432`).

---

## Decisions the maintainer must make

- **D1.** Build this at all, or stop at Alternative 1 (docs plus fuzz
  harness plus the Phase 0 fixes)?
- **D2.** The handle pointer in `struct __db` under `#ifdef` (changes the
  signature in enabled builds, as compression does), or the zero-change
  side table (costs a lookup on every put)?
- **D3.** Catalog location: subdatabase in the same file, per-environment
  file, or both?
- **D4.** Heap and Queue fail open on an old libdb. Leave them out, or bump
  their format version for typed files?
- **D5.** Should a standalone `db_recover` be guaranteed to work on typed
  environments? If verification shows that recovery needs the comparator,
  is "recover from inside the application" acceptable?
- **D6.** Phase 0: should the non-unique-secondary error change from
  `EINVAL` to a constraint error, given that existing callers may test for
  `EINVAL`?
- **D7.** Should a codec without `DB_CODEC_TOTAL` be refused under
  `DB_OPTREAD`, or should the core's copy-then-validate path (RFC 0007)
  be built instead?
- **D8.** Is a `dlopen` codec plugin for the utilities in scope?
