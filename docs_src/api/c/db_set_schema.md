---
title: "db_set_schema()"
api-name: "db_set_schema()"
---
## db_set_schema()

``` c
#include <db.h>

int
db_set_schema(DB *db, const DB_SCHEMA *schema, u_int32_t flags);
```

The `db_set_schema()` function attaches a **typed-data schema** to a database
handle, before the handle is opened. A schema declares how the key and/or data
bytes of each record are structured (through record codecs and per-field type
codecs), and the constraints libdb must enforce on every put — NOT NULL,
MAXLEN, VALID and UNIQUE. See RFC 0014 for the full model.

`db_set_schema()` is available **only in an `--enable-typed` build**. In a
default build the symbol is absent from the library, so a program that calls it
fails to link: typed databases cannot be created, read or written by a build
that does not understand them (fail-closed). To test for the typed API at
compile time, check `DB_HAVE_TYPED`: `db.h` defines it (to 1) only in an
`--enable-typed` (meson `-Dtyped=enabled`) build.

    #ifdef DB_HAVE_TYPED
        ret = db_set_schema(dbp, &schema, 0);
    #endif

`db_set_schema()` is a **free function rather than a `DB` method** by design.
Adding a method pointer to the `DB` handle would change `sizeof(DB)` (the public
ABI) and the environment build signature; the latter would make existing
environments refuse to attach. For the same reason the typed layer stores its
per-handle state in a process-global side table, not in any libdb structure that
is part of the ABI or the region signature.

The schema is **deep-copied** into libdb-owned memory: the `DB_SCHEMA`, both
record codecs, their `types[]` arrays, the constraint array and all names are
copied. The `DB_CODEC` structures themselves and any `unique_db` handles are
*not* copied — the caller must keep them alive until
<a href="dbclose.md" class="xref" title="DB-&gt;close()">DB-&gt;close()</a>, as
with any libdb callback. Scribbling the caller's original arrays after
`db_set_schema()` returns does not change the stored schema; see
<a href="db_get_schema.md" class="olink">db_get_schema()</a>.

`db_set_schema()` must be called **before**
<a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> and may
not be called after it. A typed database must be opened as a **named
subdatabase in a file** (both a file name and a subdatabase name to
<a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a>): this is
where libdb stores the schema catalog that lets a second process verify that its
codecs match the ones the database was created with.

The `db_set_schema()` method returns a non-zero error value on failure and 0 on
success.

## Phase 1 restrictions

The first release of typed data imposes these limits; each is reported as
`EINVAL` from `db_set_schema()` or from
<a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a>:

- The access method must be `DB_BTREE` or `DB_HASH`. Heap, Queue and Recno are
  refused.
- Duplicates (`DB_DUP`/`DB_DUPSORT`), compression and database partitioning are
  not supported on a typed database.
- Partial puts (`DB_DBT_PARTIAL`) are not supported on a typed database.
- A comparator, prefix or hash function already set on the handle
  (`set_bt_compare`, `set_bt_prefix`, `set_h_compare`, `set_h_hash`) is refused;
  for a typed key the generated comparator or hash is installed by libdb.
- A UNIQUE constraint on a field requires that field's codec to be
  EQUALIMAGE or MEMCMP, or to supply a `sortkey` function; a purely
  byte-compared secondary key cannot express the equality a UNIQUE index needs.
- `flags` must be 0.

## Parameters

**schema**  
A pointer to a `DB_SCHEMA` describing the record layout and constraints. It is
deep-copied; the codecs it references are not.

**flags**  
Must be 0 in this release.

## Return value

`db_set_schema()` returns 0 on success and a non-zero error on failure.

## Catalog, fingerprint and portability

When a typed subdatabase is created, libdb writes a schema catalog record
alongside it (a `__db_typed` subdatabase in the same file) and sets a flag on
the subdatabase's meta page. The catalog records, for each codec, a **golden
vector fingerprint**: a SHA-1 over the codec's name, version, flags and the
results of its `compare`, `equal`, `hash`, `validate` and `sortkey` functions on
the golden vectors the codec supplies. When a second process opens the
database, libdb recomputes that fingerprint from the codecs in the presented
schema and compares it to the stored one; a mismatch is reported as
<a href="../../guides/programmer_reference/program_errorret.md" class="olink">DB_CODEC_MISMATCH</a>.

**The fingerprint is a sampling check, not a proof of equivalence.** It can
only observe the codec's behaviour on the finite set of golden vectors. Two
codecs that agree on every vector but differ on some other input share a
fingerprint and will not be caught; the vectors must therefore exercise the
distinctions that matter (ordering, equality classes, NULL handling). Choosing
good vectors is the application's responsibility, and bumping a codec's
`version` whenever its behaviour changes is the reliable signal — an honest
version difference is reported as `EINVAL`, which is unambiguous.

**Fail-closed across tools and versions.** The meta-page flag that marks a
subdatabase typed is an access-method flag that older or default builds do not
recognise. Such a build refuses to open the subdatabase (`EINVAL`, "illegal
flag specified to DB-&gt;open"), which is the intended result: a library that
does not understand the schema must not read or write the data (C13, C14).

- <a href="db_dump.md" class="olink">db_dump</a> / <a href="db_load.md" class="olink">db_load</a>:
  the catalog records travel with a dump, but the meta-page flag does not, so a
  dump/load round trip yields an *untyped* database that a typed open then
  refuses. Loading a typed database as typed is a later-phase feature; in this
  release treat dump/load of a typed subdatabase as fail-closed.
- <a href="db_recover.md" class="olink">db_recover</a>: a `db_recover` built
  `--enable-typed` recovers a typed environment with **no codecs registered** —
  recovery replays the log and never calls a comparator or constraint. A default
  or older `db_recover` fails closed on the meta flag, which is correct: a
  library that does not know the bit must not rewrite the file.
- <a href="dbrename.md" class="xref" title="DB-&gt;rename()">DB-&gt;rename()</a>:
  renaming a typed subdatabase moves its catalog record together with it, so
  **rename a typed subdatabase only with a typed (`--enable-typed`) libdb**. A
  default build has no hook to move the catalog record and would leave it
  stranded under the old name.

## Errors

`EINVAL`  
`schema` is `NULL`, `flags` is non-zero, the schema fails one of the validation
rules above, a codec's golden-vector self-check fails (for example the vectors
are not in compare order, or a MEMCMP or EQUALIMAGE claim is contradicted), or
`db_set_schema()` was called after
<a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a>.

## Class

[DB](db.md)

## See also

<a href="db_get_schema.md" class="olink">db_get_schema()</a>,
<a href="db_record_fixed.md" class="olink">db_record_fixed()</a>,
<a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a>,
[Database and related methods](db.md)
