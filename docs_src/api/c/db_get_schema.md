---
title: "db_get_schema()"
api-name: "db_get_schema()"
---
## db_get_schema()

``` c
#include <db.h>

int
db_get_schema(DB *db, const DB_SCHEMA **schemap);
```

The `db_get_schema()` function returns the schema attached to a database handle
by <a href="db_set_schema.md" class="olink">db_set_schema()</a>. The schema
returned is the **libdb-owned deep copy**, not the caller's original: the
`DB_SCHEMA` and everything it points at (names, `types[]` arrays, constraints)
belong to libdb and remain valid until
<a href="dbclose.md" class="xref" title="DB-&gt;close()">DB-&gt;close()</a>. Because
it is a copy, modifying the arrays the caller passed to
<a href="db_set_schema.md" class="olink">db_set_schema()</a> after that call does
not change what `db_get_schema()` returns.

The caller must not modify or free the returned schema.

`db_get_schema()` is available **only in an `--enable-typed` build**; in a
default build the symbol is absent and a caller fails to link (fail-closed). It
is a **free function rather than a `DB` method** for the same ABI and
build-signature reasons as <a href="db_set_schema.md" class="olink">db_set_schema()</a>.

The `db_get_schema()` method returns a non-zero error value on failure and 0 on
success.

## Parameters

**schemap**  
A pointer to a `const DB_SCHEMA *` that is filled with a pointer to the
libdb-owned schema.

## Return value

`db_get_schema()` returns 0 on success and a non-zero error on failure.

## Errors

`EINVAL`  
`schemap` is `NULL`, or no schema has been set on this handle.

## Class

[DB](db.md)

## See also

<a href="db_set_schema.md" class="olink">db_set_schema()</a>,
<a href="db_record_fixed.md" class="olink">db_record_fixed()</a>,
[Database and related methods](db.md)
