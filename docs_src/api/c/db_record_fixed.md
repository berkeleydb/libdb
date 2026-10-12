---
title: "db_record_fixed()"
api-name: "db_record_fixed()"
---
## db_record_fixed()

``` c
#include <db.h>

int
db_record_fixed(DB_RECORD_CODEC *rc, const char *name, u_int32_t version,
    u_int32_t nfields, const u_int32_t *off, const u_int32_t *len,
    const DB_CODEC **types);
```

The `db_record_fixed()` function fills in an application-owned
`DB_RECORD_CODEC` describing a **fixed layout**: field `i` of a record is the
`len[i]` bytes beginning at offset `off[i]`. It is a convenience for the common
case of a record whose fields are at known, constant positions, so the
application does not have to write its own `field()` callback.

`db_record_fixed()` is available **only in an `--enable-typed` build**; in a
default build the symbol is absent and a caller fails to link (fail-closed).

The record codec it produces is **total over arbitrary bytes**
(`DB_CODEC_TOTAL`): a field whose `(off, len)` slice runs past the end of the
record makes the whole record *malformed* (the locator returns
`DB_CODEC_MALFORMED`) rather than reading out of bounds. This is the property a
non-MEMCMP typed Btree key needs when `DB_OPTREAD` is enabled; see
<a href="dbset_bt_compare.md" class="olink">DB-&gt;set_bt_compare()</a>.

The `off[]`, `len[]` and `types[]` arrays are **referenced, not copied** (the
same lifetime contract as any libdb callback's data): they must outlive the
codec. A small internal allocation carries `off[]`/`len[]` to the locator and is
owned by the codec.

The `db_record_fixed()` method returns a non-zero error value on failure and 0
on success.

## Parameters

**rc**  
The application-owned `DB_RECORD_CODEC` to fill in.

**name**  
A stable name for the record codec (at most 255 bytes, no embedded NUL).

**version**  
A version number; bump it whenever the layout changes.

**nfields**  
The number of fields, 1 to 65535.

**off**  
An array of `nfields` byte offsets, one per field. Referenced, not copied.

**len**  
An array of `nfields` byte lengths, one per field. Referenced, not copied.

**types**  
An array of `nfields` `DB_CODEC *` type codecs, one per field, none `NULL`.
Referenced, not copied.

## Return value

`db_record_fixed()` returns 0 on success and a non-zero error on failure.

## Errors

`EINVAL`  
Any of `rc`, `name`, `off`, `len` or `types` is `NULL`, or `nfields` is 0 or
greater than 65535.

`ENOMEM`  
The internal allocation that carries the layout to the locator could not be
made.

## Class

[DB](db.md)

## See also

<a href="db_set_schema.md" class="olink">db_set_schema()</a>,
<a href="db_get_schema.md" class="olink">db_get_schema()</a>,
[Database and related methods](db.md)
