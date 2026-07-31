---
title: "DBcursor->cmp()"
api-name: "DBcursor->cmp()"
source: docs/api_reference/C/dbccmp.html
---
## DBcursor-\>cmp()

``` c
#include <db.h>

int
DBcursor->cmp(DBC *DBcursor, 
    DBC *other_cursor, int *result, u_int32_t flags);  
```

The `DBcursor->cmp()` method compares two cursors for equality. Two cursors are equal if and only if they are positioned on the same item in the same database.

The `DBcursor->cmp()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### other_cursor

The **other_cursor** parameter references another cursor handle that will be used as the comparator.

#### result

If the call is successful and both cursors are positioned on the same item, result is set to zero. If the call is successful but the cursors are not positioned on the same item, result is set to a non-zero value. If the call is unsuccessful, the value of result should be ignored.

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Errors

The `DBcursor->cmp()` method may fail and return one of the following non-zero errors:

#### EINVAL

- If either of the cursors are already closed.

- If the cursors have been opened against different databases.

- If either of the cursors have not been positioned.

- If the other_dbc parameter is NULL.

- If the result parameter is NULL.

### Class

<a href="dbc.md" class="link" title="Chapter 3.  The DBcursor Handle">DBcursor</a>

### See Also

<a href="dbc.md#dbclist" class="xref" title="Database Cursors and Related Methods">Database Cursors and Related Methods</a>
