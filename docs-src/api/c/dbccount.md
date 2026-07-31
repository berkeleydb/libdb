---
title: "DBcursor->count()"
api-name: "DBcursor->count()"
source: docs/api_reference/C/dbccount.html
---
## DBcursor-\>count()

``` c
#include <db.h>

int
DBcursor->count(DBC *DBcursor, db_recno_t *countp, u_int32_t flags);  
```

The `DBcursor->count()` method returns a count of the number of data items for the key to which the cursor refers.

The `DBcursor->count()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### countp

The **countp** parameter references memory into which the count of the number of duplicate data items is copied.

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Errors

The `DBcursor->count()` method may fail and return one of the following non-zero errors:

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

#### EINVAL

If the cursor has not been initialized; or if an invalid flag value or parameter was specified.

### Class

<a href="dbc.md" class="link" title="Chapter 3.  The DBcursor Handle">DBcursor</a>

### See Also

<a href="dbc.md#dbclist" class="xref" title="Database Cursors and Related Methods">Database Cursors and Related Methods</a>
