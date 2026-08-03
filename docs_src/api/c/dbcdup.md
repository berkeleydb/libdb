---
title: "DBcursor->dup()"
api-name: "DBcursor->dup()"
source: docs/api_reference/C/dbcdup.html
---
## DBcursor-\>dup()

``` c
#include <db.h>

int
DBcursor->dup(DBC *DBcursor, DBC **cursorp, u_int32_t flags);  
```

The `DBcursor->dup()` method creates a new cursor that uses the same transaction and locker ID as the original cursor. This is useful when an application is using locking and requires two or more cursors in the same thread of control.

The `DBcursor->dup()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### cursorp

The `DBcursor->dup()` method returns the newly created cursor in **cursorp**.

#### flags

The **flags** parameter must be set to 0 or the following flag:

- `DB_POSITION`

  The newly created cursor is initialized to refer to the same position in the database as the original cursor (if any) and hold the same locks (if any). If the DB_POSITION flag is not specified, or the original cursor does not hold a database position and locks, the created cursor is uninitialized and will behave like a cursor newly created using the <a href="dbcursor.md" class="xref" title="DB-&gt;cursor()">DB-&gt;cursor()</a> method.

### Errors

The `DBcursor->dup()` method may fail and return one of the following non-zero errors:

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="dbc.md" class="link" title="Chapter 3.  The DBcursor Handle">DBcursor</a>

### See Also

<a href="dbc.md#dbclist" class="xref" title="Database Cursors and Related Methods">Database Cursors and Related Methods</a>
