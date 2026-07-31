---
title: "DBcursor->del()"
api-name: "DBcursor->del()"
source: docs/api_reference/C/dbcdel.html
---
## DBcursor-\>del()

``` c
#include <db.h>

int
DBcursor->del(DBC *DBcursor, u_int32_t flags);  
```

The `DBcursor->del()` method deletes the key/data pair to which the cursor refers.

When called on a cursor opened on a database that has been made into a secondary index using the <a href="dbassociate.md" class="xref" title="DB-&gt;associate()">DB-&gt;associate()</a> method, the <a href="dbdel.md" class="xref" title="DB-&gt;del()">DB-&gt;del()</a> method deletes the key/data pair from the primary database and all secondary indices.

The cursor position is unchanged after a delete, and subsequent calls to cursor functions expecting the cursor to refer to an existing key will fail.

The `DBcursor->del()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_KEYEMPTY" class="olink">DB_KEYEMPTY</a> if the element has already been deleted. The `DBcursor->del()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter must be set to 0 or one of the following values:

- `DB_CONSUME`

  If the database is of type DB_QUEUE then this flag may be set to force the head of the queue to move to the first non-deleted item in the queue. Normally this is only done if the deleted item is exactly at the head when deleted.

### Errors

The `DBcursor->del()` method may fail and return one of the following non-zero errors:

#### DB_FOREIGN_CONFLICT

A <a href="dbassociate_foreign.md" class="link" title="DB-&gt;associate_foreign()">foreign key constraint violation</a> has occurred. This can be caused by one of two things:

1.  An attempt was made to add a record to a constrained database, and the key used for that record does not exist in the foreign key database.

2.  <a href="dbassociate_foreign.md#associate_foreign_DB_FOREIGN_ABORT" class="link">DB_FOREIGN_ABORT</a> was declared for a foreign key database, and then subsequently a record was deleted from the foreign key database without first removing it from the constrained secondary database.

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

#### DB_SECONDARY_BAD

A secondary index references a nonexistent primary key.

#### EACCES

An attempt was made to modify a read-only database.

#### EINVAL

If the cursor has not been initialized; or if an invalid flag value or parameter was specified.

#### EPERM

Write attempted on read-only cursor when the <a href="envopen.md#envopen_DB_INIT_CDB" class="link">DB_INIT_CDB</a> flag was specified to <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a>.

### Class

<a href="dbc.md" class="link" title="Chapter 3.  The DBcursor Handle">DBcursor</a>

### See Also

<a href="dbc.md#dbclist" class="xref" title="Database Cursors and Related Methods">Database Cursors and Related Methods</a>
