---
title: "DBcursor->put()"
api-name: "DBcursor->put()"
source: docs/api_reference/C/dbcput.html
---
## DBcursor-\>put()

``` c
#include <db.h>

int
DBcursor->put(DBC *DBcursor, DBT *key, DBT *data, u_int32_t flags);  
```

The `DBcursor->put()` method stores key/data pairs into the database.

Unless otherwise specified, the `DBcursor->put()` method returns a non-zero error value on failure and 0 on success.

If `DBcursor->put()` fails for any reason, the state of the cursor will be unchanged. If `DBcursor->put()` succeeds and an item is inserted into the database, the cursor is always positioned to refer to the newly inserted item.

### Parameters

#### key

The key <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> operated on.

If creating a new record in a Heap database, the key <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> must be empty. The `put` method will return the new record's <a href="db_heap_rid.md" class="link" title="DB_HEAP_RID">Record ID (RID)</a> in the key <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>.

#### data

The data <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> operated on.

#### flags

The **flags** parameter must be set to one of the following values:

- `DB_AFTER`

  In the case of the Btree and Hash access methods, insert the data element as a duplicate element of the key to which the cursor refers. The new element appears immediately after the current cursor position. It is an error to specify DB_AFTER if the underlying Btree or Hash database is not configured for unsorted duplicate data items. The **key** parameter is ignored.

  In the case of the Recno access method, it is an error to specify DB_AFTER if the underlying Recno database was not created with the <a href="dbset_flags.md#dbset_flags_DB_RENUMBER" class="link">DB_RENUMBER</a> flag. If the <a href="dbset_flags.md#dbset_flags_DB_RENUMBER" class="link">DB_RENUMBER</a> flag was specified, a new key is created, all records after the inserted item are automatically renumbered, and the key of the new record is returned in the structure to which the **key** parameter refers. The initial value of the **key** parameter is ignored. See <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> for more information.

  The DB_AFTER flag may not be specified to the Queue access method.

  The `DBcursor->put()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if the current cursor record has already been deleted and the underlying access method is Hash.

- `DB_BEFORE`

  In the case of the Btree and Hash access methods, insert the data element as a duplicate element of the key to which the cursor refers. The new element appears immediately before the current cursor position. It is an error to specify DB_AFTER if the underlying Btree or Hash database is not configured for unsorted duplicate data items. The **key** parameter is ignored.

  In the case of the Recno access method, it is an error to specify DB_BEFORE if the underlying Recno database was not created with the <a href="dbset_flags.md#dbset_flags_DB_RENUMBER" class="link">DB_RENUMBER</a> flag. If the <a href="dbset_flags.md#dbset_flags_DB_RENUMBER" class="link">DB_RENUMBER</a> flag was specified, a new key is created, the current record and all records after it are automatically renumbered, and the key of the new record is returned in the structure to which the **key** parameter refers. The initial value of the **key** parameter is ignored. See <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> for more information.

  The DB_BEFORE flag may not be specified to the Queue access method.

  The `DBcursor->put()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if the current cursor record has already been deleted and the underlying access method is Hash.

- `DB_CURRENT`

  Overwrite the data of the key/data pair to which the cursor refers with the specified data item. The **key** parameter is ignored.

  The `DBcursor->put()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if the current cursor record has already been deleted.

- `DB_KEYFIRST`

  Insert the specified key/data pair into the database.

  If the underlying database supports duplicate data items, and if the key already exists in the database and a duplicate sort function has been specified, the inserted data item is added in its sorted location. If the key already exists in the database and no duplicate sort function has been specified, the inserted data item is added as the first of the data items for that key.

- `DB_KEYLAST`

  Insert the specified key/data pair into the database.

  If the underlying database supports duplicate data items, and if the key already exists in the database and a duplicate sort function has been specified, the inserted data item is added in its sorted location. If the key already exists in the database, and no duplicate sort function has been specified, the inserted data item is added as the last of the data items for that key.

- `DB_NODUPDATA`

  In the case of the Btree and Hash access methods, insert the specified key/data pair into the database, unless a key/data pair comparing equally to it already exists in the database. If a matching key/data pair already exists in the database, <a href="dbcput.md#dbcput_DB_KEYEXIST" class="xref" title="DB_KEYEXIST">DB_KEYEXIST</a> is returned. The DB_NODUPDATA flag may only be specified if the underlying database has been configured to support sorted duplicate data items.

  The DB_NODUPDATA flag may not be specified to the Queue or Recno access methods.

### Errors

The `DBcursor->put()` method may fail and return one of the following non-zero errors:

#### DB_KEYEXIST

An attempt was made to insert a duplicate key into a database not configured for duplicate data.

#### DB_FOREIGN_CONFLICT

A <a href="dbassociate_foreign.md" class="link" title="DB-&gt;associate_foreign()">foreign key constraint violation</a> has occurred. This can be caused by one of two things:

1.  An attempt was made to add a record to a constrained database, and the key used for that record does not exist in the foreign key database.

2.  <a href="dbassociate_foreign.md#associate_foreign_DB_FOREIGN_ABORT" class="link">DB_FOREIGN_ABORT</a> was declared for a foreign key database, and then subsequently a record was deleted from the foreign key database without first removing it from the constrained secondary database.

#### DB_HEAP_FULL

An attempt was made to add or update a record in a Heap database. However, the size of the database was constrained using the <a href="dbset_heapsize.md" class="xref" title="DB-&gt;set_heapsize()">DB-&gt;set_heapsize()</a> method, and that limit has been reached.

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

#### EACCES

An attempt was made to modify a read-only database.

#### EINVAL

If the DB_AFTER, DB_BEFORE or DB_CURRENT flags were specified and the cursor has not been initialized; the DB_AFTER or DB_BEFORE flags were specified and a duplicate sort function has been specified; the DB_CURRENT flag was specified, a duplicate sort function has been specified, and the data item of the referenced key/data pair does not compare equally to the **data** parameter; the DB_AFTER or DB_BEFORE flags were specified, and the underlying access method is Queue; an attempt was made to add a record to a fixed-length database that was too large to fit; an attempt was made to add a record to a secondary index; or if an invalid flag value or parameter was specified.

#### EPERM

Write attempted on read-only cursor when the <a href="envopen.md#envopen_DB_INIT_CDB" class="link">DB_INIT_CDB</a> flag was specified to <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a>.

### Class

<a href="dbc.md" class="link" title="Chapter 3.  The DBcursor Handle">DBcursor</a>

### See Also

<a href="dbc.md#dbclist" class="xref" title="Database Cursors and Related Methods">Database Cursors and Related Methods</a>
