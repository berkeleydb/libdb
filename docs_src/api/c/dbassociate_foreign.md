---
title: "DB->associate_foreign()"
api-name: "DB->associate_foreign()"
source: docs/api_reference/C/dbassociate_foreign.html
---
## DB-\>associate_foreign()

``` c
#include <db.h>

int
DB->associate_foreign(DB *foreign, DB *secondary,,
   int (*callback)(DB *secondary,
   const DBT *key, DBT *data, const DBT *foreignkey, int *changed), 
   u_int32_t flags);  
```

The `DB->associate_foreign()` function is used to declare one database a foreign constraint for a secondary database. The <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle that you call the `associate_foreign()` method from is the foreign database.

After a foreign database has been "associated" with a secondary database, all keys inserted into the secondary must exist in the foreign database. Attempting to add a record with a foreign key that does not exist in the foreign database will cause the put method to fail and return `DB_FOREIGN_CONFLICT`.

Deletions in the foreign database affect the secondary in a manner defined by the flags parameter. See <a href="../../guides/programmer_reference/am_foreign.md" class="olink">Foreign Indices</a> in the *Berkeley DB Programmer's Reference Guide* for more information.

The `DB->associate_foreign()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### foreign

The **foreign** parameter should be a database handle for the foreign database.

#### secondary

The **secondary** parameter should be an open database handle of a database that contains a secondary index who's keys also exist in the **foreign** database.

#### callback

The **callback** parameter is a callback function that nullifies the foreign key portion of a data <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>.

The callback parameter must be NULL if either DB_FOREIGN_ABORT or DB_FOREIGN_CASCADE is set.

The callback takes four arguments:

- `secondary`

  The **secondary** parameter is the database handle for the secondary.

- `key`

  The **key** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> referencing the primary key.

- `data`

  The **data** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> referencing the primary data item to be updated.

- `foreignkey`

  The **foreignkey** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> referencing the foreign key which is being deleted.

- `changed`

  The **changed** parameter is a pointer to a boolean value, indicated whether **data** has changed.

### Note

Berkeley DB is not re-entrant. Callback functions should not attempt to make library calls (for example, to release locks or close open handles). Re-entering Berkeley DB is not guaranteed to work correctly, and the results are undefined.

#### flags

The **flags** parameter must be set to one of the following values:

- `DB_FOREIGN_ABORT`

  Abort the deletion of a key in the foreign database and return DB_FOREIGN_CONFLICT if that key exists in the secondary database. The deletion should be protected by a transaction to ensure database integrity after the aborted delete.

- `DB_FOREIGN_CASCADE`

  The deletion of a key in the foreign database will also delete that key from the secondary database (and the corresponding entry in the secondary's primary database.)

- `DB_FOREIGN_NULLIFY`

  The deletion of a key in the foreign database will call the nullification function passed to associate_foreign and update the secondary database with the changed data.

### Errors

The `DB->associate_foreign()` method may fail and return one of the following non-zero errors:

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

#### EINVAL

If the foreign database handle is a secondary index; the foreign database handle has been configured to allow duplicates; the foreign database handle is a renumbering recno database; callback is configured and DB_FOREIGN_NULLIFY is not; DB_FOREIGN_NULLIFY is configured and callback is not.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
