---
title: "DB->associate()"
api-name: "DB->associate()"
source: docs/api_reference/C/dbassociate.html
---
## DB-\>associate()

``` c
#include <db.h>

int
DB->associate(DB *primary, DB_TXN *txnid, DB *secondary,
    int (*callback)(DB *secondary,
    const DBT *key, const DBT *data, DBT *result), u_int32_t flags);  
```

The `DB->associate()` function is used to declare one database a secondary index for a primary database. The <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle that you call the `associate()` method from is the primary database.

After a secondary database has been "associated" with a primary database, all updates to the primary will be automatically reflected in the secondary and all reads from the secondary will return corresponding data from the primary. Note that as primary keys must be unique for secondary indices to work, the primary database must be configured without support for duplicate data items. See <a href="../../guides/programmer_reference/am_second.md" class="olink">Secondary Indices</a> in the *Berkeley DB Programmer's Reference Guide* for more information.

The `DB->associate()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### primary

The **primary** parameter should be a database handle for the primary database that is to be indexed.

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; if the operation is part of a Berkeley DB Concurrent Data Store group, the **txnid** parameter is a handle returned from <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a>; otherwise NULL. If no transaction handle is specified, but the operation occurs in a transactional database, the operation will be implicitly transaction protected.

#### secondary

The **secondary** parameter should be an open database handle of either a newly created and empty database that is to be used to store a secondary index, or of a database that was previously associated with the same primary and contains a secondary index. Note that it is not safe to associate as a secondary database a handle that is in use by another thread of control or has open cursors. If the handle was opened with the <a href="envopen.md#envopen_DB_THREAD" class="link">DB_THREAD</a> flag it is safe to use it in multiple threads of control after the `DB->associate()` method has returned. Note also that either secondary keys must be unique or the secondary database must be configured with support for duplicate data items.

#### callback

The **callback** parameter is a callback function that creates the set of secondary keys corresponding to a given primary key and data pair.

The callback parameter may be NULL if both the primary and secondary database handles were opened with the <a href="dbopen.md#dbopen_DB_RDONLY" class="link">DB_RDONLY</a> flag.

The callback takes four arguments:

- `secondary`

  The **secondary** parameter is the database handle for the secondary.

- `key`

  The **key** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> referencing the primary key.

- `data`

  The **data** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> referencing the primary data item.

- `result`

  The **result** parameter is a zeroed <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> in which the callback function should fill in **data** and **size** fields that describe the secondary key or keys.

### Note

Berkeley DB is not re-entrant. Callback functions should not attempt to make library calls (for example, to release locks or close open handles). Re-entering Berkeley DB is not guaranteed to work correctly, and the results are undefined.

The result <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> can have the following flags set in its **flags** field:

- `DB_DBT_APPMALLOC`

  If the callback function needs to allocate memory for the **result** data field (rather than simply pointing into the primary key or datum), DB_DBT_APPMALLOC should be set in the **flags** field of the **result** <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>, which indicates that Berkeley DB should free the memory when it is done with it.

- `DB_DBT_MULTIPLE`

  To return multiple secondary keys, DB_DBT_MULTIPLE should be set in the **flags** field of the **result** <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>, which indicates Berkeley DB should treat the **size** field as the number of secondary keys (zero or more), and the **data** field as a pointer to an array of that number of <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>s describing the set of secondary keys.

  **When multiple secondary keys are returned, keys may not be repeated**. In other words, there must be no repeated record numbers in the array for Recno and Queue databases, and keys must not compare equally using the secondary database's comparison function for Btree and Hash databases. If keys are repeated, operations may fail and the secondary may become inconsistent with the primary.

  The DB_DBT_APPMALLOC flag may be set for any <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> in the array of returned <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>'s to indicate that Berkeley DB should free the memory referenced by that particular <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>'s data field when it is done with it.

  The DB_DBT_APPMALLOC flag may be combined with DB_DBT_MULTIPLE in the **result** <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>'s **flag** field to indicate that Berkeley DB should free the array once it is done with all of the returned keys.

In addition, the callback can optionally return the following special value:

- `DB_DONOTINDEX`

  If any key/data pair in the primary yields a null secondary key and should be left out of the secondary index, the callback function may optionally return DB_DONOTINDEX. Otherwise, the callback function should return 0 in case of success or an error outside of the Berkeley DB name space in case of failure; the error code will be returned from the Berkeley DB call that initiated the callback.

  If the callback function returns DB_DONOTINDEX for any key/data pairs in the primary database, the secondary index will not contain any reference to those key/data pairs, and such operations as cursor iterations and range queries will reflect only the corresponding subset of the database. If this is not desirable, the application should ensure that the callback function is well-defined for all possible values and never returns DB_DONOTINDEX.

  Returning DB_DONOTINDEX is equivalent to setting DB_DBT_MULTIPLE on the **result** <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> and setting the **size** field to zero.

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_CREATE`

  If the secondary database is empty, walk through the primary and create an index to it in the empty secondary. This operation is potentially very expensive.

  If the secondary database has been opened in an environment configured with transactions, the entire secondary index creation is performed in the context of a single transaction.

  Care should be taken not to use a newly-populated secondary database in another thread of control until the `DB->associate()` call has returned successfully in the first thread.

  If transactions are not being used, care should be taken not to modify a primary database being used to populate a secondary database, in another thread of control, until the `DB->associate()` call has returned successfully in the first thread. If transactions are being used, Berkeley DB will perform appropriate locking and the application need not do any special operation ordering.

- `DB_IMMUTABLE_KEY`

  Specifies the secondary key is immutable.

  This flag can be used to optimize updates when the secondary key in a primary record will never be changed after the primary record is inserted. For immutable secondary keys, a best effort is made to avoid calling the secondary callback function when primary records are updated. This optimization may reduce the overhead of update operations significantly if the callback function is expensive.

  Be sure to specify this flag only if the secondary key in the primary record is never changed. If this rule is violated, the secondary index will become corrupted, that is, it will become out of sync with the primary.

### Errors

The `DB->associate()` method may fail and return one of the following non-zero errors:

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

#### EINVAL

If the secondary database handle has already been associated with this or another database handle; the secondary database handle is not open; the primary database has been configured to allow duplicates; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
