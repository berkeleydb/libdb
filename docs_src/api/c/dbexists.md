---
title: "DB->exists()"
api-name: "DB->exists()"
source: docs/api_reference/C/dbexists.html
---
## DB-\>exists()

``` c
#include <db.h>

int
DB->exists(DB *db, DB_TXN *txnid, DBT *key, u_int32_t flags);  
```

The `DB->exists()` method returns whether the specified key appears in the database.

The `DB->exists()` method will return <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if the specified key is not in the database. The `DB->exists()` method will return <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_KEYEMPTY" class="olink">DB_KEYEMPTY</a> if the database is a Queue or Recno database and the specified key exists, but was never explicitly created by the application or was later deleted.

### Parameters

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; if the operation is part of a Berkeley DB Concurrent Data Store group, the **txnid** parameter is a handle returned from <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a>; otherwise NULL. If no transaction handle is specified, but the operation occurs in a transactional database, the operation will be implicitly transaction protected.

#### key

The key <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> operated on.

#### flags

The **flags** parameter must be set to zero or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_READ_COMMITTED`

  Configure a transactional read operation to have degree 2 isolation (the read is not repeatable).

- `DB_READ_UNCOMMITTED`

  Configure a transactional read operation to have degree 1 isolation, reading modified but not yet committed data. Silently ignored if the <a href="dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="link">DB_READ_UNCOMMITTED</a> flag was not specified when the underlying database was opened.

- `DB_RMW`

  Acquire write locks instead of read locks when doing the read, if locking is configured. Setting this flag can eliminate deadlock during a read-modify-write cycle by acquiring the write lock during the read part of the cycle so that another thread of control acquiring a read lock for the same item, in its own read-modify-write cycle, will not result in deadlock.

  Because the `DB->exists()` method will not hold locks across Berkeley DB calls in non-transactional operations, the <a href="dbcget.md#dbcget_DB_RMW" class="link">DB_RMW</a> flag to the `DB->exists()` call is meaningful only in the presence of transactions.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
