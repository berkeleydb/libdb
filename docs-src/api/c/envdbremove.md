---
title: "DB_ENV->dbremove()"
api-name: "DB_ENV->dbremove()"
source: docs/api_reference/C/envdbremove.html
---
## DB_ENV-\>dbremove()

``` c
#include <db.h>

int
DB_ENV->dbremove(DB_ENV *dbenv, DB_TXN *txnid,
    const char *file, const char *database, u_int32_t flags);  
```

The `DB_ENV->dbremove()` method removes the database specified by the **file** and **database** parameters. If no **database** is specified, the underlying file represented by **file** is removed, incidentally removing all of the databases it contained.

Applications should never remove databases with open <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handles, or in the case of removing a file, when any database in the file has an open handle.

The `DB_ENV->dbremove()` method returns a non-zero error value on failure and 0 on success.

`DB_ENV->dbremove()` is affected by any database directory specified using the <a href="envset_data_dir.md" class="xref" title="DB_ENV-&gt;set_data_dir()">DB_ENV-&gt;set_data_dir()</a> method, or by setting the `set_data_dir` string in the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file.

### Parameters

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; if the operation is part of a Berkeley DB Concurrent Data Store group, the **txnid** parameter is a handle returned from <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a>; otherwise NULL. If no transaction handle is specified, but the `DB_AUTO_COMMIT` flag is specified to either this method or the environment handle, the operation will be implicitly transaction protected.

#### file

The **file** parameter is the physical file which contains the database(s) to be removed.

#### database

The **database** parameter is the database to be removed.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_AUTO_COMMIT`

  Enclose the `DB_ENV->dbremove()` call within a transaction. If the call succeeds, changes made by the operation will be recoverable. If the call fails, the operation will have made no changes.

### Environment Variables

The environment variable `DB_HOME` may be used as the path of the database environment home.

### Errors

The `DB_ENV->dbremove()` method may fail and return one of the following non-zero errors:

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### EINVAL

If the method was called before <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

#### ENOENT

The file or directory does not exist.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
