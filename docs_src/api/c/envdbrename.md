---
title: "DB_ENV->dbrename()"
api-name: "DB_ENV->dbrename()"
source: docs/api_reference/C/envdbrename.html
---
## DB_ENV-\>dbrename()

``` c
#include <db.h>

int
DB_ENV->dbrename(DB_ENV *dbenv, DB_TXN *txnid, const char *file,
    const char *database, const char *newname, u_int32_t flags);  
```

The `DB_ENV->dbrename()` method renames the database specified by the **file** and **database** parameters to **newname**. If no **database** is specified, the underlying file represented by **file** is renamed using the value supplied to **newname**, incidentally renaming all of the databases it contained.

Applications should not rename databases that are currently in use. If an underlying file is being renamed and logging is currently enabled in the database environment, no database in the file may be open when the `DB_ENV->dbrename()` method is called.

The `DB_ENV->dbrename()` method returns a non-zero error value on failure and 0 on success.

`DB_ENV->dbrename()` is affected by any database directory specified using the <a href="envset_data_dir.md" class="xref" title="DB_ENV-&gt;set_data_dir()">DB_ENV-&gt;set_data_dir()</a> method, or by setting the `set_data_dir` string in the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file.

### Parameters

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; if the operation is part of a Berkeley DB Concurrent Data Store group, the **txnid** parameter is a handle returned from <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a>; otherwise NULL. If no transaction handle is specified, but the `DB_AUTO_COMMIT` flag is specified to either this method or the environment handle, the operation will be implicitly transaction protected.

#### file

The **file** parameter is the physical file which contains the database(s) to be renamed.

When using a Unicode build on Windows (the default), the **file** argument will be interpreted as a UTF-8 string, which is equivalent to ASCII for Latin characters.

#### database

The **database** parameter is the database to be renamed.

#### newname

The **newname** parameter is the new name of the database or file.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_AUTO_COMMIT`

  Enclose the `DB_ENV->dbrename()` call within a transaction. If the call succeeds, changes made by the operation will be recoverable. If the call fails, the operation will have made no changes.

### Environment Variables

The environment variable `DB_HOME` may be used as the path of the database environment home.

### Errors

The `DB_ENV->dbrename()` method may fail and return one of the following non-zero errors:

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
