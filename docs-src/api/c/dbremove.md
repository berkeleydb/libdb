---
title: "DB->remove()"
api-name: "DB->remove()"
source: docs/api_reference/C/dbremove.html
---
## DB-\>remove()

``` c
#include <db.h>

int
DB->remove(DB *db,
    const char *file, const char *database, u_int32_t flags);  
```

The `DB->remove()` method removes the database specified by the **file** and **database** parameters. If no **database** is specified, the underlying file represented by **file** is removed, incidentally removing all of the databases it contained.

Applications should never remove databases with open <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handles, or in the case of removing a file, when any database in the file has an open handle. For example, some architectures do not permit the removal of files with open system handles. On these architectures, attempts to remove databases currently in use by any thread of control in the system may fail.

The `DB->remove()` method should not be called if the remove is intended to be transactionally safe; the <a href="envdbremove.md" class="xref" title="DB_ENV-&gt;dbremove()">DB_ENV-&gt;dbremove()</a> method should be used instead.

The `DB->remove()` method may not be called after calling the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method on any <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle. If the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method has already been called on a <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle, close the existing handle and create a new one before calling `DB->remove. ()`

The <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle may not be accessed again after `DB->remove()` is called, regardless of its return.

The `DB->remove()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### file

The **file** parameter is the physical file which contains the database(s) to be removed.

#### database

The **database** parameter is the database to be removed.

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Environment Variables

If the database was opened within a database environment, the environment variable `DB_HOME` may be used as the path of the database environment home.

`DB->remove()` is affected by any database directory specified using the <a href="envset_data_dir.md" class="xref" title="DB_ENV-&gt;set_data_dir()">DB_ENV-&gt;set_data_dir()</a> method, or by setting the "set_data_dir" string in the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file.

### Errors

The `DB->remove()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

#### ENOENT

The file or directory does not exist.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
