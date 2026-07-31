---
title: "DB->rename()"
api-name: "DB->rename()"
source: docs/api_reference/C/dbrename.html
---
## DB-\>rename()

``` c
#include <db.h>

int
DB->rename(DB *db, const char *file,
    const char *database, const char *newname, u_int32_t flags);  
```

The `DB->rename()` method renames the database specified by the **file** and **database** parameters to **newname**. If no **database** is specified, the underlying file represented by **file** is renamed, incidentally renaming all of the databases it contained.

Applications should not rename databases that are currently in use. If an underlying file is being renamed and logging is currently enabled in the database environment, no database in the file may be open when the `DB->rename()` method is called. In particular, some architectures do not permit renaming files with open handles. On these architectures, attempts to rename databases that are currently in use by any thread of control in the system may fail.

The `DB->rename()` method should not be called if the rename is intended to be transactionally safe; the <a href="envdbrename.md" class="xref" title="DB_ENV-&gt;dbrename()">DB_ENV-&gt;dbrename()</a> method should be used instead.

The `DB->rename()` method may not be called after calling the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method on any <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle. If the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method has already been called on a <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle, close the existing handle and create a new one before calling `DB->rename()`.

The <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle may not be accessed again after `DB->rename()` is called, regardless of its return.

The `DB->rename()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### file

The **file** parameter is the physical file which contains the database(s) to be renamed.

When using a Unicode build on Windows (the default), the **file** argument will be interpreted as a UTF-8 string, which is equivalent to ASCII for Latin characters.

#### database

The **database** parameter is the database to be renamed.

#### newname

The **newname** parameter is the new name of the database or file.

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Environment Variables

If the database was opened within a database environment, the environment variable `DB_HOME` may be used as the path of the database environment home.

`DB->rename()` is affected by any database directory specified using the <a href="envset_data_dir.md" class="xref" title="DB_ENV-&gt;set_data_dir()">DB_ENV-&gt;set_data_dir()</a> method, or by setting the "set_data_dir" string in the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file.

### Errors

The `DB->rename()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

#### ENOENT

The file or directory does not exist.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
