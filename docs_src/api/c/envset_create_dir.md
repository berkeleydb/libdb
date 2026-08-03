---
title: "DB_ENV->set_create_dir()"
api-name: "DB_ENV->set_create_dir()"
source: docs/api_reference/C/envset_create_dir.html
---
## DB_ENV-\>set_create_dir()

``` c
#include <db.h>

int
DB_ENV->set_create_dir(DB_ENV *dbenv, const char *dir);  
```

Sets the path of a directory to be used as the location to create the access method database files. When the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> function is used to create a file it will be created relative to this path.

If no database directories are specified, database files will be created either by absolute paths or relative to the environment home directory. See <a href="../../guides/programmer_reference/env_naming.md" class="olink">Berkeley DB File Naming</a> for more information.

The database environment's create directory may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_create_dir", one or more whitespace characters, and the directory name.

The `DB_ENV->set_create_dir()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_create_dir()` method may be called at any time.

The `DB_ENV->set_create_dir()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dir

The **dir** parameter is a directory to be used to create database files. This directory must be one of the directories specified via a call to <a href="envadd_data_dir.md" class="xref" title="DB_ENV-&gt;add_data_dir()">DB_ENV-&gt;add_data_dir()</a>

When using a Unicode build on Windows (the default), this argument will be interpreted as a UTF-8 string, which is equivalent to ASCII for Latin characters.

### Errors

The `DB_ENV->set_create_dir()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
