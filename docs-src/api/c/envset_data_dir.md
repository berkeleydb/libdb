---
title: "DB_ENV->set_data_dir()"
api-name: "DB_ENV->set_data_dir()"
source: docs/api_reference/C/envset_data_dir.html
---
## DB_ENV-\>set_data_dir()

``` c
#include <db.h>

int
DB_ENV->set_data_dir(DB_ENV *dbenv, const char *dir);  
```

### Note

This interface has been deprecated. You should use <a href="envadd_data_dir.md" class="xref" title="DB_ENV-&gt;add_data_dir()">DB_ENV-&gt;add_data_dir()</a> and <a href="envset_create_dir.md" class="xref" title="DB_ENV-&gt;set_create_dir()">DB_ENV-&gt;set_create_dir()</a> instead.

Set the path of a directory to be used as the location of the access method database files. Paths specified to the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> function will be searched relative to this path. Paths set using this method are additive, and specifying more than one will result in each specified directory being searched for database files. If any directories are specified, database files will always be created in the first path specified.

If no database directories are specified, database files must be named either by absolute paths or relative to the environment home directory. See <a href="../../programmer_reference/env_naming.html" class="olink">Berkeley DB File Naming</a> for more information.

The database environment's data directories may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_data_dir", one or more whitespace characters, and the directory name. Note that if you use this method for your application, and you also want to use the <a href="db_recover.md" class="xref" title="db_recover">db_recover</a> or <a href="db_archive.md" class="xref" title="db_archive">db_archive</a> utilities, then you should create a <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file and set the "set_data_dir" parameter in it.

The `DB_ENV->set_data_dir()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_data_dir()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. If the database environment already exists when <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> is called, the information specified to `DB_ENV->set_data_dir()` must be consistent with the existing environment or corruption can occur.

The `DB_ENV->set_data_dir()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dir

The **dir** parameter is a directory to be used as a location for database files. This directory must currently exist at environment open time.

When using a Unicode build on Windows (the default), this argument will be interpreted as a UTF-8 string, which is equivalent to ASCII for Latin characters.

### Errors

The `DB_ENV->set_data_dir()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
