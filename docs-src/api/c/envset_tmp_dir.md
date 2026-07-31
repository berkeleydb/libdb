---
title: "DB_ENV->set_tmp_dir()"
api-name: "DB_ENV->set_tmp_dir()"
source: docs/api_reference/C/envset_tmp_dir.html
---
## DB_ENV-\>set_tmp_dir()

``` c
#include <db.h>

int
DB_ENV->set_tmp_dir(DB_ENV *dbenv, const char *dir);  
```

Specify the path of a directory to be used as the location of temporary files. The files created to back in-memory access method databases will be created relative to this path. These temporary files can be quite large, depending on the size of the database.

If no directories are specified, the following alternatives are checked in the specified order. The first existing directory path is used for all temporary files.

1.  The value of the environment variable **TMPDIR**.

2.  The value of the environment variable **TEMP**.

3.  The value of the environment variable **TMP**.

4.  The value of the environment variable **TempFolder**.

5.  The value returned by the **GetTempPath** interface.

6.  The directory **/var/tmp**.

7.  The directory **/usr/tmp**.

8.  The directory **/temp**.

9.  The directory **/tmp**.

10. The directory **C:/temp**.

11. The directory **C:/tmp**.

### Note

Environment variables are only checked if one of the <a href="envopen.md#envopen_DB_USE_ENVIRON" class="link">DB_USE_ENVIRON</a> or <a href="envopen.md#envopen_DB_USE_ENVIRON_ROOT" class="link">DB_USE_ENVIRON_ROOT</a> flags were specified.

### Note

The GetTempPath interface is only checked on Win/32 platforms.

The database environment's temporary file directory may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_tmp_dir", one or more whitespace characters, and the directory name. Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->set_tmp_dir()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_tmp_dir()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dir

The **dir** parameter is the directory to be used to store temporary files. This directory must currently exist at environment open time.

When using a Unicode build on Windows (the default), the this argument will be interpreted as a UTF-8 string, which is equivalent to ASCII for Latin characters.

### Errors

The `DB_ENV->set_tmp_dir()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
