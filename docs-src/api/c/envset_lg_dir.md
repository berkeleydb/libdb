---
title: "DB_ENV->set_lg_dir()"
api-name: "DB_ENV->set_lg_dir()"
source: docs/api_reference/C/envset_lg_dir.html
---
## DB_ENV-\>set_lg_dir()

``` c
#include <db.h>

int
DB_ENV->set_lg_dir(DB_ENV *dbenv, const char *dir);  
```

The path of a directory to be used as the location of logging files. Log files created by the Log Manager subsystem will be created in this directory.

If no logging directory is specified, log files are created in the environment home directory. See <a href="../../programmer_reference/env_naming.html" class="olink">Berkeley DB File Naming</a> for more information.

For the greatest degree of recoverability from system or application failure, database files and log files should be located on separate physical devices.

The database environment's logging directory may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_lg_dir", one or more whitespace characters, and the directory name. Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time. Note that if you use this method for your application, and you also want to use the <a href="db_recover.md" class="xref" title="db_recover">db_recover</a>, <a href="db_printlog.md" class="xref" title="db_printlog">db_printlog</a>, <a href="db_archive.md" class="xref" title="db_archive">db_archive</a>, or <a href="db_log_verify.md" class="xref" title="db_log_verify">db_log_verify</a> utilities, then you should set create a <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file and set the "set_lg_dir" parameter in it.

The `DB_ENV->set_lg_dir()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_lg_dir()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. If the database environment already exists when <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> is called, the information specified to `DB_ENV->set_lg_dir()` must be consistent with the existing environment or corruption can occur.

The `DB_ENV->set_lg_dir()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dir

The **dir** parameter is the directory used to store the logging files. This directory must currently exist at environment open time.

When using a Unicode build on Windows (the default), the **dir** argument will be interpreted as a UTF-8 string, which is equivalent to ASCII for Latin characters.

### Errors

The `DB_ENV->set_lg_dir()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>
