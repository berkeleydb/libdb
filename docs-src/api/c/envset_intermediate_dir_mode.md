---
title: "DB_ENV->set_intermediate_dir_mode()"
api-name: "DB_ENV->set_intermediate_dir_mode()"
source: docs/api_reference/C/envset_intermediate_dir_mode.html
---
## DB_ENV-\>set_intermediate_dir_mode()

``` c
#include <db.h>

int
DB_ENV->set_intermediate_dir_mode(DB_ENV *dbenv, const char *mode);  
```

By default, Berkeley DB does not create intermediate directories needed for recovery, that is, if the file **/a/b/c/mydatabase** is being recovered, and the directory path **b/c** does not exist, recovery will fail. This default behavior is because Berkeley DB does not know what permissions are appropriate for intermediate directory creation, and creating the directory might result in a security problem.

The `DB_ENV->set_intermediate_dir_mode()` method causes Berkeley DB to create any intermediate directories needed during recovery, using the specified permissions.

On UNIX systems or in IEEE/ANSI Std 1003.1 (POSIX) environments, created directories are owned by the process owner; the group ownership of created directories is based on the system and directory defaults, and is not further specified by Berkeley DB.

The database environment's intermediate directory permissions may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_intermediate_dir_mode", one or more whitespace characters, and the directory permissions. Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->set_intermediate_dir_mode()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_intermediate_dir_mode()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->set_intermediate_dir_mode()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### mode

The **mode** parameter specifies the directory permissions.

Directory permissions are interpreted as a string of nine characters, using the character set **r** (read), **w** (write), **x** (execute or search), and **-** (none). The first character is the read permissions for the directory owner (set to either **r** or **-**). The second character is the write permissions for the directory owner (set to either **w** or **-**). The third character is the execute permissions for the directory owner (set to either **x** or **-**).

Similarly, the second set of three characters are the read, write and execute/search permissions for the directory group, and the third set of three characters are the read, write and execute/search permissions for all others. For example, the string **rwx------** would configure read, write and execute/search access for the owner only. The string **rwxrwx---** would configure read, write and execute/search access for both the owner and the group. The string **rwxr-----** would configure read, write and execute/search access for the directory owner and read-only access for the directory group.

### Errors

The `DB_ENV->set_intermediate_dir_mode()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
