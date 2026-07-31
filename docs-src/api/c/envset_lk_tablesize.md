---
title: "DB_ENV->set_lk_tablesize()"
api-name: "DB_ENV->set_lk_tablesize()"
source: docs/api_reference/C/envset_lk_tablesize.html
---
## DB_ENV-\>set_lk_tablesize()

``` c
#include <db.h>

int
DB_ENV->set_lk_tablesize(DB_ENV *dbenv, u_int32_t tablesize);  
```

Sets the number of buckets in the lock object hash table in the Berkeley DB environment. The default value is estimated based on defaults, initial and (deprecated) maximum settings of the number of lock objects allocated. The maximum memory allocation is also considered. The table is generally set to be close to the number of lock objects in the system to avoid collisions and delay in processing lock operations.

The database environment's tablesize may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_lk_tablesize", one or more whitespace characters, and the size of the table. Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->set_lk_tablesize()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->set_lk_tablesize()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. If the database environment already exists when <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> is called, the information specified to `DB_ENV->set_lk_tablesize()` will be ignored.

The `DB_ENV->set_lk_tablesize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### tablesize

The **tablesize** parameter provides the size of the lock object hash table to be configured in the Berkeley DB environment.

### Errors

The `DB_ENV->set_lk_tablesize()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
