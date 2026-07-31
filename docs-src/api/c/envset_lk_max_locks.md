---
title: "DB_ENV->set_lk_max_locks()"
api-name: "DB_ENV->set_lk_max_locks()"
source: docs/api_reference/C/envset_lk_max_locks.html
---
## DB_ENV-\>set_lk_max_locks()

``` c
#include <db.h>

int
DB_ENV->set_lk_max_locks(DB_ENV *dbenv, u_int32_t max);  
```

This method is deprecated. Instead, use <a href="envset_memory_init.md" class="xref" title="DB_ENV-&gt;set_memory_init()">DB_ENV-&gt;set_memory_init()</a>, <a href="envset_memory_max.md" class="xref" title="DB_ENV-&gt;set_memory_max()">DB_ENV-&gt;set_memory_max()</a>, and <a href="envset_lk_tablesize.md" class="xref" title="DB_ENV-&gt;set_lk_tablesize()">DB_ENV-&gt;set_lk_tablesize()</a>.

Set the maximum number of locks supported by the Berkeley DB environment. This value is used by <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> to estimate how much space to allocate for various lock-table data structures. The default value is 1000 locks. The final value specified for the locks should be more than or equal to the number of lock table partitions. For specific information on configuring the size of the lock subsystem, see <a href="../../guides/programmer_reference/lock_max.md" class="olink">Configuring locking: sizing the system</a>.

The database environment's maximum number of locks may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_lk_max_locks", one or more whitespace characters, and the number of locks. Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->set_lk_max_locks()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->set_lk_max_locks()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. If the database environment already exists when <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> is called, the information specified to `DB_ENV->set_lk_max_locks()` will be ignored.

The `DB_ENV->set_lk_max_locks()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### max

The **max** parameter is the maximum number of locks supported by the Berkeley DB environment.

### Errors

The `DB_ENV->set_lk_max_locks()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
