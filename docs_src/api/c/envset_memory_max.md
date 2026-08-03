---
title: "DB_ENV->set_memory_max()"
api-name: "DB_ENV->set_memory_max()"
source: docs/api_reference/C/envset_memory_max.html
---
## DB_ENV-\>set_memory_max()

``` c
#include <db.h>

int
DB_ENV->set_memory_max(DB_ENV *dbenv, u_int32_t gbytes, u_int32_t bytes); 
```

This method sets the maximum amount of memory to be used by shared structures in the main environment region. These are the structures used to coordinate access to the environment other than mutexes and those in the page cache (memory pool). If the region files are in memory mapped files, or if `DB_PRIVATE` is specified, the memory specified by this method is not allocated completely at startup. As memory is needed, the shared region will be extended or, in the case of `DB_PRIVATE`, more memory will be allocated using the system malloc call. For memory mapped files, a mapped region will be allocated to this size but the underlying file will only be allocated sufficient memory to hold the initial allocation of shared memory structures as set by <a href="envset_memory_init.md" class="xref" title="DB_ENV-&gt;set_memory_init()">DB_ENV-&gt;set_memory_init()</a>.

If no memory maximum is specified then it is calculated from defaults, initial settings or (deprecated) maximum settings of the various shared structures. In the case of environments created with `DB_PRIVATE`, no maximum need be set and the shared structure allocation will grow as needed until the process memory limit is exhausted.

The database environment's maximum memory may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_memory_max", one or more whitespace characters, followed by the maximum to be allocated. Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->set_memory_max()` method must be called prior to opening the database environment.

### Parameters

#### gbytes

The maximum memory is set to **gbytes** gigabytes plus **bytes**.

#### bytes

The maximum memory is set to **gbytes** gigabytes plus **bytes**.

### Errors

The `DB_ENV->set_memory_max()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
