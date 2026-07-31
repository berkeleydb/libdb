---
title: "DB_ENV->set_memory_init()"
api-name: "DB_ENV->set_memory_init()"
source: docs/api_reference/C/envset_memory_init.html
---
## DB_ENV-\>set_memory_init()

``` c
#include <db.h>

int
DB_ENV->set_memory_init(DB_ENV *dbenv, DB_MEM_CONFIG type, 
                        u_int32_t count);  
```

This method sets the number of objects to allocate and initialize for a specified structure when an environment is created. Doing this helps avoid memory contention after startup. Using this method is optional; failure to use this method causes BDB to allocate a minimal number of structures that will grow dynamically. These structures are all allocated from the main environment region. The amount of memory in this region can be set via the <a href="envset_memory_max.md" class="xref" title="DB_ENV-&gt;set_memory_max()">DB_ENV-&gt;set_memory_max()</a> method. If this method is not called then memory will be limited to the initial settings or by the (deprecated) set maximum interfaces.

The database environment's initialization may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_memory_init", one or more whitespace characters, followed by the struct specification, more white space and the count to be allocated. Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->set_memory_init()` method must be called prior to opening the database environment. It may be called as often as needed to set the different configurations.

### Parameters

#### type

The **type** parameter must be set to one of the following:

- `DB_MEM_LOCK`

  Initialize locks. A thread uses this structure to lock a page (or record for the QUEUE access method) and hold it to the end of a transactions.

- `DB_MEM_LOCKOBJECT`

  Initialize lock objects. For each page (or record) which is locked in the system, a lock object will be allocated.

- `DB_MEM_LOCKER`

  Initialize lockers. Each thread which is active in a transactional environment will use a locker structure either for each transaction which is active, or for each non-transactional cursor that is active.

- `DB_MEM_LOGID`

  Initialize the log fileid structures. For each database handle which is opened for writing in a transactional environment, a log fileid structure is used.

- `DB_MEM_TRANSACTION`

  Initialize transaction structures. Each active transaction uses a transaction structure until it either commits or aborts.

  ### Note

  Currently transaction structures are not preallocated. This setting will be used to preallocate memory and objects related to transactions such as locker structures and mutexes.

- `DB_MEM_THREAD`

  Initialize thread identification structures. If thread tracking is enabled then each active thread will use a structure. Note that since a thread does not signal the BDB library that it will no longer be making calls, unused structures may accumulate until a cleanup is triggered either using a high water mark or by running <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a>.

#### count

The **count** parameter sets the number of specified objects to initialize.

The **count** specified for locks and lock objects should be at least 5 times the number of lock table partitions. You can examine the current number of lock table partitions configured for your environment using the <a href="envget_lk_partitions.md" class="xref" title="DB_ENV-&gt;get_lk_partitions()">DB_ENV-&gt;get_lk_partitions()</a> method.

### Errors

The `DB_ENV->set_memory_init()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
