---
title: "DB_ENV->get_memory_init()"
api-name: "DB_ENV->get_memory_init()"
source: docs/api_reference/C/envget_memory_init.html
---
## DB_ENV-\>get_memory_init()

``` c
#include <db.h>

int
DB_ENV->get_memory_init(DB_ENV *dbenv, DB_MEM_CONFIG type, 
                        u_int32_t *countp);  
```

The `DB_ENV->get_memory_init()` method returns the number of objects to allocate and initialize when an environment is created. The count is returned for a specific named structure. The count for each structure is set using the <a href="envset_memory_init.md" class="xref" title="DB_ENV-&gt;set_memory_init()">DB_ENV-&gt;set_memory_init()</a> method.

The `DB_ENV->get_memory_init()` method may be called at any time during the life of the application.

The `DB_ENV->get_memory_init()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### type

The **struct** parameter identifies the structure for which you want an object count returned. It must be one of the following values:

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

- `DB_MEM_THREAD`

  Initialize thread identification structures. If thread tracking is enabled then each active thread will use a structure. Note that since a thread does not signal the BDB library that it will no longer be making calls, unused structures may accumulate until a cleanup is triggered either using a high water mark or by running <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a>.

#### countp

The **countp** parameter references memory into which the object count for the specified structure is copied.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
