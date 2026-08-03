---
title: "DB_ENV->get_shm_key()"
api-name: "DB_ENV->get_shm_key()"
source: docs/api_reference/C/envget_shm_key.html
---
## DB_ENV-\>get_shm_key()

``` c
#include <db.h>

int
DB_ENV->get_shm_key(DB_ENV *dbenv, long *shm_keyp);  
```

The `DB_ENV->get_shm_key()` method returns the base segment ID. This is used for Berkeley DB environment shared memory regions created in system memory on VxWorks or systems supporting X/Open-style shared memory interfaces. It may be specified using the <a href="envset_shm_key.md" class="xref" title="DB_ENV-&gt;set_shm_key()">DB_ENV-&gt;set_shm_key()</a> method.

The `DB_ENV->get_shm_key()` method may be called at any time during the life of the application.

The `DB_ENV->get_shm_key()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### shm_keyp

The `DB_ENV->get_shm_key()` method returns the base segment ID in **shm_keyp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envset_shm_key.md" class="xref" title="DB_ENV-&gt;set_shm_key()">DB_ENV-&gt;set_shm_key()</a>
