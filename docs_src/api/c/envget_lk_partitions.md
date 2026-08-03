---
title: "DB_ENV->get_lk_partitions()"
api-name: "DB_ENV->get_lk_partitions()"
source: docs/api_reference/C/envget_lk_partitions.html
---
## DB_ENV-\>get_lk_partitions()

``` c
#include <db.h>

int
DB_ENV->get_lk_partitions(DB_ENV *dbenv, u_int32_t *lk_partitions);  
```

The `DB_ENV->get_lk_partitions()` method returns the number of lock table partitions used in the Berkeley DB environment. You can configure this using the <a href="envset_lk_partitions.md" class="xref" title="DB_ENV-&gt;set_lk_partitions()">DB_ENV-&gt;set_lk_partitions()</a> method.

The `DB_ENV->get_lk_partitions()` method may be called at any time during the life of the application.

The `DB_ENV->get_lk_partitions()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lk_partitions

The `DB_ENV->get_lk_partitions()` method returns the number of partitions in **lk_partitions**.

### Errors

The `DB_ENV->get_lk_partitions()` method may fail and return one of the following non-zero errors:

#### EINVAL

The method was called on an environment which had been opened without being configured for locking.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>, <a href="envset_lk_partitions.md" class="xref" title="DB_ENV-&gt;set_lk_partitions()">DB_ENV-&gt;set_lk_partitions()</a>
