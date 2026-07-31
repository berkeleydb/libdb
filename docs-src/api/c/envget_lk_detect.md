---
title: "DB_ENV->get_lk_detect()"
api-name: "DB_ENV->get_lk_detect()"
source: docs/api_reference/C/envget_lk_detect.html
---
## DB_ENV-\>get_lk_detect()

``` c
#include <db.h>

int
DB_ENV->get_lk_detect(DB_ENV *dbenv, u_int32_t *lk_detectp);  
```

The `DB_ENV->get_lk_detect()` method returns the deadlock detector configuration. You can manage this using the <a href="envset_lk_detect.md" class="xref" title="DB_ENV-&gt;set_lk_detect()">DB_ENV-&gt;set_lk_detect()</a> method.

The `DB_ENV->get_lk_detect()` method may be called at any time during the life of the application.

The `DB_ENV->get_lk_detect()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lk_detectp

The `DB_ENV->get_lk_detect()` method returns the deadlock detector configuration in **lk_detectp**.

### Errors

The `DB_ENV->get_lk_detect()` method may fail and return one of the following non-zero errors:

#### EINVAL

The method was called on an environment which had been opened without being configured for locking.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>, <a href="envset_lk_detect.md" class="xref" title="DB_ENV-&gt;set_lk_detect()">DB_ENV-&gt;set_lk_detect()</a>
