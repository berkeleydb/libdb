---
title: "DB_ENV->get_lk_conflicts()"
api-name: "DB_ENV->get_lk_conflicts()"
source: docs/api_reference/C/envget_lk_conflicts.html
---
## DB_ENV-\>get_lk_conflicts()

``` c
#include <db.h>

int
DB_ENV->get_lk_conflicts(DB_ENV *dbenv,
    const u_int8_t **lk_conflictsp, int *lk_modesp);  
```

The `DB_ENV->get_lk_conflicts()` method returns the current conflicts array. You can specify a conflicts array using <a href="envset_lk_conflicts.md" class="xref" title="DB_ENV-&gt;set_lk_conflicts()">DB_ENV-&gt;set_lk_conflicts()</a>

The `DB_ENV->get_lk_conflicts()` method may be called at any time during the life of the application.

The `DB_ENV->get_lk_conflicts()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lk_conflictsp

The **lk_conflictsp** parameter references memory into which a pointer to the current conflicts array is copied.

#### lk_modesp

The **lk_modesp** parameter references memory into which the size of the current conflicts array is copied.

### Errors

The `DB_ENV->get_lk_conflicts()` method may fail and return one of the following non-zero errors:

#### EINVAL

The method was called on an environment which had been opened without being configured for locking.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>, <a href="envset_lk_conflicts.md" class="xref" title="DB_ENV-&gt;set_lk_conflicts()">DB_ENV-&gt;set_lk_conflicts()</a>
