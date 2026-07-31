---
title: "DB_ENV->set_lk_conflicts()"
api-name: "DB_ENV->set_lk_conflicts()"
source: docs/api_reference/C/envset_lk_conflicts.html
---
## DB_ENV-\>set_lk_conflicts()

``` c
#include <db.h>

int
DB_ENV->set_lk_conflicts(DB_ENV *dbenv,
    u_int8_t *conflicts, int nmodes);  
```

Set the locking conflicts matrix.

If `DB_ENV->set_lk_conflicts()` is never called, a standard conflicts array is used; see <a href="../../guides/programmer_reference/lock_stdmode.md" class="olink">Standard Lock Modes</a> for more information.

The `DB_ENV->set_lk_conflicts()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->set_lk_conflicts()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. If the database environment already exists when <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> is called, the information specified to `DB_ENV->set_lk_conflicts()` will be ignored.

The `DB_ENV->set_lk_conflicts()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### conflicts

The **conflicts** parameter is the new locking conflicts matrix. The **conflicts** parameter is an **nmodes** by **nmodes** array. A non-0 value for the array element indicates that requested_mode and held_mode conflict:

``` c
        conflicts[requested_mode][held_mode] 
```

The <span class="emphasis">*not-granted*</span> mode must be represented by 0.

#### nmodes

The **nmodes** parameter is the size of the lock conflicts matrix.

### Errors

The `DB_ENV->set_lk_conflicts()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

#### ENOMEM

The conflicts array could not be copied.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
