---
title: "db_env_create"
api-name: "db_env_create"
source: docs/api_reference/C/envcreate.html
---
## db_env_create

``` c
#include <db.h>

int
db_env_create(DB_ENV **dbenvp, u_int32_t flags);  
```

The `db_env_create()` function creates a `DB_ENV` structure that is the handle for a Berkeley DB environment. This function allocates memory for the structure, returning a pointer to the structure in the memory to which **dbenvp** refers. To release the allocated memory and discard the handle, call the <a href="envclose.md" class="xref" title="DB_ENV-&gt;close()">DB_ENV-&gt;close()</a> or <a href="envremove.md" class="xref" title="DB_ENV-&gt;remove()">DB_ENV-&gt;remove()</a> methods.

`DB_ENV` handles are free-threaded if the <a href="envopen.md#envopen_DB_THREAD" class="link">DB_THREAD</a> flag is specified to the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method when the environment is opened. The `DB_ENV` handle should not be closed while any other handle remains open that is using it as a reference (for example, <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> or <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>). Once either the <a href="envclose.md" class="xref" title="DB_ENV-&gt;close()">DB_ENV-&gt;close()</a> or <a href="envremove.md" class="xref" title="DB_ENV-&gt;remove()">DB_ENV-&gt;remove()</a> methods are called, the handle may not be accessed again, regardless of the method's return.

Before the handle may be used, you must open it using the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.

The DB_ENV handle contains a special field, `app_private`, which is declared as type `void *`. This field is provided for the use of the application program. It is initialized to NULL and is not further used by Berkeley DB in any way.

The `db_env_create()` method returns a non-zero error value on failure and 0 on success.

The **flags** parameter must be set to 0.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
