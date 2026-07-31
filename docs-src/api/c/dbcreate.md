---
title: "db_create"
api-name: "db_create"
source: docs/api_reference/C/dbcreate.html
---
## db_create

``` c
#include <db.h>

int db_create(DB **dbp, DB_ENV *dbenv, u_int32_t flags);  
```

The `db_create()` function creates a <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> structure that is the handle for a Berkeley DB database. This function allocates memory for the structure, returning a pointer to the structure in the memory to which **dbp** refers. To release the allocated memory and discard the handle, call the <a href="dbclose.md" class="xref" title="DB-&gt;close()">DB-&gt;close()</a>, <a href="dbremove.md" class="xref" title="DB-&gt;remove()">DB-&gt;remove()</a>, <a href="dbrename.md" class="xref" title="DB-&gt;rename()">DB-&gt;rename()</a>, or <a href="dbverify.md" class="xref" title="DB-&gt;verify()">DB-&gt;verify()</a> methods.

DB handles are free-threaded if the <a href="envopen.md#envopen_DB_THREAD" class="link">DB_THREAD</a> flag is specified to the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method when the database is opened or if the database environment in which the database is opened is free-threaded. The handle should not be closed while any other handle that refers to the database is in use; for example, database handles must not be closed while cursor handles into the database remain open, or transactions that include operations on the database have not yet been committed or aborted. Once the <a href="dbclose.md" class="xref" title="DB-&gt;close()">DB-&gt;close()</a>, <a href="dbremove.md" class="xref" title="DB-&gt;remove()">DB-&gt;remove()</a>, <a href="dbrename.md" class="xref" title="DB-&gt;rename()">DB-&gt;rename()</a>, or <a href="dbverify.md" class="xref" title="DB-&gt;verify()">DB-&gt;verify()</a> methods are called, the handle may not be accessed again, regardless of the method's return.

The DB handle contains a special field, `app_private`, which is declared as type `void *`. This field is provided for the use of the application program. It is initialized to NULL and is not further used by Berkeley DB in any way.

The `db_create` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### dbp

The **dbp** parameter references the memory into which the returned structure pointer is stored.

#### dbenv

If the **dbenv** parameter is NULL, the database is standalone; that is, it is not part of any Berkeley DB environment.

If the **dbenv** parameter is not NULL, the database is created within the specified Berkeley DB environment. The database access methods automatically make calls to the other subsystems in Berkeley DB, based on the enclosing environment. For example, if the environment has been configured to use locking, the access methods will automatically acquire the correct locks when reading and writing pages of the database.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_XA_CREATE`

  Instead of creating a standalone database, create a database intended to be accessed via applications running under an X/Open conformant Transaction Manager. The database will be opened in the environment specified by the OPENINFO parameter of the GROUPS section of the ubbconfig file. See the <a href="../../guides/programmer_reference/xa_xa_intro.md" class="olink">XA Introduction</a> section in the Berkeley DB Reference Guide for more information.

### Errors

The `db_create()` function may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
