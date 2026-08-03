---
title: "DB->set_append_recno()"
api-name: "DB->set_append_recno()"
source: docs/api_reference/C/dbset_append_recno.html
---
## DB-\>set_append_recno()

``` c
#include <db.h>

int
DB->set_append_recno(DB *,
    int (*db_append_recno_fcn)(DB *dbp, DBT *data, db_recno_t recno));  
```

When using the <a href="dbput.md#dbput_DB_APPEND" class="link">DB_APPEND</a> option of the <a href="dbput.md" class="xref" title="DB-&gt;put()">DB-&gt;put()</a> method, it may be useful to modify the stored data based on the generated key. If a callback function is specified using the `DB->set_append_recno()` method, it will be called after the record number has been selected, but before the data has been stored.

The `DB->set_append_recno()` method configures operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle, not all operations performed on the underlying database.

The `DB->set_append_recno()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

The `DB->set_append_recno()` method returns a non-zero error value on failure and 0 on success.

### Note

Berkeley DB is not re-entrant. Callback functions should not attempt to make library calls (for example, to release locks or close open handles). Re-entering Berkeley DB is not guaranteed to work correctly, and the results are undefined.

### Parameters

#### db_append_recno_fcn

The **db_append_recno_fcn** parameter is a function to call after the record number has been selected but before the data has been stored into the database. The function takes three parameters:

- `dbp`

  The **dbp** parameter is the enclosing database handle.

- `data`

  The **data** parameter is the data <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> to be stored.

- `recno`

  The **recno** parameter is the generated record number.

The called function may modify the data <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>. If the function needs to allocate memory for the **data** field, the **flags** field of the returned <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> should be set to `DB_DBT_APPMALLOC`, which indicates that Berkeley DB should free the memory when it is done with it.

The callback function must return 0 on success and **errno** or a value outside of the Berkeley DB error name space on failure.

### Errors

The `DB->set_append_recno()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
