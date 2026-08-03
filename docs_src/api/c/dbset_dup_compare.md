---
title: "DB->set_dup_compare()"
api-name: "DB->set_dup_compare()"
source: docs/api_reference/C/dbset_dup_compare.html
---
## DB-\>set_dup_compare()

``` c
#include <db.h>

int
DB->set_dup_compare(DB *db,
    int (*dup_compare_fcn)(DB *db, const DBT *dbt1, const DBT *dbt2));  
```

Set the duplicate data item comparison function. The comparison function is called whenever it is necessary to compare a data item specified by the application with a data item currently stored in the database. Calling `DB->set_dup_compare()` implies calling <a href="dbset_flags.md" class="xref" title="DB-&gt;set_flags()">DB-&gt;set_flags()</a> with the <a href="dbset_flags.md#dbset_flags_DB_DUPSORT" class="link">DB_DUPSORT</a> flag.

If no comparison function is specified, the data items are compared lexically, with shorter data items collating before longer data items.

The `DB->set_dup_compare()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_dup_compare()` must be the same as that historically used to create the database or corruption can occur.

The `DB->set_dup_compare()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dup_compare_fcn

The **dup_compare_fcn** function is the application-specified duplicate data item comparison function. The function takes three arguments:

- `db`

  The **db** parameter is the enclosing database handle.

- `dbt1`

  The **dbt1** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the application supplied data item.

- `dbt2`

  The **dbt2** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the current tree's data item.

The **dup_compare_fcn** function must return an integer value less than, equal to, or greater than zero if the first data item parameter is considered to be respectively less than, equal to, or greater than the second data item parameter. In addition, the comparison function must cause the data items in the set to be <span class="emphasis">*well-ordered*</span>. The comparison function must correctly handle any data item values used by the application (possibly including zero-length data items). The **data** and **size** fields of the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> are the only fields that may be used for the purposes of this comparison, and no particular alignment of the memory to which the **data** field refers may be assumed.

### Errors

The `DB->set_dup_compare()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
