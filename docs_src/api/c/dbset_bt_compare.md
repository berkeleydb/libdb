---
title: "DB->set_bt_compare()"
api-name: "DB->set_bt_compare()"
source: docs/api_reference/C/dbset_bt_compare.html
---
## DB-\>set_bt_compare()

``` c
#include <db.h>

int
DB->set_bt_compare(DB *db,
    int (*bt_compare_fcn)(DB *db, const DBT *dbt1, const DBT *dbt2));  
```

Set the Btree key comparison function. The comparison function is called whenever it is necessary to compare a key specified by the application with a key currently stored in the tree.

If no comparison function is specified, the keys are compared lexically, with shorter keys collating before longer keys.

The `DB->set_bt_compare()` method configures operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle, not all operations performed on the underlying database.

The `DB->set_bt_compare()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_bt_compare()` must be the same as that historically used to create the database or corruption can occur.

The `DB->set_bt_compare()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### bt_compare_fcn

The **bt_compare_fcn** function is the application-specified Btree comparison function. The comparison function takes three parameters:

- `db`

  The **db** parameter is the enclosing database handle.

- `dbt1`

  The **dbt1** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the application supplied key.

- `dbt2`

  The **dbt2** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the current tree's key.

The **bt_compare_fcn** function must return an integer value less than, equal to, or greater than zero if the first key parameter is considered to be respectively less than, equal to, or greater than the second key parameter. In addition, the comparison function must cause the keys in the database to be <span class="emphasis">*well-ordered*</span>. The comparison function must correctly handle any key values used by the application (possibly including zero-length keys). In addition, when Btree key prefix comparison is being performed (see <a href="dbset_bt_prefix.md" class="xref" title="DB-&gt;set_bt_prefix()">DB-&gt;set_bt_prefix()</a> for more information), the comparison routine may be passed a prefix of any database key. The **data** and **size** fields of the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> are the only fields that may be used for the purposes of this comparison, and no particular alignment of the memory to which by the **data** field refers may be assumed.

### Errors

The `DB->set_bt_compare()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
