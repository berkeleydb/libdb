---
title: "DB->set_create_dir()"
api-name: "DB->set_create_dir()"
source: docs/api_reference/C/dbset_create_dir.html
---
## DB-\>set_create_dir()

``` c
#include <db.h>

int
DB->set_create_dir(DB *db, const char *dir);  
```

Specify which directory a database should be created in or looked for.

The `DB->set_create_dir()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

The `DB->set_create_dir()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dir

The **dir** will be used to create or locate the database file specified in the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method call. The directory must be one of the directories in the environment list specified by <a href="envadd_data_dir.md" class="xref" title="DB_ENV-&gt;add_data_dir()">DB_ENV-&gt;add_data_dir()</a>.

### Errors

The `DB->set_create_dir()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
