---
title: "DB->get_create_dir()"
api-name: "DB->get_create_dir()"
source: docs/api_reference/C/dbget_create_dir.html
---
## DB-\>get_create_dir()

``` c
#include <db.h>

int
DB->get_create_dir(DB *db, const char **dirp);  
```

Determine which directory a database file will be created in or was found in.

The `DB->get_create_dir()` method may be called at any time.

The `DB->get_create_dir()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dirp

The **dirp** will be set to the directory specified in the call to <a href="dbset_create_dir.md" class="xref" title="DB-&gt;set_create_dir()">DB-&gt;set_create_dir()</a> method on this handle or to the directory that the database was found in after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> has been called.

### Errors

The `DB->get_create_dir()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
