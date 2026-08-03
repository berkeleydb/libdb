---
title: "DB->get_type()"
api-name: "DB->get_type()"
source: docs/api_reference/C/dbget_type.html
---
## DB-\>get_type()

``` c
#include <db.h>

int
DB->get_type(DB *db, DBTYPE *type);  
```

The `DB->get_type()` method returns the type of the underlying access method (and file format). The type value is one of DB_BTREE, DB_HASH, DB_RECNO, or DB_QUEUE. This value may be used to determine the type of the database after a return from <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> with the **type** parameter set to DB_UNKNOWN.

The `DB->get_type()` method may not be called before the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

The `DB->get_type()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### type

The **type** parameter references memory into which the type of the underlying access method is copied.

### Errors

The `DB->get_type()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called before <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
