---
title: "DB->get_multiple()"
api-name: "DB->get_multiple()"
source: docs/api_reference/C/dbget_multiple.html
---
## DB-\>get_multiple()

``` c
#include <db.h>

int
DB->get_multiple(DB *db);  
```

This method returns non-zero if the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle references a physical file supporting multiple databases, and 0 otherwise.

In this case, the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle is a handle on a database whose key values are the names of the databases stored in the physical file and whose data values are opaque objects. No keys or data values may be modified or stored using the database handle.

This method may not be called before the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
