---
title: "DB->fd()"
api-name: "DB->fd()"
source: docs/api_reference/C/dbfd.html
---
## DB-\>fd()

``` c
#include <db.h>

int
DB->fd(DB *db, int *fdp);  
```

The `DB->fd()` method provides access to a file descriptor representative of the underlying database. A file descriptor referring to the same file will be returned to all processes that call <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> with the same **file** parameter.

This file descriptor may be safely used as a parameter to the **fcntl**(2) and **flock**(2) locking functions.

The `DB->fd()` method only supports a coarse-grained form of locking. Applications should instead use the Berkeley DB lock manager where possible.

The `DB->fd()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### fdp

The **fdp** parameter references memory into which the current file descriptor is copied.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
