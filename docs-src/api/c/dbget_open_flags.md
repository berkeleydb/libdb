---
title: "DB->get_open_flags()"
api-name: "DB->get_open_flags()"
source: docs/api_reference/C/dbget_open_flags.html
---
## DB-\>get_open_flags()

``` c
#include <db.h>

int
DB->get_open_flags(DB *db, u_int32_t *flagsp);  
```

The `DB->get_open_flags()` method returns the current open method flags. That is, this method returns the flags that were specified when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called.

The `DB->get_open_flags()` method may not be called before the `DB->open()` method is called.

The `DB->get_open_flags()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flagsp

The `DB->get_open_flags()` method returns the current open method flags in **flagsp**.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
