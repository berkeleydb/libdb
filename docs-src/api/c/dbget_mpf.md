---
title: "DB->get_mpf()"
api-name: "DB->get_mpf()"
source: docs/api_reference/C/dbget_mpf.html
---
## DB-\>get_mpf()

``` c
#include <db.h>

DB_MPOOLFILE *
DB->get_mpf(DB *db);  
```

The `DB->get_mpf()` method returns the handle for the cache file underlying the database.

The `DB->get_mpf()` method should be used with caution on a replication client site. This method exposes an internal structure that may not be valid after a client site synchronizes with its master site.

The `DB->get_mpf()` method may be called at any time during the life of the application.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
