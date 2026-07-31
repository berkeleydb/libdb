---
title: "DB->get_transactional()"
api-name: "DB->get_transactional()"
source: docs/api_reference/C/dbget_transactional.html
---
## DB-\>get_transactional()

``` c
#include <db.h>

int
DB->get_transactional(DB *db);  
```

The `DB->get_transactional()` method returns non-zero if the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle has been opened in a transactional mode, otherwise it returns `0`.

The `DB->get_transactional()` method may be called at any time during the life of the application.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
