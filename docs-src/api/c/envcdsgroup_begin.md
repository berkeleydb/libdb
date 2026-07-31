---
title: "DB_ENV->cdsgroup_begin()"
api-name: "DB_ENV->cdsgroup_begin()"
source: docs/api_reference/C/envcdsgroup_begin.html
---
## DB_ENV-\>cdsgroup_begin()

``` c
#include <db.h>

int
DB_ENV->cdsgroup_begin(DB_ENV *dbenv, DB_TXN **tid);  
```

The `DB_ENV->cdsgroup_begin()` method allocates a locker ID in an environment configured for Berkeley DB Concurrent Data Store applications. It copies a pointer to a <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a> that uniquely identifies the locker ID into the memory to which **tid** refers. Calling the <a href="txncommit.md" class="xref" title="DB_TXN-&gt;commit()">DB_TXN-&gt;commit()</a> method will discard the allocated locker ID.

See <a href="../../programmer_reference/cam.html#cam_intro" class="olink">Berkeley DB Concurrent Data Store applications</a> for more information about when this is required.

The `DB_ENV->cdsgroup_begin()` method may be called at any time during the life of the application.

The `DB_ENV->cdsgroup_begin()` method returns a non-zero error value on failure and 0 on success.

### Errors

The `DB_ENV->cdsgroup_begin()` method may fail and return one of the following non-zero errors:

#### ENOMEM

The maximum number of lockers has been reached.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
