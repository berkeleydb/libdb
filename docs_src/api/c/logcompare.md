---
title: "log_compare"
api-name: "log_compare"
source: docs/api_reference/C/logcompare.html
---
## log_compare

``` c
#include <db.h>

int
log_compare(const DB_LSN *lsn0, const DB_LSN *lsn1);  
```

The `log_compare()` method allows the caller to compare two `DB_LSN` structures, returning 0 if they are equal, 1 if **lsn0** is greater than **lsn1**, and -1 if **lsn0** is less than **lsn1**.

### Parameters

#### lsn0

The **lsn0** parameter is one of the `DB_LSN` structures to be compared.

#### lsn1

The **lsn1** parameter is one of the `DB_LSN` structures to be compared.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>
