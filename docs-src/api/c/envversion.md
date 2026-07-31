---
title: "db_version"
api-name: "db_version"
source: docs/api_reference/C/envversion.html
---
## db_version

``` c
#include <db.h>

char *
db_version(int *major, int *minor, int *patch);  
```

The `db_version()` method returns a pointer to a string, suitable for display, containing Berkeley DB version information. For a method that returns this information as well as Oracle release numbers, see <a href="envfullversion.md" class="xref" title="db_full_version">db_full_version</a>.

### Parameters

#### major

If **major** is non-NULL, the major version of the Berkeley DB release is copied to the memory to which it refers.

#### minor

If **minor** is non-NULL, the minor version of the Berkeley DB release is copied to the memory to which it refers.

#### patch

If **patch** is non-NULL, the patch version of the Berkeley DB release is copied to the memory to which it refers.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
