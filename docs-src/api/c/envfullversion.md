---
title: "db_full_version"
api-name: "db_full_version"
source: docs/api_reference/C/envfullversion.html
---
## db_full_version

``` c
#include <db.h>

char *
db_full_version(int *family, int *release, int *major, int *minor, 
                int *patch); 
```

The `db_full_version()` method returns a pointer to a string, suitable for display, containing Berkeley DB version information. The string includes Oracle family and release numbers, as well as Berkeley DB's traditional major, minor, and patch numbers.

### Parameters

#### family

If **family** is non-NULL, the Oracle family number of the Berkeley DB release is copied to the memory to which it refers.

#### release

If **release** is non-NULL, the Oracle release number of the Berkeley DB release is copied to the memory to which it refers.

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
