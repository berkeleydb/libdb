---
title: "db_strerror"
api-name: "db_strerror"
source: docs/api_reference/C/envstrerror.html
---
## db_strerror

``` c
#include <db.h>

char *
db_strerror(int error);  
```

The `db_strerror()` method returns an error message string corresponding to the error number **error** parameter.

This function is a superset of the ANSI C X3.159-1989 (ANSI C) **strerror**(3) function. If the error number **error** is greater than or equal to 0, then the string returned by the system function **strerror**(3) is returned. If the error number is less than 0, an error string appropriate to the corresponding Berkeley DB library error is returned. See <a href="../../guides/programmer_reference/program_errorret.md" class="olink">Error returns to applications</a> for more information.

### Parameters

#### error

The **error** parameter is the error number for which an error message string is wanted.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
