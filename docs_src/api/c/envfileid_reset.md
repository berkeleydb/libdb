---
title: "DB_ENV->fileid_reset()"
api-name: "DB_ENV->fileid_reset()"
source: docs/api_reference/C/envfileid_reset.html
---
## DB_ENV-\>fileid_reset()

``` c
#include <db.h>

int
DB_ENV->fileid_reset(DB_ENV *dbenv, const char *file, u_int32_t flags);  
```

The `DB_ENV->fileid_reset()` method allows database files to be copied, and then the copy used in the same database environment as the original.

All databases contain an ID string used to identify the database in the database environment cache. If a physical database file is copied, and used in the same environment as another file with the same ID strings, corruption can occur. The `DB_ENV->fileid_reset()` method creates new ID strings for all of the databases in the physical file.

The `DB_ENV->fileid_reset()` method modifies the physical file, in-place. Applications should not reset IDs in files that are currently in use.

The `DB_ENV->fileid_reset()` method may be called at any time during the life of the application.

The `DB_ENV->fileid_reset()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### file

The name of the physical file in which new file IDs are to be created.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_ENCRYPT`

  The file contains encrypted databases.

### Errors

The `DB_ENV->fileid_reset()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
