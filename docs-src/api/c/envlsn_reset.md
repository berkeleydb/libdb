---
title: "DB_ENV->lsn_reset()"
api-name: "DB_ENV->lsn_reset()"
source: docs/api_reference/C/envlsn_reset.html
---
## DB_ENV-\>lsn_reset()

``` c
#include <db.h>

int
DB_ENV->lsn_reset(DB_ENV *dbenv, const char *file, u_int32_t flags);  
```

The `DB_ENV->lsn_reset()` method allows database files to be moved from one transactional database environment to another.

Database pages in transactional database environments contain references to the environment's log files (that is, log sequence numbers, or LSNs). Copying or moving a database file from one database environment to another, and then modifying it, can result in data corruption if the LSNs are not first cleared.

Note that LSNs should be reset before moving or copying the database file into a new database environment, rather than moving or copying the database file and then resetting the LSNs. Berkeley DB has consistency checks that may be triggered if an application calls `DB_ENV->lsn_reset()` on a database in a new environment when the database LSNs still reflect the old environment.

The `DB_ENV->lsn_reset()` method modifies the physical file, in-place. Applications should not reset LSNs in files that are currently in use.

The `DB_ENV->lsn_reset()` method may be called at any time during the life of the application.

The `DB_ENV->lsn_reset()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### file

The name of the physical file in which the LSNs are to be cleared.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_ENCRYPT`

  The file contains encrypted databases.

### Errors

The `DB_ENV->lsn_reset()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
