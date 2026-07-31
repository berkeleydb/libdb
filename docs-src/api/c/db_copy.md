---
title: "db_copy"
api-name: "db_copy"
source: docs/api_reference/C/db_copy.html
---
## db_copy

``` c
#include <db.h>

int 
db_copy(DB_ENV *dbenv, const char *dbfile, const char *target, 
        const char *password);  
```

The `db_copy()` routine copies the named database file to the target directory. An optional password can be specified for encrypted database files. This routine can be used on operating systems that do not support atomic file system reads to create a hot backup of a database file. If the specified database file is for a QUEUE database with extents, all extent files for that database will be copied as well.

### Parameters

#### dbenv

An open environment handle for the environment containing the database file.

#### dbfile

The path name to the file to be backed up. The file name is resolved using the usual BDB library name resolution rules.

#### target

The directory to which you want the database copied. This is specified relative to the current directory of the executing process or as an absolute path.

#### password

Specified only if the database file is encrypted. The resulting backup file will be encrypted as well.
