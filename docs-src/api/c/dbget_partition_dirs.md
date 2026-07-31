---
title: "DB->get_partition_dirs()"
api-name: "DB->get_partition_dirs()"
source: docs/api_reference/C/dbget_partition_dirs.html
---
## DB-\>get_partition_dirs()

``` c
#include <db.h>

int
DB->get_partition_dirs(DB *db, const char ***dirsp);  
```

Determine which directorise the database partitions files will be created in or were found in.

The `DB->get_partition_dirs()` method may be called at any time.

The `DB->get_partition_dirs()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dirsp

The **dirsp** will be set to the array of directories specified in the call to <a href="dbset_partition_dirs.md" class="xref" title="DB-&gt;set_partition_dirs()">DB-&gt;set_partition_dirs()</a> method on this handle or to the directoreies that the database partitions were found in after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> has been called.

### Errors

The `DB->get_partition_dirs()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
