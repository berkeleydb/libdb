---
title: "DB->set_partition_dirs()"
api-name: "DB->set_partition_dirs()"
source: docs/api_reference/C/dbset_partition_dirs.html
---
## DB-\>set_partition_dirs()

``` c
#include <db.h>

int
DB->set_partition_dirs(DB *db, const char **dirs);  
```

Specify which directories the database extents should be created in or looked for. If the number of directories is less than the number of partitions, the directories will be used in a round robin fashion.

The `DB->set_partition_dirs()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

The `DB->set_partition_dirs()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dirs

The **dirs** points to an array of directories that will be used to create or locate the database extent files specified in the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method call. The directories must be included in the environment list specified by <a href="envadd_data_dir.md" class="xref" title="DB_ENV-&gt;add_data_dir()">DB_ENV-&gt;add_data_dir()</a>.

### Errors

The `DB->set_partition_dirs()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
