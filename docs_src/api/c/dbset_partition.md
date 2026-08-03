---
title: "DB->set_partition()"
api-name: "DB->set_partition()"
source: docs/api_reference/C/dbset_partition.html
---
## DB-\>set_partition()

``` c
#include <db.h>

int
DB->set_partition(DB * db, u_int32_t parts,  DBT *keys,
    u_int32_t (*db_partition_fcn) (DB *db, DBT *key));  
```

Set up partitioning for a database. Partitioning may be used on either BTREE or HASH databases. Partitions may be specified by either a set of keys specifying a range of values in each partition or with a callback function that returns the number of the partition to put a specific key. Partition range keys may only be specified for BTREE databases.

Partitions are implimented as separate database files and can help reduce contention within a logical database. Contention can come from multiple threads of control accessing database pages simultaniously. Typically these pages are the root of a btree and the metadata page which contains allocation information in both BTREE and HASH databases. Each partition has its own metadata and root pages.

### Parameters

Exactly one of the parameters **keys** and **partition_fcn** must be NULL.

#### parts

The **parts** parameter is the number of partitions to create. The value must be 2 or greater.

#### keys

The **keys** parameter is an array of DBT structures containing the keys that specify the range of key values to be stored in each partition. Each key specifies the minimum value that may be stored in the corresponding partition. The number of keys must be one less than the number of partitions specified by the **parts** parameter since the first partition will hold any key less than the first key in the array.

#### db_partition_fcn

The **db_partition_fcn** parameter is the application-specified partitioning function. The function returns an integer which will be used modulo the number of partitions specified by the **parts** parameter. The function will be called with two parameters:

- `db`

  The **db** parameter is the database handle.

- `key`

  The **key** parameter is the key for which a partition number should be returned.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
