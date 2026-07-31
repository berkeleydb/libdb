---
title: "Chapter 3. Database Records"
api-name: "Chapter 3. Database Records"
source: docs/gsg/C/DBEntry.html
---
## Chapter 3. Database Records

**Table of Contents**

<span class="sect1"> [Using Database Records](DBEntry.md#usingDbEntry) </span>

<span class="sect1"> [Reading and Writing Database Records](usingDbt.md) </span>

<span class="sect2"> [Writing Records to the Database](usingDbt.md#databaseWrite) </span>

<span class="sect2"> [Getting Records from the Database](usingDbt.md#CoreDatabaseRead) </span>

<span class="sect2"> [Deleting Records](usingDbt.md#recordDelete) </span>

<span class="sect2"> [Data Persistence](usingDbt.md#datapersist) </span>

<span class="sect1"> [Using C Structures with DB](cstructs.md) </span>

<span class="sect2"> [C Structures with Pointers](cstructs.md#cstructdynamic) </span>

<span class="sect1"> [Database Usage Example](DbUsage.md) </span>

DB records contain two parts — a key and some data. Both the key and its corresponding data are encapsulated in `DBT` structures. Therefore, to access a DB record, you need two such structures, one for the key and one for the data.

`DBT` structures provide a `void *` field that you use to point to your data, and another field that identifies the data length. They can therefore be used to store anything from simple primitive data to complex structures so long as the information you want to store resides in a single contiguous block of memory.

This chapter describes `DBT` usage. It also introduces storing and retrieving key/value pairs from a database.

## Using Database Records

Each database record is comprised of two `DBT` structures — one for the key and another for the data.

To store a database record where the key and/or the data are primitive data (`int`, `float`, and so forth), or where the key and/or the data contain an array, we need only to point to the memory location where that data resides and identify its length. For example:

``` c
#include <db.h>
#include <string.h>

...

DBT key, data;
float money = 122.45;
char *description = "Grocery bill.";

/* Zero out the DBTs before using them. */
memset(&key, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));

key.data = &money;
key.size = sizeof(float);

data.data = description;
data.size = strlen(description) + 1; 
```

To retrieve the record, simply assign the `void *` returned in the `DBT` to the appropriate variable.

Note that in the following example we do not allow DB to assign the memory for the retrieval of the money value. The reason why is that some systems may require float values to have a specific alignment, and the memory as returned by DB may not be properly aligned (the same problem may exist for structures on some systems). We tell DB to use our memory instead of its own by specifying the `DB_DBT_USERMEM` flag. Be aware that when we do this, we must also identify how much user memory is available through the use of the `ulen` field.

``` c
#include <db.h>
#include <string.h>

...

float money;
DBT key, data;
char *description;

/* Initialize the DBTs */
memset(&key, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));

key.data = &money;
key.ulen = sizeof(float);
key.flags = DB_DBT_USERMEM;

/* Database retrieval code goes here */

/* 
 * Money is set into the memory that we supplied.
 */
description = data.data;
```
