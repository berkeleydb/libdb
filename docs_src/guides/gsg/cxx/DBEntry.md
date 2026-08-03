---
title: "Chapter 3. Database Records"
api-name: "Chapter 3. Database Records"
source: docs/gsg/CXX/DBEntry.html
---
## Chapter 3. Database Records

**Table of Contents**

<span class="sect1"> [Using Database Records](DBEntry.md#usingDbEntry) </span>

<span class="sect1"> [Reading and Writing Database Records](usingDbt.md) </span>

<span class="sect2"> [Writing Records to the Database](usingDbt.md#databaseWrite) </span>

<span class="sect2"> [Getting Records from the Database](usingDbt.md#CoreDatabaseRead) </span>

<span class="sect2"> [Deleting Records](usingDbt.md#recordDelete) </span>

<span class="sect2"> [Data Persistence](usingDbt.md#datapersist) </span>

<span class="sect1"> [Database Usage Example](DbCXXUsage.md) </span>

DB records contain two parts — a key and some data. Both the key and its corresponding data are encapsulated in `Dbt` class objects. Therefore, to access a DB record, you need two such objects, one for the key and one for the data.

`Dbt` objects provide a `void *` data member that you use to point to your data, and another member that identifies the data length. They can therefore be used to store anything from simple primitive data to complex class objects so long as the information you want to store resides in a single contiguous block of memory.

This chapter describes `Dbt` usage. It also introduces storing and retrieving key/value pairs from a database.

## Using Database Records

Each database record is comprised of two `Dbt` objects — one for the key and another for the data.

``` c
#include <db_cxx.h>
#include <string.h>

...

float money = 122.45;
char *description = "Grocery bill.";

Dbt key(&money, sizeof(float));
Dbt data(description, strlen(description)+1); 
```

Note that in the following example we do not allow DB to assign the memory for the retrieval of the money value. The reason why is that some systems may require float values to have a specific alignment, and the memory as returned by DB may not be properly aligned (the same problem may exist for structures on some systems). We tell DB to use our memory instead of its own by specifying the `DB_DBT_USERMEM` flag. Be aware that when we do this, we must also identify how much user memory is available through the use of the `ulen` field.

``` c
#include <db_cxx.h>
#include <string.h>

...

Dbt key, data;
float money;
char *description;

key.set_data(&money);
key.set_ulen(sizeof(float));
key.set_flags(DB_DBT_USERMEM);

// Database retrieval code goes here

// Money is set into the memory that we supplied.
description = (char *)data.get_data();
```
