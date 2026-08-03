---
title: "Reading Secondary Databases"
api-name: "Reading Secondary Databases"
source: docs/gsg/CXX/readSecondary.html
---
## Reading Secondary Databases

Like a primary database, you can read records from your secondary database either by using the `Db::get()` or `Db::pget()` methods, or by using a cursor on the secondary database. The main difference between reading secondary and primary databases is that when you read a secondary database record, the secondary record's data is not returned to you. Instead, the primary key and data corresponding to the secondary key are returned to you.

For example, assuming your secondary database contains keys related to a person's full name:

``` c
#include <db_cxx.h>
#include <string.h>

...

// The string to search for
char *search_name = "John Doe";

// Instantiate our Dbt's
Dbt key(search_name, strlen(search_name) + 1);
Dbt pkey, pdata; // Primary key and data

Db my_secondary_database(NULL, 0);
// Primary and secondary database opens omitted for brevity

// Returns the key from the secondary database, and the data from the 
// associated primary database entry.
my_secondary_database.get(NULL, &key, &pdata, 0);

// Returns the key from the secondary database, and the key and data 
// from the associated primary database entry.
my_secondary_database.pget(NULL, &key, &pkey, &pdata, 0);
```

Note that, just like a primary database, if your secondary database supports duplicate records then `Db::get()` and `Db::pget()` only return the first record found in a matching duplicates set. If you want to see all the records related to a specific secondary key, then use a cursor opened on the secondary database. Cursors are described in <a href="Cursors.md" class="xref" title="Chapter 4. Using Cursors">Using Cursors</a>.
