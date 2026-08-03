---
title: "Reading Secondary Databases"
api-name: "Reading Secondary Databases"
source: docs/gsg/C/readSecondary.html
---
## Reading Secondary Databases

Like a primary database, you can read records from your secondary database either by using the `DB->get()` or `DB->pget()` methods, or by using a cursor on the secondary database. The main difference between reading secondary and primary databases is that when you read a secondary database record, the secondary record's data is not returned to you. Instead, the primary key and data corresponding to the secondary key are returned to you.

For example, assuming your secondary database contains keys related to a person's full name:

``` c
#include <db.h>
#include <string.h>

...

DB *my_secondary_database;
DBT key; /* Used for the search key */
DBT pkey, pdata; /* Used to return the primary key and data */
char *search_name = "John Doe";

/* Primary and secondary database opens omitted for brevity */

/* Zero out the DBTs before using them. */
memset(&key, 0, sizeof(DBT));
memset(&pkey, 0, sizeof(DBT));
memset(&pdata, 0, sizeof(DBT));

key.data = search_name;
key.size = strlen(search_name) + 1;

/* Returns the key from the secondary database, and the data from the 
 * associated primary database entry.
 */
my_secondary_database->get(my_secondary_database, NULL, 
  &key, &pdata, 0);

/* Returns the key from the secondary database, and the key and data 
 * from the associated primary database entry.
 */
my_secondary_database->pget(my_secondary_database, NULL, 
  &key, &pkey, &pdata, 0); 
```

Note that, just like a primary database, if your secondary database supports duplicate records then `DB->get()` and `DB->pget()` only return the first record found in a matching duplicates set. If you want to see all the records related to a specific secondary key, then use a cursor opened on the secondary database. Cursors are described in <a href="Cursors.md" class="xref" title="Chapter 4. Using Cursors">Using Cursors</a>.
