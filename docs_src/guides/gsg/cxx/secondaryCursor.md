---
title: "Using Cursors with Secondary Databases"
api-name: "Using Cursors with Secondary Databases"
source: docs/gsg/CXX/secondaryCursor.html
---
## Using Cursors with Secondary Databases

Just like cursors on a primary database, you can use cursors on secondary databases to iterate over the records in a secondary database. Like cursors used with primary databases, you can also use cursors with secondary databases to search for specific records in a database, to seek to the first or last record in the database, to get the next duplicate record, and so forth. For a complete description on cursors and their capabilities, see <a href="Cursors.md" class="xref" title="Chapter 4. Using Cursors">Using Cursors</a>.

However, when you use cursors with secondary databases:

- Any data returned is the data contained on the primary database record referenced by the secondary record.

- You cannot use `DB_GET_BOTH` and related flags with `Db::get()` and a secondary database. Instead, you must use `Db::pget()`. Also, in that case the primary and secondary key given on the call to `Db::pget()` must match the secondary key and associated primary record key in order for that primary record to be returned as a result of the call.

For example, suppose you are using the databases, classes, and key extractors described in <a href="keyCreator.md" class="xref" title="Implementing Key Extractors">Implementing Key <span>Extractors</span></a> . Then the following searches for a person's name in the secondary database, and deletes all secondary and primary records that use that name.

``` c
#include <db_cxx.h>

...

Db my_database(NULL, 0);
Db my_index(NULL, 0);

// Get a cursor on the secondary database
Dbc *cursorp;
my_index.cursor(NULL, &cursorp, 0);

// Name to delete
char *search_name = "John Doe"; 

// Instantiate Dbts as normal
Dbt key(search_name, strlen(search_name) + 1);
Dbt data;

 
// Position the cursor
while (cursorp->get(&key, &data, DB_SET) == 0)
    cursorp->del(0); 
```
