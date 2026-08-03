---
title: "Using Cursors with Secondary Databases"
api-name: "Using Cursors with Secondary Databases"
source: docs/gsg/C/secondaryCursor.html
---
## Using Cursors with Secondary Databases

Just like cursors on a primary database, you can use cursors on secondary databases to iterate over the records in a secondary database. Like cursors used with primary databases, you can also use cursors with secondary databases to search for specific records in a database, to seek to the first or last record in the database, to get the next duplicate record, and so forth. For a complete description on cursors and their capabilities, see <a href="Cursors.md" class="xref" title="Chapter 4. Using Cursors">Using Cursors</a>.

However, when you use cursors with secondary databases:

- Any data returned is the data contained on the primary database record referenced by the secondary record.

- You cannot use `DB_GET_BOTH` and related flags with `DB->get()` and a secondary database. Instead, you must use `DB->pget()`. Also, in that case the primary and secondary key given on the call to `DB->pget()` must match the secondary key and associated primary record key in order for that primary record to be returned as a result of the call.

For example, suppose you are using the databases, classes, and key extractors described in <a href="keyCreator.md" class="xref" title="Implementing Key Extractors">Implementing Key <span>Extractors</span></a> . Then the following searches for a person's name in the secondary database, and deletes all secondary and primary records that use that name.

``` c
#include <db.h>
#include <string.h>

...

DB *sdbp;          /* Secondary DB handle */
DBC *cursorp;      /* Cursor */
DBT key, data;     /* DBTs used for the delete */
char *search_name = "John Doe"; /* Name to delete */

/* Primary and secondary database opens omitted for brevity. */

/* Get a cursor on the secondary database */
sdbp->cursor(sdbp, NULL, &cursorp, 0);

/*
 * Zero out the DBT before using it.
 */
memset(&key, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));

key.data = search_name;
key.size = strlen(search_name) + 1;

 
/* Position the cursor */
while (cursorp->get(cursorp, &key, &data, DB_SET) == 0)
    cursorp->del(cursorp, 0); 
```
