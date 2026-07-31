---
title: "Deleting Records Using Cursors"
api-name: "Deleting Records Using Cursors"
source: docs/gsg/C/DeleteEntryWCursor.html
---
## Deleting Records Using Cursors

To delete a record using a cursor, simply position the cursor to the record that you want to delete and then call `DBC->del()`.

For example:

``` c
#include <db.h>
#include <string.h>

...

DB *dbp;
DBC *cursorp;
DBT key, data;
char *key1str = "My first string";
int ret;

/* Database open omitted */

/* Initialize our DBTs. */
memset(&key, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));

/* Set up our DBTs */
key.data = key1str;
key.size = strlen(key1str) + 1;

/* Get the cursor */
dbp->cursor(dbp, NULL, &cursorp, 0);

/* Iterate over the database, deleting each record in turn. */
while ((ret = cursorp->get(cursorp, &key,
               &data, DB_SET)) == 0) {
    cursorp->del(cursorp, 0);
}

/* Cursors must be closed */
if (cursorp != NULL)
    cursorp->close(cursorp); 

if (dbp != NULL)
    dbp->close(dbp, 0);
```
