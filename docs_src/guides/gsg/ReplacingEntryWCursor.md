---
title: "Replacing Records Using Cursors"
api-name: "Replacing Records Using Cursors"
source: docs/gsg/C/ReplacingEntryWCursor.html
---
## Replacing Records Using Cursors

You replace the data for a database record by using `DBC->put()` with the `DB_CURRENT` flag.

``` c
#include <db.h>
#include <string.h>

...

DB *dbp;
DBC *cursorp;
DBT key, data;
char *key1str = "My first string";
char *replacement_data = "replace me";
int ret;

/* Initialize our DBTs. */
memset(&key, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));

/* Set up our DBTs */
key.data = key1str;
key.size = strlen(key1str) + 1;

/* Database open omitted */

/* Get the cursor */
dbp->cursor(dbp, NULL, &cursorp, 0);

/* Position the cursor */
ret = cursorp->get(cursorp, &key, &data, DB_SET);
if (ret == 0) {
    data.data = replacement_data;
    data.size = strlen(replacement_data) + 1;
    cursorp->put(cursorp, &key, &data, DB_CURRENT);
}

/* Cursors must be closed */
if (cursorp != NULL) 
    cursorp->close(cursorp); 

if (dbp != NULL)
    dbp->close(dbp, 0);
```

Note that you cannot change a record's key using this method; the key parameter is always ignored when you replace a record.

When replacing the data portion of a record, if you are replacing a record that is a member of a sorted duplicates set, then the replacement will be successful only if the new record sorts identically to the old record. This means that if you are replacing a record that is a member of a sorted duplicates set, and if you are using the default lexicographic sort, then the replacement will fail due to violating the sort order. However, if you provide a custom sort routine that, for example, sorts based on just a few bytes out of the data item, then potentially you can perform a direct replacement and still not violate the restrictions described here.

Under these circumstances, if you want to replace the data contained by a duplicate record, and you are not using a custom sort routine, then delete the record and create a new record with the desired key and data.
