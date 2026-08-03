---
title: "Replacing Records Using Cursors"
api-name: "Replacing Records Using Cursors"
source: docs/gsg/CXX/ReplacingEntryWCursor.html
---
## Replacing Records Using Cursors

You replace the data for a database record by using `Dbc::put()` with the `DB_CURRENT` flag.

``` c
#include <db_cxx.h>
#include <string.h>

...

Db my_database(NULL, 0);
Dbc *cursorp;

int ret;
char *key1str = "My first string";
char *replacement_data = "replace me";

try {
    // Database open omitted

    // Get the cursor
    my_database.cursor(NULL, &cursorp, 0);

    // Set up our DBTs 
    Dbt key(key1str, strlen(key1str) + 1);
    Dbt data;

    // Position the cursor */
    ret = cursorp->get(&key, &data, DB_SET);
    if (ret == 0) {
        data.set_data(replacement_data);
        data.set_size(strlen(replacement_data) + 1);
        cursorp->put(&key, &data, DB_CURRENT);
    }
} catch(DbException &e) {
        my_database.err(e.get_errno(), "Error!");
} catch(std::exception &e) {
        my_database.errx("Error! %s", e.what());
}

// Cursors must be closed
if (cursorp != NULL)
    cursorp->close(); 

my_database.close(0);
```

Note that you cannot change a record's key using this method; the key parameter is always ignored when you replace a record.

When replacing the data portion of a record, if you are replacing a record that is a member of a sorted duplicates set, then the replacement will be successful only if the new record sorts identically to the old record. This means that if you are replacing a record that is a member of a sorted duplicates set, and if you are using the default lexicographic sort, then the replacement will fail due to violating the sort order. However, if you provide a custom sort routine that, for example, sorts based on just a few bytes out of the data item, then potentially you can perform a direct replacement and still not violate the restrictions described here.

Under these circumstances, if you want to replace the data contained by a duplicate record, and you are not using a custom sort routine, then delete the record and create a new record with the desired key and data.
