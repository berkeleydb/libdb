---
title: "Deleting Records Using Cursors"
api-name: "Deleting Records Using Cursors"
source: docs/gsg/CXX/DeleteEntryWCursor.html
---
## Deleting Records Using Cursors

To delete a record using a cursor, simply position the cursor to the record that you want to delete and then call `Dbc::del()`.

For example:

``` c
#include <db_cxx.h>
#include <string.h>

...

char *key1str = "My first string";
Db my_database(NULL, 0);
Dbc *cursorp;

try {
    // Database open omitted 

    // Get the cursor
    my_database.cursor(NULL, &cursorp, 0);

    // Set up our DBTs
    Dbt key(key1str, strlen(key1str) + 1);
    Dbt data;

    // Iterate over the database, deleting each record in turn. 
    int ret;
    while ((ret = cursorp->get(&key, &data, 
                                  DB_SET)) == 0) {
        cursorp->del(0);
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
