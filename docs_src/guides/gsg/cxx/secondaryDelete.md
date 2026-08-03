---
title: "Deleting Secondary Database Records"
api-name: "Deleting Secondary Database Records"
source: docs/gsg/CXX/secondaryDelete.html
---
## Deleting Secondary Database Records

In general, you will not modify a secondary database directly. In order to modify a secondary database, you should modify the primary database and simply allow DB to manage the secondary modifications for you.

However, as a convenience, you can delete secondary database records directly. Doing so causes the associated primary key/data pair to be deleted. This in turn causes DB to delete all secondary database records that reference the primary record.

You can use the `Db::del()` method to delete a secondary database record. Note that if your secondary database contains duplicate records, then deleting a record from the set of duplicates causes all of the duplicates to be deleted as well.

### Note

You can delete a secondary database record using the previously described mechanism only if the primary database is opened for write access.

For example:

``` c
#include <db_cxx.h>
#include <string.h>

...

Db my_database(NULL, 0); // Primary
Db my_index(NULL, 0);    // Secondary

// Open the primary
my_database.open(NULL,       // Transaction pointer
                 "my_db.db", // On-disk file that holds the database.
                NULL,        // Optional logical database name
                DB_BTREE,    // Database access method
                DB_CREATE,   // Open flags
                0);          // File mode (using defaults)

// Setup the secondary to use sorted duplicates.
// This is often desireable for secondary databases.
my_index.set_flags(DB_DUPSORT);

// Open the secondary
my_index.open(NULL,              // Transaction pointer
              "my_secondary.db", // On-disk file that holds the database.
              NULL,              // Optional logical database name
              DB_BTREE,          // Database access method
              DB_CREATE,         // Open flags.
              0);                // File mode (using defaults)

// Now associate the primary and the secondary
my_database.associate(NULL,          // Txn id
                      &my_index,     // Associated secondary database
                      get_sales_rep, // Callback used for key extraction.
                      0);            // Flags 

// Name to delete
char *search_name = "John Doe";

// Get a search key
Dbt key(search_name, strlen(search_name) + 1);
                      
// Now delete the secondary record. This causes the associated primary
// record to be deleted. If any other secondary databases have secondary
// records referring to the deleted primary record, then those secondary
// records are also deleted.
my_index.del(NULL, &key, 0); 
```
