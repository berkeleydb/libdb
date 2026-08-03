---
title: "Putting Records Using Cursors"
api-name: "Putting Records Using Cursors"
source: docs/gsg/CXX/PutEntryWCursor.html
---
## Putting Records Using Cursors

You can use cursors to put records into the database. DB's behavior when putting records into the database differs depending on the flags that you use when writing the record, on the access method that you are using, and on whether your database supports sorted duplicates.

Note that when putting records to the database using a cursor, the cursor is positioned at the record you inserted.

You use `Dbc::put()` to put (write) records to the database. You can use the following flags with this method:

- `DB_NODUPDATA`

  If the provided key already exists in the database, then this method returns `DB_KEYEXIST`.

  If the key does not exist, then the order that the record is put into the database is determined by the insertion order in use by the database. If a comparison function has been provided to the database, the record is inserted in its sorted location. Otherwise (assuming BTree), lexicographical sorting is used, with shorter items collating before longer items.

  This flag can only be used for the BTree and Hash access methods, and only if the database has been configured to support sorted duplicate data items (`DB_DUPSORT` was specified at database creation time).

  This flag cannot be used with the Queue or Recno access methods.

  For more information on duplicate records, see <a href="btree.md#duplicateRecords" class="xref" title="Allowing Duplicate Records">Allowing Duplicate Records</a>.

- `DB_KEYFIRST`

  For databases that do not support duplicates, this method behaves exactly the same as if a default insertion was performed. If the database supports duplicate records, and a duplicate sort function has been specified, the inserted data item is added in its sorted location. If the key already exists in the database and no duplicate sort function has been specified, the inserted data item is added as the first of the data items for that key.

- `DB_KEYLAST`

  Behaves exactly as if `DB_KEYFIRST` was used, except that if the key already exists in the database and no duplicate sort function has been specified, the inserted data item is added as the last of the data items for that key.

For example:

``` c
#include <db_cxx.h>
#include <string.h>

...

char *key1str = "My first string";
char *data1str = "My first data";
char *key2str = "A second string";
char *data2str = "My second data";
char *data3str = "My third data";

Db my_database(NULL, 0);
Dbc *cursorp;

try {
    // Set up our DBTs
    Dbt key1(key1str, strlen(key1str) + 1);
    Dbt data1(data1str, strlen(data1str) + 1);

    Dbt key2(key2str, strlen(key2str) + 1);
    Dbt data2(data2str, strlen(data2str) + 1);
    Dbt data3(data3str, strlen(data3str) + 1);

    // Database open omitted

    // Get the cursor
    my_database.cursor(NULL, &cursorp, 0);

    // Assuming an empty database, this first put places
    // "My first string"/"My first data" in the first 
    // position in the database
    int ret = cursorp->put(&key1, &data1, DB_KEYFIRST); 

    // This put places "A second string"/"My second data" in the
    // the database according to its key sorts against the key 
    // used for the currently existing database record. Most likely
    // this record would appear first in the database.
    ret = cursorp->put(&key2, &data2, 
            DB_KEYFIRST); /* Added according to sort order */

    // If duplicates are not allowed, the currently existing record that 
    // uses "key2" is overwritten with the data provided on this put.
    // That is, the record "A second string"/"My second data" becomes
    // "A second string"/"My third data"
    // 
    // If duplicates are allowed, then "My third data" is placed in the
    // duplicates list according to how it sorts against "My second data".
    ret = cursorp->put(&key2, &data3, 
            DB_KEYFIRST); // If duplicates are not allowed, record 
                          // is overwritten with new data. Otherwise, 
                          // the record is added to the beginning of 
                          // the duplicates list.
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
