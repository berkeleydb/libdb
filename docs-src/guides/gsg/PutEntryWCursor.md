---
title: "Putting Records Using Cursors"
api-name: "Putting Records Using Cursors"
source: docs/gsg/C/PutEntryWCursor.html
---
## Putting Records Using Cursors

You can use cursors to put records into the database. DB's behavior when putting records into the database differs depending on the flags that you use when writing the record, on the access method that you are using, and on whether your database supports sorted duplicates.

Note that when putting records to the database using a cursor, the cursor is positioned at the record you inserted.

You use `DBC->put()` to put (write) records to the database. You can use the following flags with this method:

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
#include <db.h>
#include <string.h>

...

DB *dbp;
DBC *cursorp;
DBT data1, data2, data3;
DBT key1, key2;
char *key1str = "My first string";
char *data1str = "My first data";
char *key2str = "A second string";
char *data2str = "My second data";
char *data3str = "My third data";
int ret;

/* Set up our DBTs */
key1.data = key1str;
key1.size = strlen(key1str) + 1;
data1.data = data1str;
data1.size = strlen(data1str) + 1;

key2.data = key2str;
key2.size = strlen(key2str) + 1;
data2.data = data2str;
data2.size = strlen(data2str) + 1;
data3.data = data3str;
data3.size = strlen(data3str) + 1;

/* Database open omitted */

/* Get the cursor */
dbp->cursor(dbp, NULL, &cursorp, 0);

/* 
 * Assuming an empty database, this first put places
 * "My first string"/"My first data" in the first 
 * position in the database
 */
ret = cursorp->put(cursorp, &key1, 
  &data1, DB_KEYFIRST); 

/*
 * This put places "A second string"/"My second data" in the
 * the database according to its key sorts against the key 
 * used for the currently existing database record. Most likely
 * this record would appear first in the database.
 */
ret = cursorp->put(cursorp, &key2, 
  &data2, DB_KEYFIRST); /* Added according to sort order */

/*
 * If duplicates are not allowed, the currently existing record that 
 * uses "key2" is overwritten with the data provided on this put.
 * That is, the record "A second string"/"My second data" becomes
 * "A second string"/"My third data"
 *
 * If duplicates are allowed, then "My third data" is placed in the
 * duplicates list according to how it sorts against "My second data".
 */
ret = cursorp->put(cursorp, &key2, 
  &data3, DB_KEYFIRST); /* If duplicates are not allowed, record 
                         * is overwritten with new data. Otherwise, 
                         * the record is added to the beginning of 
                         * the duplicates list.
                         */ 
```
