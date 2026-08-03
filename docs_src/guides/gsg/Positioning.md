---
title: "Getting Records Using the Cursor"
api-name: "Getting Records Using the Cursor"
source: docs/gsg/C/Positioning.html
---
## Getting Records Using the Cursor

<span class="sect2"> [Searching for Records](Positioning.md#cursorsearch) </span>

<span class="sect2"> [Working with Duplicate Records](Positioning.md#getdups) </span>

To iterate over database records, from the first record to the last, simply open the cursor and then use the `DBC->get()` method. Note that you need to supply the `DB_NEXT` flag to this method. For example:

``` c
#include <db.h>
#include <string.h>

...

DB *my_database;
DBC *cursorp;
DBT key, data;
int ret;

/* Database open omitted for clarity */

/* Get a cursor */
my_database->cursor(my_database, NULL, &cursorp, 0); 

/* Initialize our DBTs. */
memset(&key, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));

/* Iterate over the database, retrieving each record in turn. */
while ((ret = cursorp->get(cursorp, &key, &data, DB_NEXT)) == 0) {
        /* Do interesting things with the DBTs here. */
}
if (ret != DB_NOTFOUND) {
        /* Error handling goes here */
}

/* Cursors must be closed */
if (cursorp != NULL) 
    cursorp->close(cursorp); 

if (my_database != NULL) 
    my_database->close(my_database, 0);
```

To iterate over the database from the last record to the first, use `DB_PREV` instead of `DB_NEXT`:

``` c
#include <db.h>
#include <string.h>

...

DB *my_database;
DBC *cursorp;
DBT key, data;
int ret;

/* Database open omitted for clarity */

/* Get a cursor */
my_database->cursor(my_database, NULL, &cursorp, 0); 

/* Initialize our DBTs. */
memset(&key, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));

/* Iterate over the database, retrieving each record in turn. */
while ((ret = cursorp->get(cursorp, &key,
      &data, DB_PREV)) == 0) {
        /* Do interesting things with the DBTs here. */
}
if (ret != DB_NOTFOUND) {
        /* Error handling goes here */
}

// Cursors must be closed
if (cursorp != NULL) 
    cursorp->close(cursorp); 

if (my_database != NULL)
    my_database->close(my_database, 0);
```

### Searching for Records

You can use cursors to search for database records. You can search based on just a key, or you can search based on both the key and the data. You can also perform partial matches if your database supports sorted duplicate sets. In all cases, the key and data parameters of these methods are filled with the key and data values of the database record to which the cursor is positioned as a result of the search.

Also, if the search fails, then cursor's state is left unchanged and `DB_NOTFOUND` is returned.

To use a cursor to search for a record, use DBT-\>get(). When you use this method, you can provide the following flags:

### Note

Notice in the following list that the cursor flags use the keyword `SET` when the cursor examines just the key portion of the records (in this case, the cursor is set to the record whose key matches the value provided to the cursor). Moreover, when the cursor uses the keyword `GET`, then the cursor is positioned to both the key <span class="emphasis">*and*</span> the data values provided to the cursor.

Regardless of the keyword you use to get a record with a cursor, the cursor's key and data `DBT`s are filled with the data retrieved from the record to which the cursor is positioned.

- `DB_SET`

  Moves the cursor to the first record in the database with the specified key.

- `DB_SET_RANGE`

  Identical to `DB_SET` unless you are using the BTree access. In this case, the cursor moves to the first record in the database whose key is greater than or equal to the specified key. This comparison is determined by the comparison function that you provide for the database. If no comparison function is provided, then the default lexicographical sorting is used.

  For example, suppose you have database records that use the following strings as keys:

  ``` c
  Alabama
  Alaska
  Arizona
  ```

  Then providing a search key of `Alaska` moves the cursor to the second key noted above. Providing a key of `Al` moves the cursor to the first key (`Alabama`), providing a search key of `Alas` moves the cursor to the second key (`Alaska`), and providing a key of `Ar` moves the cursor to the last key (`Arizona`).

- `DB_GET_BOTH`

  Moves the cursor to the first record in the database that uses the specified key and data.

- `DB_GET_BOTH_RANGE`

  Moves the cursor to the first record in the database whose key matches the specified key and whose data is greater than or equal to the specified data. If the database supports duplicate records, then on matching the key, the cursor is moved to the duplicate record with the smallest data that is greater than or equal to the specified data.

  For example, suppose your database uses BTree and it has database records that use the following key/data pairs:

  ``` c
  Alabama/Athens
  Alabama/Florence
  Alaska/Anchorage
  Alaska/Fairbanks
  Arizona/Avondale
  Arizona/Florence 
  ```

  then providing:

  | a search key of ... | and a search data of ... | moves the cursor to ... |
  |---------------------|--------------------------|-------------------------|
  | Alaska              | Fa                       | Alaska/Fairbanks        |
  | Arizona             | Fl                       | Arizona/Florence        |
  | Alaska              | An                       | Alaska/Anchorage        |

For example, assuming a database containing sorted duplicate records of U.S. States/U.S Cities key/data pairs (both as strings), then the following code fragment can be used to position the cursor to any record in the database and print its key/data values:

``` c
#include <db.h>
#include <string.h>

...

DBC *cursorp;
DBT key, data;
DB *dbp;
int ret;
char *search_data = "Fa";
char *search_key = "Alaska";

/* database open omitted for clarity */

/* Get a cursor */
dbp->cursor(dbp, NULL, &cursorp, 0);

/* Set up our DBTs */
key.data = search_key;
key.size = strlen(search_key) + 1;
data.data = search_data;
data.size = strlen(search_data) + 1;

/*
 * Position the cursor to the first record in the database whose
 * key matches the search key and whose data begins with the 
 * search data.
 */
ret = cursorp->get(cursorp, &key, &data, DB_GET_BOTH_RANGE);
if (!ret) {
    /* Do something with the data */
} else {
    /* Error handling goes here */
}

/* Close the cursor */
if (cursorp != NULL)
    cursorp->close(cursorp);

/* Close the database */
if (dbp != NULL)
    dbp->close(dbp, 0); 
```

### Working with Duplicate Records

A record is a duplicate of another record if the two records share the same key. For duplicate records, only the data portion of the record is unique.

Duplicate records are supported only for the BTree or Hash access methods. For information on configuring your database to use duplicate records, see <a href="btree.md#duplicateRecords" class="xref" title="Allowing Duplicate Records">Allowing Duplicate Records</a>.

If your database supports duplicate records, then it can potentially contain multiple records that share the same key. By default, normal database get operations will only return the first such record in a set of duplicate records. Typically, subsequent duplicate records are accessed using a cursor. The following `DBC->get()` flags are interesting when working with databases that support duplicate records:

-  `DB_NEXT`, `DB_PREV`

  Shows the next/previous record in the database, regardless of whether it is a duplicate of the current record. For an example of using these methods, see <a href="Positioning.md" class="xref" title="Getting Records Using the Cursor">Getting Records Using the Cursor</a>.

- `DB_GET_BOTH_RANGE`

  Useful for seeking the cursor to a specific record, regardless of whether it is a duplicate record. See <a href="Positioning.md#cursorsearch" class="xref" title="Searching for Records">Searching for Records</a> for more information.

-  `DB_NEXT_NODUP`, `DB_PREV_NODUP`

  Gets the next/previous non-duplicate record in the database. This allows you to skip over all the duplicates in a set of duplicate records. If you call `DBC->get()` with `DB_PREV_NODUP`, then the cursor is positioned to the last record for the previous key in the database. For example, if you have the following records in your database:

  ``` c
  Alabama/Athens
  Alabama/Florence
  Alaska/Anchorage
  Alaska/Fairbanks
  Arizona/Avondale
  Arizona/Florence
  ```

  and your cursor is positioned to `Alaska/Fairbanks`, and you then call `DBC->get()` with `DB_PREV_NODUP`, then the cursor is positioned to Alabama/Florence. Similarly, if you call `DBC->get()` with `DB_NEXT_NODUP`, then the cursor is positioned to the first record corresponding to the next key in the database.

  If there is no next/previous key in the database, then `DB_NOTFOUND` is returned, and the cursor is left unchanged.

- `DB_NEXT_DUP`

  Gets the next record that shares the current key. If the cursor is positioned at the last record in the duplicate set and you call `DBC->get()` with `DB_NEXT_DUP`, then `DB_NOTFOUND` is returned and the cursor is left unchanged.

For example, the following code fragment positions a cursor to a key and displays it and all its duplicates.

``` c
#include <db.h>
#include <string.h>

...

DB *dbp;
DBC *cursorp;
DBT key, data;
int ret;
char *search_key = "Al";

/* database open omitted for clarity */

/* Get a cursor */
dbp->cursor(dbp, NULL, &cursorp, 0);

/* Set up our DBTs */
key.data = search_key;
key.size = strlen(search_key) + 1;

/*
 * Position the cursor to the first record in the database whose
 * key and data begin with the correct strings.
 */
ret = cursorp->get(cursorp, &key, &data, DB_SET);
while (ret != DB_NOTFOUND) {
    printf("key: %s, data: %s\n", (char *)key.data, (char *)data.data);
    ret = cursorp->get(cursorp, &key, &data, DB_NEXT_DUP);
}

/* Close the cursor */
if (cursorp != NULL)
    cursorp->close(cursorp);

/* Close the database */
if (dbp != NULL)
    dbp->close(dbp, 0); 
```
