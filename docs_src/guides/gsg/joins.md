---
title: "Database Joins"
api-name: "Database Joins"
source: docs/gsg/C/joins.html
---
## Database Joins

<span class="sect2"> [Using Join Cursors](joins.md#joinUsage) </span>

If you have two or more secondary databases associated with a primary database, then you can retrieve primary records based on the intersection of multiple secondary entries. You do this using a join cursor.

Throughout this document we have presented a structure that stores information on grocery vendors. That structure is fairly simple with a limited number of data members, few of which would be interesting from a query perspective. But suppose, instead, that we were storing information on something with many more characteristics that can be queried, such as an automobile. In that case, you may be storing information such as color, number of doors, fuel mileage, automobile type, number of passengers, make, model, and year, to name just a few.

In this case, you would still likely be using some unique value to key your primary entries (in the United States, the automobile's VIN would be ideal for this purpose). You would then create a structure that identifies all the characteristics of the automobiles in your inventory.

To query this data, you might then create multiple secondary databases, one for each of the characteristics that you want to query. For example, you might create a secondary for color, another for number of doors, another for number of passengers, and so forth. Of course, you will need a unique key extractor function for each such secondary database. You do all of this using the concepts and techniques described throughout this chapter.

Once you have created this primary database and all interesting secondaries, what you have is the ability to retrieve automobile records based on a single characteristic. You can, for example, find all the automobiles that are red. Or you can find all the automobiles that have four doors. Or all the automobiles that are minivans.

The next most natural step, then, is to form compound queries, or joins. For example, you might want to find all the automobiles that are red, and that were built by Toyota, and that are minivans. You can do this using a join cursor.

### Using Join Cursors

To use a join cursor:

- Open two or more cursors for secondary databases that are associated with the same primary database.

- Position each such cursor to the secondary key value in which you are interested. For example, to build on the previous description, the cursor for the color database is positioned to the `red` records while the cursor for the model database is positioned to the `minivan` records, and the cursor for the make database is positioned to `Toyota`.

- Create an array of cursors, and place in it each of the cursors that are participating in your join query. Note that this array must be null terminated.

- Obtain a join cursor. You do this using the `DB->join()` method. You must pass this method the array of secondary cursors that you opened and positioned in the previous steps.

- Iterate over the set of matching records until the return code is not `0`.

- Close your cursor.

- If you are done with them, close all your cursors.

For example:

``` c
#include <db.h>
#include <string.h>

...

DB *automotiveDB;
DB *automotiveColorDB;
DB *automotiveMakeDB;
DB *automotiveTypeDB;
DBC *color_curs, *make_curs, *type_curs, *join_curs;
DBC *carray[4];
DBT key, data;
int ret;

char *the_color = "red";
char *the_type = "minivan";
char *the_make = "Toyota";

/* Database and secondary database opens omitted for brevity.
 * Assume a primary database handle:
 *   automotiveDB
 * Assume 3 secondary database handles:
 *   automotiveColorDB  -- secondary database based on automobile color
 *   automotiveMakeDB  -- secondary database based on the manufacturer
 *   automotiveTypeDB  -- secondary database based on automobile type
 */

/* initialize pointers and structures */
color_curs = NULL;
make_curs = NULL;
type_curs = NULL;
join_curs = NULL;

memset(&key, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));

/* open the cursors */
if (( ret =
    automotiveColorDB->cursor(automotiveColorDB, NULL, 
      &color_curs, 0)) != 0) {
        /* Error handling goes here */
}

if (( ret =
    automotiveMakeDB->cursor(automotiveMakeDB, NULL, 
      &make_curs, 0)) != 0) {
        /* Error handling goes here */
}

if (( ret =
    automotiveTypeDB->cursor(automotiveTypeDB, NULL, 
      &type_curs, 0)) != 0) {
        /* Error handling goes here */
}

/* Position the cursors */
key.data = the_color;
key.size = strlen(the_color) + 1;
if ((ret = color_curs->get(color_curs, &key, &data, DB_SET)) != 0)
    /* Error handling goes here */

key.data = the_make;
key.size = strlen(the_make) + 1;
if ((ret = make_curs->get(make_curs, &key, &data, DB_SET)) != 0)
    /* Error handling goes here */

key.data = the_type;
key.size = strlen(the_type) + 1;
if ((ret = type_curs->get(type_curs, &key, &data, DB_SET)) != 0)
    /* Error handling goes here */

/* Set up the cursor array */
carray[0] = color_curs;
carray[1] = make_curs;
carray[2] = type_curs;
carray[3] = NULL;

/* Create the join */
if ((ret = automotiveDB->join(automotiveDB, carray, &join_curs, 0)) != 0)
    /* Error handling goes here */

/* Iterate using the join cursor */
while ((ret = join_curs->get(join_curs, &key, &data, 0)) == 0) {
    /* Do interesting things with the key and data */
}

/*
 * If we exited the loop because we ran out of records,
 * then it has completed successfully.
 */
if (ret == DB_NOTFOUND) {
    /* 
     * Close all our cursors and databases as is appropriate,  and 
     * then exit with a normal exit status (0). 
     */
} 
```
