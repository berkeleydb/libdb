---
title: "Deleting Secondary Database Records"
api-name: "Deleting Secondary Database Records"
source: docs/gsg/C/secondaryDelete.html
---
## Deleting Secondary Database Records

In general, you will not modify a secondary database directly. In order to modify a secondary database, you should modify the primary database and simply allow DB to manage the secondary modifications for you.

However, as a convenience, you can delete secondary database records directly. Doing so causes the associated primary key/data pair to be deleted. This in turn causes DB to delete all secondary database records that reference the primary record.

You can use the `DB->del()` method to delete a secondary database record. Note that if your secondary database contains duplicate records, then deleting a record from the set of duplicates causes all of the duplicates to be deleted as well.

### Note

You can delete a secondary database record using the previously described mechanism only if the primary database is opened for write access.

For example:

``` c
#include <db.h>
#include <string.h>

...

DB *dbp, *sdbp;    /* Primary and secondary DB handles */
DBT key;           /* DBTs used for the delete */
int ret;           /* Function return value */
char *search_name = "John Doe"; /* Name to delete */

/* Primary */
ret = db_create(&dbp, NULL, 0);
if (ret != 0) {
  /* Error handling goes here */
}

/* Secondary */
ret = db_create(&sdbp, NULL, 0);
if (ret != 0) {
  /* Error handling goes here */
}

/* Usually we want to support duplicates for secondary databases */
ret = sdbp->set_flags(sdbp, DB_DUPSORT);
if (ret != 0) {
  /* Error handling goes here */
}

/* open the primary database */
ret = dbp->open(dbp,        /* DB structure pointer */
                NULL,       /* Transaction pointer */
                "my_db.db", /* On-disk file that holds the database.
                             * Required. */
                NULL,       /* Optional logical database name */
                DB_BTREE,   /* Database access method */
                0,          /* Open flags */
                0);         /* File mode (using defaults) */
if (ret != 0) {
  /* Error handling goes here */
}

/* open the secondary database */
ret = sdbp->open(sdbp,          /* DB structure pointer */
                 NULL,          /* Transaction pointer */
                 "my_secdb.db", /* On-disk file that holds the database.
                                 * Required. */
                 NULL,          /* Optional logical database name */
                 DB_BTREE,      /* Database access method */
                 0,             /* Open flags */
                 0);            /* File mode (using defaults) */
if (ret != 0) {
  /* Error handling goes here */
}

/* Now associate the secondary to the primary */
dbp->associate(dbp,            /* Primary database */
               NULL,           /* TXN id */
               sdbp,           /* Secondary database */
               get_sales_rep,  /* Callback used for key creation. */
               0);             /* Flags */

/*
 * Zero out the DBT before using it.
 */
memset(&key, 0, sizeof(DBT));

key.data = search_name;
key.size = strlen(search_name) + 1;

/* Now delete the secondary record. This causes the associated primary
 * record to be deleted. If any other secondary databases have secondary
 * records referring to the deleted primary record, then those secondary
 * records are also deleted.
 */
 sdbp->del(sdbp, NULL, &key, 0); 
```
