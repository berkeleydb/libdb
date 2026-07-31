---
title: "Chapter 2. Databases"
api-name: "Chapter 2. Databases"
source: docs/gsg/C/databases.html
---
## Chapter 2. Databases

**Table of Contents**

<span class="sect1"> [Opening Databases](databases.md#DBOpen) </span>

<span class="sect1"> [Closing Databases](coredbclose.md) </span>

<span class="sect1"> [Database Open Flags](DBOpenFlags.md) </span>

<span class="sect1"> [Administrative Methods](CoreDBAdmin.md) </span>

<span class="sect1"> [Error Reporting Functions](dbErrorReporting.md) </span>

<span class="sect1"> [Managing Databases in Environments](CoreEnvUsage.md) </span>

<span class="sect1"> [Database Example](CoreDbUsage.md) </span>

In Berkeley DB, a database is a collection of <span class="emphasis">*records*</span>. Records, in turn, consist of key/data pairings.

Conceptually, you can think of a database as containing a two-column table where column 1 contains a key and column 2 contains data. Both the key and the data are managed using `DBT` structures (see <a href="DBEntry.md" class="xref" title="Chapter 3. Database Records">Database Records</a> for details on this structure). So, fundamentally, using a DB database involves putting, getting, and deleting database records, which in turns involves efficiently managing information contained in `DBT` structures. The next several chapters of this book are dedicated to those activities.

## Opening Databases

To open a database, you must first use the `db_create()` function to initialize a `DB` handle. Once you have initialized the `DB` handle, you use its `open()` method to open the database.

Note that by default, DB does not create databases if they do not already exist. To override this behavior, specify the <a href="DBOpenFlags.md" class="link" title="Database Open Flags"><code class="literal">DB_CREATE</code></a> flag on the `open()` method.

The following code fragment illustrates a database open:

``` c
#include <db.h> 

...

DB *dbp;           /* DB structure handle */
u_int32_t flags;   /* database open flags */
int ret;           /* function return value */

/* Initialize the structure. This
 * database is not opened in an environment, 
 * so the environment pointer is NULL. */
ret = db_create(&dbp, NULL, 0);
if (ret != 0) {
  /* Error handling goes here */
}

/* Database open flags */
flags = DB_CREATE;    /* If the database does not exist, 
                       * create it.*/

/* open the database */
ret = dbp->open(dbp,        /* DB structure pointer */
                NULL,       /* Transaction pointer */
                "my_db.db", /* On-disk file that holds the database. */
                NULL,       /* Optional logical database name */
                DB_BTREE,   /* Database access method */
                flags,      /* Open flags */
                0);         /* File mode (using defaults) */
if (ret != 0) {
  /* Error handling goes here */
}
```
