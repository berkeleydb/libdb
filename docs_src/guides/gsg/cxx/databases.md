---
title: "Chapter 2. Databases"
api-name: "Chapter 2. Databases"
source: docs/gsg/CXX/databases.html
---
## Chapter 2. Databases

**Table of Contents**

<span class="sect1"> [Opening Databases](databases.md#DBOpen) </span>

<span class="sect1"> [Closing Databases](coredbclose.md) </span>

<span class="sect1"> [Database Open Flags](DBOpenFlags.md) </span>

<span class="sect1"> [Administrative Methods](CoreDBAdmin.md) </span>

<span class="sect1"> [Error Reporting Functions](dbErrorReporting.md) </span>

<span class="sect1"> [Managing Databases in Environments](CoreEnvUsage.md) </span>

<span class="sect1"> [Database Example](CoreDbCXXUsage.md) </span>

In Berkeley DB, a database is a collection of <span class="emphasis">*records*</span>. Records, in turn, consist of key/data pairings.

Conceptually, you can think of a database as containing a two-column table where column 1 contains a key and column 2 contains data. Both the key and the data are managed using `Dbt` class instances (see <a href="DBEntry.md" class="xref" title="Chapter 3. Database Records">Database Records</a> for details on this class ). So, fundamentally, using a DB database involves putting, getting, and deleting database records, which in turns involves efficiently managing information encapsulated by `Dbt` objects. The next several chapters of this book are dedicated to those activities.

## Opening Databases

You open a database by instantiating a `Db` object and then calling its `open()` method.

Note that by default, DB does not create databases if they do not already exist. To override this behavior, specify the <a href="DBOpenFlags.md" class="link" title="Database Open Flags"><code class="literal">DB_CREATE</code></a> flag on the `open()` method.

The following code fragment illustrates a database open:

``` c
#include <db_cxx.h>

...

Db db(NULL, 0);               // Instantiate the Db object

u_int32_t oFlags = DB_CREATE; // Open flags;

try {
    // Open the database
    db.open(NULL,                // Transaction pointer 
            "my_db.db",          // Database file name 
            NULL,                // Optional logical database name
            DB_BTREE,            // Database access method
            oFlags,              // Open flags
            0);                  // File mode (using defaults)
// DbException is not subclassed from std::exception, so
// need to catch both of these.
} catch(DbException &e) {
    // Error handling code goes here    
} catch(std::exception &e) {
    // Error handling code goes here
} 
```
