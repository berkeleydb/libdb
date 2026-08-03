---
title: "Chapter 7. Databases"
api-name: "Chapter 7. Databases"
source: docs/gsg/JAVA/databases.html
---
## Chapter 7. Databases

**Table of Contents**

<span class="sect1"> [Opening Databases](databases.md#DBOpen) </span>

<span class="sect1"> [Closing Databases](coredbclose.md) </span>

<span class="sect1"> [Database Properties](dbprops.md) </span>

<span class="sect1"> [Administrative Methods](DBAdmin.md) </span>

<span class="sect1"> [Error Reporting Functions](dbErrorReporting.md) </span>

<span class="sect1"> [Managing Databases in Environments](CoreEnvUsage.md) </span>

<span class="sect1"> [Database Example](CoreJavaUsage.md) </span>

In Berkeley DB, a database is a collection of <span class="emphasis">*records*</span>. Records, in turn, consist of key/data pairings.

Conceptually, you can think of a `Database` as containing a two-column table where column 1 contains a key and column 2 contains data. Both the key and the data are managed using `DatabaseEntry` class instances (see <a href="DBEntry.md" class="xref" title="Chapter 8. Database Records">Database Records</a> for details on this class ). So, fundamentally, using a DB `Database` involves putting, getting, and deleting database records, which in turns involves efficiently managing information encapsulated by `DatabaseEntry` objects. The next several chapters of this book are dedicated to those activities.

Also, note that in the previous section of this book, <a href="dpl.md" class="xref" title="Part I. Programming with the Direct Persistence Layer">Programming with the Direct Persistence Layer</a>, we described the DPL The DPL handles all database management for you, including creating all primary and secondary databases as is required by your application. That said, if you are using the DPL you can access the underlying database for a given index if necessary. See the Javadoc for the DPL for more information.

## Opening Databases

You open a database by instantiating a `Database` object.

Note that by default, DB does not create databases if they do not already exist. To override this behavior, set the <a href="dbprops.md" class="link" title="Database Properties">creation property</a> to true.

The following code fragment illustrates a database open:

``` c
package db.GettingStarted;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;

import java.io.FileNotFoundException;
...

Database myDatabase = null;

...

try {
    // Open the database. Create it if it does not already exist.
    DatabaseConfig dbConfig = new DatabaseConfig();
    dbConfig.setAllowCreate(true);
    myDatabase = new Database ("sampleDatabase.db",
                               null, 
                               dbConfig); 
} catch (DatabaseException dbe) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
    // Exception handling goes here
}
```
