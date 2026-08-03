---
title: "Chapter 9. Using Cursors"
api-name: "Chapter 9. Using Cursors"
source: docs/gsg/JAVA/Cursors.html
---
## Chapter 9. Using Cursors

**Table of Contents**

<span class="sect1"> [Opening and Closing Cursors](Cursors.md#openCursor) </span>

<span class="sect1"> [Getting Records Using the Cursor](Positioning.md) </span>

<span class="sect2"> [Searching for Records](Positioning.md#cursorsearch) </span>

<span class="sect2"> [Working with Duplicate Records](Positioning.md#getdups) </span>

<span class="sect1"> [Putting Records Using Cursors](PutEntryWCursor.md) </span>

<span class="sect1"> [Deleting Records Using Cursors](DeleteEntryWCursor.md) </span>

<span class="sect1"> [Replacing Records Using Cursors](ReplacingEntryWCursor.md) </span>

<span class="sect1"> [Cursor Example](cursorJavaUsage.md) </span>

Cursors provide a mechanism by which you can iterate over the records in a database. Using cursors, you can get, put, and delete database records. If a database allows duplicate records, then cursors are the easiest way that you can access anything other than the first record for a given key.

This chapter introduces cursors. It explains how to open and close them, how to use them to modify databases, and how to use them with duplicate records.

## Opening and Closing Cursors

To use a cursor, you must open it using the `Database.openCursor()` method. When you open a cursor, you can optionally pass it a `CursorConfig` object to set cursor properties. The cursor properties that you can set allows you to control the isolation level that the cursor will obey. See the *Berkeley DB Getting Started with Transaction Processing* guide for more information.

For example:

``` c
package db.GettingStarted;
    
import com.sleepycat.db.Cursor;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseException;

import java.io.FileNotFoundException;

...
Database myDatabase = null;
Cursor myCursor = null;

try {
    myDatabase = new Database("myDB", null, null);

    myCursor = myDatabase.openCursor(null, null);
} catch (FileNotFoundException fnfe) {
    // Exception handling goes here ...
} catch (DatabaseException dbe) {
    // Exception handling goes here ...
}
```

To close the cursor, call the `Cursor.close()` method. Note that if you close a database that has cursors open in it, then it will throw an exception and close any open cursors for you. For best results, close your cursors from within a `finally` block. However, it is recommended that you always close all cursor handles immediately after their use to ensure concurrency and to release resources such as page locks.

``` c
package db.GettingStarted;
    
import com.sleepycat.db.Cursor;
import com.sleepycat.db.Database;

...
try {
    ...
} catch ... {
} finally {
    try {
        if (myCursor != null) {
            myCursor.close();
        }

        if (myDatabase != null) {
            myDatabase.close();
        }
    } catch(DatabaseException dbe) {
        System.err.println("Error in close: " + dbe.toString());
    }
} 
```
