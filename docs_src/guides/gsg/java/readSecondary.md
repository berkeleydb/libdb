---
title: "Reading Secondary Databases"
api-name: "Reading Secondary Databases"
source: docs/gsg/JAVA/readSecondary.html
---
## Reading Secondary Databases

Like a primary database, you can read records from your secondary database either by using the `SecondaryDatabase.get()` method, or by using a `SecondaryCursor`. The main difference between reading secondary and primary databases is that when you read a secondary database record, the secondary record's data is not returned to you. Instead, the primary key and data corresponding to the secondary key are returned to you.

For example, assuming your secondary database contains keys related to a person's full name:

``` c
package db.GettingStarted;

import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.OperationStatus;
import com.sleepycat.db.SecondaryDatabase;

...
SecondaryDatabase mySecondaryDatabase = null;
try {
    // Omitting all database opens
    ...

    String searchName = "John Doe";
    DatabaseEntry searchKey = 
        new DatabaseEntry(searchName.getBytes("UTF-8"));
    DatabaseEntry primaryKey = new DatabaseEntry();
    DatabaseEntry primaryData = new DatabaseEntry();

    // Get the primary key and data for the user 'John Doe'.
    OperationStatus retVal = mySecondaryDatabase.get(null, searchKey, 
                                                     primaryKey, 
                                                     primaryData, 
                                                     LockMode.DEFAULT); 
} catch (Exception e) {
    // Exception handling goes here
}
```

Note that, just like `Database.get()`, if your secondary database supports duplicate records then `SecondaryDatabase.get()` only return the first record found in a matching duplicates set. If you want to see all the records related to a specific secondary key, then use a `SecondaryCursor` (described in <a href="secondaryCursor.md" class="xref" title="Using Secondary Cursors"><span>Using Secondary Cursors</span></a> ).
