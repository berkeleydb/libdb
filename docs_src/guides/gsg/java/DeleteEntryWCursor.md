---
title: "Deleting Records Using Cursors"
api-name: "Deleting Records Using Cursors"
source: docs/gsg/JAVA/DeleteEntryWCursor.html
---
## Deleting Records Using Cursors

To delete a record using a cursor, simply position the cursor to the record that you want to delete and then call

For example:

``` c
package db.GettingStarted;
    
import com.sleepycat.db.Cursor;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.OperationStatus; 

...

Cursor cursor = null;
Database myDatabase = null;
try {
    ...
    // Database open omitted for brevity
    ...
    // Create DatabaseEntry objects
    // searchKey is some String.
    DatabaseEntry theKey = new DatabaseEntry(searchKey.getBytes("UTF-8"));
    DatabaseEntry theData = new DatabaseEntry();

    // Open a cursor using a database handle
    cursor = myDatabase.openCursor(null, null);

    // Position the cursor. Ignoring the return value for clarity
    OperationStatus retVal = cursor.getSearchKey(theKey, theData, 
                                                 LockMode.DEFAULT);
    
    // Count the number of records using the given key. If there is only
    // one, delete that record.
    if (cursor.count() == 1) {
            System.out.println("Deleting " + 
                               new String(theKey.getData(), "UTF-8") + 
                               "|" + 
                               new String(theData.getData(), "UTF-8"));
            cursor.delete();
    }
} catch (Exception e) {
    // Exception handling goes here
} finally {
   // Make sure to close the cursor
   cursor.close();
}
```
