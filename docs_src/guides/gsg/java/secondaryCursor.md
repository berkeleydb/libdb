---
title: "Using Secondary Cursors"
api-name: "Using Secondary Cursors"
source: docs/gsg/JAVA/secondaryCursor.html
---
## Using Secondary Cursors

Just like cursors on a primary database, you can use secondary cursors to iterate over the records in a secondary database. Like normal cursors, you can also use secondary cursors to search for specific records in a database, to seek to the first or last record in the database, to get the next duplicate record, and so forth. For a complete description on cursors and their capabilities, see <a href="Cursors.md" class="xref" title="Chapter 9. Using Cursors">Using Cursors</a>.

However, when you use secondary cursors:

- Any data returned is the data contained on the primary database record referenced by the secondary record.

- `SecondaryCursor.getSearchBoth()` and related methods do not search based on a key/data pair. Instead, you search based on a secondary key and a primary key. The data returned is the primary data that most closely matches the two keys provided for the search.

For example, suppose you are using the databases, classes, and key creators described in <a href="keyCreator.md" class="xref" title="Implementing Key Creators">Implementing Key <span>Creators</span></a> . Then the following searches for a person's name in the secondary database, and deletes all secondary and primary records that use that name.

``` c
package db.GettingStarted;

import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.OperationStatus;
import com.sleepycat.db.SecondaryDatabase;
import com.sleepycat.db.SecondaryCursor;
  
...
try {
    SecondaryDatabase mySecondaryDatabase = null;
    // Database opens omitted for brevity
    ...

    String secondaryName = "John Doe";
    DatabaseEntry secondaryKey = 
        new DatabaseEntry(secondaryName.getBytes("UTF-8"));

    DatabaseEntry foundData = new DatabaseEntry();

    SecondaryCursor mySecCursor = 
        mySecondaryDatabase.openSecondaryCursor(null, null);

    OperationStatus retVal = mySecCursor.getSearchKey(secondaryKey, 
                                                  foundData, 
                                                  LockMode.DEFAULT);
    while (retVal == OperationStatus.SUCCESS) {
        mySecCursor.delete();
        retVal = mySecCursor.getNextDup(secondaryKey, 
                                        foundData, 
                                        LockMode.DEFAULT);
    } 
} catch (Exception e) {
    // Exception handling goes here
}
```
