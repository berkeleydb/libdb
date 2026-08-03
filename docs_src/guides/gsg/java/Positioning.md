---
title: "Getting Records Using the Cursor"
api-name: "Getting Records Using the Cursor"
source: docs/gsg/JAVA/Positioning.html
---
## Getting Records Using the Cursor

<span class="sect2"> [Searching for Records](Positioning.md#cursorsearch) </span>

<span class="sect2"> [Working with Duplicate Records](Positioning.md#getdups) </span>

To iterate over database records, from the first record to the last, simply open the cursor and then use the `Cursor.getNext()` method. For example:

``` c
package db.GettingStarted;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Cursor;
import com.sleepycat.db.LockMode;  
import com.sleepycat.db.OperationStatus; 

...

Cursor cursor = null;
try {
    ...
    Database myDatabase = null;
    // Database open omitted for brevity
    ...

    // Open the cursor. 
    cursor = myDatabase.openCursor(null, null);

    // Cursors need a pair of DatabaseEntry objects to operate. These hold
    // the key and data found at any given position in the database.
    DatabaseEntry foundKey = new DatabaseEntry();
    DatabaseEntry foundData = new DatabaseEntry();

    // To iterate, just call getNext() until the last database record has 
    // been read. All cursor operations return an OperationStatus, so just
    // read until we no longer see OperationStatus.SUCCESS
    while (cursor.getNext(foundKey, foundData, LockMode.DEFAULT) ==
        OperationStatus.SUCCESS) {
        // getData() on the DatabaseEntry objects returns the byte array
        // held by that object. We use this to get a String value. If the
        // DatabaseEntry held a byte array representation of some other 
        // data type (such as a complex object) then this operation would
        // look considerably different.
        String keyString = new String(foundKey.getData(), "UTF-8");
        String dataString = new String(foundData.getData(), "UTF-8");
        System.out.println("Key | Data : " + keyString + " | " + 
                       dataString + "");
    }
} catch (DatabaseException de) {
    System.err.println("Error accessing database." + de);
} finally {
    // Cursors must be closed.
    cursor.close();
}
```

To iterate over the database from the last record to the first, instantiate the cursor, and then use `Cursor.getPrev()` until you read the first record in the database. For example:

``` c
package db.GettingStarted;
    
import com.sleepycat.db.Cursor;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.LockMode;  
import com.sleepycat.db.OperationStatus; 

...

Cursor cursor = null;
Database myDatabase = null;
try {
    ...
    // Database open omitted for brevity
    ...

    // Open the cursor. 
    cursor = myDatabase.openCursor(null, null);

    // Get the DatabaseEntry objects that the cursor will use.
    DatabaseEntry foundKey = new DatabaseEntry();
    DatabaseEntry foundData = new DatabaseEntry();

    // Iterate from the last record to the first in the database
    while (cursor.getPrev(foundKey, foundData, LockMode.DEFAULT) == 
        OperationStatus.SUCCESS) {

        String theKey = new String(foundKey.getData(), "UTF-8");
        String theData = new String(foundData.getData(), "UTF-8");
        System.out.println("Key | Data : " +  theKey + " | " + 
                           theData + "");
    }
} catch (DatabaseException de) {
    System.err.println("Error accessing database." + de);
} finally {
    // Cursors must be closed.
    cursor.close();
}
```

### Searching for Records

You can use cursors to search for database records. You can search based on just a key, or you can search based on both the key and the data. You can also perform partial matches if your database supports sorted duplicate sets. In all cases, the key and data parameters of these methods are filled with the key and data values of the database record to which the cursor is positioned as a result of the search.

Also, if the search fails, then cursor's state is left unchanged and `OperationStatus.NOTFOUND` is returned.

The following `Cursor` methods allow you to perform database searches:

- `Cursor.getSearchKey()`

  Moves the cursor to the first record in the database with the specified key.

- `Cursor.getSearchKeyRange()`

  Identical to `Cursor.getSearchKey()` unless you are using the BTree access. In this case, the cursor moves to the first record in the database whose key is greater than or equal to the specified key. This comparison is determined by the comparator that you provide for the database. If no comparator is provided, then the default lexicographical sorting is used.

  For example, suppose you have database records that use the following Strings as keys:

  ``` c
  Alabama
  Alaska
  Arizona
  ```

  Then providing a search key of `Alaska` moves the cursor to the second key noted above. Providing a key of `Al` moves the cursor to the first key (`Alabama`), providing a search key of `Alas` moves the cursor to the second key (`Alaska`), and providing a key of `Ar` moves the cursor to the last key (`Arizona`).

- `Cursor.getSearchBoth()`

  Moves the cursor to the first record in the database that uses the specified key and data.

- `Cursor.getSearchBothRange()`

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

For example, assuming a database containing sorted duplicate records of U.S. States/U.S Cities key/data pairs (both as Strings), then the following code fragment can be used to position the cursor to any record in the database and print its key/data values:

``` c
package db.GettingStarted;

import com.sleepycat.db.Cursor;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.OperationStatus; 

...
  
// For this example, hard code the search key and data
String searchKey = "Alaska";
String searchData = "Fa";

Cursor cursor = null;
Database myDatabase = null;
try {
    ...
    // Database open omitted for brevity
    ...

    // Open the cursor. 
    cursor = myDatabase.openCursor(null, null);

    DatabaseEntry theKey = 
         new DatabaseEntry(searchKey.getBytes("UTF-8"));
    DatabaseEntry theData = 
         new DatabaseEntry(searchData.getBytes("UTF-8"));

    // Open a cursor using a database handle
    cursor = myDatabase.openCursor(null, null);

    // Perform the search
    OperationStatus retVal = cursor.getSearchBothRange(theKey, theData, 
                                                       LockMode.DEFAULT);
    // NOTFOUND is returned if a record cannot be found whose key 
    // matches the search key AND whose data begins with the search data.
    if (retVal == OperationStatus.NOTFOUND) {
        System.out.println(searchKey + "/" + searchData + 
                           " not matched in database " + 
                           myDatabase.getDatabaseName());
    } else {
        // Upon completing a search, the key and data DatabaseEntry 
        // parameters for getSearchBothRange() are populated with the 
        // key/data values of the found record.
        String foundKey = new String(theKey.getData(), "UTF-8");
        String foundData = new String(theData.getData(), "UTF-8");
        System.out.println("Found record " + foundKey + "/" + foundData + 
                           "for search key/data: " + searchKey + 
                           "/" + searchData);
    }

} catch (Exception e) {
    // Exception handling goes here
} finally {
   // Make sure to close the cursor
   cursor.close();
}
```

### Working with Duplicate Records

A record is a duplicate of another record if the two records share the same key. For duplicate records, only the data portion of the record is unique.

Duplicate records are supported only for the BTree or Hash access methods. For information on configuring your database to use duplicate records, see <a href="btree.md#duplicateRecords" class="xref" title="Allowing Duplicate Records">Allowing Duplicate Records</a>.

If your database supports duplicate records, then it can potentially contain multiple records that share the same key. By default, normal database get operations will only return the first such record in a set of duplicate records. Typically, subsequent duplicate records are accessed using a cursor. The following `Cursor` methods are interesting when working with databases that support duplicate records:

-  `Cursor.getNext()`, `Cursor.getPrev()`

  Shows the next/previous record in the database, regardless of whether it is a duplicate of the current record. For an example of using these methods, see <a href="Positioning.md" class="xref" title="Getting Records Using the Cursor">Getting Records Using the Cursor</a>.

- `Cursor.getSearchBothRange()`

  Useful for seeking the cursor to a specific record, regardless of whether it is a duplicate record. See <a href="Positioning.md#cursorsearch" class="xref" title="Searching for Records">Searching for Records</a> for more information.

-  `Cursor.getNextNoDup()`, `Cursor.getPrevNoDup()`

  Gets the next/previous non-duplicate record in the database. This allows you to skip over all the duplicates in a set of duplicate records. If you call `Cursor.getPrevNoDup()`, then the cursor is positioned to the last record for the previous key in the database. For example, if you have the following records in your database:

  ``` c
  Alabama/Athens
  Alabama/Florence
  Alaska/Anchorage
  Alaska/Fairbanks
  Arizona/Avondale
  Arizona/Florence
  ```

  and your cursor is positioned to `Alaska/Fairbanks`, and you then call `Cursor.getPrevNoDup()`, then the cursor is positioned to Alabama/Florence. Similarly, if you call `Cursor.getNextNoDup()`, then the cursor is positioned to the first record corresponding to the next key in the database.

  If there is no next/previous key in the database, then `OperationStatus.NOTFOUND` is returned, and the cursor is left unchanged.

- Gets the next record that shares the current key. If the cursor is positioned at the last record in the duplicate set and you call `Cursor.getNextDup()`, then `OperationStatus.NOTFOUND` is returned and the cursor is left unchanged. Likewise, if you call `getPrevDup()` and the cursor is positioned at the first record in the duplicate set, then `OperationStatus.NOTFOUND` is returned and the cursor is left unchanged.

- `Cursor.count()`

  Returns the total number of records that share the current key.

For example, the following code fragment positions a cursor to a key and displays it and all its duplicates. Note that the following code fragment assumes that the database contains only String objects for the keys and data.

``` c
package db.GettingStarted;
      
import com.sleepycat.db.Cursor;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
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

    // Position the cursor
    // Ignoring the return value for clarity
    OperationStatus retVal = cursor.getSearchKey(theKey, theData, 
                                                 LockMode.DEFAULT);
    
    // Count the number of duplicates. If the count is greater than 1, 
    // print the duplicates.
    if (cursor.count() > 1) {
        while (retVal == OperationStatus.SUCCESS) {
            String keyString = new String(theKey.getData(), "UTF-8");
            String dataString = new String(theData.getData(), "UTF-8");
            System.out.println("Key | Data : " +  keyString + " | " + 
                               dataString + "");
   
            retVal = cursor.getNextDup(theKey, theData, LockMode.DEFAULT);
        }
    }
} catch (Exception e) {
    // Exception handling goes here
} finally {
   // Make sure to close the cursor
   cursor.close();
}
```
