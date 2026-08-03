---
title: "BTree Configuration"
api-name: "BTree Configuration"
source: docs/gsg/JAVA/btree.html
---
## BTree Configuration

<span class="sect2"> [Allowing Duplicate Records](btree.md#duplicateRecords) </span>

<span class="sect2"> [Setting Comparison Functions](btree.md#comparators) </span>

In going through the previous chapters in this book, you may notice that we touch on some topics that are specific to BTree, but we do not cover those topics in any real detail. In this section, we will discuss configuration issues that are unique to BTree.

Specifically, in this section we describe:

- Allowing duplicate records.

- Setting comparator callbacks.

### Allowing Duplicate Records

BTree databases can contain duplicate records. One record is considered to be a duplicate of another when both records use keys that compare as equal to one another.

By default, keys are compared using a lexicographical comparison, with shorter keys collating higher than longer keys. You can override this default using the `DatabaseConfig.setBtreeComparator()` method. See the next section for details.

By default, DB databases do not allow duplicate records. As a result, any attempt to write a record that uses a key equal to a previously existing record results in the previously existing record being overwritten by the new record.

Allowing duplicate records is useful if you have a database that contains records keyed by a commonly occurring piece of information. It is frequently necessary to allow duplicate records for secondary databases.

For example, suppose your primary database contained records related to automobiles. You might in this case want to be able to find all the automobiles in the database that are of a particular color, so you would index on the color of the automobile. However, for any given color there will probably be multiple automobiles. Since the index is the secondary key, this means that multiple secondary database records will share the same key, and so the secondary database must support duplicate records.

#### Sorted Duplicates

Duplicate records can be stored in sorted or unsorted order. You can cause DB to automatically sort your duplicate records by setting `DatabaseConfig.setSortedDuplicates()` to `true`. Note that this property must be set prior to database creation time and it cannot be changed afterwards.

If sorted duplicates are supported, then the `java.util.Comparator` implementation identified to `DatabaseConfig.setDuplicateComparator()` is used to determine the location of the duplicate record in its duplicate set. If no such function is provided, then the default lexicographical comparison is used.

#### Unsorted Duplicates

For performance reasons, BTrees should always contain sorted records. (BTrees containing unsorted entries must potentially spend a great deal more time locating an entry than does a BTree that contains sorted entries). That said, DB provides support for suppressing automatic sorting of duplicate records because it may be that your application is inserting records that are already in a sorted order.

That is, if the database is configured to support unsorted duplicates, then the assumption is that your application will manually perform the sorting. In this event, expect to pay a significant performance penalty. Any time you place records into the database in a sort order not know to DB, you will pay a performance penalty

That said, this is how DB behaves when inserting records into a database that supports non-sorted duplicates:

- If your application simply adds a duplicate record using `Database.put()`, then the record is inserted at the end of its sorted duplicate set.

- If a cursor is used to put the duplicate record to the database, then the new record is placed in the duplicate set according to the actual method used to perform the put. The relevant methods are:

  - `Cursor.putAfter()`

    The data is placed into the database as a duplicate record. The key used for this operation is the key used for the record to which the cursor currently refers. Any key provided on the call is therefore ignored.

    The duplicate record is inserted into the database immediately after the cursor's current position in the database.

  - `Cursor.putBefore()`

    Behaves the same as `Cursor.putAfter()` except that the new record is inserted immediately before the cursor's current location in the database.

  - `Cursor.putKeyFirst()`

    If the key already exists in the database, and the database is configured to use duplicates without sorting, then the new record is inserted as the first entry in the appropriate duplicates list.

  - `Cursor.putKeyLast()`

    Behaves identically to `Cursor.putKeyFirst()` except that the new duplicate record is inserted as the last record in the duplicates list.

#### Configuring a Database to Support Duplicates

Duplicates support can only be configured at database creation time. You do this by specifying the appropriate `DatabaseConfig` method before the database is opened for the first time.

The methods that you can use are:

- `DatabaseConfig.setUnsortedDuplicates()`

  The database supports non-sorted duplicate records.

- `DatabaseConfig.setSortedDuplicates()`

  The database supports sorted duplicate records. Note that this flag also sets the flag for you.

The following code fragment illustrates how to configure a database to support sorted duplicate records:

``` c
package db.GettingStarted;

import java.io.FileNotFoundException;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.DatabaseType;

...

Database myDb = null;

try {
    // Typical configuration settings
    DatabaseConfig myDbConfig = new DatabaseConfig();
    myDbConfig.setType(DatabaseType.BTREE);
    myDbConfig.setAllowCreate(true);

    // Configure for sorted duplicates
    myDbConfig.setSortedDuplicates(true);

   // Open the database
   myDb = new Database("mydb.db", null, myDbConfig);
} catch(DatabaseException dbe) {
    System.err.println("MyDbs: " + dbe.toString());
    System.exit(-1);
} catch(FileNotFoundException fnfe) {
    System.err.println("MyDbs: " + fnfe.toString());
    System.exit(-1);
} 
```

### Setting Comparison Functions

By default, DB uses a lexicographical comparison function where shorter records collate before longer records. For the majority of cases, this comparison works well and you do not need to manage it in any way.

However, in some situations your application's performance can benefit from setting a custom comparison routine. You can do this either for database keys, or for the data if your database supports sorted duplicate records.

Some of the reasons why you may want to provide a custom sorting function are:

- Your database is keyed using strings and you want to provide some sort of language-sensitive ordering to that data. Doing so can help increase the locality of reference that allows your database to perform at its best.

- You are using a little-endian system (such as x86) and you are using integers as your database's keys. Berkeley DB stores keys as byte strings and little-endian integers do not sort well when viewed as byte strings. There are several solutions to this problem, one being to provide a custom comparison function. See <a href="http://download.oracle.com/docs/cd/E17076_02/html/programmer_reference/am_misc_faq.html" class="ulink" target="_top">http://download.oracle.com/docs/cd/E17076_02/html/programmer_reference/am_misc_faq.html</a> for more information.

- You you do not want the entire key to participate in the comparison, for whatever reason. In this case, you may want to provide a custom comparison function so that only the relevant bytes are examined.

#### Creating Java Comparators

You set a BTree's key comparator using `DatabaseConfig.setBtreeComparator()`. You can also set a BTree's duplicate data comparison function using `DatabaseConfig.setDuplicateComparator()`.

If the database already exists when it is opened, the comparator provided to these methods must be the same as that historically used to create the database or corruption can occur.

You override the default comparison function by providing a Java `Comparator` class to the database. The Java `Comparator` interface requires you to implement the `Comparator.compare()` method (see <a href="http://download.oracle.com/javase/1.4.2/docs/api/java/util/Comparator.html" class="ulink" target="_top">http://download.oracle.com/javase/1.4.2/docs/api/java/util/Comparator.html</a> for details).

DB hands your `Comparator.compare()` method the `byte` arrays that you stored in the database. If you know how your data is organized in the `byte` array, then you can write a comparison routine that directly examines the contents of the arrays. Otherwise, you have to reconstruct your original objects, and then perform the comparison.

For example, suppose you want to perform unicode lexical comparisons instead of UTF-8 byte-by-byte comparisons. Then you could provide a comparator that uses `String.compareTo()`, which performs a Unicode comparison of two strings (note that for single-byte roman characters, Unicode comparison and UTF-8 byte-by-byte comparisons are identical – this is something you would only want to do if you were using multibyte unicode characters with DB). In this case, your comparator would look like the following:

``` c
package db.GettingStarted;

import java.util.Comparator;

public class MyDataComparator implements Comparator {

    public MyDataComparator() {}

    public int compare(Object d1, Object d2) {

        byte[] b1 = (byte[])d1;
        byte[] b2 = (byte[])d2;

        String s1 = new String(b1);
        String s2 = new String(b2);
        return s1.compareTo(s2);
    }
} 
```

To use this comparator:

``` c
package db.GettingStarted;

import java.io.FileNotFoundException;
import java.util.Comparator;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseException;

...

Database myDatabase = null;
try {
    // Get the database configuration object
    DatabaseConfig myDbConfig = new DatabaseConfig();
    myDbConfig.setAllowCreate(true);

    // Set the duplicate comparator class
    MyDataComparator mdc = new MyDataComparator();
    myDbConfig.setDuplicateComparator(mdc);

    // Open the database that you will use to store your data
    myDbConfig.setSortedDuplicates(true);
    myDatabase = new Database("myDb", null, myDbConfig);
} catch (DatabaseException dbe) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
    // Exception handling goes here
}
```
