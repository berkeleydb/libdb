---
title: "Database Properties"
api-name: "Database Properties"
source: docs/gsg/JAVA/dbprops.html
---
## Database Properties

You can set database properties using the `DatabaseConfig` class. For each of the properties that you can set, there is a corresponding getter method. Also, you can always retrieve the `DatabaseConfig` object used by your database using the `Database.getConfig()` method.

There are a large number of properties that you can set using this class (see the javadoc for a complete listing). From the perspective of this manual, some of the more interesting properties are:

- `DatabaseConfig.setAllowCreate()`

  If `true`, the database is created when it is opened. If false, the database open fails if the database does not exist. This property has no meaning if the database currently exists. Default is `false`.

- `DatabaseConfig.setBtreeComparator()`

  Sets the class that is used to compare the keys found on two database records. This class is used to determine the sort order for two records in the database. By default, byte for byte comparison is used. For more information, see <a href="btree.md#comparators" class="xref" title="Setting Comparison Functions">Setting Comparison Functions</a>.

- `DatabaseConfig.setDuplicateComparator()`

  Sets the class that is used to compare two duplicate records in the database. For more information, see <a href="btree.md#comparators" class="xref" title="Setting Comparison Functions">Setting Comparison Functions</a>.

- `DatabaseConfig.setSortedDuplicates()`

  If `true`, duplicate records are allowed in the database. If this value is `false`, then putting a duplicate record into the database results in an error return from the put call. Note that this property can be set only at database creation time. Default is `false`.

  Note that your database must not support duplicates if it is to be associated with one or more secondary indices. Secondaries are described in <a href="indexes.md" class="xref" title="Chapter 10. Secondary Databases">Secondary Databases</a>.

- `DatabaseConfig.setExclusiveCreate()`

  If `true`, the database open fails if the database currently exists. That is, the open must result in the creation of a new database. Default is `false`.

- `DatabaseConfig.setReadOnly()`

  If true, the database is opened for read activities only. Default is `false`.

- `DatabaseConfig.setTruncate()`

  If true, the database is truncated; that is, it is emptied of all content.

- `DatabaseConfig.setType()`

  Identifies the type of database that you want to create. This manual will exclusively use `DatabaseType.BTREE`.

In addition to these, there are also methods that allow you to control the IO stream used for error reporting purposes. These are described later in this manual.

For example:

``` c
package db.GettingStarted;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.DatabaseType;

import java.io.FileNotFoundException;

...
Database myDatabase = null;
try {
    DatabaseConfig dbConfig = new DatabaseConfig();
    dbConfig.setAllowCreate(true);
    dbConfig.setSortedDuplicates(true);
    dbConfig.setType(DatabaseType.BTREE);
    myDatabase = new Database("sampleDatabase.db",
                              null,
                              dbConfig); 
} catch (DatabaseException dbe) {
    // Exception handling goes here.
} catch (FileNotFoundException fnfe) {
    // Exception handling goes here
}
```
