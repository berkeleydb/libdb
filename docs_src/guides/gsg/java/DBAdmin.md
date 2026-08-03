---
title: "Administrative Methods"
api-name: "Administrative Methods"
source: docs/gsg/JAVA/DBAdmin.html
---
## Administrative Methods

Both the `Environment` and `Database` classes provide methods that are useful for manipulating databases. These methods are:

- `Database.getDatabaseName()`

  Returns the database's name.

  ``` c
  String dbName = myDatabase.getDatabaseName();
  ```

- `Database.rename()`

  Renames the specified database. If no value is given for the *`database`* parameter, then the entire file referenced by this method is renamed.

  Never rename a database that has handles opened for it. Never rename a file that contains databases with opened handles.

  ``` c
  import java.io.FileNotFoundException;
  ...
  myDatabase.close();
  try {
      myDatabase.rename("mydb.db",     // Database file to rename
                        null,          // Database to rename. Not used so
                                       // the entire file is renamed.
                        "newdb.db",    // New name to use.
                        null);         // DatabaseConfig object. 
                                       // None provided.
  } catch (FileNotFoundException fnfe) {
      // Exception handling goes here
  }
  ```

- `Environment.truncateDatabase()`

  Deletes every record in the database and optionally returns the number of records that were deleted. Note that it is much less expensive to truncate a database without counting the number of records deleted than it is to truncate and count.

  ``` c
  int numDiscarded = 
      myEnv.truncate(null,                          // txn handle
                     myDatabase.getDatabaseName(),  // database name
                     true);                         // If true, then the 
                                                    // number of records 
                                                    // deleted are counted.
  System.out.println("Discarded " + numDiscarded +
                     " records from database " + 
                     myDatabase.getDatabaseName()); 
  ```
