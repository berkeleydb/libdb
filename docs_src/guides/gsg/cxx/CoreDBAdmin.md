---
title: "Administrative Methods"
api-name: "Administrative Methods"
source: docs/gsg/CXX/CoreDBAdmin.html
---
## Administrative Methods

The following `Db` methods may be useful to you when managing DB databases:

- `Db::get_open_flags()`

  Returns the current open flags. It is an error to use this method on an unopened database.

  ``` c
  #include <db_cxx.h>
  ...
  Db db(NULL, 0);
  u_int32_t open_flags;

  // Database open and subsequent operations omitted for clarity

  db.get_open_flags(&open_flags); 
  ```

- `Db::remove()`

  Removes the specified database. If no value is given for the *`database`* parameter, then the entire file referenced by this method is removed.

  Never remove a database that has handles opened for it. Never remove a file that contains databases with opened handles.

  ``` c
  #include <db_cxx.h>
  ...
  Db db(NULL, 0);

  // Database handle creation omitted for clarity

  db.remove("mydb.db",             // Database file to remove 
            NULL,                  // Database to remove. This is
                                   // NULL so the entire file is
                                   // removed.  
           0);                     // Flags. None used.
  ```

- `Db::rename()`

  Renames the specified database. If no value is given for the *`database`* parameter, then the entire file referenced by this method is renamed.

  Never rename a database that has handles opened for it. Never rename a file that contains databases with opened handles.

  ``` c
  #include <db_cxx.h>
  ...
  Db db(NULL, 0);

  // Database handle creation omitted for clarity

  db.rename("mydb.db",             // Database file to rename
            NULL,                  // Database to rename. This is
                                   // NULL so the entire file is
                                   // renamed. 
           "newdb.db",             // New database file name
           0);                     // Flags. None used.
  ```
