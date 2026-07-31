---
title: "Administrative Methods"
api-name: "Administrative Methods"
source: docs/gsg/C/CoreDBAdmin.html
---
## Administrative Methods

The following `DB` methods may be useful to you when managing DB databases:

- `DB->get_open_flags()`

  Returns the current open flags. It is an error to use this method on an unopened database.

  ``` c
  #include <db.h>
  ...
  DB *dbp;
  u_int32_t open_flags;

  /* Database open and subsequent operations omitted for clarity */

  dbp->get_open_flags(dbp, &open_flags); 
  ```

- `DB->remove()`

  Removes the specified database. If no value is given for the *`database`* parameter, then the entire file referenced by this method is removed.

  Never remove a database that has handles opened for it. Never remove a file that contains databases with opened handles.

  ``` c
  #include <db.h>
  ...
  DB *dbp;

  /* Database handle creation omitted for clarity */

  dbp->remove(dbp,                   /* Database pointer */
              "mydb.db",             /* Database file to remove */
              NULL,                  /* Database to remove. This is
                                      * NULL so the entire file is
                                      * removed.  */
             0);                     /* Flags. None used. */
  ```

- `DB->rename()`

  Renames the specified database. If no value is given for the *`database`* parameter, then the entire file referenced by this method is renamed.

  Never rename a database that has handles opened for it. Never rename a file that contains databases with opened handles.

  ``` c
  #include <db.h>
  ...
  DB *dbp;

  /* Database handle creation omitted for clarity */

  dbp->rename(dbp,                    /* Database pointer */
               "mydb.db",             /* Database file to rename */
               NULL,                  /* Database to rename. This is
                                       * NULL so the entire file is
                                       * renamed.  */
              "newdb.db",             /* New database file name */
              0);                     /* Flags. None used. */
  ```
