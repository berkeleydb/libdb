---
title: "Error Reporting Functions"
api-name: "Error Reporting Functions"
source: docs/gsg/C/dbErrorReporting.html
---
## Error Reporting Functions

To simplify error reporting and handling, the `DB` structure offers several useful methods.

- `set_errcall()`

  Defines the function that is called when an error message is issued by DB. The error prefix and message are passed to this callback. It is up to the application to display this information correctly.

- `set_errfile()`

  Sets the C library `FILE *` to be used for displaying error messages issued by the DB library.

- `set_errpfx()`

  Sets the prefix used for any error messages issued by the DB library.

- `err()`

  Issues an error message. The error message is sent to the callback function as defined by `set_errcall`. If that method has not been used, then the error message is sent to the file defined by `set_errfile()`. If none of these methods have been used, then the error message is sent to standard error.

  The error message consists of the prefix string (as defined by `set_errpfx()`), an optional `printf`-style formatted message, the error message, and a trailing newline.

- `errx()`

  Behaves identically to `err()` except that the DB message text associated with the supplied error value is not appended to the error string.

In addition, you can use the `db_strerror()` function to directly return the error string that corresponds to a particular error number.

For example, to send all error messages for a given database handle to a callback for handling, first create your callback. Do something like this:

``` c
/* 
 * Function called to handle any database error messages
 * issued by DB. 
 */
void
my_error_handler(const DB_ENV *dbenv, const char *error_prefix,
    const char *msg)
{
  /* 
   * Put your code to handle the error prefix and error
   * message here. Note that one or both of these parameters
   * may be NULL depending on how the error message is issued
   * and how the DB handle is configured.
   */
} 
```

And then register the callback as follows:

``` c
#include <db.h>
#include <stdio.h>

...

DB *dbp;
int ret;

/*
 * Create a database and initialize it for error
 * reporting.
 */
ret = db_create(&dbp, NULL, 0);
if (ret != 0) {
        fprintf(stderr, "%s: %s\n", "my_program",
          db_strerror(ret));
        return(ret);
}

/* Set up error handling for this database */
dbp->set_errcall(dbp, my_error_handler);
dbp->set_errpfx(dbp, "my_example_program"); 
```

And to issue an error message:

``` c
ret = dbp->open(dbp, 
                NULL,
                "mydb.db", 
                NULL,
                DB_BTREE,
                DB_CREATE,
                0);
if (ret != 0) {
    dbp->err(dbp, ret,
      "Database open failed: %s", "mydb.db");
    return(ret);
}
```
