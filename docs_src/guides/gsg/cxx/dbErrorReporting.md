---
title: "Error Reporting Functions"
api-name: "Error Reporting Functions"
source: docs/gsg/CXX/dbErrorReporting.html
---
## Error Reporting Functions

To simplify error reporting and handling, the `Db` class offers several useful methods.

- `set_error_stream()`

  Sets the C++ `ostream` to be used for displaying error messages issued by the DB library.

- `set_errcall()`

  Defines the function that is called when an error message is issued by DB. The error prefix and message are passed to this callback. It is up to the application to display this information correctly.

- `set_errfile()`

  Sets the C library `FILE *` to be used for displaying error messages issued by the DB library.

- `set_errpfx()`

  Sets the prefix used for any error messages issued by the DB library.

- `err()`

  Issues an error message. The error message is sent to the callback function as defined by `set_errcall`. If that method has not been used, then the error message is sent to the file defined by `set_errfile()` or `set_error_stream()`. If none of these methods have been used, then the error message is sent to standard error.

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
my_error_handler(const DbEnv *dbenv, const char *error_prefix,
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
#include <db_cxx.h>
...

Db db(NULL, 0);
std::string dbFileName("my_db.db");

try
{
    // Set up error handling for this database
    db.set_errcall(my_error_handler);
    db.set_errpfx("my_example_program"); 
```

And to issue an error message:

``` c
    // Open the database
    db.open(NULL, dbFileName.c_str(), NULL, DB_BTREE, DB_CREATE, 0);
}
    // Must catch both DbException and std::exception
    catch(DbException &e)
    {
        db.err(e.get_errno(), "Database open failed %s", 
            dbFileName.c_str());
        throw e;
    }
    catch(std::exception &e)
    {
        // No DB error number available, so use errx
        db.errx("Error opening database: %s", e.what());
        throw e;
    } 
```
