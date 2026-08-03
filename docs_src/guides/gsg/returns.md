---
title: "Error Returns"
api-name: "Error Returns"
source: docs/gsg/C/returns.html
---
## Error Returns

Before continuing, it is useful to spend a few moments on error returns in DB.

The DB interfaces always return a value of 0 on success. If the operation does not succeed for any reason, the return value will be non-zero.

If a system error occurred (for example, DB ran out of disk space, or permission to access a file was denied, or an illegal argument was specified to one of the interfaces), DB returns an `errno` value. All of the possible values of `errno` are greater than 0.

If the operation did not fail due to a system error, but was not successful either, DB returns a special error value. For example, if you tried to retrieve data from the database and the record for which you are searching does not exist, DB would return `DB_NOTFOUND`, a special error value that means the requested key does not appear in the database. All of the possible special error values are less than 0.

DB also offers programmatic support for displaying error return values. First, the `db_strerror` function returns a pointer to the error message corresponding to any DB error return, similar to the ANSI C `strerror` function, but is able to handle both system error returns and DB-specific return values.

Second, there are two error functions, `DB->err` and `DB->errx`. These functions work like the ANSI C `printf` function, taking a printf-style format string and argument list, and optionally appending the standard error string to a message constructed from the format string and other arguments.
