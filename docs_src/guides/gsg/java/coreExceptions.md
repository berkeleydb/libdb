---
title: "Exception Handling"
api-name: "Exception Handling"
source: docs/gsg/JAVA/coreExceptions.html
---
## Exception Handling

Before continuing, it is useful to spend a few moments on exception handling in DB with the java.

Most DB methods throw `DatabaseException` in the event of a serious error. So your DB code must either catch this exception or declare it to be throwable. Be aware that `DatabaseException` extends `java.lang.Exception`. For example:

``` c
import com.sleepycat.db.DatabaseException;

    ...
try 
{
    // DB and other code goes here
}
catch(DatabaseException e)
{
  // DB error handling goes here
} 
```

You can obtain the DB error number for a `DatabaseException` by using `DatabaseException.getErrno()`. You can also obtain any error message associated with that error using `DatabaseException.getMessage()`.
