---
title: "Exception Handling"
api-name: "Exception Handling"
source: docs/gsg/CXX/coreExceptions.html
---
## Exception Handling

Before continuing, it is useful to spend a few moments on exception handling in DB with the C++ API.

By default, most DB methods throw `DbException` in the event of a serious error.

You can obtain the DB error number for a `DbException` by using `DbException::get_errno()`. You can also obtain the informational message associated with that error number using `DbException::what()`.

If for some reason you do not want to manage `DbException` objects in your `try` blocks, you can configure DB to suppress them by setting `DB_CXX_NO_EXCEPTIONS` for your database and environment handles. In this event, you must manage your DB error conditions using the integer value returned by all DB methods. Be aware that this manual assumes that you want to manage your error conditions using `DbException` objects.
