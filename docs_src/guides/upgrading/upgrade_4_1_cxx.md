---
title: "C++ exceptions"
api-name: "C++ exceptions"
source: docs/upgrading/upgrade_4_1_cxx.html
---
## C++ exceptions

With default flags, the C++ <a href="../api_reference/CXX/env.html" class="olink">DbEnv</a> and <a href="../api_reference/CXX/db.html" class="olink">Db</a> classes can throw exceptions from their constructors. For example, this can happen if invalid parameters are passed in or the underlying C structures could not be created. If the objects are created in an environment that is not configured for exceptions (that is, the <a href="../api_reference/CXX/envcreate.html#env_DB_CXX_NO_EXCEPTIONS" class="olink">DB_CXX_NO_EXCEPTIONS</a> flag is specified), errors from the constructor will be returned when the handle's open method is called.

In addition, the behavior of the <a href="../api_reference/CXX/env.html" class="olink">DbEnv</a> and <a href="../api_reference/CXX/db.html" class="olink">Db</a> destructors has changed to simplify exception handling in applications. The destructors will now close the handle if the handle's close method was not called prior to the object being destroyed. The return value of the call is discarded, and no exceptions will be thrown. Applications should call the close method in normal situations so any errors while closing can be handled by the application.

This change allows applications to be structured as follows:

``` c
try {
    DbEnv env(0);
    env.open(/* ... */);
    Db db(&env, 0);
    db.open(/* ... */);
    /* ... */
    db.close(0);
    env.close(0);
} catch (DbException &dbe) {
    // Handle the exception, the handles have already been closed.
}
```
