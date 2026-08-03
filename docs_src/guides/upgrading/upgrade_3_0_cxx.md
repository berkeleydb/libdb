---
title: "additional C++ changes"
api-name: "additional C++ changes"
source: docs/upgrading/upgrade_3_0_cxx.html
---
## additional C++ changes

The Db::set_error_model method is gone. The way to change the C++ API to return errors rather than throw exceptions is via a flag on the DbEnv or Db constructor. For example:

``` c
int dberr;
DbEnv *dbenv = new DbEnv(DB_CXX_NO_EXCEPTIONS);
```

creates an environment that will never throw exceptions, and method returns should be checked instead.

There are a number of smaller changes to the API that bring the C, C++ and Java APIs much closer in terms of functionality and usage. Please refer to the pages for upgrading C applications for further details.
