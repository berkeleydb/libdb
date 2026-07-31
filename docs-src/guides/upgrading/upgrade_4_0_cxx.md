---
title: "C++ ostream objects"
api-name: "C++ ostream objects"
source: docs/upgrading/upgrade_4_0_cxx.html
---
## C++ ostream objects

In the 4.0 release, the Berkeley DB C++ API has been changed to use the ISO standard C++ API in preference to the older, less portable interfaces, where available. This means the Berkeley DB methods that used to take an ostream object as a parameter now expect a std::ostream. Specifically, the following methods have changed:

``` c
DbEnv::set_error_stream
Db::set_error_stream
Db::verify
```

On many platforms, the old and the new C++ styles are interchangeable; on some platforms (notably Windows systems), they are incompatible. If your code uses these methods and you have trouble with the 4.0 release, you should update code that looks like this:

``` c
#include <iostream.h>
#include <db_cxx.h>

void foo(Db db) {
    db.set_error_stream(&cerr);
}
```

to look like this:

``` c
#include <iostream>
#include <db_cxx.h>

using std::cerr;

void foo(Db db) {
    db.set_error_stream(&cerr);
}
```
