---
title: "Programmatic APIs"
api-name: "Programmatic APIs"
source: docs/programmer_reference/arch_apis.html
---
## Programmatic APIs

<span class="sect2"> [C](arch_apis.md#idp51640232) </span>

<span class="sect2"> [C++](arch_apis.md#idp51656168) </span>

<span class="sect2"> [STL](arch_apis.md#idp51646944) </span>

<span class="sect2"> [Java](arch_apis.md#idp51647760) </span>

<span class="sect2"> [Dbm/Ndbm, Hsearch](arch_apis.md#idp51664896) </span>

The Berkeley DB subsystems can be accessed through interfaces from multiple languages. Applications can use Berkeley DB via C, C++ or Java, as well as a variety of scripting languages such as Perl, Python, Ruby or Tcl. Environments can be shared among applications written by using any of these interfaces. For example, you might have a local server written in C or C++, a script for an administrator written in Perl or Tcl, and a Web-based user interface written in Java -- all sharing a single database environment.

### C

The Berkeley DB library is written entirely in ANSI C. C applications use a single include file:

``` c
#include <db.h>
```

### C++

The C++ classes provide a thin wrapper around the C API, with the major advantages being improved encapsulation and an optional exception mechanism for errors. C++ applications use a single include file:

``` c
#include <db_cxx.h>
```

The classes and methods are named in a fashion that directly corresponds to structures and functions in the C interface. Likewise, arguments to methods appear in the same order as the C interface, except to remove the explicit **this** pointer. The \#defines used for flags are identical between the C and C++ interfaces.

As a rule, each C++ object has exactly one structure from the underlying C API associated with it. The C structure is allocated with each constructor call and deallocated with each destructor call. Thus, the rules the user needs to follow in allocating and deallocating structures are the same between the C and C++ interfaces.

To ensure portability to many platforms, both new and old, Berkeley DB makes as few assumptions as possible about the C++ compiler and library. For example, it does not expect STL, templates, or namespaces to be available. The newest C++ feature used is exceptions, which are used liberally to transmit error information. Even the use of exceptions can be disabled at runtime.

### STL

dbstl is an C++ STL style API for Berkeley DB, based on the C++ API above. With it, you can store data/objects of any type into or retrieve them from Berkeley DB databases as if you are using C++ STL containers. The full functionality of Berkeley DB can still be utilized via dbstl with little performance overhead, e.g. you can use all transaction and/or replication functionality of Berkeley DB.

dbstl container/iterator class templates reside in header files dbstl_vector.h, dbstl_map.h and dbstl_set.h. Among them, dbstl_vector.h contains dbstl::db_vector and its iterators; dbstl_map.h contains dbstl::db_map, dbstl::db_multimap and their iterators; dbstl_set.h contains dbstl::db_set and dbstl::db_multiset and their iterators. You should include needed header file(s) to use the container/iterator. Note that we don't use the file name with no extention --- To use dbstl::db_vector, you should do this:

``` c
#include "dbstl_vector.h"
```

rather than this:

``` c
#include "dbstl_vector"
```

And these header files reside in "stl" directory inside Berkeley DB source root directory. If you have installed Berkeley DB, they are also available in the "include" directory in the directory where Berkeley DB is installed.

Apart from the above three header files, you may also need to include db_exception.h and db_utility.h files. The db_exception.h file contains all exception classes of dbstl, which integrate seamlessly with Berkeley DB C++ API exceptions and C++ standard exception classes in std namespace. And the db_utility.h file contains the DbstlElemTraits which helps you to store complex objects. These five header files are all that you need to include in order to make use of dbstl.

All symbols of dbstl, including classes, class templates, global functions, etc, reside in the namespace "dbstl", so in order to use them, you may also want to do this:

``` c
using namespace dbstl;
```

The dbstl library is always at the same place where Berkeley DB library is located, you will need to build it and link with it to use dbstl.

While making use of dbstl, you will probably want to create environment or databases directly, or set/get configurations to Berkeley DB environment or databases, etc. You are allowed to do so via Berkeley DB C/C++ API.

### Java

The Java classes provide a layer around the C API that is almost identical to the C++ layer. The classes and methods are, for the most part identical to the C++ layer. Berkeley DB constants and \#defines are represented as "static final int" values. Error conditions are communicated as Java exceptions.

As in C++, each Java object has exactly one structure from the underlying C API associated with it. The Java structure is allocated with each constructor or open call, but is deallocated only by the Java garbage collector. Because the timing of garbage collection is not predictable, applications should take care to do a close when finished with any object that has a close method.

### Dbm/Ndbm, Hsearch

Berkeley DB supports the standard UNIX <a href="../../api/c/dbm.md" class="olink">dbm</a> and <a href="../../api/c/hsearch.md" class="olink">hsearch</a> interfaces. After including a new header file and recompiling, programs will run orders of magnitude faster, and underlying databases can grow as large as necessary. Also, historic <a href="../../api/c/dbm.md" class="olink">dbm</a> applications can fail once some number of entries are inserted into the database, in which the number depends on the effectiveness of the internal hashing function on the particular data set. This is not a problem with Berkeley DB.
