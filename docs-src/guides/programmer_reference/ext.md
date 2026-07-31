---
title: "Chapter 22.  Berkeley DB Extensions"
api-name: "Chapter 22.  Berkeley DB Extensions"
source: docs/programmer_reference/ext.html
---
## Chapter 22.  Berkeley DB Extensions

**Table of Contents**

<span class="sect1"> [Using Berkeley DB with Apache](ext.md#ext_mod) </span>

<span class="sect1"> [Using Berkeley DB with Perl](ext_perl.md) </span>

<span class="sect1"> [Using Berkeley DB with PHP](ext_php.md) </span>

## Using Berkeley DB with Apache

A mod_db4 Apache module is included in the Berkeley DB distribution, providing a safe framework for running Berkeley DB applications in an Apache 1.3 environment. Apache natively provides no interface for communication between threads or processes, so the mod_db4 module exists to provide this communication.

In general, it is dangerous to run Berkeley DB in a multiprocess system without some facility to coordinate database recovery between processes sharing the database environment after application or system failure. Failure to run recovery after failure can include process hangs and an inability to access the database environment. The mod_db4 Apache module oversees the proper management of Berkeley DB database environment resources. Developers building applications using Berkeley DB as the storage manager within an Apache module should employ this technique for proper resource management.

Specifically, mod_db4 provides the following facilities:

1.  New constructors for <a href="../../api/c/env.md" class="olink">DB_ENV</a> and <a href="../../api/c/db.md" class="olink">DB</a> handles, which install replacement open/close methods.
2.  Transparent caching of open <a href="../../api/c/env.md" class="olink">DB_ENV</a> and <a href="../../api/c/db.md" class="olink">DB</a> handles.
3.  Reference counting on all structures, allowing the module to detect the initial opening of any managed database and automatically perform recovery.
4.  Automatic detection of unexpected failures (segfaults, or a module actually calling exit() and avoiding shut down phases), and automatic termination of all child processes with open database resources to attempt consistency.

mod_db4 is designed to be used as an alternative interface to Berkeley DB. To have another Apache module (for example, mod_foo) use mod_db4, do not link mod_foo against the Berkeley DB library. In your mod_foo makefile, you should:

``` c
#include "mod_db4_export.h"
```

and add your Apache include directory to your CPPFLAGS.

In mod_foo, to create a mod_db4 managed <a href="../../api/c/env.md" class="olink">DB_ENV</a> handle, use the following:

``` c
int mod_db4_db_env_create(DB_ENV **dbenvp, u_int32_t flags);
```

which takes identical arguments to <a href="../../api/c/envcreate.md" class="olink">db_env_create()</a>.

To create a mod_db4 managed <a href="../../api/c/db.md" class="olink">DB</a> handle, use the following:

``` c
int mod_db4_db_create(DB **dbp, DB_ENV *dbenv, u_int32_t flags);
```

which takes identical arguments to <a href="../../api/c/dbcreate.md" class="olink">db_create()</a>.

Otherwise the API is completely consistent with the standard Berkeley DB API.

The mod_db4 module requires the Berkeley DB library be compiled with C++ extensions and the MM library. (The MM library provides an abstraction layer which allows related processes to share data easily. On systems where shared memory or other inter-process communication mechanisms are not available, the MM library emulates them using temporary files. MM is used in several operating systems to provide shared memory pools to Apache modules.)

To build this apache module, perform the following steps:

``` c
% ./configure --with-apxs=[path to the apxs utility] \
    --with-db4=[Berkeley DB library installation directory] \
    --with-mm=[libmm installation directory]
% make
% make install
```

Post-installation, modules can use this extension via the functions documented in \$APACHE_INCLUDEDIR/mod_db4_export.h.
