---
title: "DB->set_h_hash()"
api-name: "DB->set_h_hash()"
source: docs/api_reference/C/dbset_h_hash.html
---
## DB-\>set_h_hash()

``` c
#include <db.h>

int
DB->set_h_hash(DB *db,
    u_int32_t (*h_hash_fcn)(DB *dbp, const void *bytes, 
    u_int32_t length));  
```

Set a user-defined hash function; if no hash function is specified, a default hash function is used. Because no hash function performs equally well on all possible data, the user may find that the built-in hash function performs poorly with a particular data set.

The `DB->set_h_hash()` method configures operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle, not all operations performed on the underlying database.

The `DB->set_h_hash()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_h_hash()` must be the same as that historically used to create the database or corruption can occur.

The `DB->set_h_hash()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### h_hash_fcn

The **h_hash_fcn** parameter is the application-specified hash function.

Application-specified hash functions take a pointer to a byte string and a length as parameters, and return a value of type **u_int32_t**. The hash function must handle any key values used by the application (possibly including zero-length keys).

### Errors

The `DB->set_h_hash()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
