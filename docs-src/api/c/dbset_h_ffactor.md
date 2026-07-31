---
title: "DB->set_h_ffactor()"
api-name: "DB->set_h_ffactor()"
source: docs/api_reference/C/dbset_h_ffactor.html
---
## DB-\>set_h_ffactor()

``` c
#include <db.h>

int
DB->set_h_ffactor(DB *db, u_int32_t h_ffactor);  
```

Set the desired density within the hash table. If no value is specified, the fill factor will be selected dynamically as pages are filled.

The density is an approximation of the number of keys allowed to accumulate in any one bucket, determining when the hash table grows or shrinks. If you know the average sizes of the keys and data in your data set, setting the fill factor can enhance performance. A reasonable rule computing fill factor is to set it to the following:

``` c
             (pagesize - 32) / (average_key_size + average_data_size + 8) 
```

The `DB->set_h_ffactor()` method configures a database, not only operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle.

The `DB->set_h_ffactor()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_h_ffactor()` will be ignored.

The `DB->set_h_ffactor()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### h_ffactor

The **h_ffactor** parameter is the desired density within the hash table.

### Errors

The `DB->set_h_ffactor()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
