---
title: "set_global_dbfile_suffix_number"
api-name: "set_global_dbfile_suffix_number"
source: docs/api_reference/STL/stldbstl_global_functionsset_global_dbfile_suffix_number.html
---
## set_global_dbfile_suffix_number

### Function Details

``` c
 void set_global_dbfile_suffix_number(u_int32_t num)
 
```

If exisiting random temporary database name generation mechanism is still causing name clashes, users can set this global suffix number which will be append to each temporary database file name and incremented after each append, and by default it is 0.

#### Parameters

##### num

Starting number to append to each temporary db file name.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
