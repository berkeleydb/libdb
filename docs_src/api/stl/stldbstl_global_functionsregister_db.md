---
title: "register_db"
api-name: "register_db"
source: docs/api_reference/STL/stldbstl_global_functionsregister_db.html
---
## register_db

### Function Details

``` c
 void register_db(Db *pdb1)
 
```

Register a Db handle "pdb1".

This handle and handles opened in it will be closed by ResourceManager , so application code must not try to close or delete it. Users can do enough configuration before opening the Db then register it via this function. All database handles should be registered via this function in each thread using the handle. The only exception is the database handle opened by <a href="stldbstl_global_functionsopen_db.md" class="link" title="open_db">dbstl::open_db</a> should not be registered in the thread of the <a href="stldbstl_global_functionsopen_db.md" class="link" title="open_db">dbstl::open_db</a> call.

#### Parameters

##### pdb1

The database handle to register into dbstl for current thread.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
