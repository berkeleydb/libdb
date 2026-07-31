---
title: "db_env_set_func_exists"
api-name: "db_env_set_func_exists"
source: docs/api_reference/C/db_env_set_func_exists.html
---
## db_env_set_func_exists

``` c
#include <db.h>

int
db_env_set_func_exists(int (*func_exists)(const char *path, 
                       int *isdirp)); 
```

The Berkeley DB library requires the ability to determine whether a file exists and whether it is a file of type directory.

The `db_env_set_func_exists()` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_exists()` function may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_exists()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_exists

The **func_exists** parameter is the function which returns if a file exists and if it is a file of type directory.

The **path** parameter to this function is the pathname of the file to be checked.

If the **isdirp** parameter is non-NULL, it must be set to non-0 if **path** is a directory, and 0 if **path** is not a directory.

The **func_exists** function must return the value of **errno** on failure and 0 on success.

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
