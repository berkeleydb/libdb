---
title: "db_env_set_func_dirlist"
api-name: "db_env_set_func_dirlist"
source: docs/api_reference/C/db_env_set_func_dirlist.html
---
## db_env_set_func_dirlist

``` c
#include <db.h>

int
db_env_set_func_dirlist(int (*func_dirlist)(const char *dir, 
    char ***namesp, int *cntp));  
```

The Berkeley DB library requires the ability to read through a directory and create a list of files that the directory contains.

The db_env_set_func_dirlist method configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the db_env_set_func_dirlist method may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_dirlist()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_dirlist

The **func_dirlist** parameter is the function which reads through a directory and returns a list of the files it contains.

The **dir** parameter to this function is the name of the directory to be searched.

The function must return a pointer to an array of nul-terminated file names into the memory location to which the **namesp** parameter refers, and a count of the number of elements in the array into the memory location to which **cntp** refers.

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
