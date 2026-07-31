---
title: "db_env_set_func_dirfree"
api-name: "db_env_set_func_dirfree"
source: docs/api_reference/C/db_env_set_func_dirfree.html
---
## db_env_set_func_dirfree

``` c
#include <db.h>

int
db_env_set_func_dirfree(void (*func_dirfree)(char **namesp, int cnt));  
```

The Berkeley DB library requires the ability to return any memory allocated as part of the routine which reads through a directory and creates a list of files that the directory contains (see <a href="db_env_set_func_dirlist.md" class="xref" title="db_env_set_func_dirlist">db_env_set_func_dirlist</a>).

The `db_env_set_func_dirfree()` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_dirfree()` function may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_dirfree()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_dirfree

The **func_dirfree** parameter is a function which frees the memory returned from the <a href="db_env_set_func_dirlist.md" class="xref" title="db_env_set_func_dirlist">db_env_set_func_dirlist</a> function.

The **namesp** and **cnt** parameters to this function are the same values as were returned by the <a href="db_env_set_func_dirlist.md" class="xref" title="db_env_set_func_dirlist">db_env_set_func_dirlist</a> function.

### See Also

<a href="../../programmer_reference/program_runtime.html" class="olink">Run-time configuration</a>
