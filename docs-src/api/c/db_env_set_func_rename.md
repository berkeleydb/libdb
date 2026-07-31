---
title: "db_env_set_func_rename"
api-name: "db_env_set_func_rename"
source: docs/api_reference/C/db_env_set_func_rename.html
---
## db_env_set_func_rename

``` c
#include <db.h>

int
db_env_set_func_rename(int (*func_rename)(const char *from, 
    const char *to));  
```

Replace Berkeley DB calls to the IEEE/ANSI Std 1003.1 (POSIX) **rename** function with **func_rename**, which must conform to the standard interface specification.

The `db_env_set_func_rename()` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_rename()` function may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_rename()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_rename

The **func_rename** parameter is the replacement function. It must conform to the standard interface specification.

### See Also

<a href="../../programmer_reference/program_runtime.html" class="olink">Run-time configuration</a>
