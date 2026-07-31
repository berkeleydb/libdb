---
title: "db_env_set_func_open"
api-name: "db_env_set_func_open"
source: docs/api_reference/C/db_env_set_func_open.html
---
## db_env_set_func_open

``` c
#include <db.h>

int
db_env_set_func_open(int (*func_open)(const char *path, int flags, 
    int mode)); 
```

Replace Berkeley DB calls to the IEEE/ANSI Std 1003.1 (POSIX) **open** function with **func_open**, which must conform to the standard interface specification.

The `db_env_set_func_open()` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_open()` function may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_open()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_open

The **func_open** parameter is the replacement function. It must conform to the standard interface specification.

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
