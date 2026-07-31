---
title: "db_env_set_func_fsync"
api-name: "db_env_set_func_fsync"
source: docs/api_reference/C/db_env_set_func_fsync.html
---
## db_env_set_func_fsync

``` c
#include <db.h>

int
db_env_set_func_fsync(int (*func_fsync)(int fd));  
```

Replace Berkeley DB calls to the IEEE/ANSI Std 1003.1 (POSIX) **fsync** function with **func_fsync**, which must conform to the standard interface specification.

The `db_env_set_func_fsync()` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_fsync()` function may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_fsync` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_fsync

The **func_fsync** parameter is the replacement function. It must conform to the standard interface specification.

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
