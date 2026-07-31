---
title: "db_env_set_func_pwrite"
api-name: "db_env_set_func_pwrite"
source: docs/api_reference/C/db_env_set_func_pwrite.html
---
## db_env_set_func_pwrite

``` c
#include <db.h>

int
db_env_set_func_pwrite(ssize_t (*func_pwrite)(int fd, const void *buf, 
    size_t nbytes, off_t offset));  
```

Replace Berkeley DB calls to the IEEE/ANSI Std 1003.1 (POSIX) **pwrite** function with **func_pwrite**, which must conform to the standard interface specification.

The `db_env_set_func_pwrite()` configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_pwrite()` may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_pwrite()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_pwrite

The **func_pwrite** parameter is the replacement function. It must conform to the standard interface specification.

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
