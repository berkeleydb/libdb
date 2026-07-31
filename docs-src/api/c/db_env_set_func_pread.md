---
title: "db_env_set_func_pread"
api-name: "db_env_set_func_pread"
source: docs/api_reference/C/db_env_set_func_pread.html
---
## db_env_set_func_pread

``` c
#include <db.h>

int
db_env_set_func_pread(ssize_t (*func_pread)(int fd, void *buf, 
    size_t nbytes, off_t offset));  
```

Replace Berkeley DB calls to the IEEE/ANSI Std 1003.1 (POSIX) **pread** function with **func_pread**, which must conform to the standard interface specification.

The `db_env_set_func_pread()` configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_pread()` may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_pread()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_pread

The **func_pread** parameter is the replacement function. It must conform to the standard interface specification.

### See Also

<a href="../../programmer_reference/program_runtime.html" class="olink">Run-time configuration</a>
