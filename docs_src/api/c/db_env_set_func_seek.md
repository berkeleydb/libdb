---
title: "db_env_set_func_seek"
api-name: "db_env_set_func_seek"
source: docs/api_reference/C/db_env_set_func_seek.html
---
## db_env_set_func_seek

``` c
#include <db.h>

int
db_env_set_func_seek(int (*func_seek)(int fd, off_t offset, int whence)); 
```

The Berkeley DB library requires the ability to specify that a subsequent read from or write to a file will occur at a specific location in that file.

The `db_env_set_func_seek()` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_seek()` function may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_seek()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_seek

The **func_seek** parameter is the function which seeks to a specific location in a file.

The **fd** parameter is an open file descriptor on the file.

The **seek** function must cause a subsequent read from or write to the file to occur at the byte offset specified by the **offset** parameter.

The **whence** parameter specifies where in the file the byte offset is relative to, as described by the IEEE/ANSI Std 1003.1 (POSIX) **lseek** system call.

The **func_seek** function must return the value of **errno** on failure and 0 on success.

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
