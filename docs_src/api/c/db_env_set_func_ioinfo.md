---
title: "db_env_set_func_ioinfo"
api-name: "db_env_set_func_ioinfo"
source: docs/api_reference/C/db_env_set_func_ioinfo.html
---
## db_env_set_func_ioinfo

``` c
#include <db.h>

int
db_env_set_func_ioinfo(int (*func_ioinfo)(const char *path,
    int fd, u_int32_t *mbytesp, u_int32_t *bytesp, u_int32_t *iosizep)); 
```

The Berkeley DB library requires the ability to determine the size and I/O characteristics of a file.

The `db_env_set_func_ioinfo()` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_ioinfo()` function may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_ioinfo()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_ioinfo

The **func_ioinfo** parameter is the function which returns the size and I/O characteristics of a file.

The **path** parameter is the pathname of the file to be checked, and the **fd** parameter is an open file descriptor on the file.

If the **mbytesp** and **bytesp** parameters are non-NULL, the **ioinfo** function must return in them the size of the file: the number of megabytes in the file into the memory location to which the **mbytesp** parameter refers, and the number of bytes over and above that number of megabytes into the memory location to which the **bytesp** parameter refers.

In addition, if the **iosizep** parameter is non-NULL, the **ioinfo** function must return the optimum granularity for I/O operations to the file into the memory location to which it refers.

The **func_ioinfo** function must return the value of **errno** on failure and 0 on success.

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
