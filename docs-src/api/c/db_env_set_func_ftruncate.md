---
title: "db_env_set_func_ftruncate"
api-name: "db_env_set_func_ftruncate"
source: docs/api_reference/C/db_env_set_func_ftruncate.html
---
## db_env_set_func_ftruncate

``` c
#include <db.h>

int
db_env_set_func_ftruncate(int (*func_ftruncate)(int fd, off_t offset)); 
```

The Berkeley DB library requires the ability to truncate a file.

The `db_env_set_func_ftruncate` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_ftruncate` function may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_ftruncate()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_ftruncate

The **func_ftruncate** parameter is the function which truncates a file.

The **fd** parameter is an open file descriptor on the file.

The **ftruncate** function must truncate the file to the byte length specified by the **offset** parameter.

The **func_ftruncate** function must return the value of **errno** on failure and 0 on success.

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
