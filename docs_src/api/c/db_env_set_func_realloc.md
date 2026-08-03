---
title: "db_env_set_func_realloc"
api-name: "db_env_set_func_realloc"
source: docs/api_reference/C/db_env_set_func_realloc.html
---
## db_env_set_func_realloc

``` c
#include <db.h>

int
db_env_set_func_realloc(void *(*func_realloc)(void *ptr, size_t size)); 
```

Replace Berkeley DB calls to the ANSI C X3.159-1989 (ANSI C) standard **realloc** function with **func_realloc**, which must conform to the standard interface specification.

The `db_env_set_func_realloc()` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_realloc()` function may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_realloc()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_realloc

The **func_realloc** parameter is the replacement function. It must conform to the standard interface specification.

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
