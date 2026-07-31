---
title: "db_env_set_func_file_map"
api-name: "db_env_set_func_file_map"
source: docs/api_reference/C/db_env_set_func_file_map.html
---
## db_env_set_func_file_map

``` c
#include <db.h>

int
db_env_set_func_file_map(int (*func_file_map)(DB_ENV *dbenv, char *path, 
    size_t len, int is_rdonly, void **addr),
    int (*func_file_unmap)(DB_ENV *dbenv, void *addr));  
```

The Berkeley DB library optionally uses the ability to map a file into memory.

The `db_env_set_func_file_map()` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_file_map()` function may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_file_map()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_file_map

The **func_file_map** parameter is the function which maps a file into memory. The function takes 5 parameters:

- **dbenv**

  The **dbenv** parameter is the enclosing database environment handle.

- **path**

  The **path** parameter is the name of file. Repeated requests for the mapping of the same name should return a reference to the same memory.

- **len**

  The **len** parameter is the length, in bytes, of the file.

- **is_rdonly**

  The **is_rdonly** parameter will be non-zero if the mapped file is read-only.

- **addr**

  The **addr** parameter is the memory location into which a pointer to the mapped file is returned.

The **func_file_map** function must return the value of **errno** on failure and 0 on success.

#### func_file_unmap

The **func_file_unmap** parameter is the function which unmaps a file from memory. The function takes 2 parameters:

- **dbenv**

  The **dbenv** parameter is the enclosing database environment handle.

- `addr`

  The **addr** parameter is the value returned by the **func_file_map** function when the file or region was mapped into memory.

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
