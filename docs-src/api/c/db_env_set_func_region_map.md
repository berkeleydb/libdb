---
title: "db_env_set_func_region_map"
api-name: "db_env_set_func_region_map"
source: docs/api_reference/C/db_env_set_func_region_map.html
---
## db_env_set_func_region_map

``` c
#include <db.h>

int
db_env_set_func_region_map(int (*func_region_map)(DB_ENV *dbenv, 
    char *path, size_t len, int *is_create, void **addr),
    int (*func_region_unmap)(DB_ENV *dbenv, void *addr));  
```

The Berkeley DB library optionally uses the ability to create shared memory regions (which may or may not be backed by physical files). The memory will be used as a shared memory region for synchronization between Berkeley DB threads/processes; while the returned memory may be of any kind (for example, anonymous memory), it must be able to support semaphores.

The `db_env_set_func_region_map()` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_region_map()` function may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_region_map()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_region_map

The **func_region_map** parameter is the function which creates shared memory regions. The function takes 5 parameters:

- **dbenv**

  The **dbenv** parameter is the enclosing database environment handle. This handle is provided to uniquely identify a shared memory region: the **dbenv** parameter and the path are a unique identifier pair for mapping any new region, and the **dbenv** parameter and the address are a unique identifier pair for unmapping any region.

- **path**

  The **path** parameter is the name of the region. Repeated requests for the shared regions of the same name, in the same database environment, should return a reference to the same memory.

- **len**

  The **len** parameter is the length, in bytes, needed for the region.

- **is_create**

  The memory referenced by the **is_create** parameter will be non-zero if flags to Berkeley DB allowed creation of the mapped region; the memory referenced by the **is_create** parameter must be set to non-zero if the region is created by the **func_region_map** function, and set to zero if the region is not created by the function. This returned information will determine if the region is subsequently initialized by Berkeley DB.

- **addr**

  The **addr** parameter is the memory location into which a pointer to the region or mapped file is returned.

#### func_region_unmap

The **func_region_unmap** parameter is the function which unmaps a shared memory region. The function takes 2 parameters:

- **dbenv**

  The **dbenv** parameter is the enclosing database environment handle.

- **addr**

  The **addr** parameter is the value returned by the **func_region_map** function when the region was mapped into memory.

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
