---
title: "db_env_set_win_security"
api-name: "db_env_set_win_security"
source: src/dbinc/globals.h
---
## db_env_set_win_security

``` c
#include <db.h>

int
db_env_set_win_security(SECURITY_ATTRIBUTES *sa);  
```

Set the Windows security attributes used by Berkeley DB when it creates the operating-system objects (shared memory and mutexes) that back a database environment. This interface is Windows-specific and has no effect on other platforms.

On Windows, the objects Berkeley DB creates to implement mutexes are normally initialized by the first Berkeley DB API call that locks a mutex, using the process's default security attributes. If those defaults would make the objects inaccessible to other threads or processes that must share the environment (for example, ones running with lesser privileges), the application may call `db_env_set_win_security()` first to supply an explicit **SECURITY_ATTRIBUTES** structure.

The `db_env_set_win_security()` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

The `db_env_set_win_security()` function must be called before the first Berkeley DB API call that locks a mutex — normally before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_win_security()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### sa

The **sa** parameter is a pointer to a Windows **SECURITY_ATTRIBUTES** structure that Berkeley DB applies to the operating-system objects it creates.

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
