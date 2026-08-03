---
title: "db_env_set_func_assert"
api-name: "db_env_set_func_assert"
source: src/dbinc_auto/ext_prot.in
---
## db_env_set_func_assert

``` c
#include <db.h>

int
db_env_set_func_assert(void (*func_assert)(const char *msg, const char *file, int line));  
```

Replace the Berkeley DB call that is invoked when a Berkeley DB assertion fails with **func_assert**. By default, a failed assertion writes a diagnostic message to the error output and aborts the process; **func_assert** lets an application redirect or override that behavior (for example, to log the failure rather than call **abort**).

The **func_assert** function is called with the text of the failed assertion in **msg**, the name of the source file in **file**, and the line number within that file in **line**.

The `db_env_set_func_assert()` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_assert()` function may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_assert()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_assert

The **func_assert** parameter is the replacement function. It is called with the failed assertion text (**msg**), source file name (**file**), and line number (**line**).

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
