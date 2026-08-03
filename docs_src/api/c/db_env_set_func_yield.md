---
title: "db_env_set_func_yield"
api-name: "db_env_set_func_yield"
source: docs/api_reference/C/db_env_set_func_yield.html
---
## db_env_set_func_yield

``` c
#include <db.h>

int
db_env_set_func_yield(int (*func_yield)(u_long secs, u_long usecs));  
```

The Berkeley DB library requires the ability to yield the processor from the current thread of control to any other waiting threads of control.

The **func_yield** function must be able to cause the rescheduling of all participants in the current Berkeley DB environment, whether threaded or not. It may be incorrect to supply a thread **yield** function if more than a single process is operating in the Berkeley DB environment. This is because many thread-yield functions will not allow other processes to run, and the contested lock may be held by another process, not by another thread.

The `db_env_set_func_yield()` function configures all operations performed by a process and all of its threads of control, not operations confined to a single database environment.

Although the `db_env_set_func_yield()` function may be called at any time during the life of the application, it should normally be called before making calls to the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> or <a href="dbcreate.md" class="xref" title="db_create">db_create</a> methods.

The `db_env_set_func_yield()` function returns a non-zero error value on failure and 0 on success.

### Parameters

#### func_yield

The **func_yield** parameter is the function which yields the processor.

The **secs** parameter is the number of seconds to pause before the thread of control should run again, or 0.

The **usecs** parameter is the number of microseconds to pause before the thread of control should run again, or 0.

The **func_yield** function must return the value of **errno** on failure and 0 on success.

### See Also

<a href="../../guides/programmer_reference/program_runtime.md" class="olink">Run-time configuration</a>
