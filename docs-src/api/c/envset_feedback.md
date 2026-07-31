---
title: "DB_ENV->set_feedback()"
api-name: "DB_ENV->set_feedback()"
source: docs/api_reference/C/envset_feedback.html
---
## DB_ENV-\>set_feedback()

``` c
#include <db.h>

int
DB_ENV->set_feedback(DB_ENV *dbenv,
    void (*db_feedback_fcn)(DB_ENV *dbenv, int opcode, int percent));  
```

Some operations performed by the Berkeley DB library can take non-trivial amounts of time. The `DB_ENV->set_feedback()` method can be used by applications to monitor progress within these operations. When an operation is likely to take a long time, Berkeley DB will call the specified callback function with progress information.

### Note

Berkeley DB is not re-entrant. Callback functions should not attempt to make library calls (for example, to release locks or close open handles). Re-entering Berkeley DB is not guaranteed to work correctly, and the results are undefined.

It is up to the callback function to display this information in an appropriate manner.

The `DB_ENV->set_feedback()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->set_feedback()` method may be called at any time during the life of the application.

The `DB_ENV->set_feedback()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### db_feedback_fcn

The **db_feedback_fcn** parameter is the application-specified feedback function called to report Berkeley DB operation progress. The callback function must take three parameters:

- `dbenv`

  The **dbenv** parameter is a reference to the enclosing database environment.

- `opcode`

  The **opcode** parameter is an operation code. The **opcode** parameter may take on any of the following values:

  - `DB_RECOVER`

    The environment is being recovered.

- `percent`

  The **percent** parameter is the percent of the operation that has been completed, specified as an integer value between 0 and 100.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
