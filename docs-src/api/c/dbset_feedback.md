---
title: "DB->set_feedback()"
api-name: "DB->set_feedback()"
source: docs/api_reference/C/dbset_feedback.html
---
## DB-\>set_feedback()

``` c
#include <db.h>

int
DB->set_feedback(DB *,
    void (*db_feedback_fcn)(DB *dbp, int opcode, int percent));  
```

Some operations performed by the Berkeley DB library can take non-trivial amounts of time. The `DB->set_feedback()` method can be used by applications to monitor progress within these operations. When an operation is likely to take a long time, Berkeley DB will call the specified callback function with progress information.

It is up to the callback function to display this information in an appropriate manner.

The `DB->set_feedback()` method may be called at any time during the life of the application.

The `DB->set_feedback()` method returns a non-zero error value on failure and 0 on success.

### Note

Berkeley DB is not re-entrant. Callback functions should not attempt to make library calls (for example, to release locks or close open handles). Re-entering Berkeley DB is not guaranteed to work correctly, and the results are undefined.

### Parameters

#### db_feedback_fcn

The **db_feedback_fcn** parameter is the application-specified feedback function called to report Berkeley DB operation progress. The callback function must take three parameters:

- `dbp`

  The **dbp** parameter is a reference to the enclosing database.

- `opcode`

  The **opcode** parameter is an operation code. The **opcode** parameter may take on any of the following values:

  - `DB_UPGRADE`

    The underlying database is being upgraded.

  - `DB_VERIFY`

    The underlying database is being verified.

- `percent`

  The **percent** parameter is the percent of the operation that has been completed, specified as an integer value between 0 and 100.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
