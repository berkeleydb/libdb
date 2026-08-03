---
title: "DB_ENV->get_verbose()"
api-name: "DB_ENV->get_verbose()"
source: docs/api_reference/C/envget_verbose.html
---
## DB_ENV-\>get_verbose()

``` c
#include <db.h>

int
DB_ENV->get_verbose(DB_ENV *dbenv, u_int32_t which, int *onoffp);  
```

The `DB_ENV->get_verbose()` method returns whether the specified **which** parameter is currently set or not. These parameters are set using the <a href="envset_verbose.md" class="xref" title="DB_ENV-&gt;set_verbose()">DB_ENV-&gt;set_verbose()</a> method.

The `DB_ENV->get_verbose()` method may be called at any time during the life of the application.

The `DB_ENV->get_verbose()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### which

The **which** parameter is the message value for which configuration is being checked. Must be set to one of the following values:

- `DB_VERB_DEADLOCK`

  Display additional information when doing deadlock detection.

- `DB_VERB_FILEOPS`

  Display additional information when performing filesystem operations such as open, close or rename. May not be available on all platforms.

- `DB_VERB_FILEOPS_ALL`

  Display additional information when performing all filesystem operations, including read and write. May not be available on all platforms.

- `DB_VERB_RECOVERY`

  Display additional information when performing recovery.

- `DB_VERB_REGISTER`

  Display additional information concerning support for the <a href="envopen.md#envopen_DB_REGISTER" class="link">DB_REGISTER</a> flag to the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.

- `DB_VERB_REPLICATION`

  Display all detailed information about replication. This includes the information displayed by all of the other DB_VERB_REP\_\* and DB_VERB_REPMGR\_\* values.

- `DB_VERB_REP_ELECT`

  Display detailed information about replication elections.

- `DB_VERB_REP_LEASE`

  Display detailed information about replication master leases.

- `DB_VERB_REP_MISC`

  Display detailed information about general replication processing not covered by the other DB_VERB_REP\_\* values.

- `DB_VERB_REP_MSGS`

  Display detailed information about replication message processing.

- `DB_VERB_REP_SYNC`

  Display detailed information about replication client synchronization.

- `DB_VERB_REP_SYSTEM`

  Saves replication system information to a system-owned file. This value is on by default.

- `DB_VERB_REPMGR_CONNFAIL`

  Display detailed information about Replication Manager connection failures.

- `DB_VERB_REPMGR_MISC`

  Display detailed information about general Replication Manager processing.

- `DB_VERB_WAITSFOR`

  Display the waits-for table when doing deadlock detection.

#### onoffp

The **onoffp** parameter references memory into which the configuration of the specified **which** parameter is copied.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
