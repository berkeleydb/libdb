---
title: "DB_ENV->set_verbose()"
api-name: "DB_ENV->set_verbose()"
source: docs/api_reference/C/envset_verbose.html
---
## DB_ENV-\>set_verbose()

``` c
#include <db.h>

int
DB_ENV->set_verbose(DB_ENV *dbenv, u_int32_t which, int onoff);  
```

The `DB_ENV->set_verbose()` method turns specific additional informational and debugging messages in the Berkeley DB message output on and off. To see the additional messages, verbose messages must also be configured for the application. For more information on verbose messages, see the <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a> method.

The database environment's messages may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_verbose", one or more whitespace characters, and the method **which** parameter as a string and optionally one or more whitespace characters, and the string "on" or "off". If the optional string is omitted, the default is "on"; for example, "set_verbose DB_VERB_RECOVERY" or "set_verbose DB_VERB_RECOVERY on". Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->set_verbose()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_verbose()` method may be called at any time during the life of the application.

The `DB_ENV->set_verbose()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### which

The **which** parameter must be set to one of the following values:

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

#### onoff

If the **onoff** parameter is set to non-zero, the additional messages are output.

### Errors

The `DB_ENV->set_verbose()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
