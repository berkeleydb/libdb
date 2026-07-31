---
title: "DB_ENV->set_timeout()"
api-name: "DB_ENV->set_timeout()"
source: docs/api_reference/C/envset_timeout.html
---
## DB_ENV-\>set_timeout()

``` c
#include <db.h>

int
DB_ENV->set_timeout(DB_ENV *dbenv, db_timeout_t timeout, 
    u_int32_t flags);  
```

The `DB_ENV->set_timeout()` method sets timeout values for locks or transactions in the database environment, and the wait time for a process to exit the environment when <a href="envopen.md#envopen_DB_REGISTER" class="link">DB_REGISTER</a> recovery is needed.

DB_SET_LOCK_TIMEOUT and DB_SET_TXN_TIMEOUT timeouts are checked whenever a thread of control blocks on a lock or when deadlock detection is performed. In the case of DB_SET_LOCK_TIMEOUT, the lock is one requested explicitly through the Lock subsystem interfaces. In the case of DB_SET_TXN_TIMEOUT, the lock is one requested on behalf of a transaction. In either case, it may be a lock requested by the database access methods underlying the application. These timeouts are only checked when the lock request first blocks or when deadlock detection is performed, the accuracy of the timeout depends on how often deadlock detection is performed.

Lock and transaction timeout values specified for the database environment may be overridden on a per-lock or per-transaction basis. See <a href="lockvec.md" class="xref" title="DB_ENV-&gt;lock_vec()">DB_ENV-&gt;lock_vec()</a> and <a href="txnset_timeout.md" class="xref" title="DB_TXN-&gt;set_timeout()">DB_TXN-&gt;set_timeout()</a> for more information.

The `DB_ENV->set_timeout()` method may not be used in a database environment without a locking subsystem.

The `DB_ENV->set_timeout()` method may be called at any time during the life of the application.

The `DB_ENV->set_timeout()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### timeout

The **timeout** parameter is the timeout value. It must be specified as an unsigned 32-bit number of microseconds, limiting the maximum timeout to roughly 71 minutes.

#### flags

The **flags** parameter must be set to one of the following values:

- `DB_SET_LOCK_TIMEOUT`

  Set the timeout value for locks in this database environment.

  The database environment's lock timeout value may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_lock_timeout", one or more whitespace characters, and the lock timeout value. Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

  This flag configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

- `DB_SET_REG_TIMEOUT`

  Set the timeout value on how long to wait for processes to exit the environment before recovery is started when the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method was called with the <a href="envopen.md#envopen_DB_REGISTER" class="link">DB_REGISTER</a> flag and recovery must be performed.

  This wait timeout value may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_reg_timeout", one or more whitespace characters, and the wait timeout value. Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

  This flag configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

- `DB_SET_TXN_TIMEOUT`

  Set the timeout value for transactions in this database environment.

  The database environment's transaction timeout value may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_txn_timeout", one or more whitespace characters, and the transaction timeout value. Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

  This flag configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

### Errors

The `DB_ENV->set_timeout()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
