---
title: "DB_ENV->get_timeout()"
api-name: "DB_ENV->get_timeout()"
source: docs/api_reference/C/envget_timeout.html
---
## DB_ENV-\>get_timeout()

``` c
#include <db.h>

int
DB_ENV->get_timeout(DB_ENV *dbenv, db_timeout_t *timeoutp, 
    u_int32_t flag);  
```

The `DB_ENV->get_timeout()` method returns a value, in microseconds, representing either lock or transaction timeouts. These values are set using the <a href="envset_timeout.md" class="xref" title="DB_ENV-&gt;set_timeout()">DB_ENV-&gt;set_timeout()</a> method.

The `DB_ENV->get_timeout()` method may be called at any time during the life of the application.

The `DB_ENV->get_timeout()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### timeoutp

The **timeoutp** parameter references memory into which the timeout value of the specified **flag** parameter is copied.

#### flag

The **flags** parameter must be set to one of the following values:

- `DB_SET_LOCK_TIMEOUT`

  Return the timeout value for locks in this database environment.

- `DB_SET_REG_TIMEOUT`

  Return the timeout value for how long to wait for processes to exit the environment before recovery is started. This flag only has meaning when the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method was called with the <a href="envopen.md#envopen_DB_REGISTER" class="link">DB_REGISTER</a> flag and recovery must be performed.

- `DB_SET_TXN_TIMEOUT`

  Return the timeout value for transactions in this database environment.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envset_timeout.md" class="xref" title="DB_ENV-&gt;set_timeout()">DB_ENV-&gt;set_timeout()</a>
