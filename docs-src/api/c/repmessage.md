---
title: "DB_ENV->rep_process_message()"
api-name: "DB_ENV->rep_process_message()"
source: docs/api_reference/C/repmessage.html
---
## DB_ENV-\>rep_process_message()

``` c
#include <db.h>

int
DB_ENV->rep_process_message(DB_ENV *env,
    DBT *control, DBT *rec, int envid, DB_LSN *ret_lsnp)  
```

The `DB_ENV->rep_process_message()` method processes an incoming replication message sent by a member of the replication group to the local database environment.

The `DB_ENV->rep_process_message()` method is not called by most replication applications. It should only be called by Base API applications implementing their own network transport layer, explicitly holding replication group elections and handling replication messages outside of the Replication Manager framework.

For implementation reasons, all incoming replication messages must be processed using the same <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle. It is not required that a single thread of control process all messages, only that all threads of control processing messages use the same handle.

Before calling this method, the enclosing database environment must already have been opened by calling the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method and must already have been configured to send replication messages by calling the <a href="reptransport.md" class="xref" title="DB_ENV-&gt;rep_set_transport()">DB_ENV-&gt;rep_set_transport()</a> method.

The `DB_ENV->rep_process_message()` method has additional return values:

- **DB_REP_DUPMASTER**

  The `DB_ENV->rep_process_message()` method will return `DB_REP_DUPMASTER` if the replication group has more than one master. The application should reconfigure itself as a client by calling the <a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a> method, and then call for an election by calling <a href="repelect.md" class="xref" title="DB_ENV-&gt;rep_elect()">DB_ENV-&gt;rep_elect()</a>.

- **DB_REP_HOLDELECTION**

  The `DB_ENV->rep_process_message()` method will return `DB_REP_HOLDELECTION` if an election is needed. The application should call for an election by calling <a href="repelect.md" class="xref" title="DB_ENV-&gt;rep_elect()">DB_ENV-&gt;rep_elect()</a>.

- **DB_REP_IGNORE**

  The `DB_ENV->rep_process_message()` method will return `DB_REP_IGNORE` if this message cannot be processed. This is an indication that this message is irrelevant to the current replication state (for example, an old message from a previous generation arrives and is processed late).

- **DB_REP_ISPERM**

  The `DB_ENV->rep_process_message()` method will return `DB_REP_ISPERM` if processing this message results in the processing of records that are permanent. The maximum LSN of the permanent records stored is returned.

- **DB_REP_JOIN_FAILURE**

  The `DB_ENV->rep_process_message()` method will return `DB_REP_JOIN_FAILURE` if a new master has been chosen but the client is unable to synchronize with the new master. This is possibly because the client has turned off automatic internal initialization by setting the <a href="repconfig.md#config_DB_REP_CONF_AUTOINIT" class="link">DB_REP_CONF_AUTOINIT</a> flag to `0`.

- **DB_REP_NEWSITE**

  The `DB_ENV->rep_process_message()` method will return `DB_REP_NEWSITE` if the system received contact information from a new environment. The **rec** parameter contains the opaque data specified to the <a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a> **cdata** parameter. The application should take whatever action is needed to establish a communication channel with this new environment.

- **DB_REP_NOTPERM**

  The `DB_ENV->rep_process_message()` method will return `DB_REP_NOTPERM` if a message carrying a <a href="reptransport.md#transport_DB_REP_PERMANENT" class="link">DB_REP_PERMANENT</a> flag was processed successfully, but was not written to disk. The LSN of this record is returned. The application should take whatever action is deemed necessary to retain its recoverability characteristics.

Unless otherwise specified, the `DB_ENV->rep_process_message()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### control

The **control** parameter should reference a copy of the **control** parameter specified by Berkeley DB on the sending environment. See the <a href="reptransport.md" class="xref" title="DB_ENV-&gt;rep_set_transport()">DB_ENV-&gt;rep_set_transport()</a> method for more information.

#### rec

The **rec** parameter should reference a copy of the **rec** parameter specified by Berkeley DB on the sending environment. See the <a href="reptransport.md" class="xref" title="DB_ENV-&gt;rep_set_transport()">DB_ENV-&gt;rep_set_transport()</a> method for more information.

#### envid

The **envid** parameter should contain the local identifier that corresponds to the environment that sent the message to be processed (see <a href="../../programmer_reference/rep_id.html" class="olink">Replication environment IDs</a> for more information).

#### ret_lsnp

If `DB_ENV->rep_process_message()` method returns `DB_REP_NOTPERM` then the **ret_lsnp** parameter will contain the log sequence number of this permanent log message that could not be written to disk. If `DB_ENV->rep_process_message()` method returns `DB_REP_ISPERM` then the **ret_lsnp** parameter will contain largest log sequence number of the permanent records that are now written to disk as a result of processing this message. In all other cases the value of **ret_lsnp** is undefined.

### Errors

The `DB_ENV->rep_process_message()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the database environment was not already configured to communicate with a replication group by a call to <a href="reptransport.md" class="xref" title="DB_ENV-&gt;rep_set_transport()">DB_ENV-&gt;rep_set_transport()</a>; if the database environment was not already opened; if this method is called from a Replication Manager application; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
