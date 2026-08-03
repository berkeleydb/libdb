---
title: "DB_ENV->rep_set_transport()"
api-name: "DB_ENV->rep_set_transport()"
source: docs/api_reference/C/reptransport.html
---
## DB_ENV-\>rep_set_transport()

``` c
#include <db.h>

int
DB_ENV->rep_set_transport(DB_ENV *env, int envid,
    int (*send)(DB_ENV *dbenv,
    const DBT *control, const DBT *rec, const DB_LSN *lsnp,
    int envid, u_int32_t flags));  
```

The `DB_ENV->rep_set_transport()` method initializes the communication infrastructure for a database environment participating in a replicated application.

The `DB_ENV->rep_set_transport()` method is not called by most replication applications. It should only be called by Base API applications implementing their own network transport layer, explicitly holding replication group elections and handling replication messages outside of the Replication Manager framework.

The `DB_ENV->rep_set_transport()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->rep_set_transport()` method may be called at any time during the life of the application.

The `DB_ENV->rep_set_transport()` method returns a non-zero error value on failure and 0 on success.

### Note

Berkeley DB is not re-entrant. The callback function for this method should not attempt to make library calls (for example, to release locks or close open handles). Re-entering Berkeley DB is not guaranteed to work correctly, and the results are undefined.

### Parameters

#### envid

The **envid** parameter is the local environment's ID. It must be a non-negative integer and uniquely identify this Berkeley DB database environment (see <a href="../../guides/programmer_reference/rep_id.md" class="olink">Replication environment IDs</a> for more information).

#### send

The **send** callback function is used to transmit data using the replication application's communication infrastructure. The parameters to **send** are as follows:

- `dbenv`

  The **dbenv** parameter is the enclosing database environment handle.

- `control`

  The **control** parameter is the first of the two data elements to be transmitted by the **send** function.

- `rec`

  The **rec** parameter is the second of the two data elements to be transmitted by the **send** function.

- `lsnp`

  If the type of message to be sent has an LSN associated with it, then the **lsnp** parameter contains the LSN of the record being sent. This LSN can be used to determine that certain records have been processed successfully by clients.

- `envid`

  The **envid** parameter is a positive integer identifier that specifies the replication environment to which the message should be sent (see <a href="../../guides/programmer_reference/rep_id.md" class="olink">Replication environment IDs</a> for more information).

  The special identifier `DB_EID_BROADCAST` indicates that a message should be broadcast to every environment in the replication group. The application may use a true broadcast protocol or may send the message in sequence to each machine with which it is in communication. In both cases, the sending site should not be asked to process the message.

  The special identifier <a href="../../guides/programmer_reference/rep_id.md#rep_id.DB_EID_INVALID" class="olink">DB_EID_INVALID</a> indicates an invalid environment ID. This may be used to initialize values that are subsequently checked for validity.

- `flags`

  The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

  - `DB_REP_ANYWHERE`

    The message is a client request that can be satisfied by another client as well as by the master.

  - `DB_REP_NOBUFFER`

    The record being sent should be transmitted immediately and not buffered or delayed.

  - `DB_REP_PERMANENT`

    The record being sent is critical for maintaining database integrity (for example, the message includes a transaction commit). The application should take appropriate action to enforce the reliability guarantees it has chosen, such as waiting for acknowledgement from one or more clients.

  - `DB_REP_REREQUEST`

    The message is a client request that has already been made and to which no response was received.

It may sometimes be useful to pass application-specific data to the send function; see <a href="../../guides/programmer_reference/env_faq.md" class="olink">Environment FAQ</a> for a discussion on how to do this.

The **send** function must return 0 on success and non-zero on failure. If the send function fails, the message being sent is necessary to maintain database integrity, and the local log is not configured for synchronous flushing, the local log will be flushed; otherwise, any error from the **send** function will be ignored.

### Errors

The `DB_ENV->rep_set_transport()` method may fail and return one of the following non-zero errors:

#### EINVAL

The method is called from a Replication Manager application; or an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
