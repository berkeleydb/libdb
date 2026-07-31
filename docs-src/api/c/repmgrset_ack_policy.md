---
title: "DB_ENV->repmgr_set_ack_policy()"
api-name: "DB_ENV->repmgr_set_ack_policy()"
source: docs/api_reference/C/repmgrset_ack_policy.html
---
## DB_ENV-\>repmgr_set_ack_policy()

``` c
#include <db.h>

int
DB_ENV->repmgr_set_ack_policy(DB_ENV *env, int ack_policy);  
```

The `DB_ENV->repmgr_set_ack_policy()` method specifies how master and client sites will handle acknowledgment of replication messages which are necessary for "permanent" records. The current implementation requires all sites in a replication group configure the same acknowledgement policy.

The database environment's replication subsystem may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "repmgr_set_ack_policy", one or more whitespace characters, and the **ack_policy** parameter specified as a string. For example, "repmgr_set_ack_policy DB_REPMGR_ACKS_ALL". Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

Waiting for client acknowledgements is always limited by the <a href="repset_timeout.md#set_timeout_DB_REP_ACK_TIMEOUT" class="link">DB_REP_ACK_TIMEOUT</a> specified by the <a href="repset_timeout.md" class="xref" title="DB_ENV-&gt;rep_set_timeout()">DB_ENV-&gt;rep_set_timeout()</a> method. If an insufficient number of client acknowledgements have been received, then the master will invoke the event callback function, if set, with the <a href="envevent_notify.md#event_notify_DB_EVENT_REP_PERM_FAILED" class="link">DB_EVENT_REP_PERM_FAILED</a> value. (See the <a href="../../guides/programmer_reference/rep_mgr_ack.md" class="olink">Choosing a Replication Manager Ack Policy</a> section in the *Berkeley DB Programmer's Reference Guide* for more information.)

The `DB_ENV->repmgr_set_ack_policy()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->repmgr_set_ack_policy()` method may be called at any time during the life of the application.

The `DB_ENV->repmgr_set_ack_policy()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### ack_policy

Some acknowledgement policies use the concept of an electable peer, which is a client capable of being subsequently elected master of the replication group. The **ack_policy** parameter must be set to one of the following values:

- **DB_REPMGR_ACKS_ALL**

  The master should wait until all replication clients have acknowledged each permanent replication message.

- **DB_REPMGR_ACKS_ALL_AVAILABLE**

  The master should wait until all currently connected replication clients have acknowledged each permanent replication message. This policy will then invoke the <a href="envevent_notify.md#event_notify_DB_EVENT_REP_PERM_FAILED" class="link">DB_EVENT_REP_PERM_FAILED</a> event if fewer than a quorum of clients acknowledged during that time.

- **DB_REPMGR_ACKS_ALL_PEERS**

  The master should wait until all electable peers have acknowledged each permanent replication message.

- **DB_REPMGR_ACKS_NONE**

  The master should not wait for any client replication message acknowledgments.

- **DB_REPMGR_ACKS_ONE**

  The master should wait until at least one client site has acknowledged each permanent replication message.

- **DB_REPMGR_ACKS_ONE_PEER**

  The master should wait until at least one electable peer has acknowledged each permanent replication message.

- **DB_REPMGR_ACKS_QUORUM**

  The master should wait until it has received acknowledgements from the minimum number of electable peers sufficient to ensure that the effect of the permanent record remains durable if an election is held. This is the default acknowledgement policy.

### Errors

The `DB_ENV->repmgr_set_ack_policy()` method may fail and return one of the following non-zero errors:

#### EINVAL

If this method is called from a base replication API application; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
