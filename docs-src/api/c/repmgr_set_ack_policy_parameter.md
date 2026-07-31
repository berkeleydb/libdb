---
title: "repmgr_set_ack_policy"
api-name: "repmgr_set_ack_policy"
source: docs/api_reference/C/repmgr_set_ack_policy_parameter.html
---
## repmgr_set_ack_policy

Specifies how master and client sites will handle acknowledgment of replication messages which are necessary for "permanent" records.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `repmgr_set_ack_policy`, one or more whitespace characters, and the ack_policy parameter specified as a string. For example:

``` c
repmgr_set_ack_policy DB_REPMGR_ACKS_ALL
```

Specifies that the master should wait until all replication clients have acknowledged each permanent replication message.

The ack_policy parameters are:

- DB_REPMGR_ACKS_ALL
- DB_REPMGR_ACKS_ALL_AVAILABLE
- DB_REPMGR_ACKS_ALL_PEERS
- DB_REPMGR_ACKS_NONE
- DB_REPMGR_ACKS_ONE
- DB_REPMGR_ACKS_ONE_PEER
- DB_REPMGR_ACKS_QUORUM

For more information, see <a href="repmgrset_ack_policy.md" class="xref" title="DB_ENV-&gt;repmgr_set_ack_policy()">DB_ENV-&gt;repmgr_set_ack_policy()</a>.
