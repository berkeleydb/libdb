---
title: "rep_set_timeout"
api-name: "rep_set_timeout"
source: docs/api_reference/C/rep_set_timeout_parameter.html
---
## rep_set_timeout

Specifies a variety of replication timeout values.

The syntax of this parameter in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `rep_set_timeout`, one or more whitespace characters, and the flag specified as a string and the timeout specified as two parts. For example:

``` c
rep_set_timeout DB_REP_CONNECTION_RETRY 15000000
```

Specifies the connection retry timeout as 15 seconds.

The flag value can be any one of the following:

- DB_REP_ACK_TIMEOUT
- DB_REP_CHECKPOINT_DELAY
- DB_REP_CONNECTION_RETRY
- DB_REP_ELECTION_TIMEOUT
- DB_REP_ELECTION_RETRY
- DB_REP_FULL_ELECTION_TIMEOUT
- DB_REP_HEARTBEAT_MONITOR
- DB_REP_HEARTBEAT_SEND
- DB_REP_LEASE_TIMEOUT

For more information, see <a href="repset_timeout.md" class="xref" title="DB_ENV-&gt;rep_set_timeout()">DB_ENV-&gt;rep_set_timeout()</a>.
