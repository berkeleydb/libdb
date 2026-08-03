---
title: "Replication Connection Status in the Java API"
api-name: "Replication Connection Status in the Java API"
source: docs/installation/upgrade_11gr2_53_conn_status.html
---
## Replication Connection Status in the Java API

<span class="sect2"> [New Function](upgrade_11gr2_53_conn_status.md#idp804776) </span>

<span class="sect2"> [New Class](upgrade_11gr2_53_conn_status.md#idp771568) </span>

<span class="sect2"> [Deprecated Function](upgrade_11gr2_53_conn_status.md#idp809200) </span>

The Java function `ReplicationManagerSiteInfo.isConnected()` is now deprecated. To get the replication connection status, use `ReplicationManagerSiteInfo.getConnectionStatus()`, which returns the new class `ReplicationManagerConnectionStatus`, which has the values `CONNECTED`, `DISCONNECTED`, and `UNKNOWN`.

### New Function

- `ReplicationManagerSiteInfo.getConnectionStatus()`

### New Class

- `ReplicationManagerConnectionStatus`

### Deprecated Function

- `ReplicationManagerSiteInfo.isConnected()`
