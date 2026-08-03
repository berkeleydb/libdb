---
title: "Bulk transfer"
api-name: "Bulk transfer"
source: docs/programmer_reference/rep_bulk.html
---
## Bulk transfer

Sites in a replication group may be configured to use bulk transfer by calling the <a href="../../api/c/repconfig.md" class="olink">DB_ENV-&gt;rep_set_config()</a> method with the <a href="../../api/c/repconfig.md#config_DB_REP_CONF_BULK" class="olink">DB_REP_CONF_BULK</a> flag. When configured for bulk transfer, sites will accumulate records in a buffer and transfer them to another site in a single network transfer. Configuring bulk transfer makes sense for master sites, of course. Additionally, applications using client-to-client synchronization may find it helpful to configure bulk transfer for client sites as well.

When a master is generating new log records, or any information request is made of a master, and bulk transfer has been configured, records will accumulate in a bulk buffer. The bulk buffer will be sent to the client if either the buffer is full or if a permanent record (for example, a transaction commit or checkpoint record) is queued for the client.

When a client is responding to another client's request for information, and bulk transfer has been configured, records will accumulate in a bulk buffer. The bulk buffer will be sent to the client when the buffer is full or when the client's request has been satisfied; no particular type of record will cause the buffer to be sent.

The size of the bulk buffer itself is internally determined and cannot be configured. However, the overall size of a transfer may be limited using the <a href="../../api/c/repset_limit.md" class="olink">DB_ENV-&gt;rep_set_limit()</a> method.
