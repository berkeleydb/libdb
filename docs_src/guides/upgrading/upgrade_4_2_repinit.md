---
title: "Replication"
api-name: "Replication"
source: docs/upgrading/upgrade_4_2_repinit.html
---
## Replication

<span class="sect2"> [Replication initialization](upgrade_4_2_repinit.md#idp50804696) </span>

<span class="sect2"> [Database methods and replication clients](upgrade_4_2_repinit.md#idp50772032) </span>

<span class="sect2"> [DB_ENV-\>rep_process_message()](upgrade_4_2_repinit.md#idp50779672) </span>

### Replication initialization

In the Berkeley DB 4.2 release, replication environments must be specifically initialized by any process that will ever do anything other than open databases in read-only mode (that is, any process which might call any of the Berkeley DB replication interfaces or modify databases). This initialization is done when the replication database environment handle is opened, by specifying the <a href="../../api/c/envopen.md#envopen_DB_INIT_REP" class="olink">DB_INIT_REP</a> flag to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method.

### Database methods and replication clients

All of the <a href="../../api/c/db.md" class="olink">DB</a> object methods may now return `DB_REP_HANDLE_DEAD` when a replication client changes masters. When this happens the <a href="../../api/c/db.md" class="olink">DB</a> handle is no longer able to be used and the application must close the handle using the <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a> method and open a new handle. This new return value is returned when a client unrolls a transaction in order to synchronize with the new master. Otherwise, if the application was permitted to use the original handle, it's possible the handle might attempt to access nonexistent resources.

### <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a>

The <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> method has new return values and an log sequence number (LSN) associated with those return values. The new argument is **ret_lsnp**, which is the returned LSN when the <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> method returns <a href="../../api/c/repmessage.md#repmsg_DB_REP_ISPERM" class="olink">DB_REP_ISPERM</a> or <a href="../../api/c/repmessage.md#repmsg_DB_REP_NOTPERM" class="olink">DB_REP_NOTPERM</a>. See <a href="../../guides/programmer_reference/rep_trans.md" class="olink">Transactional guarantees</a> for more information.
