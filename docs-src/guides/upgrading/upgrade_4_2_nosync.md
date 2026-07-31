---
title: "Client replication environments"
api-name: "Client replication environments"
source: docs/upgrading/upgrade_4_2_nosync.html
---
## Client replication environments

In previous Berkeley DB releases, replication clients always behaved as if <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a> behavior was configured, that is, clients would not write or synchronously flush their log when receiving a transaction commit or prepare message. However, applications needing a high level of transactional guarantee may need a write and synchronous flush on the client. By default in the Berkeley DB 4.2 release, client database environments write and synchronously flush their logs when receiving a transaction commit or prepare message. Applications not needing such a high level of transactional guarantee should use the environment's <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a> flag to configure their client database environments to not do the write or flush on transaction commit, as this will increase their performance. Regardless of the setting of the <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a> flag, clients will always write and flush on transaction prepare.
