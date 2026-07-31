---
title: "Configuring transactions"
api-name: "Configuring transactions"
source: docs/programmer_reference/txn_config.html
---
## Configuring transactions

The application may change the number of simultaneous outstanding transactions supported by the Berkeley DB environment by calling the <a href="../../api/c/envset_tx_max.md" class="olink">DB_ENV-&gt;set_tx_max()</a> method. This will also set the size of the underlying transaction subsystem's region. When the number of outstanding transactions is reached, additional calls to <a href="../../api/c/txnbegin.md" class="olink">DB_ENV-&gt;txn_begin()</a> will fail until some active transactions complete.

The application can limit how long a transaction runs or blocks on contested resources. The <a href="../../api/c/envset_timeout.md" class="olink">DB_ENV-&gt;set_timeout()</a> method specifies the length of the timeout. This value is checked whenever deadlock detection is performed or when the transaction is about to block on a lock that cannot be immediately granted. Because timeouts are only checked at these times, the accuracy of the timeout depends on how often deadlock detection is performed or how frequently the transaction blocks.

There is an additional parameter used in configuring transactions; the <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a>. Setting the <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a> flag to <a href="../../api/c/envset_flags.md" class="olink">DB_ENV-&gt;set_flags()</a> when opening a transaction region changes the behavior of transactions to not write or synchronously flush the log during transaction commit.

This change may significantly increase application transactional throughput. However, it means that although transactions will continue to exhibit the ACI (atomicity, consistency, and isolation) properties, they will not have D (durability). Database integrity will be maintained, but it is possible that some number of the most recently committed transactions may be undone during recovery instead of being redone.
