---
title: "Performance Tuning"
api-name: "Performance Tuning"
source: docs/gsg_txn/JAVA/perftune-intro.html
---
## Performance Tuning

From a performance perspective, the use of transactions is not free. Depending on how you configure them, transaction commits usually require your application to perform disk I/O that a non-transactional application does not perform. Also, for multi-threaded and multi-process applications, the use of transactions can result in increased lock contention due to extra locking requirements driven by transactional isolation guarantees.

There is therefore a performance tuning component to transactional applications that is not applicable for non-transactional applications (although some tuning considerations do exist whether or not your application uses transactions). Where appropriate, these tuning considerations are introduced in the following chapters. However, for a more complete description of them, see the <a href="http://download.oracle.com/docs/cd/E17076_02/html/programmer_reference/transapp_tune.html" class="ulink" target="_top">Transaction tuning</a> and <a href="http://download.oracle.com/docs/cd/E17076_02/html/programmer_reference/transapp_throughput.html" class="ulink" target="_top">Transaction throughput</a> sections of the *Berkeley DB Programmer's Reference Guide*.
