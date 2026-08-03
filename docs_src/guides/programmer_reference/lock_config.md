---
title: "Configuring locking"
api-name: "Configuring locking"
source: docs/programmer_reference/lock_config.html
---
## Configuring locking

The <a href="../../api/c/envset_lk_detect.md" class="olink">DB_ENV-&gt;set_lk_detect()</a> method specifies that the deadlock detector should be run whenever a lock is about to block. This option provides for rapid detection of deadlocks at the expense of potentially frequent invocations of the deadlock detector. On a fast processor with a highly contentious application where response time is critical, this is a good choice. An option argument to the <a href="../../api/c/envset_lk_detect.md" class="olink">DB_ENV-&gt;set_lk_detect()</a> method indicates which lock requests should be rejected.

The application can limit how long it blocks on a contested resource. The <a href="../../api/c/envset_timeout.md" class="olink">DB_ENV-&gt;set_timeout()</a> method specifies the length of the timeout. This value is checked whenever deadlock detection is performed, so the accuracy of the timeout depends upon the frequency of deadlock detection.

In general, when applications are not specifying lock and transaction timeout values, the <a href="../../api/c/lockdetect.md#detect_DB_LOCK_DEFAULT" class="olink">DB_LOCK_DEFAULT</a> option is probably the correct first choice, and other options should only be selected based on evidence that they improve transaction throughput. If an application has long-running transactions, <a href="../../api/c/lockdetect.md#detect_DB_LOCK_YOUNGEST" class="olink">DB_LOCK_YOUNGEST</a> will guarantee that transactions eventually complete, but it may do so at the expense of a large number of lock request rejections (and therefore, transaction aborts).

The alternative to using the <a href="../../api/c/envset_lk_detect.md" class="olink">DB_ENV-&gt;set_lk_detect()</a> method is to explicitly perform deadlock detection using the Berkeley DB <a href="../../api/c/lockdetect.md" class="olink">DB_ENV-&gt;lock_detect()</a> method.

The <a href="../../api/c/envset_lk_conflicts.md" class="olink">DB_ENV-&gt;set_lk_conflicts()</a> method allows you to specify your own locking conflicts matrix. This is an advanced configuration option, and is almost never necessary.
