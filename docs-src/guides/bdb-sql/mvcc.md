---
title: "Using Multiversion Concurrency Control"
api-name: "Using Multiversion Concurrency Control"
source: docs/bdb-sql/mvcc.html
---
## Using Multiversion Concurrency Control

Multiversion Concurrency Control (MVCC) enables snapshot isolation. Snapshot isolation means that whenever a transaction would take a read lock on a page, it makes a copy of the page instead, and then performs its operations on that copied page. This frees other writers from blocking due to a read locks held by other transactions.

You should use snapshot isolation whenever you have a lot of read-only transactions operating at the same time that read-write transactions. In this case, snapshot isolation will improve transaction throughput, albeit at the cost of greater resource usage.

MVCC is described in more detail in <a href="../../guides/programmer_reference/transapp_read.md#snapshot_isolation" class="olink">Snapshot Isolation.</a>

To use MVCC, you must enable it before you access any database tables. Once MVCC is enabled, you can turn snapshot isolation on and off at anytime during the life of your application.

To turn MVCC on, use:

**PRAGMA multiversion** = on \| off;

This PRAGMA must be enabled before you access any database tables during your application runtime, or an error is returned. Turning MVCC on automatically enables snapshot isolation. By default MVCC is turned off.

Once MVCC is enabled, you can turn snapshot isolation on and off using:

**PRAGMA snapshot_isolation** = on \| off;

This PRAGMA can be used at any time during the life of your application <span class="emphasis">*after*</span> MVCC has been turned on. If you attempt to enable or disable snapshot isolation before MVCC is enabled, an error is returned.
