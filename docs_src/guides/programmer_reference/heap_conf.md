---
title: "Heap access method specific configuration"
api-name: "Heap access method specific configuration"
source: docs/programmer_reference/heap_conf.html
---
## Heap access method specific configuration

Configuring the Heap access method is fairly simple. Beyond the general configuration required for any access method, you can configure how large the database will become, as well as the amount by which the database grows.

If you provide no configuration relative to the heap size, then the database will grow without bound. Whether this is desirable depends on how much disk space is available to your application.

You can limit the size of the on-disk database file by using the <a href="../../api/c/dbset_heapsize.md" class="olink">DB-&gt;set_heapsize()</a> method. If the size specified on this method is reached, then further attempts to insert/update records will fail with a `DB_HEAP_FULL` error message.

Heap databases are organized into regions, and each region is a constant size. The size of the region in a heap database is limited by the page size, the first page of the region contains a bitmap describing the available space on the remaining pages in the region. When the database experiences write contention, a region is added to reduce contention. This means heap databases can grow in size very quickly. In order to control the amount by which the database increases, the size of the region is configurable via <a href="../../api/c/dbset_heap_regionsize.md" class="olink">DB-&gt;set_heap_regionsize()</a>.
