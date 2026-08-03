---
title: "Configure the Region Size of Heap Databases"
api-name: "Configure the Region Size of Heap Databases"
source: docs/installation/upgrade_11gr2_53_heap_regionsize.html
---
## Configure the Region Size of Heap Databases

<span class="sect2"> [New Functions](upgrade_11gr2_53_heap_regionsize.md#idp775064) </span>

The region size of heap databases is now configurable. Configuring the region size is useful in controlling the growth of a heap database. To set the region size, call <a href="../../api/c/dbset_heap_regionsize.md" class="olink">DB-&gt;set_heap_regionsize()</a> with the number of pages that the region should have, before the database is created. The function is ignored if it is called after the database is created.

### New Functions

- <a href="../../api/c/dbset_heap_regionsize.md" class="olink">DB-&gt;set_heap_regionsize()</a>
- <a href="../../api/c/dbget_heap_regionsize.md" class="olink">DB-&gt;get_heap_regionsize()</a>
