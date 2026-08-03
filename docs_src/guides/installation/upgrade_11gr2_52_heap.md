---
title: "Heap Access Method"
api-name: "Heap Access Method"
source: docs/installation/upgrade_11gr2_52_heap.html
---
## Heap Access Method

<span class="sect2"> [New Functions](upgrade_11gr2_52_heap.md#idp936848) </span>

<span class="sect2"> [Modified Functions](upgrade_11gr2_52_heap.md#idp930424) </span>

<span class="sect2"> [New Definition](upgrade_11gr2_52_heap.md#idp931776) </span>

Databases can now be configured as heaps by passing the access type `DB_HEAP` to <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a>. Heap size can be configured with <a href="../../api/c/dbset_heapsize.md" class="olink">DB-&gt;set_heapsize()</a>, and <a href="../../api/c/dbstat.md" class="olink">DB-&gt;stat()</a> now returns heap statistics in the structure `DB_HEAP_STAT` when applied to a heap database.

### New Functions

- <a href="../../api/c/dbset_heapsize.md" class="olink">DB-&gt;set_heapsize()</a>
- <a href="../../api/c/dbget_heapsize.md" class="olink">DB-&gt;get_heapsize()</a>

### Modified Functions

- <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> now accepts `DB_HEAP` as an access type.
- <a href="../../api/c/dbstat.md" class="olink">DB-&gt;stat()</a> now returns heap statistics in the structure `DB_HEAP_STAT`.

### New Definition

- `DB_HEAP_RID` is the defined heap key value.
