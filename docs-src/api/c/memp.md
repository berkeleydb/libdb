---
title: "Chapter 8.  The DB_MPOOLFILE Handle"
api-name: "Chapter 8.  The DB_MPOOLFILE Handle"
source: docs/api_reference/C/memp.html
---
## Chapter 8.  The DB_MPOOLFILE Handle

``` c
#include <db.h>

typedef struct __db_mpoolfile DB_MPOOLFILE;  
```

The memory pool interfaces for the Berkeley DB database environment are methods of the <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle. The <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> memory pool methods and the `DB_MPOOLFILE` class provide general-purpose, page-oriented buffer management of files. Although designed to work with the other <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>classes, they are also useful for more general purposes. The memory pools are referred to in this document as simply <span class="emphasis">*the cache*</span>.

The cache may be shared between processes. The cache is usually filled by pages from one or more files. Pages in the cache are replaced in LRU (least-recently-used) order, with each new page replacing the page that has been unused the longest. Pages retrieved from the cache using <a href="mempfget.md" class="xref" title="DB_MPOOLFILE-&gt;get()">DB_MPOOLFILE-&gt;get()</a> are <span class="emphasis">*pinned*</span> in the cache until they are returned to the control of the cache using the <a href="mempput.md" class="xref" title="DB_MPOOLFILE-&gt;put()">DB_MPOOLFILE-&gt;put()</a> method.

The `DB_MPOOLFILE` object is the handle for a file in the cache. The handle is not free-threaded. Once the <a href="mempfclose.md" class="xref" title="DB_MPOOLFILE-&gt;close()">DB_MPOOLFILE-&gt;close()</a> method is called, the handle may not be accessed again, regardless of that method's return.

## Memory Pools and Related Methods

<table data-border="1" width="80%">
<thead>
<tr>
<th>Memory Pools and Related Methods</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><a href="dbget_mpf.md" class="xref" title="DB-&gt;get_mpf()">DB-&gt;get_mpf()</a></td>
<td>Return the DB_MPOOLFILE for a DB</td>
</tr>
<tr>
<td><a href="mempstat.md" class="xref" title="DB_ENV-&gt;memp_stat()">DB_ENV-&gt;memp_stat()</a></td>
<td>Return cache statistics</td>
</tr>
<tr>
<td><a href="mempstat_print.md" class="xref" title="DB_ENV-&gt;memp_stat_print()">DB_ENV-&gt;memp_stat_print()</a></td>
<td>Print cache statistics</td>
</tr>
<tr>
<td><a href="mempsync.md" class="xref" title="DB_ENV-&gt;memp_sync()">DB_ENV-&gt;memp_sync()</a></td>
<td>Flush all pages from the cache</td>
</tr>
<tr>
<td><a href="memptrickle.md" class="xref" title="DB_ENV-&gt;memp_trickle()">DB_ENV-&gt;memp_trickle()</a></td>
<td>Flush some pages from the cache</td>
</tr>
<tr>
<td colspan="2"><strong>Memory Pool Configuration</strong></td>
</tr>
<tr>
<td><a href="mempregister.md" class="xref" title="DB_ENV-&gt;memp_register()">DB_ENV-&gt;memp_register()</a></td>
<td>Register a custom file type</td>
</tr>
<tr>
<td><a href="envset_cache_max.md" class="xref" title="DB_ENV-&gt;set_cache_max()">DB_ENV-&gt;set_cache_max()</a>, <a href="envget_cache_max.md" class="xref" title="DB_ENV-&gt;get_cache_max()">DB_ENV-&gt;get_cache_max()</a></td>
<td>Set/get the maximum cache size</td>
</tr>
<tr>
<td><a href="envset_cachesize.md" class="xref" title="DB_ENV-&gt;set_cachesize()">DB_ENV-&gt;set_cachesize()</a>, <a href="envget_cachesize.md" class="xref" title="DB_ENV-&gt;get_cachesize()">DB_ENV-&gt;get_cachesize()</a></td>
<td>Set/get the environment cache size</td>
</tr>
<tr>
<td><a href="mempset_mp_max_openfd.md" class="xref" title="DB_ENV-&gt;set_mp_max_openfd()">DB_ENV-&gt;set_mp_max_openfd()</a>, <a href="mempget_mp_max_openfd.md" class="xref" title="DB_ENV-&gt;get_mp_max_openfd()">DB_ENV-&gt;get_mp_max_openfd()</a></td>
<td>Set/get the maximum number of open file descriptors</td>
</tr>
<tr>
<td><a href="mempset_mp_max_write.md" class="xref" title="DB_ENV-&gt;set_mp_max_write()">DB_ENV-&gt;set_mp_max_write()</a>, <a href="mempget_mp_max_write.md" class="xref" title="DB_ENV-&gt;get_mp_max_write()">DB_ENV-&gt;get_mp_max_write()</a></td>
<td>Set/get the maximum number of sequential disk writes</td>
</tr>
<tr>
<td><a href="envset_mp_mmapsize.md" class="xref" title="DB_ENV-&gt;set_mp_mmapsize()">DB_ENV-&gt;set_mp_mmapsize()</a>, <a href="envget_mp_mmapsize.md" class="xref" title="DB_ENV-&gt;get_mp_mmapsize()">DB_ENV-&gt;get_mp_mmapsize()</a></td>
<td>Set/get maximum file size to memory map when opened read-only</td>
</tr>
<tr>
<td><a href="envset_mp_mtxcount.md" class="xref" title="DB_ENV-&gt;set_mp_mtxcount()">DB_ENV-&gt;set_mp_mtxcount()</a>, <a href="envget_mp_mtxcount.md" class="xref" title="DB_ENV-&gt;get_mp_mtxcount()">DB_ENV-&gt;get_mp_mtxcount()</a></td>
<td>Set/get the number of mutexes allocated to the hash table</td>
</tr>
<tr>
<td><a href="envset_mp_pagesize.md" class="xref" title="DB_ENV-&gt;set_mp_pagesize()">DB_ENV-&gt;set_mp_pagesize()</a>, <a href="envget_mp_pagesize.md" class="xref" title="DB_ENV-&gt;get_mp_pagesize()">DB_ENV-&gt;get_mp_pagesize()</a></td>
<td>Set/get page size to configure the buffer pool</td>
</tr>
<tr>
<td><a href="envset_mp_tablesize.md" class="xref" title="DB_ENV-&gt;set_mp_tablesize()">DB_ENV-&gt;set_mp_tablesize()</a>, <a href="envget_mp_tablesize.md" class="xref" title="DB_ENV-&gt;get_mp_tablesize()">DB_ENV-&gt;get_mp_tablesize()</a></td>
<td>Set/get the hash table size</td>
</tr>
<tr>
<td colspan="2"><strong>Memory Pool Files</strong></td>
</tr>
<tr>
<td><a href="mempfcreate.md" class="xref" title="DB_ENV-&gt;memp_fcreate()">DB_ENV-&gt;memp_fcreate()</a></td>
<td>Create a memory pool file handle</td>
</tr>
<tr>
<td><a href="mempfclose.md" class="xref" title="DB_MPOOLFILE-&gt;close()">DB_MPOOLFILE-&gt;close()</a></td>
<td>Close a file in the cache</td>
</tr>
<tr>
<td><a href="mempfget.md" class="xref" title="DB_MPOOLFILE-&gt;get()">DB_MPOOLFILE-&gt;get()</a></td>
<td>Get page from a file in the cache</td>
</tr>
<tr>
<td><a href="mempfopen.md" class="xref" title="DB_MPOOLFILE-&gt;open()">DB_MPOOLFILE-&gt;open()</a></td>
<td>Open a file in the cache</td>
</tr>
<tr>
<td><a href="mempput.md" class="xref" title="DB_MPOOLFILE-&gt;put()">DB_MPOOLFILE-&gt;put()</a></td>
<td>Return a page to the cache</td>
</tr>
<tr>
<td><a href="mempfsync.md" class="xref" title="DB_MPOOLFILE-&gt;sync()">DB_MPOOLFILE-&gt;sync()</a></td>
<td>Flush pages from a file from the cache</td>
</tr>
<tr>
<td colspan="2"><strong>Memory Pool File Configuration</strong></td>
</tr>
<tr>
<td><a href="mempset_clear_len.md" class="xref" title="DB_MPOOLFILE-&gt;set_clear_len()">DB_MPOOLFILE-&gt;set_clear_len()</a>, <a href="mempget_clear_len.md" class="xref" title="DB_MPOOLFILE-&gt;get_clear_len()">DB_MPOOLFILE-&gt;get_clear_len()</a></td>
<td>Set/get number of bytes to clear when creating a new page</td>
</tr>
<tr>
<td><a href="mempset_fileid.md" class="xref" title="DB_MPOOLFILE-&gt;set_fileid()">DB_MPOOLFILE-&gt;set_fileid()</a>, <a href="mempget_fileid.md" class="xref" title="DB_MPOOLFILE-&gt;get_fileid()">DB_MPOOLFILE-&gt;get_fileid()</a></td>
<td>Set/get file unique identifier</td>
</tr>
<tr>
<td><a href="mempset_flags.md" class="xref" title="DB_MPOOLFILE-&gt;set_flags()">DB_MPOOLFILE-&gt;set_flags()</a>, <a href="mempget_flags.md" class="xref" title="DB_MPOOLFILE-&gt;get_flags()">DB_MPOOLFILE-&gt;get_flags()</a></td>
<td>Set/get file options</td>
</tr>
<tr>
<td><a href="mempset_ftype.md" class="xref" title="DB_MPOOLFILE-&gt;set_ftype()">DB_MPOOLFILE-&gt;set_ftype()</a>, <a href="mempget_ftype.md" class="xref" title="DB_MPOOLFILE-&gt;get_ftype()">DB_MPOOLFILE-&gt;get_ftype()</a></td>
<td>Set/get file type</td>
</tr>
<tr>
<td><a href="mempset_lsn_offset.md" class="xref" title="DB_MPOOLFILE-&gt;set_lsn_offset()">DB_MPOOLFILE-&gt;set_lsn_offset()</a>, <a href="mempget_lsn_offset.md" class="xref" title="DB_MPOOLFILE-&gt;get_lsn_offset()">DB_MPOOLFILE-&gt;get_lsn_offset()</a></td>
<td>Set/get file log-sequence-number offset</td>
</tr>
<tr>
<td><a href="mempset_maxsize.md" class="xref" title="DB_MPOOLFILE-&gt;set_maxsize()">DB_MPOOLFILE-&gt;set_maxsize()</a>, <a href="mempget_maxsize.md" class="xref" title="DB_MPOOLFILE-&gt;get_maxsize()">DB_MPOOLFILE-&gt;get_maxsize()</a></td>
<td>Set/get maximum file size</td>
</tr>
<tr>
<td><a href="mempset_pgcookie.md" class="xref" title="DB_MPOOLFILE-&gt;set_pgcookie()">DB_MPOOLFILE-&gt;set_pgcookie()</a>, <a href="mempget_pgcookie.md" class="xref" title="DB_MPOOLFILE-&gt;get_pgcookie()">DB_MPOOLFILE-&gt;get_pgcookie()</a></td>
<td>Set/get file cookie for pgin/pgout</td>
</tr>
<tr>
<td><a href="mempset_priority.md" class="xref" title="DB_MPOOLFILE-&gt;set_priority()">DB_MPOOLFILE-&gt;set_priority()</a>, <a href="mempget_priority.md" class="xref" title="DB_MPOOLFILE-&gt;get_priority()">DB_MPOOLFILE-&gt;get_priority()</a></td>
<td>Set/get cache file priority</td>
</tr>
</tbody>
</table>
