---
title: "Chapter 18.  The Memory Pool Subsystem"
api-name: "Chapter 18.  The Memory Pool Subsystem"
source: docs/programmer_reference/mp.html
---
## Chapter 18.  The Memory Pool Subsystem

**Table of Contents**

<span class="sect1"> [Introduction to the memory pool subsystem](mp.md#mp_intro) </span>

<span class="sect1"> [Configuring the memory pool](mp_config.md) </span>

<span class="sect1"> [Warming the memory pool](mp_warm.md) </span>

<span class="sect2"> [The warm_cache() function](mp_warm.md#warm_cache) </span>

## Introduction to the memory pool subsystem

The Memory Pool subsystem is the general-purpose shared memory buffer pool used by Berkeley DB. This module is useful outside of the Berkeley DB package for processes that require page-oriented, shared and cached file access. (However, such "use outside of Berkeley DB" is not supported in replicated environments.)

A <span class="emphasis">*memory pool*</span> is a memory cache shared among any number of threads of control. The <a href="../../api/c/envopen.md#envopen_DB_INIT_MPOOL" class="olink">DB_INIT_MPOOL</a> flag to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method opens and optionally creates a memory pool. When that pool is no longer in use, it should be closed using the <a href="../../api/c/envclose.md" class="olink">DB_ENV-&gt;close()</a> method.

The <a href="../../api/c/mempfcreate.md" class="olink">DB_ENV-&gt;memp_fcreate()</a> method returns a <a href="../../api/c/memp.md" class="olink">DB_MPOOLFILE</a> handle on an underlying file within the memory pool. The file may be opened using the <a href="../../api/c/mempfopen.md" class="olink">DB_MPOOLFILE-&gt;open()</a> method. The <a href="../../api/c/mempfget.md" class="olink">DB_MPOOLFILE-&gt;get()</a> method is used to retrieve pages from files in the pool. All retrieved pages must be subsequently returned using the <a href="../../api/c/mempput.md" class="olink">DB_MPOOLFILE-&gt;put()</a> method. At the time pages are returned, they may be marked **dirty**, which causes them to be written to the underlying file before being discarded from the pool. If there is insufficient room to bring a new page in the pool, a page is selected to be discarded from the pool using a least-recently-used algorithm. All dirty pages in the pool from the file may be flushed using the <a href="../../api/c/mempfsync.md" class="olink">DB_MPOOLFILE-&gt;sync()</a> method. When the file handle is no longer in use, it should be closed using the <a href="../../api/c/mempfclose.md" class="olink">DB_MPOOLFILE-&gt;close()</a> method.

There are additional configuration interfaces that apply when opening a new file in the memory pool:

- The <a href="../../api/c/mempset_clear_len.md" class="olink">DB_MPOOLFILE-&gt;set_clear_len()</a> method specifies the number of bytes to clear when creating a new page in the memory pool.
- The <a href="../../api/c/mempset_fileid.md" class="olink">DB_MPOOLFILE-&gt;set_fileid()</a> method specifies a unique ID associated with the file.
- The <a href="../../api/c/mempset_ftype.md" class="olink">DB_MPOOLFILE-&gt;set_ftype()</a> method specifies the type of file for the purposes of page input and output processing.
- The <a href="../../api/c/mempset_lsn_offset.md" class="olink">DB_MPOOLFILE-&gt;set_lsn_offset()</a> method specifies the byte offset of each page's log sequence number (<a href="../../api/c/lsn.md" class="olink">DB_LSN</a>) for the purposes of transaction checkpoints.
- The <a href="../../api/c/mempset_pgcookie.md" class="olink">DB_MPOOLFILE-&gt;set_pgcookie()</a> method specifies an application provided argument for the purposes of page input and output processing.

There are additional interfaces for the memory pool as a whole:

- It is possible to gradually flush buffers from the pool in order to maintain a consistent percentage of clean buffers in the pool using the <a href="../../api/c/memptrickle.md" class="olink">DB_ENV-&gt;memp_trickle()</a> method.
- Because special-purpose processing may be necessary when pages are read or written (for example, endian conversion, or page checksums), the <a href="../../api/c/mempregister.md" class="olink">DB_ENV-&gt;memp_register()</a> function allows applications to specify automatic input and output processing in these cases.
- The <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility uses the <a href="../../api/c/mempstat.md" class="olink">DB_ENV-&gt;memp_stat()</a> method to display statistics about the efficiency of the pool.
- All dirty pages in the pool may be flushed using the <a href="../../api/c/mempsync.md" class="olink">DB_ENV-&gt;memp_sync()</a> method. In addition, <a href="../../api/c/mempsync.md" class="olink">DB_ENV-&gt;memp_sync()</a> takes an argument that is specific to database systems, and which allows the memory pool to be flushed up to a specified log sequence number (<a href="../../api/c/lsn.md" class="olink">DB_LSN</a>).
- The entire pool may be discarded using the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> method.

For more information on the memory pool subsystem methods, see the <a href="../../api/c/memp.md#memplist" class="olink">Memory Pools and Related Methods</a> section in the *Berkeley DB C API Reference Guide.*
