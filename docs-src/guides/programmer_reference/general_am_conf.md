---
title: "General access method configuration"
api-name: "General access method configuration"
source: docs/programmer_reference/general_am_conf.html
---
## General access method configuration

<span class="sect2"> [Selecting a page size](general_am_conf.md#am_conf_pagesize) </span>

<span class="sect2"> [Selecting a cache size](general_am_conf.md#am_conf_cachesize) </span>

<span class="sect2"> [Selecting a byte order](general_am_conf.md#am_conf_byteorder) </span>

<span class="sect2"> [Duplicate data items](general_am_conf.md#am_conf_dup) </span>

<span class="sect2"> [Non-local memory allocation](general_am_conf.md#am_conf_malloc) </span>

There are a series of configuration tasks which are common to all access methods. They are described in the following sections.

### Selecting a page size

The size of the pages used in the underlying database can be specified by calling the <a href="../../api/c/dbset_pagesize.md" class="olink">DB-&gt;set_pagesize()</a> method. The minimum page size is 512 bytes and the maximum page size is 64K bytes, and must be a power of two. If no page size is specified by the application, a page size is selected based on the underlying filesystem I/O block size. (A page size selected in this way has a lower limit of 512 bytes and an upper limit of 16K bytes.)

There are several issues to consider when selecting a pagesize: overflow record sizes, locking, I/O efficiency, and recoverability.

First, the page size implicitly sets the size of an overflow record. Overflow records are key or data items that are too large to fit on a normal database page because of their size, and are therefore stored in overflow pages. Overflow pages are pages that exist outside of the normal database structure. For this reason, there is often a significant performance penalty associated with retrieving or modifying overflow records. Selecting a page size that is too small, and which forces the creation of large numbers of overflow pages, can seriously impact the performance of an application.

Second, in the Btree, Hash and Recno access methods, the finest-grained lock that Berkeley DB acquires is for a page. (The Queue access method generally acquires record-level locks rather than page-level locks.) Selecting a page size that is too large, and which causes threads or processes to wait because other threads of control are accessing or modifying records on the same page, can impact the performance of your application.

Third, the page size specifies the granularity of I/O from the database to the operating system. Berkeley DB will give a page-sized unit of bytes to the operating system to be scheduled for reading/writing from/to the disk. For many operating systems, there is an internal **block size** which is used as the granularity of I/O from the operating system to the disk. Generally, it will be more efficient for Berkeley DB to write filesystem-sized blocks to the operating system and for the operating system to write those same blocks to the disk.

Selecting a database page size smaller than the filesystem block size may cause the operating system to coalesce or otherwise manipulate Berkeley DB pages and can impact the performance of your application. When the page size is smaller than the filesystem block size and a page written by Berkeley DB is not found in the operating system's cache, the operating system may be forced to read a block from the disk, copy the page into the block it read, and then write out the block to disk, rather than simply writing the page to disk. Additionally, as the operating system is reading more data into its buffer cache than is strictly necessary to satisfy each Berkeley DB request for a page, the operating system buffer cache may be wasting memory.

Alternatively, selecting a page size larger than the filesystem block size may cause the operating system to read more data than necessary. On some systems, reading filesystem blocks sequentially may cause the operating system to begin performing read-ahead. If requesting a single database page implies reading enough filesystem blocks to satisfy the operating system's criteria for read-ahead, the operating system may do more I/O than is required.

Fourth, when using the Berkeley DB Transactional Data Store product, the page size may affect the errors from which your database can recover See <a href="transapp_reclimit.md" class="xref" title="Berkeley DB recoverability">Berkeley DB recoverability</a> for more information.

### Note

The <a href="../../api/c/db_tuner.md" class="olink">db_tuner</a> utility suggests a page size for btree databases that optimizes cache efficiency and storage space requirements. This utility works only when given a pre-populated database. So, it is useful when tuning an existing application and not when first implementing an application.

### Selecting a cache size

The size of the cache used for the underlying database can be specified by calling the <a href="../../api/c/dbset_cachesize.md" class="olink">DB-&gt;set_cachesize()</a> method. Choosing a cache size is, unfortunately, an art. Your cache must be at least large enough for your working set plus some overlap for unexpected situations.

When using the Btree access method, you must have a cache big enough for the minimum working set for a single access. This will include a root page, one or more internal pages (depending on the depth of your tree), and a leaf page. If your cache is any smaller than that, each new page will force out the least-recently-used page, and Berkeley DB will re-read the root page of the tree anew on each database request.

If your keys are of moderate size (a few tens of bytes) and your pages are on the order of 4KB to 8KB, most Btree applications will be only three levels. For example, using 20 byte keys with 20 bytes of data associated with each key, a 8KB page can hold roughly 400 keys (or 200 key/data pairs), so a fully populated three-level Btree will hold 32 million key/data pairs, and a tree with only a 50% page-fill factor will still hold 16 million key/data pairs. We rarely expect trees to exceed five levels, although Berkeley DB will support trees up to 255 levels.

The rule-of-thumb is that cache is good, and more cache is better. Generally, applications benefit from increasing the cache size up to a point, at which the performance will stop improving as the cache size increases. When this point is reached, one of two things have happened: either the cache is large enough that the application is almost never having to retrieve information from disk, or, your application is doing truly random accesses, and therefore increasing size of the cache doesn't significantly increase the odds of finding the next requested information in the cache. The latter is fairly rare -- almost all applications show some form of locality of reference.

That said, it is important not to increase your cache size beyond the capabilities of your system, as that will result in reduced performance. Under many operating systems, tying down enough virtual memory will cause your memory and potentially your program to be swapped. This is especially likely on systems without unified OS buffer caches and virtual memory spaces, as the buffer cache was allocated at boot time and so cannot be adjusted based on application requests for large amounts of virtual memory.

For example, even if accesses are truly random within a Btree, your access pattern will favor internal pages to leaf pages, so your cache should be large enough to hold all internal pages. In the steady state, this requires at most one I/O per operation to retrieve the appropriate leaf page.

You can use the <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility to monitor the effectiveness of your cache. The following output is excerpted from the output of that utility's **-m** option:

``` c
prompt: db_stat -m
131072  Cache size (128K).
4273    Requested pages found in the cache (97%).
134     Requested pages not found in the cache.
18      Pages created in the cache.
116     Pages read into the cache.
93      Pages written from the cache to the backing file.
5       Clean pages forced from the cache.
13      Dirty pages forced from the cache.
0       Dirty buffers written by trickle-sync thread.
130     Current clean buffer count.
4       Current dirty buffer count.
```

The statistics for this cache say that there have been 4,273 requests of the cache, and only 116 of those requests required an I/O from disk. This means that the cache is working well, yielding a 97% cache hit rate. The <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility will present these statistics both for the cache as a whole and for each file within the cache separately.

### Selecting a byte order

Database files created by Berkeley DB can be created in either little- or big-endian formats. The byte order used for the underlying database is specified by calling the <a href="../../api/c/dbset_lorder.md" class="olink">DB-&gt;set_lorder()</a> method. If no order is selected, the native format of the machine on which the database is created will be used.

Berkeley DB databases are architecture independent, and any format database can be used on a machine with a different native format. In this case, as each page that is read into or written from the cache must be converted to or from the host format, and databases with non-native formats will incur a performance penalty for the run-time conversion.

**It is important to note that the Berkeley DB access methods do no data conversion for application specified data. Key/data pairs written on a little-endian format architecture will be returned to the application exactly as they were written when retrieved on a big-endian format architecture.**

### Duplicate data items

The Btree and Hash access methods support the creation of multiple data items for a single key item. By default, multiple data items are not permitted, and each database store operation will overwrite any previous data item for that key. To configure Berkeley DB for duplicate data items, call the <a href="../../api/c/dbset_flags.md" class="olink">DB-&gt;set_flags()</a> method with the <a href="../../api/c/dbset_flags.md#dbset_flags_DB_DUP" class="olink">DB_DUP</a> flag. Only one copy of the key will be stored for each set of duplicate data items. If the Btree access method comparison routine returns that two keys compare equally, it is undefined which of the two keys will be stored and returned from future database operations.

By default, Berkeley DB stores duplicates in the order in which they were added, that is, each new duplicate data item will be stored after any already existing data items. This default behavior can be overridden by using the <a href="../../api/c/dbcput.md" class="olink">DBC-&gt;put()</a> method and one of the <a href="../../api/c/dbcput.md#dbcput_DB_AFTER" class="olink">DB_AFTER</a>, <a href="../../api/c/dbcput.md#dbcput_DB_BEFORE" class="olink">DB_BEFORE</a>, <a href="../../api/c/dbcput.md#dbcput_DB_KEYFIRST" class="olink">DB_KEYFIRST</a> or <a href="../../api/c/dbcput.md#dbcput_DB_KEYLAST" class="olink">DB_KEYLAST</a> flags. Alternatively, Berkeley DB may be configured to sort duplicate data items.

When stepping through the database sequentially, duplicate data items will be returned individually, as a key/data pair, where the key item only changes after the last duplicate data item has been returned. For this reason, duplicate data items cannot be accessed using the <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> method, as it always returns the first of the duplicate data items. Duplicate data items should be retrieved using a Berkeley DB cursor interface such as the <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> method.

There is a flag that permits applications to request the following data item only if it **is** a duplicate data item of the current entry, see <a href="../../api/c/dbcget.md#dbcget_DB_NEXT_DUP" class="olink">DB_NEXT_DUP</a> for more information. There is a flag that permits applications to request the following data item only if it **is not** a duplicate data item of the current entry, see <a href="../../api/c/dbcget.md#dbcget_DB_NEXT_NODUP" class="olink">DB_NEXT_NODUP</a> and <a href="../../api/c/dbcget.md#dbcget_DB_PREV_NODUP" class="olink">DB_PREV_NODUP</a> for more information.

It is also possible to maintain duplicate records in sorted order. Sorting duplicates will significantly increase performance when searching them and performing equality joins — both of which are common operations when using secondary indices. To configure Berkeley DB to sort duplicate data items, the application must call the <a href="../../api/c/dbset_flags.md" class="olink">DB-&gt;set_flags()</a> method with the <a href="../../api/c/dbset_flags.md#dbset_flags_DB_DUPSORT" class="olink">DB_DUPSORT</a> flag. Note that <a href="../../api/c/dbset_flags.md#dbset_flags_DB_DUPSORT" class="olink">DB_DUPSORT</a> automatically turns on the <a href="../../api/c/dbset_flags.md#dbset_flags_DB_DUP" class="olink">DB_DUP</a> flag for you, so you do not have to also set that flag; however, it is not an error to also set <a href="../../api/c/dbset_flags.md#dbset_flags_DB_DUP" class="olink">DB_DUP</a> when configuring for sorted duplicate records.

When configuring sorted duplicate records, you can also specify a custom comparison function using the <a href="../../api/c/dbset_dup_compare.md" class="olink">DB-&gt;set_dup_compare()</a> method. If the <a href="../../api/c/dbset_flags.md#dbset_flags_DB_DUPSORT" class="olink">DB_DUPSORT</a> flag is given, but no comparison routine is specified, then Berkeley DB defaults to the same lexicographical sorting used for Btree keys, with shorter items collating before longer items.

If the duplicate data items are unsorted, applications may store identical duplicate data items, or, for those that just like the way it sounds, <span class="emphasis">*duplicate duplicates*</span>.

**It is an error to attempt to store identical duplicate data items when duplicates are being stored in a sorted order.** Any such attempt results in the error message "Duplicate data items are not supported with sorted data" with a `DB_KEYEXIST` return code.

Note that you can suppress the error message "Duplicate data items are not supported with sorted data" by using the <a href="../../api/c/dbput.md#put_DB_NODUPDATA" class="olink">DB_NODUPDATA</a> flag. Use of this flag does not change the database's basic behavior; storing duplicate data items in a database configured for sorted duplicates is still an error and so you will continue to receive the `DB_KEYEXIST` return code if you try to do that.

For further information on how searching and insertion behaves in the presence of duplicates (sorted or not), see the <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a>, <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> and <a href="../../api/c/dbcput.md" class="olink">DBC-&gt;put()</a> documentation.

### Non-local memory allocation

Berkeley DB allocates memory for returning key/data pairs and statistical information which becomes the responsibility of the application. There are also interfaces where an application will allocate memory which becomes the responsibility of Berkeley DB.

On systems in which there may be multiple library versions of the standard allocation routines (notably Windows NT), transferring memory between the library and the application will fail because the Berkeley DB library allocates memory from a different heap than the application uses to free it, or vice versa. To avoid this problem, the <a href="../../api/c/envset_alloc.md" class="olink">DB_ENV-&gt;set_alloc()</a> and <a href="../../api/c/dbset_alloc.md" class="olink">DB-&gt;set_alloc()</a> methods can be used to give Berkeley DB references to the application's allocation routines.
