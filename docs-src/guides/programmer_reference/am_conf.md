---
title: "Chapter 2.  Access Method Configuration"
api-name: "Chapter 2.  Access Method Configuration"
source: docs/programmer_reference/am_conf.html
---
## Chapter 2.  Access Method Configuration

**Table of Contents**

<span class="sect1"> [What are the available access methods?](am_conf.md#am_conf_intro) </span>

<span class="sect2"> [Btree](am_conf.md#idp50599376) </span>

<span class="sect2"> [Hash](am_conf.md#idp50705400) </span>

<span class="sect2"> [Heap](am_conf.md#idp50708952) </span>

<span class="sect2"> [Queue](am_conf.md#idm1385000) </span>

<span class="sect2"> [Recno](am_conf.md#idp50715336) </span>

<span class="sect1"> [Selecting an access method](am_conf_select.md) </span>

<span class="sect2"> [Btree or Heap?](am_conf_select.md#idp50702528) </span>

<span class="sect2"> [Hash or Btree?](am_conf_select.md#idp50755552) </span>

<span class="sect2"> [Queue or Recno?](am_conf_select.md#idp50569200) </span>

<span class="sect1"> [Logical record numbers](am_conf_logrec.md) </span>

<span class="sect1"> [General access method configuration](general_am_conf.md) </span>

<span class="sect2"> [Selecting a page size](general_am_conf.md#am_conf_pagesize) </span>

<span class="sect2"> [Selecting a cache size](general_am_conf.md#am_conf_cachesize) </span>

<span class="sect2"> [Selecting a byte order](general_am_conf.md#am_conf_byteorder) </span>

<span class="sect2"> [Duplicate data items](general_am_conf.md#am_conf_dup) </span>

<span class="sect2"> [Non-local memory allocation](general_am_conf.md#am_conf_malloc) </span>

<span class="sect1"> [Btree access method specific configuration](bt_conf.md) </span>

<span class="sect2"> [Btree comparison](bt_conf.md#am_conf_bt_compare) </span>

<span class="sect2"> [Btree prefix comparison](bt_conf.md#am_conf_bt_prefix) </span>

<span class="sect2"> [Minimum keys per page](bt_conf.md#am_conf_bt_minkey) </span>

<span class="sect2"> [Retrieving Btree records by logical record number](bt_conf.md#am_conf_bt_recnum) </span>

<span class="sect2"> [Compression](bt_conf.md#am_conf_bt_compress) </span>

<span class="sect1"> [Hash access method specific configuration](hash_conf.md) </span>

<span class="sect2"> [Page fill factor](hash_conf.md#am_conf_h_ffactor) </span>

<span class="sect2"> [Specifying a database hash](hash_conf.md#am_conf_h_hash) </span>

<span class="sect2"> [Hash table size](hash_conf.md#am_conf_h_nelem) </span>

<span class="sect1"> [Heap access method specific configuration](heap_conf.md) </span>

<span class="sect1"> [Queue and Recno access method specific configuration](rq_conf.md) </span>

<span class="sect2"> [Managing record-based databases](rq_conf.md#am_conf_recno) </span>

<span class="sect2"> [Selecting a Queue extent size](rq_conf.md#am_conf_extentsize) </span>

<span class="sect2"> [Flat-text backing files](rq_conf.md#am_conf_re_source) </span>

<span class="sect2"> [Logically renumbering records](rq_conf.md#am_conf_renumber) </span>

## What are the available access methods?

<span class="sect2"> [Btree](am_conf.md#idp50599376) </span>

<span class="sect2"> [Hash](am_conf.md#idp50705400) </span>

<span class="sect2"> [Heap](am_conf.md#idp50708952) </span>

<span class="sect2"> [Queue](am_conf.md#idm1385000) </span>

<span class="sect2"> [Recno](am_conf.md#idp50715336) </span>

Berkeley DB currently offers five access methods: Btree, Hash, Heap, Queue and Recno.

### Btree

The Btree access method is an implementation of a sorted, balanced tree structure. Searches, insertions, and deletions in the tree all take <span class="emphasis">*O(height)*</span> time, where <span class="emphasis">*height*</span> is the number of levels in the Btree from the root to the leaf pages. The upper bound on the height is <span class="emphasis">*log base_b N*</span>, where <span class="emphasis">*base_b*</span> is the smallest number of keys on a page, and <span class="emphasis">*N*</span> is the total number of keys stored.

Inserting unordered data into a Btree can result in pages that are only half-full. DB makes ordered (or inverse ordered) insertion the best case, resulting in nearly full-page space utilization.

### Hash

The Hash access method data structure is an implementation of Extended Linear Hashing, as described in "Linear Hashing: A New Tool for File and Table Addressing", Witold Litwin, <span class="emphasis">*Proceedings of the 6th International Conference on Very Large Databases (VLDB)*</span>, 1980.

### Heap

The Heap access method stores records in a heap file. Records are referenced solely by the page and offset at which they are written. Because records are written in a heap file, compaction is not necessary when deleting records, which allows for more efficient use of space than if Btree is in use. The Heap access method is intended for platforms with constrained disk space, especially if those systems are performing a great many record creation and deletions.

### Queue

The Queue access method stores fixed-length records with logical record numbers as keys. It is designed for fast inserts at the tail and has a special cursor consume operation that deletes and returns a record from the head of the queue. The Queue access method uses record level locking.

### Recno

The Recno access method stores both fixed and variable-length records with logical record numbers as keys, optionally backed by a flat text (byte stream) file.
