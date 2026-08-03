---
title: "Reviewing Berkeley DB log files"
api-name: "Reviewing Berkeley DB log files"
source: docs/installation/debug_printlog.html
---
## Reviewing Berkeley DB log files

<span class="sect2"> [Augmenting the Log for Debugging](debug_printlog.md#idp121880) </span>

<span class="sect2"> [Extracting Committed Transactions and Transaction Status](debug_printlog.md#idp53840) </span>

<span class="sect2"> [Extracting Transaction Histories](debug_printlog.md#idp41744) </span>

<span class="sect2"> [Extracting File Histories](debug_printlog.md#idp154152) </span>

<span class="sect2"> [Extracting Page Histories](debug_printlog.md#idp158032) </span>

<span class="sect2"> [Other log processing tools](debug_printlog.md#idp124648) </span>

If you are running with transactions and logging, the <a href="../../api/c/db_printlog.md" class="olink">db_printlog</a> utility can be a useful debugging aid. The <a href="../../api/c/db_printlog.md" class="olink">db_printlog</a> utility will display the contents of your log files in a human readable (and machine-readable) format.

The <a href="../../api/c/db_printlog.md" class="olink">db_printlog</a> utility will attempt to display any and all log files present in a designated db_home directory. For each log record, the <a href="../../api/c/db_printlog.md" class="olink">db_printlog</a> utility will display a line of the form:

``` c
[22][28]db_big: rec: 43 txnid 80000963 prevlsn [21][10483281]
```

The opening numbers in square brackets are the <span class="emphasis">*log sequence number*</span> (<span class="emphasis">*LSN*</span>) of the log record being displayed. The first number indicates the log file in which the record appears, and the second number indicates the offset in that file of the record.

The first character string identifies the particular log operation being reported. The log records corresponding to particular operations are described following. The rest of the line consists of name/value pairs.

The rec field indicates the record type (this is used to dispatch records in the log to appropriate recovery functions).

The txnid field identifies the transaction for which this record was written. A txnid of 0 means that the record was written outside the context of any transaction. You will see these most frequently for checkpoints.

Finally, the prevlsn contains the LSN of the last record for this transaction. By following prevlsn fields, you can accumulate all the updates for a particular transaction. During normal abort processing, this field is used to quickly access all the records for a particular transaction.

After the initial line identifying the record type, each field of the log record is displayed, one item per line. There are several fields that appear in many different records and a few fields that appear only in some records.

The following table presents each currently written log record type with a brief description of the operation it describes. Any of these record types may have the string "\_debug" appended if they were written because <a href="../../api/c/dbset_flags.md#dbset_flags_DB_TXN_NOT_DURABLE" class="olink">DB_TXN_NOT_DURABLE</a> was specified and the system was configured with <a href="build_unix_conf.md#build_unix_conf.--enable-diagnostic" class="link">--enable-diagnostic</a>.

| Log Record Type | Description |
|----|----|
| bam_adj | Used when we insert/remove an index into/from the page header of a Btree page. |
| bam_cadjust | Keeps track of record counts in a Btree or Recno database. |
| bam_cdel | Used to mark a record on a page as deleted. |
| bam_curadj | Used to adjust a cursor location when a nearby record changes in a Btree database. |
| bam_merge | Used to merge two Btree database pages during compaction. |
| bam_pgno | Used to replace a page number in a Btree record. |
| bam_rcuradj | Used to adjust a cursor location when a nearby record changes in a Recno database. |
| bam_relink | Fix leaf page prev/next chain when a page is removed. |
| bam_repl | Describes a replace operation on a record. |
| bam_root | Describes an assignment of a root page. |
| bam_rsplit | Describes a reverse page split. |
| bam_split | Describes a page split. |
| crdel_inmem_create | Record the creation of an in-memory named database. |
| crdel_inmem_remove | Record the removal of an in-memory named database. |
| crdel_inmem_rename | Record the rename of an in-memory named database. |
| crdel_metasub | Describes the creation of a metadata page for a subdatabase. |
| db_addrem | Add or remove an item from a page of duplicates. |
| db_big | Add an item to an overflow page (<span class="emphasis">*overflow pages*</span> contain items too large to place on the main page) |
| db_cksum | Unable to checksum a page. |
| db_debug | Log debugging message. |
| db_noop | This marks an operation that did nothing but update the LSN on a page. |
| db_ovref | Increment or decrement the reference count for a big item. |
| db_pg_alloc | Indicates we allocated a page to a database. |
| db_pg_free | Indicates we freed a page (freed pages are added to a freelist and reused). |
| db_pg_freedata | Indicates we freed a page that still contained data entries (freed pages are added to a freelist and reused.) |
| db_pg_init | Indicates we reinitialized a page during a truncate. |
| db_pg_sort | Sort the free page list and free pages at the end of the file. |
| dbreg_register | Records an open of a file (mapping the filename to a log-id that is used in subsequent log operations). |
| fop_create | Create a file in the file system. |
| fop_file_remove | Remove a name in the file system. |
| fop_remove | Remove a file in the file system. |
| fop_rename | Rename a file in the file system. |
| fop_write | Write bytes to an object in the file system. |
| ham_chgpg | Used to adjust a cursor location when a Hash page is removed, and its elements are moved to a different Hash page. |
| ham_copypage | Used when we empty a bucket page, but there are overflow pages for the bucket; one needs to be copied back into the actual bucket. |
| ham_curadj | Used to adjust a cursor location when a nearby record changes in a Hash database. |
| ham_groupalloc | Allocate some number of contiguous pages to the Hash database. |
| ham_insdel | Insert/delete an item on a Hash page. |
| ham_metagroup | Update the metadata page to reflect the allocation of a sequence of contiguous pages. |
| ham_newpage | Adds or removes overflow pages from a Hash bucket. |
| ham_replace | Handle updates to records that are on the main page. |
| ham_splitdata | Record the page data for a split. |
| heap_addrem | Add or remove an entry from a Heap database. |
| heap_pg_alloc | Indicates we allocated a page to a Heap database. |
| heap_trunc_meta | Records the truncation of the meta page in a Heap database. |
| heap_trunc_page | Records the truncation of a data page in a Heap database. |
| qam_add | Describes the actual addition of a new record to a Queue. |
| qam_del | Delete a record in a Queue. |
| qam_delext | Delete a record in a Queue with extents. |
| qam_incfirst | Increments the record number that refers to the first record in the database. |
| qam_mvptr | Indicates we changed the reference to either or both of the first and current records in the file. |
| txn_child | Commit a child transaction. |
| txn_ckp | Transaction checkpoint. |
| txn_recycle | Transaction IDs wrapped. |
| txn_regop | Logs a regular (non-child) transaction commit. |
| txn_xa_regop | Logs a prepare message. |

### Augmenting the Log for Debugging

When debugging applications, it is sometimes useful to log not only the actual operations that modify pages, but also the underlying Berkeley DB functions being executed. This form of logging can add significant bulk to your log, but can permit debugging application errors that are almost impossible to find any other way. To turn on these log messages, specify the --enable-debug_rop and --enable-debug_wop configuration options when configuring Berkeley DB. See <a href="build_unix_conf.md" class="xref" title="Configuring Berkeley DB">Configuring Berkeley DB</a> for more information.

### Extracting Committed Transactions and Transaction Status

Sometimes, it is helpful to use the human-readable log output to determine which transactions committed and aborted. The awk script, commit.awk, (found in the db_printlog directory of the Berkeley DB distribution) allows you to do just that. The following command, where log_output is the output of db_printlog, will display a list of the transaction IDs of all committed transactions found in the log:

``` c
awk -f commit.awk log_output
```

If you need a complete list of both committed and aborted transactions, then the script status.awk will produce it. The syntax is as follows:

``` c
awk -f status.awk log_output
```

### Extracting Transaction Histories

Another useful debugging aid is to print out the complete history of a transaction. The awk script txn.awk allows you to do that. The following command line, where log_output is the output of the <a href="../../api/c/db_printlog.md" class="olink">db_printlog</a> utility and txnlist is a comma-separated list of transaction IDs, will display all log records associated with the designated transaction ids:

``` c
awk -f txn.awk TXN=txnlist log_output
```

### Extracting File Histories

The awk script fileid.awk allows you to extract all log records that refer to a designated file. The syntax for the fileid.awk script is the following, where log_output is the output of db_printlog and fids is a comma-separated list of fileids:

``` c
awk -f fileid.awk PGNO=fids log_output
```

### Extracting Page Histories

The awk script pgno.awk allows you to extract all log records that refer to designated page numbers. However, because this script will extract records with the designated page numbers for all files, it is most useful in conjunction with the fileid script. The syntax for the pgno.awk script is the following, where log_output is the output of db_printlog and pgnolist is a comma-separated list of page numbers:

``` c
awk -f pgno.awk PGNO=pgnolist log_output
```

### Other log processing tools

The awk script count.awk prints out the number of log records encountered that belonged to some transaction (that is, the number of log records excluding those for checkpoints and non-transaction-protected operations).

The script range.awk will extract a subset of a log. This is useful when the output of <a href="../../api/c/db_printlog.md" class="olink">db_printlog</a> utility is too large to be reasonably manipulated with an editor or other tool. The syntax for range.awk is the following, where **sf** and **so** represent the LSN of the beginning of the sublog you want to extract, and **ef** and **eo** represent the LSN of the end of the sublog you want to extract:

``` c
    awk -f range.awk START_FILE=sf START_OFFSET=so END_FILE=ef \
    END_OFFSET=eo log_output 
```
