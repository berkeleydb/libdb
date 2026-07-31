---
title: "Chapter 2.  The DB Handle"
api-name: "Chapter 2.  The DB Handle"
source: docs/api_reference/C/db.html
---
## Chapter 2.  The DB Handle

The DB is the handle for a single Berkeley DB database. A Berkeley DB database provides a mechanism for organizing key-data pairs of information. From the perspective of some database systems, a Berkeley DB database could be thought of as a single table within a larger database.

You create a DB handle using the <a href="dbcreate.md" class="xref" title="db_create">db_create</a> function. For most database activities, you must then open the handle using the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method. When you are done with them, handles must be closed using the <a href="dbclose.md" class="xref" title="DB-&gt;close()">DB-&gt;close()</a> method.

Alternatively, you can create a DB and then rename, remove or verify the database without performing an open. See <a href="dbrename.md" class="xref" title="DB-&gt;rename()">DB-&gt;rename()</a>, <a href="dbremove.md" class="xref" title="DB-&gt;remove()">DB-&gt;remove()</a> or <a href="dbverify.md" class="xref" title="DB-&gt;verify()">DB-&gt;verify()</a> for information on these activities.

It is possible to create databases such that they are organized within a <span class="emphasis">*database environment*</span>. Environments are optional for simple Berkeley DB applications that do not use transactions, recovery, replication or any other advanced features. For simple Berkeley DB applications, environments still offer some advantages. For example, they provide some organizational benefits on-disk (all databases are located on disk relative to the environment). Also, if you are using multiple databases, then environments allow your databases to share a common in-memory cache, which makes for more efficient usage of your hardware's resources.

See <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> for information on using database environments.

You specify the underlying organization of the data in the database (e.g. BTree, Hash, Queue, and Recno) when you open the database. When you create a database, you are free to specify any of the available database types. On subsequent opens, you must either specify the access method used when you first opened the database, or you can specify `DB_UNKNOWN` in order to have this information retrieved for you. See the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method for information on specifying database types.

## Database and Related Methods

<table data-border="1" width="80%">
<thead>
<tr>
<th>Database Operations</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><a href="dbassociate.md" class="xref" title="DB-&gt;associate()">DB-&gt;associate()</a></td>
<td>Associate a secondary index</td>
</tr>
<tr>
<td><a href="dbassociate_foreign.md" class="xref" title="DB-&gt;associate_foreign()">DB-&gt;associate_foreign()</a></td>
<td>Associate a foreign index</td>
</tr>
<tr>
<td><a href="dbclose.md" class="xref" title="DB-&gt;close()">DB-&gt;close()</a></td>
<td>Close a database</td>
</tr>
<tr>
<td><a href="dbcompact.md" class="xref" title="DB-&gt;compact()">DB-&gt;compact()</a></td>
<td>Compact a database</td>
</tr>
<tr>
<td><a href="dbcreate.md" class="xref" title="db_create">db_create</a></td>
<td>Create a database handle</td>
</tr>
<tr>
<td><a href="dbdel.md" class="xref" title="DB-&gt;del()">DB-&gt;del()</a></td>
<td>Delete items from a database</td>
</tr>
<tr>
<td><a href="dberr.md" class="xref" title="DB-&gt;err()">DB-&gt;err()</a></td>
<td>Error message</td>
</tr>
<tr>
<td><a href="dbexists.md" class="xref" title="DB-&gt;exists()">DB-&gt;exists()</a></td>
<td>Return if an item appears in a database</td>
</tr>
<tr>
<td><a href="dbfd.md" class="xref" title="DB-&gt;fd()">DB-&gt;fd()</a></td>
<td>Return a file descriptor from a database</td>
</tr>
<tr>
<td><a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a></td>
<td>Get items from a database</td>
</tr>
<tr>
<td><a href="dbget_byteswapped.md" class="xref" title="DB-&gt;get_byteswapped()">DB-&gt;get_byteswapped()</a></td>
<td>Return if the underlying database is in host order</td>
</tr>
<tr>
<td><a href="dbget_dbname.md" class="xref" title="DB-&gt;get_dbname()">DB-&gt;get_dbname()</a></td>
<td>Return the file and database name</td>
</tr>
<tr>
<td><a href="dbget_multiple.md" class="xref" title="DB-&gt;get_multiple()">DB-&gt;get_multiple()</a></td>
<td>Return if the database handle references multiple databases</td>
</tr>
<tr>
<td><a href="dbget_open_flags.md" class="xref" title="DB-&gt;get_open_flags()">DB-&gt;get_open_flags()</a></td>
<td>Returns the flags specified to DB-&gt;open</td>
</tr>
<tr>
<td><a href="dbget_type.md" class="xref" title="DB-&gt;get_type()">DB-&gt;get_type()</a></td>
<td>Return the database type</td>
</tr>
<tr>
<td><a href="dbjoin.md" class="xref" title="DB-&gt;join()">DB-&gt;join()</a></td>
<td>Perform a database join on cursors</td>
</tr>
<tr>
<td><a href="dbkey_range.md" class="xref" title="DB-&gt;key_range()">DB-&gt;key_range()</a></td>
<td>Return estimate of key location</td>
</tr>
<tr>
<td><a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a></td>
<td>Open a database</td>
</tr>
<tr>
<td><a href="dbput.md" class="xref" title="DB-&gt;put()">DB-&gt;put()</a></td>
<td>Store items into a database</td>
</tr>
<tr>
<td><a href="dbremove.md" class="xref" title="DB-&gt;remove()">DB-&gt;remove()</a></td>
<td>Remove a database</td>
</tr>
<tr>
<td><a href="dbrename.md" class="xref" title="DB-&gt;rename()">DB-&gt;rename()</a></td>
<td>Rename a database</td>
</tr>
<tr>
<td><a href="dbset_priority.md" class="xref" title="DB-&gt;set_priority()">DB-&gt;set_priority()</a>, <a href="dbget_priority.md" class="xref" title="DB-&gt;get_priority()">DB-&gt;get_priority()</a></td>
<td>Set/get cache page priority</td>
</tr>
<tr>
<td><a href="dbstat.md" class="xref" title="DB-&gt;stat()">DB-&gt;stat()</a></td>
<td>Database statistics</td>
</tr>
<tr>
<td><a href="dbstat_print.md" class="xref" title="DB-&gt;stat_print()">DB-&gt;stat_print()</a></td>
<td>Display database statistics</td>
</tr>
<tr>
<td><a href="dbsync.md" class="xref" title="DB-&gt;sync()">DB-&gt;sync()</a></td>
<td>Flush a database to stable storage</td>
</tr>
<tr>
<td><a href="dbtruncate.md" class="xref" title="DB-&gt;truncate()">DB-&gt;truncate()</a></td>
<td>Empty a database</td>
</tr>
<tr>
<td><a href="dbupgrade.md" class="xref" title="DB-&gt;upgrade()">DB-&gt;upgrade()</a></td>
<td>Upgrade a database</td>
</tr>
<tr>
<td><a href="dbverify.md" class="xref" title="DB-&gt;verify()">DB-&gt;verify()</a></td>
<td>Verify/salvage a database</td>
</tr>
<tr>
<td><a href="dbcursor.md" class="xref" title="DB-&gt;cursor()">DB-&gt;cursor()</a></td>
<td>Create a cursor handle</td>
</tr>
<tr>
<td colspan="2"><strong>Database Configuration</strong></td>
</tr>
<tr>
<td><a href="dbset_alloc.md" class="xref" title="DB-&gt;set_alloc()">DB-&gt;set_alloc()</a></td>
<td>Set local space allocation functions</td>
</tr>
<tr>
<td><a href="dbset_cachesize.md" class="xref" title="DB-&gt;set_cachesize()">DB-&gt;set_cachesize()</a>, <a href="dbget_cachesize.md" class="xref" title="DB-&gt;get_cachesize()">DB-&gt;get_cachesize()</a></td>
<td>Set/get the database cache size</td>
</tr>
<tr>
<td><a href="dbset_create_dir.md" class="xref" title="DB-&gt;set_create_dir()">DB-&gt;set_create_dir()</a>, <a href="dbget_create_dir.md" class="xref" title="DB-&gt;get_create_dir()">DB-&gt;get_create_dir()</a></td>
<td>Set/get the directory in which a database is placed</td>
</tr>
<tr>
<td><a href="dbset_dup_compare.md" class="xref" title="DB-&gt;set_dup_compare()">DB-&gt;set_dup_compare()</a></td>
<td>Set a duplicate comparison function</td>
</tr>
<tr>
<td><a href="dbset_encrypt.md" class="xref" title="DB-&gt;set_encrypt()">DB-&gt;set_encrypt()</a>, <a href="dbget_encrypt_flags.md" class="xref" title="DB-&gt;get_encrypt_flags()">DB-&gt;get_encrypt_flags()</a></td>
<td>Set/get the database cryptographic key</td>
</tr>
<tr>
<td><a href="dbset_errcall.md" class="xref" title="DB-&gt;set_errcall()">DB-&gt;set_errcall()</a></td>
<td>Set error message callback</td>
</tr>
<tr>
<td><a href="dbset_errfile.md" class="xref" title="DB-&gt;set_errfile()">DB-&gt;set_errfile()</a>, <a href="dbget_errfile.md" class="xref" title="DB-&gt;get_errfile()">DB-&gt;get_errfile()</a></td>
<td>Set/get error message FILE</td>
</tr>
<tr>
<td><a href="dbset_errpfx.md" class="xref" title="DB-&gt;set_errpfx()">DB-&gt;set_errpfx()</a>, <a href="dbget_errpfx.md" class="xref" title="DB-&gt;get_errpfx()">DB-&gt;get_errpfx()</a></td>
<td>Set/get error message prefix</td>
</tr>
<tr>
<td><a href="dbset_feedback.md" class="xref" title="DB-&gt;set_feedback()">DB-&gt;set_feedback()</a></td>
<td>Set feedback callback</td>
</tr>
<tr>
<td><a href="dbset_flags.md" class="xref" title="DB-&gt;set_flags()">DB-&gt;set_flags()</a>, <a href="dbget_flags.md" class="xref" title="DB-&gt;get_flags()">DB-&gt;get_flags()</a></td>
<td>Set/get general database configuration</td>
</tr>
<tr>
<td><a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a>, <a href="dbget_lk_exclusive.md" class="xref" title="DB-&gt;get_lk_exclusive()">DB-&gt;get_lk_exclusive()</a></td>
<td>Set/get exclusive database locking</td>
</tr>
<tr>
<td><a href="dbset_lorder.md" class="xref" title="DB-&gt;set_lorder()">DB-&gt;set_lorder()</a>, <a href="dbget_lorder.md" class="xref" title="DB-&gt;get_lorder()">DB-&gt;get_lorder()</a></td>
<td>Set/get the database byte order</td>
</tr>
<tr>
<td><a href="dbset_msgcall.md" class="xref" title="DB-&gt;set_msgcall()">DB-&gt;set_msgcall()</a></td>
<td>Set informational message callback</td>
</tr>
<tr>
<td><a href="dbset_msgfile.md" class="xref" title="DB-&gt;set_msgfile()">DB-&gt;set_msgfile()</a>, <a href="dbget_msgfile.md" class="xref" title="DB-&gt;get_msgfile()">DB-&gt;get_msgfile()</a></td>
<td>Set/get informational message FILE</td>
</tr>
<tr>
<td><a href="dbset_pagesize.md" class="xref" title="DB-&gt;set_pagesize()">DB-&gt;set_pagesize()</a>, <a href="dbget_pagesize.md" class="xref" title="DB-&gt;get_pagesize()">DB-&gt;get_pagesize()</a></td>
<td>Set/get the underlying database page size</td>
</tr>
<tr>
<td><a href="dbset_partition.md" class="xref" title="DB-&gt;set_partition()">DB-&gt;set_partition()</a></td>
<td>Set database partitioning</td>
</tr>
<tr>
<td><a href="dbset_partition_dirs.md" class="xref" title="DB-&gt;set_partition_dirs()">DB-&gt;set_partition_dirs()</a>, <a href="dbget_partition_dirs.md" class="xref" title="DB-&gt;get_partition_dirs()">DB-&gt;get_partition_dirs()</a></td>
<td>Set/get the directories used for database partitions</td>
</tr>
<tr>
<td colspan="2"><strong>Btree/Recno Configuration</strong></td>
</tr>
<tr>
<td><a href="dbset_append_recno.md" class="xref" title="DB-&gt;set_append_recno()">DB-&gt;set_append_recno()</a></td>
<td>Set record append callback</td>
</tr>
<tr>
<td><a href="dbset_bt_compare.md" class="xref" title="DB-&gt;set_bt_compare()">DB-&gt;set_bt_compare()</a></td>
<td>Set a Btree comparison function</td>
</tr>
<tr>
<td><a href="dbset_bt_compress.md" class="xref" title="DB-&gt;set_bt_compress()">DB-&gt;set_bt_compress()</a></td>
<td>Set Btree compression functions</td>
</tr>
<tr>
<td><a href="dbset_bt_minkey.md" class="xref" title="DB-&gt;set_bt_minkey()">DB-&gt;set_bt_minkey()</a>, <a href="dbget_bt_minkey.md" class="xref" title="DB-&gt;get_bt_minkey()">DB-&gt;get_bt_minkey()</a></td>
<td>Set/get the minimum number of keys per Btree page</td>
</tr>
<tr>
<td><a href="dbset_bt_prefix.md" class="xref" title="DB-&gt;set_bt_prefix()">DB-&gt;set_bt_prefix()</a></td>
<td>Set a Btree prefix comparison function</td>
</tr>
<tr>
<td><a href="dbset_re_delim.md" class="xref" title="DB-&gt;set_re_delim()">DB-&gt;set_re_delim()</a>, <a href="dbget_re_delim.md" class="xref" title="DB-&gt;get_re_delim()">DB-&gt;get_re_delim()</a></td>
<td>Set/get the variable-length record delimiter</td>
</tr>
<tr>
<td><a href="dbset_re_len.md" class="xref" title="DB-&gt;set_re_len()">DB-&gt;set_re_len()</a>, <a href="dbget_re_len.md" class="xref" title="DB-&gt;get_re_len()">DB-&gt;get_re_len()</a></td>
<td>Set/get the fixed-length record length</td>
</tr>
<tr>
<td><a href="dbset_re_pad.md" class="xref" title="DB-&gt;set_re_pad()">DB-&gt;set_re_pad()</a>, <a href="dbget_re_pad.md" class="xref" title="DB-&gt;get_re_pad()">DB-&gt;get_re_pad()</a></td>
<td>Set/get the fixed-length record pad byte</td>
</tr>
<tr>
<td><a href="dbset_re_source.md" class="xref" title="DB-&gt;set_re_source()">DB-&gt;set_re_source()</a>, <a href="dbget_re_source.md" class="xref" title="DB-&gt;get_re_source()">DB-&gt;get_re_source()</a></td>
<td>Set/get the backing Recno text file</td>
</tr>
<tr>
<td colspan="2"><strong>Hash Configuration</strong></td>
</tr>
<tr>
<td><a href="dbset_h_compare.md" class="xref" title="DB-&gt;set_h_compare()">DB-&gt;set_h_compare()</a></td>
<td>Set a Hash comparison function</td>
</tr>
<tr>
<td><a href="dbset_h_ffactor.md" class="xref" title="DB-&gt;set_h_ffactor()">DB-&gt;set_h_ffactor()</a>, <a href="dbget_h_ffactor.md" class="xref" title="DB-&gt;get_h_ffactor()">DB-&gt;get_h_ffactor()</a></td>
<td>Set/get the Hash table density</td>
</tr>
<tr>
<td><a href="dbset_h_hash.md" class="xref" title="DB-&gt;set_h_hash()">DB-&gt;set_h_hash()</a></td>
<td>Set a hashing function</td>
</tr>
<tr>
<td><a href="dbset_h_nelem.md" class="xref" title="DB-&gt;set_h_nelem()">DB-&gt;set_h_nelem()</a>, <a href="dbget_h_nelem.md" class="xref" title="DB-&gt;get_h_nelem()">DB-&gt;get_h_nelem()</a></td>
<td>Set/get the Hash table size</td>
</tr>
<tr>
<td colspan="2"><strong>Queue Configuration</strong></td>
</tr>
<tr>
<td><a href="dbset_q_extentsize.md" class="xref" title="DB-&gt;set_q_extentsize()">DB-&gt;set_q_extentsize()</a>, <a href="dbget_q_extentsize.md" class="xref" title="DB-&gt;get_q_extentsize()">DB-&gt;get_q_extentsize()</a></td>
<td>Set/get Queue database extent size</td>
</tr>
<tr>
<td colspan="2"><strong>Heap</strong></td>
</tr>
<tr>
<td><a href="dbset_heapsize.md" class="xref" title="DB-&gt;set_heapsize()">DB-&gt;set_heapsize()</a>, <a href="dbget_heapsize.md" class="xref" title="DB-&gt;get_heapsize()">DB-&gt;get_heapsize()</a></td>
<td>Set/get the database heap size</td>
</tr>
<tr>
<td><a href="dbset_heap_regionsize.md" class="xref" title="DB-&gt;set_heap_regionsize()">DB-&gt;set_heap_regionsize()</a>, <a href="dbget_heap_regionsize.md" class="xref" title="DB-&gt;get_heap_regionsize()">DB-&gt;get_heap_regionsize()</a></td>
<td>Set/get the database region size</td>
</tr>
<tr>
<td><a href="db_heap_rid.md" class="xref" title="DB_HEAP_RID">DB_HEAP_RID</a></td>
<td></td>
</tr>
<tr>
<td colspan="2"><strong>Database Utilities</strong></td>
</tr>
<tr>
<td><a href="db_copy.md" class="xref" title="db_copy">db_copy</a></td>
<td>Copy a named database to a target directory</td>
</tr>
</tbody>
</table>
