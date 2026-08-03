---
title: "Berkeley DB C API Reference"
api-name: "Berkeley DB C API Reference"
source: docs/api_reference/C/index.html
---
# Berkeley DB C API Reference

| DB C API | Description |
|----|----|
| <a href="dbassociate.md" class="xref" title="DB-&gt;associate()">DB-&gt;associate()</a> | Associate a secondary index |
| <a href="dbassociate_foreign.md" class="xref" title="DB-&gt;associate_foreign()">DB-&gt;associate_foreign()</a> | Associate a foreign index |
| <a href="dbclose.md" class="xref" title="DB-&gt;close()">DB-&gt;close()</a> | Close a database |
| <a href="dbcompact.md" class="xref" title="DB-&gt;compact()">DB-&gt;compact()</a> | Compact a database |
| <a href="db_copy.md" class="xref" title="db_copy">db_copy</a> | Copy a named database to a target directory |
| <a href="dbcreate.md" class="xref" title="db_create">db_create</a> | Create a database handle |
| <a href="dbdel.md" class="xref" title="DB-&gt;del()">DB-&gt;del()</a> | Delete items from a database |
| <a href="dberr.md" class="xref" title="DB-&gt;err()">DB-&gt;err()</a> | Error message |
| <a href="dbexists.md" class="xref" title="DB-&gt;exists()">DB-&gt;exists()</a> | Return if an item appears in a database |
| <a href="dbfd.md" class="xref" title="DB-&gt;fd()">DB-&gt;fd()</a> | Return a file descriptor from a database |
| <a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a> | Get items from a database |
| <a href="dbget_byteswapped.md" class="xref" title="DB-&gt;get_byteswapped()">DB-&gt;get_byteswapped()</a> | Return if the underlying database is in host order |
| <a href="dbget_dbname.md" class="xref" title="DB-&gt;get_dbname()">DB-&gt;get_dbname()</a> | Return the file and database name |
| <a href="dbget_multiple.md" class="xref" title="DB-&gt;get_multiple()">DB-&gt;get_multiple()</a> | Return if the database handle references multiple databases |
| <a href="dbget_open_flags.md" class="xref" title="DB-&gt;get_open_flags()">DB-&gt;get_open_flags()</a> | Returns the flags specified to DB-\>open |
| <a href="dbget_type.md" class="xref" title="DB-&gt;get_type()">DB-&gt;get_type()</a> | Return the database type |
| <a href="dbjoin.md" class="xref" title="DB-&gt;join()">DB-&gt;join()</a> | Perform a database join on cursors |
| <a href="dbkey_range.md" class="xref" title="DB-&gt;key_range()">DB-&gt;key_range()</a> | Return estimate of key location |
| <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> | Open a database |
| <a href="dbput.md" class="xref" title="DB-&gt;put()">DB-&gt;put()</a> | Store items into a database |
| <a href="dbremove.md" class="xref" title="DB-&gt;remove()">DB-&gt;remove()</a> | Remove a database |
| <a href="dbrename.md" class="xref" title="DB-&gt;rename()">DB-&gt;rename()</a> | Rename a database |
| <a href="dbset_alloc.md" class="xref" title="DB-&gt;set_alloc()">DB-&gt;set_alloc()</a> | Set local space allocation functions |
| <a href="dbset_append_recno.md" class="xref" title="DB-&gt;set_append_recno()">DB-&gt;set_append_recno()</a> | Set record append callback |
| <a href="dbset_bt_compare.md" class="xref" title="DB-&gt;set_bt_compare()">DB-&gt;set_bt_compare()</a> | Set a Btree comparison function |
| <a href="dbset_bt_compress.md" class="xref" title="DB-&gt;set_bt_compress()">DB-&gt;set_bt_compress()</a> | Set Btree compression functions |
| <a href="dbset_bt_minkey.md" class="xref" title="DB-&gt;set_bt_minkey()">DB-&gt;set_bt_minkey()</a>, <a href="dbget_bt_minkey.md" class="xref" title="DB-&gt;get_bt_minkey()">DB-&gt;get_bt_minkey()</a> | Set/get the minimum number of keys per Btree page |
| <a href="dbset_bt_prefix.md" class="xref" title="DB-&gt;set_bt_prefix()">DB-&gt;set_bt_prefix()</a> | Set a Btree prefix comparison function |
| <a href="dbset_cachesize.md" class="xref" title="DB-&gt;set_cachesize()">DB-&gt;set_cachesize()</a>, <a href="dbget_cachesize.md" class="xref" title="DB-&gt;get_cachesize()">DB-&gt;get_cachesize()</a> | Set/get the database cache size |
| <a href="dbset_create_dir.md" class="xref" title="DB-&gt;set_create_dir()">DB-&gt;set_create_dir()</a>, <a href="dbget_create_dir.md" class="xref" title="DB-&gt;get_create_dir()">DB-&gt;get_create_dir()</a> | Set/get the directory in which a database is placed |
| <a href="dbset_dup_compare.md" class="xref" title="DB-&gt;set_dup_compare()">DB-&gt;set_dup_compare()</a> | Set a duplicate comparison function |
| <a href="dbset_encrypt.md" class="xref" title="DB-&gt;set_encrypt()">DB-&gt;set_encrypt()</a>, <a href="dbget_encrypt_flags.md" class="xref" title="DB-&gt;get_encrypt_flags()">DB-&gt;get_encrypt_flags()</a> | Set/get the database cryptographic key |
| <a href="dbset_errcall.md" class="xref" title="DB-&gt;set_errcall()">DB-&gt;set_errcall()</a> | Set error message callback |
| <a href="dbset_errfile.md" class="xref" title="DB-&gt;set_errfile()">DB-&gt;set_errfile()</a>, <a href="dbget_errfile.md" class="xref" title="DB-&gt;get_errfile()">DB-&gt;get_errfile()</a> | Set/get error message FILE |
| <a href="dbset_errpfx.md" class="xref" title="DB-&gt;set_errpfx()">DB-&gt;set_errpfx()</a>, <a href="dbget_errpfx.md" class="xref" title="DB-&gt;get_errpfx()">DB-&gt;get_errpfx()</a> | Set/get error message prefix |
| <a href="dbset_feedback.md" class="xref" title="DB-&gt;set_feedback()">DB-&gt;set_feedback()</a> | Set feedback callback |
| <a href="dbset_flags.md" class="xref" title="DB-&gt;set_flags()">DB-&gt;set_flags()</a>, <a href="dbget_flags.md" class="xref" title="DB-&gt;get_flags()">DB-&gt;get_flags()</a> | Set/get general database configuration |
| <a href="dbset_h_compare.md" class="xref" title="DB-&gt;set_h_compare()">DB-&gt;set_h_compare()</a> | Set a Hash comparison function |
| <a href="dbset_h_ffactor.md" class="xref" title="DB-&gt;set_h_ffactor()">DB-&gt;set_h_ffactor()</a>, <a href="dbget_h_ffactor.md" class="xref" title="DB-&gt;get_h_ffactor()">DB-&gt;get_h_ffactor()</a> | Set/get the Hash table density |
| <a href="dbset_h_hash.md" class="xref" title="DB-&gt;set_h_hash()">DB-&gt;set_h_hash()</a> | Set a hashing function |
| <a href="dbset_h_nelem.md" class="xref" title="DB-&gt;set_h_nelem()">DB-&gt;set_h_nelem()</a>, <a href="dbget_h_nelem.md" class="xref" title="DB-&gt;get_h_nelem()">DB-&gt;get_h_nelem()</a> | Set/get the Hash table size |
| <a href="dbset_heapsize.md" class="xref" title="DB-&gt;set_heapsize()">DB-&gt;set_heapsize()</a>, <a href="dbget_heapsize.md" class="xref" title="DB-&gt;get_heapsize()">DB-&gt;get_heapsize()</a> | Set/get the database heap size |
| <a href="dbset_heap_regionsize.md" class="xref" title="DB-&gt;set_heap_regionsize()">DB-&gt;set_heap_regionsize()</a>, <a href="dbget_heap_regionsize.md" class="xref" title="DB-&gt;get_heap_regionsize()">DB-&gt;get_heap_regionsize()</a> | Set/get the database region size |
| <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a>, <a href="dbget_lk_exclusive.md" class="xref" title="DB-&gt;get_lk_exclusive()">DB-&gt;get_lk_exclusive()</a> | Set/get exclusive database locking |
| <a href="dbset_lorder.md" class="xref" title="DB-&gt;set_lorder()">DB-&gt;set_lorder()</a>, <a href="dbget_lorder.md" class="xref" title="DB-&gt;get_lorder()">DB-&gt;get_lorder()</a> | Set/get the database byte order |
| <a href="dbset_msgcall.md" class="xref" title="DB-&gt;set_msgcall()">DB-&gt;set_msgcall()</a> | Set informational message callback |
| <a href="dbset_msgfile.md" class="xref" title="DB-&gt;set_msgfile()">DB-&gt;set_msgfile()</a>, <a href="dbget_msgfile.md" class="xref" title="DB-&gt;get_msgfile()">DB-&gt;get_msgfile()</a> | Set/get informational message FILE |
| <a href="dbset_pagesize.md" class="xref" title="DB-&gt;set_pagesize()">DB-&gt;set_pagesize()</a>, <a href="dbget_pagesize.md" class="xref" title="DB-&gt;get_pagesize()">DB-&gt;get_pagesize()</a> | Set/get the underlying database page size |
| <a href="dbset_partition.md" class="xref" title="DB-&gt;set_partition()">DB-&gt;set_partition()</a> | Set database partitioning |
| <a href="dbset_partition_dirs.md" class="xref" title="DB-&gt;set_partition_dirs()">DB-&gt;set_partition_dirs()</a>, <a href="dbget_partition_dirs.md" class="xref" title="DB-&gt;get_partition_dirs()">DB-&gt;get_partition_dirs()</a> | Set/get the directories used for database partitions |
| <a href="dbset_priority.md" class="xref" title="DB-&gt;set_priority()">DB-&gt;set_priority()</a>, <a href="dbget_priority.md" class="xref" title="DB-&gt;get_priority()">DB-&gt;get_priority()</a> | Set/get cache page priority |
| <a href="dbset_q_extentsize.md" class="xref" title="DB-&gt;set_q_extentsize()">DB-&gt;set_q_extentsize()</a>, <a href="dbget_q_extentsize.md" class="xref" title="DB-&gt;get_q_extentsize()">DB-&gt;get_q_extentsize()</a> | Set/get Queue database extent size |
| <a href="dbset_re_delim.md" class="xref" title="DB-&gt;set_re_delim()">DB-&gt;set_re_delim()</a>, <a href="dbget_re_delim.md" class="xref" title="DB-&gt;get_re_delim()">DB-&gt;get_re_delim()</a> | Set/get the variable-length record delimiter |
| <a href="dbset_re_len.md" class="xref" title="DB-&gt;set_re_len()">DB-&gt;set_re_len()</a>, <a href="dbget_re_len.md" class="xref" title="DB-&gt;get_re_len()">DB-&gt;get_re_len()</a> | Set/get the fixed-length record length |
| <a href="dbset_re_pad.md" class="xref" title="DB-&gt;set_re_pad()">DB-&gt;set_re_pad()</a>, <a href="dbget_re_pad.md" class="xref" title="DB-&gt;get_re_pad()">DB-&gt;get_re_pad()</a> | Set/get the fixed-length record pad byte |
| <a href="dbset_re_source.md" class="xref" title="DB-&gt;set_re_source()">DB-&gt;set_re_source()</a>, <a href="dbget_re_source.md" class="xref" title="DB-&gt;get_re_source()">DB-&gt;get_re_source()</a> | Set/get the backing Recno text file |
| <a href="dbsort_multiple.md" class="xref" title="DB-&gt;sort_multiple()">DB-&gt;sort_multiple()</a> | Sort a set of DBTs |
| <a href="dbstat.md" class="xref" title="DB-&gt;stat()">DB-&gt;stat()</a> | Database statistics |
| <a href="dbstat_print.md" class="xref" title="DB-&gt;stat_print()">DB-&gt;stat_print()</a> | Display database statistics |
| <a href="dbsync.md" class="xref" title="DB-&gt;sync()">DB-&gt;sync()</a> | Flush a database to stable storage |
| <a href="dbtruncate.md" class="xref" title="DB-&gt;truncate()">DB-&gt;truncate()</a> | Empty a database |
| <a href="dbupgrade.md" class="xref" title="DB-&gt;upgrade()">DB-&gt;upgrade()</a> | Upgrade a database |
| <a href="dbverify.md" class="xref" title="DB-&gt;verify()">DB-&gt;verify()</a> | Verify/salvage a database |
| <a href="db_heap_rid.md" class="xref" title="DB_HEAP_RID">DB_HEAP_RID</a> |  |
| <a href="dbcursor.md" class="xref" title="DB-&gt;cursor()">DB-&gt;cursor()</a> | Create a cursor handle |
| <a href="dbcclose.md" class="xref" title="DBcursor-&gt;close()">DBcursor-&gt;close()</a> | Close a cursor handle |
| <a href="dbccmp.md" class="xref" title="DBcursor-&gt;cmp()">DBcursor-&gt;cmp()</a> | Compare two cursors for equality. |
| <a href="dbccount.md" class="xref" title="DBcursor-&gt;count()">DBcursor-&gt;count()</a> | Return count of duplicates for current key |
| <a href="dbcdel.md" class="xref" title="DBcursor-&gt;del()">DBcursor-&gt;del()</a> | Delete current key/data pair |
| <a href="dbcdup.md" class="xref" title="DBcursor-&gt;dup()">DBcursor-&gt;dup()</a> | Duplicate the cursor handle |
| <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> | Retrieve by cursor |
| <a href="dbcput.md" class="xref" title="DBcursor-&gt;put()">DBcursor-&gt;put()</a> | Store by cursor |
| <a href="dbcset_priority.md" class="xref" title="DBcursor-&gt;set_priority()">DBcursor-&gt;set_priority()</a>, <a href="dbcget_priority.md" class="xref" title="DBcursor-&gt;get_priority()">DBcursor-&gt;get_priority()</a> | Set/get the cursor's cache priority |
| <a href="DB_MULTIPLE_INIT.md" class="xref" title="DB_MULTIPLE_INIT">DB_MULTIPLE_INIT</a> | Initialize bulk get retrieval |
| <a href="DB_MULTIPLE_NEXT.md" class="xref" title="DB_MULTIPLE_NEXT">DB_MULTIPLE_NEXT</a> | Next bulk get retrieval |
| <a href="DB_MULTIPLE_KEY_NEXT.md" class="xref" title="DB_MULTIPLE_KEY_NEXT">DB_MULTIPLE_KEY_NEXT</a> | Next bulk get retrieval |
| <a href="DB_MULTIPLE_RECNO_NEXT.md" class="xref" title="DB_MULTIPLE_RECNO_NEXT">DB_MULTIPLE_RECNO_NEXT</a> | Next bulk get retrieval |
| <a href="DB_MULTIPLE_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_WRITE_INIT">DB_MULTIPLE_WRITE_INIT</a> | Initialize a bulk buffer to hold key/data pairs |
| <a href="DB_MULTIPLE_WRITE_NEXT.md" class="xref" title="DB_MULTIPLE_WRITE_NEXT">DB_MULTIPLE_WRITE_NEXT</a> | Append a data item to a bulk buffer |
| <a href="DB_MULTIPLE_RESERVE_NEXT.md" class="xref" title="DB_MULTIPLE_RESERVE_NEXT">DB_MULTIPLE_RESERVE_NEXT</a> | Reserve space for a data item in a bulk buffer |
| <a href="DB_MULTIPLE_KEY_WRITE_NEXT.md" class="xref" title="DB_MULTIPLE_KEY_WRITE_NEXT">DB_MULTIPLE_KEY_WRITE_NEXT</a> | Append a key / data pair to a bulk buffer |
| <a href="DB_MULTIPLE_KEY_RESERVE_NEXT.md" class="xref" title="DB_MULTIPLE_KEY_RESERVE_NEXT">DB_MULTIPLE_KEY_RESERVE_NEXT</a> | Reserve space for a key / data pair in a bulk buffer |
| <a href="DB_MULTIPLE_RECNO_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_RECNO_WRITE_INIT">DB_MULTIPLE_RECNO_WRITE_INIT</a> | Initialize a bulk buffer to hold recno/data pairs |
| <a href="DB_MULTIPLE_RECNO_WRITE_NEXT.md" class="xref" title="DB_MULTIPLE_RECNO_WRITE_NEXT">DB_MULTIPLE_RECNO_WRITE_NEXT</a> | Append a record number / data pair to a bulk buffer |
| <a href="DB_MULTIPLE_RECNO_RESERVE_NEXT.md" class="xref" title="DB_MULTIPLE_RECNO_RESERVE_NEXT">DB_MULTIPLE_RECNO_RESERVE_NEXT</a> | Reserve space for a record number / data pair in a bulk buffer |
| <a href="envadd_data_dir.md" class="xref" title="DB_ENV-&gt;add_data_dir()">DB_ENV-&gt;add_data_dir()</a> | Add an environment data directory |
| <a href="envbackup.md" class="xref" title="DB_ENV-&gt;backup()">DB_ENV-&gt;backup()</a> | Hot back up an entire environment |
| <a href="envclose.md" class="xref" title="DB_ENV-&gt;close()">DB_ENV-&gt;close()</a> | Close an environment |
| <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> | Create an environment handle |
| <a href="envdbbackup.md" class="xref" title="DB_ENV-&gt;dbbackup()">DB_ENV-&gt;dbbackup()</a> | Hot back up a single environment file |
| <a href="envdbremove.md" class="xref" title="DB_ENV-&gt;dbremove()">DB_ENV-&gt;dbremove()</a> | Remove a database |
| <a href="envdbrename.md" class="xref" title="DB_ENV-&gt;dbrename()">DB_ENV-&gt;dbrename()</a> | Rename a database |
| <a href="enverr.md" class="xref" title="DB_ENV-&gt;err()">DB_ENV-&gt;err()</a> | Error message |
| <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a> | Check for thread failure |
| <a href="envfileid_reset.md" class="xref" title="DB_ENV-&gt;fileid_reset()">DB_ENV-&gt;fileid_reset()</a> | Reset database file IDs |
| <a href="envfullversion.md" class="xref" title="db_full_version">db_full_version</a> | Return full version information |
| <a href="dbgetenv.md" class="xref" title="DB-&gt;get_env()">DB-&gt;get_env()</a> | Return the DB's underlying DB_ENV handle |
| <a href="envget_home.md" class="xref" title="DB_ENV-&gt;get_home()">DB_ENV-&gt;get_home()</a> | Return environment's home directory |
| <a href="envget_open_flags.md" class="xref" title="DB_ENV-&gt;get_open_flags()">DB_ENV-&gt;get_open_flags()</a> | Return flags with which the environment was opened |
| <a href="envlog_verify.md" class="xref" title="DB_ENV-&gt;log_verify()">DB_ENV-&gt;log_verify()</a> | Verify log files of an environment. |
| <a href="envlsn_reset.md" class="xref" title="DB_ENV-&gt;lsn_reset()">DB_ENV-&gt;lsn_reset()</a> | Reset database file LSNs |
| <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> | Open an environment |
| <a href="envremove.md" class="xref" title="DB_ENV-&gt;remove()">DB_ENV-&gt;remove()</a> | Remove an environment |
| <a href="envset_alloc.md" class="xref" title="DB_ENV-&gt;set_alloc()">DB_ENV-&gt;set_alloc()</a> | Set local space allocation functions |
| <a href="envset_app_dispatch.md" class="xref" title="DB_ENV-&gt;set_app_dispatch()">DB_ENV-&gt;set_app_dispatch()</a> | Configure application recovery callback |
| <a href="envset_backup_callbacks.md" class="xref" title="DB_ENV-&gt;set_backup_callbacks()">DB_ENV-&gt;set_backup_callbacks()</a>, <a href="envget_backup_callbacks.md" class="xref" title="DB_ENV-&gt;get_backup_callbacks()">DB_ENV-&gt;get_backup_callbacks()</a> | Set/get callbacks used for environment hot backups |
| <a href="envset_backup_config.md" class="xref" title="DB_ENV-&gt;set_backup_config()">DB_ENV-&gt;set_backup_config()</a>, <a href="envget_backup_config.md" class="xref" title="DB_ENV-&gt;get_backup_config()">DB_ENV-&gt;get_backup_config()</a> | Set/get environment hot backup configuration options |
| <a href="envset_data_dir.md" class="xref" title="DB_ENV-&gt;set_data_dir()">DB_ENV-&gt;set_data_dir()</a>, <a href="envget_data_dirs.md" class="xref" title="DB_ENV-&gt;get_data_dirs()">DB_ENV-&gt;get_data_dirs()</a> | Set/get the environment data directory |
| <a href="envset_data_len.md" class="xref" title="DB_ENV-&gt;set_data_len()">DB_ENV-&gt;set_data_len()</a>, <a href="envget_data_len.md" class="xref" title="DB_ENV-&gt;get_data_len()">DB_ENV-&gt;get_data_len()</a> | Set/get the command line utility byte limit |
| <a href="envset_create_dir.md" class="xref" title="DB_ENV-&gt;set_create_dir()">DB_ENV-&gt;set_create_dir()</a>, <a href="envget_create_dir.md" class="xref" title="DB_ENV-&gt;get_create_dir()">DB_ENV-&gt;get_create_dir()</a> | Add an environment data directory |
| <a href="envset_encrypt.md" class="xref" title="DB_ENV-&gt;set_encrypt()">DB_ENV-&gt;set_encrypt()</a>, <a href="envget_encrypt_flags.md" class="xref" title="DB_ENV-&gt;get_encrypt_flags()">DB_ENV-&gt;get_encrypt_flags()</a> | Set/get the environment cryptographic key |
| <a href="envevent_notify.md" class="xref" title="DB_ENV-&gt;set_event_notify()">DB_ENV-&gt;set_event_notify()</a> | Set event notification callback |
| <a href="envset_errcall.md" class="xref" title="DB_ENV-&gt;set_errcall()">DB_ENV-&gt;set_errcall()</a> | Set error message callbacks |
| <a href="envset_errfile.md" class="xref" title="DB_ENV-&gt;set_errfile()">DB_ENV-&gt;set_errfile()</a>, <a href="envget_errfile.md" class="xref" title="DB_ENV-&gt;get_errfile()">DB_ENV-&gt;get_errfile()</a> | Set/get error message FILE |
| <a href="envset_errpfx.md" class="xref" title="DB_ENV-&gt;set_errpfx()">DB_ENV-&gt;set_errpfx()</a>, <a href="envget_errpfx.md" class="xref" title="DB_ENV-&gt;get_errpfx()">DB_ENV-&gt;get_errpfx()</a> | Set/get error message prefix |
| <a href="envset_feedback.md" class="xref" title="DB_ENV-&gt;set_feedback()">DB_ENV-&gt;set_feedback()</a> | Set feedback callback |
| <a href="envset_flags.md" class="xref" title="DB_ENV-&gt;set_flags()">DB_ENV-&gt;set_flags()</a>, <a href="envget_flags.md" class="xref" title="DB_ENV-&gt;get_flags()">DB_ENV-&gt;get_flags()</a> | Environment configuration |
| <a href="envset_intermediate_dir_mode.md" class="xref" title="DB_ENV-&gt;set_intermediate_dir_mode()">DB_ENV-&gt;set_intermediate_dir_mode()</a>, <a href="envget_intermediate_dir_mode.md" class="xref" title="DB_ENV-&gt;get_intermediate_dir_mode()">DB_ENV-&gt;get_intermediate_dir_mode()</a> | Set/get intermediate directory creation mode |
| <a href="envset_isalive.md" class="xref" title="DB_ENV-&gt;set_isalive()">DB_ENV-&gt;set_isalive()</a> | Set thread is-alive callback |
| <a href="envset_memory_init.md" class="xref" title="DB_ENV-&gt;set_memory_init()">DB_ENV-&gt;set_memory_init()</a>, <a href="envget_memory_init.md" class="xref" title="DB_ENV-&gt;get_memory_init()">DB_ENV-&gt;get_memory_init()</a> | Set/get initial memory allocation |
| <a href="envset_memory_max.md" class="xref" title="DB_ENV-&gt;set_memory_max()">DB_ENV-&gt;set_memory_max()</a>, <a href="envget_memory_max.md" class="xref" title="DB_ENV-&gt;get_memory_max()">DB_ENV-&gt;get_memory_max()</a> | Set/get maximum memory allocation |
| <a href="envset_metadata_dir.md" class="xref" title="DB_ENV-&gt;set_metadata_dir()">DB_ENV-&gt;set_metadata_dir()</a>, <a href="envget_metadata_dir.md" class="xref" title="DB_ENV-&gt;get_metadata_dir()">DB_ENV-&gt;get_metadata_dir()</a> | Set/get the directory containing environment metadata |
| <a href="envset_msgcall.md" class="xref" title="DB_ENV-&gt;set_msgcall()">DB_ENV-&gt;set_msgcall()</a> | Set informational message callback |
| <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a>, <a href="envget_msgfile.md" class="xref" title="DB_ENV-&gt;get_msgfile()">DB_ENV-&gt;get_msgfile()</a> | Set/get informational message FILE |
| <a href="envset_shm_key.md" class="xref" title="DB_ENV-&gt;set_shm_key()">DB_ENV-&gt;set_shm_key()</a>, <a href="envget_shm_key.md" class="xref" title="DB_ENV-&gt;get_shm_key()">DB_ENV-&gt;get_shm_key()</a> | Set/get system memory shared segment ID |
| <a href="envset_thread_count.md" class="xref" title="DB_ENV-&gt;set_thread_count()">DB_ENV-&gt;set_thread_count()</a>, <a href="envget_thread_count.md" class="xref" title="DB_ENV-&gt;get_thread_count()">DB_ENV-&gt;get_thread_count()</a> | Set/get approximate thread count |
| <a href="envset_thread_id.md" class="xref" title="DB_ENV-&gt;set_thread_id()">DB_ENV-&gt;set_thread_id()</a> | Set thread of control ID function |
| <a href="envset_thread_id_string.md" class="xref" title="DB_ENV-&gt;set_thread_id_string()">DB_ENV-&gt;set_thread_id_string()</a> | Set thread of control ID format function |
| <a href="envset_timeout.md" class="xref" title="DB_ENV-&gt;set_timeout()">DB_ENV-&gt;set_timeout()</a>, <a href="envget_timeout.md" class="xref" title="DB_ENV-&gt;get_timeout()">DB_ENV-&gt;get_timeout()</a> | Set/get lock and transaction timeout |
| <a href="envset_tmp_dir.md" class="xref" title="DB_ENV-&gt;set_tmp_dir()">DB_ENV-&gt;set_tmp_dir()</a>, <a href="envget_tmp_dir.md" class="xref" title="DB_ENV-&gt;get_tmp_dir()">DB_ENV-&gt;get_tmp_dir()</a> | Set/get the environment temporary file directory |
| <a href="envset_verbose.md" class="xref" title="DB_ENV-&gt;set_verbose()">DB_ENV-&gt;set_verbose()</a>, <a href="envget_verbose.md" class="xref" title="DB_ENV-&gt;get_verbose()">DB_ENV-&gt;get_verbose()</a> | Set/get verbose messages |
| <a href="envstat.md" class="xref" title="DB_ENV-&gt;stat_print()">DB_ENV-&gt;stat_print()</a> | Environment statistics |
| <a href="envstrerror.md" class="xref" title="db_strerror">db_strerror</a> | Error strings |
| <a href="envversion.md" class="xref" title="db_version">db_version</a> | Return version information |
| <a href="envset_lk_conflicts.md" class="xref" title="DB_ENV-&gt;set_lk_conflicts()">DB_ENV-&gt;set_lk_conflicts()</a>, <a href="envget_lk_conflicts.md" class="xref" title="DB_ENV-&gt;get_lk_conflicts()">DB_ENV-&gt;get_lk_conflicts()</a> | Set/get lock conflicts matrix |
| <a href="envset_lk_detect.md" class="xref" title="DB_ENV-&gt;set_lk_detect()">DB_ENV-&gt;set_lk_detect()</a>, <a href="envget_lk_detect.md" class="xref" title="DB_ENV-&gt;get_lk_detect()">DB_ENV-&gt;get_lk_detect()</a> | Set/get automatic deadlock detection |
| <a href="envset_lk_max_lockers.md" class="xref" title="DB_ENV-&gt;set_lk_max_lockers()">DB_ENV-&gt;set_lk_max_lockers()</a>, <a href="envget_lk_max_lockers.md" class="xref" title="DB_ENV-&gt;get_lk_max_lockers()">DB_ENV-&gt;get_lk_max_lockers()</a> | Set/get maximum number of lockers |
| <a href="envset_lk_max_locks.md" class="xref" title="DB_ENV-&gt;set_lk_max_locks()">DB_ENV-&gt;set_lk_max_locks()</a>, <a href="envget_lk_max_locks.md" class="xref" title="DB_ENV-&gt;get_lk_max_locks()">DB_ENV-&gt;get_lk_max_locks()</a> | Set/get maximum number of locks |
| <a href="envset_lk_max_objects.md" class="xref" title="DB_ENV-&gt;set_lk_max_objects()">DB_ENV-&gt;set_lk_max_objects()</a>, <a href="envget_lk_max_objects.md" class="xref" title="DB_ENV-&gt;get_lk_max_objects()">DB_ENV-&gt;get_lk_max_objects()</a> | Set/get maximum number of lock objects |
| <a href="envset_lk_partitions.md" class="xref" title="DB_ENV-&gt;set_lk_partitions()">DB_ENV-&gt;set_lk_partitions()</a>, <a href="envget_lk_partitions.md" class="xref" title="DB_ENV-&gt;get_lk_partitions()">DB_ENV-&gt;get_lk_partitions()</a> | Set/get number of lock partitions |
| <a href="envset_lk_priority.md" class="xref" title="DB_ENV-&gt;set_lk_priority()">DB_ENV-&gt;set_lk_priority()</a>, <a href="envget_lk_priority.md" class="xref" title="DB_ENV-&gt;get_lk_priority()">DB_ENV-&gt;get_lk_priority()</a> | Set/get a locker's deadlock priority |
| <a href="envset_lk_tablesize.md" class="xref" title="DB_ENV-&gt;set_lk_tablesize()">DB_ENV-&gt;set_lk_tablesize()</a>, <a href="envget_lk_tablesize.md" class="xref" title="DB_ENV-&gt;get_lk_tablesize()">DB_ENV-&gt;get_lk_tablesize()</a> | Set/get size of the lock object hash table |
| <a href="lockdetect.md" class="xref" title="DB_ENV-&gt;lock_detect()">DB_ENV-&gt;lock_detect()</a> | Perform deadlock detection |
| <a href="lockget.md" class="xref" title="DB_ENV-&gt;lock_get()">DB_ENV-&gt;lock_get()</a> | Acquire a lock |
| <a href="lockid.md" class="xref" title="DB_ENV-&gt;lock_id()">DB_ENV-&gt;lock_id()</a> | Acquire a locker ID |
| <a href="lockid_free.md" class="xref" title="DB_ENV-&gt;lock_id_free()">DB_ENV-&gt;lock_id_free()</a> | Release a locker ID |
| <a href="lockput.md" class="xref" title="DB_ENV-&gt;lock_put()">DB_ENV-&gt;lock_put()</a> | Release a lock |
| <a href="lockstat.md" class="xref" title="DB_ENV-&gt;lock_stat()">DB_ENV-&gt;lock_stat()</a> | Return lock subsystem statistics |
| <a href="lockstat_print.md" class="xref" title="DB_ENV-&gt;lock_stat_print()">DB_ENV-&gt;lock_stat_print()</a> | Print lock subsystem statistics |
| <a href="lockvec.md" class="xref" title="DB_ENV-&gt;lock_vec()">DB_ENV-&gt;lock_vec()</a> | Acquire/release locks |
| <a href="logarchive.md" class="xref" title="DB_ENV-&gt;log_archive()">DB_ENV-&gt;log_archive()</a> | List log and database files |
| <a href="logcursor.md" class="xref" title="DB_ENV-&gt;log_cursor()">DB_ENV-&gt;log_cursor()</a> | Create a log cursor handle |
| <a href="logfile.md" class="xref" title="DB_ENV-&gt;log_file()">DB_ENV-&gt;log_file()</a> | Map Log Sequence Numbers to log files |
| <a href="logflush.md" class="xref" title="DB_ENV-&gt;log_flush()">DB_ENV-&gt;log_flush()</a> | Flush log records |
| <a href="logprintf.md" class="xref" title="DB_ENV-&gt;log_printf()">DB_ENV-&gt;log_printf()</a> | Append informational message to the log |
| <a href="logput.md" class="xref" title="DB_ENV-&gt;log_put()">DB_ENV-&gt;log_put()</a> | Write a log record |
| <a href="envlog_set_config.md" class="xref" title="DB_ENV-&gt;log_set_config()">DB_ENV-&gt;log_set_config()</a>, <a href="envlog_get_config.md" class="xref" title="DB_ENV-&gt;log_get_config()">DB_ENV-&gt;log_get_config()</a> | Configure the logging subsystem |
| <a href="logstat.md" class="xref" title="DB_ENV-&gt;log_stat()">DB_ENV-&gt;log_stat()</a> | Return log subsystem statistics |
| <a href="logstat_print.md" class="xref" title="DB_ENV-&gt;log_stat_print()">DB_ENV-&gt;log_stat_print()</a> | Print log subsystem statistics |
| <a href="envset_lg_bsize.md" class="xref" title="DB_ENV-&gt;set_lg_bsize()">DB_ENV-&gt;set_lg_bsize()</a>, <a href="envget_lg_bsize.md" class="xref" title="DB_ENV-&gt;get_lg_bsize()">DB_ENV-&gt;get_lg_bsize()</a> | Set/get log buffer size |
| <a href="envset_lg_dir.md" class="xref" title="DB_ENV-&gt;set_lg_dir()">DB_ENV-&gt;set_lg_dir()</a>, <a href="envget_lg_dir.md" class="xref" title="DB_ENV-&gt;get_lg_dir()">DB_ENV-&gt;get_lg_dir()</a> | Set/get the environment logging directory |
| <a href="envset_lg_filemode.md" class="xref" title="DB_ENV-&gt;set_lg_filemode()">DB_ENV-&gt;set_lg_filemode()</a>, <a href="envget_lg_filemode.md" class="xref" title="DB_ENV-&gt;get_lg_filemode()">DB_ENV-&gt;get_lg_filemode()</a> | Set/get log file mode |
| <a href="envset_lg_max.md" class="xref" title="DB_ENV-&gt;set_lg_max()">DB_ENV-&gt;set_lg_max()</a>, <a href="envget_lg_max.md" class="xref" title="DB_ENV-&gt;get_lg_max()">DB_ENV-&gt;get_lg_max()</a> | Set/get log file size |
| <a href="envset_lg_regionmax.md" class="xref" title="DB_ENV-&gt;set_lg_regionmax()">DB_ENV-&gt;set_lg_regionmax()</a>, <a href="envget_lg_regionmax.md" class="xref" title="DB_ENV-&gt;get_lg_regionmax()">DB_ENV-&gt;get_lg_regionmax()</a> | Set/get logging region size |
| <a href="logc.md" class="xref" title="The DB_LOGC Handle">The DB_LOGC Handle</a> | A log cursor handle |
| <a href="logcclose.md" class="xref" title="DB_LOGC-&gt;close()">DB_LOGC-&gt;close()</a> | Close a log cursor |
| <a href="logcget.md" class="xref" title="DB_LOGC-&gt;get()">DB_LOGC-&gt;get()</a> | Retrieve a log record |
| <a href="logcompare.md" class="xref" title="log_compare">log_compare</a> | Compare two Log Sequence Numbers |
| <a href="dbget_mpf.md" class="xref" title="DB-&gt;get_mpf()">DB-&gt;get_mpf()</a> | Return the DB_MPOOLFILE for a DB |
| <a href="mempfcreate.md" class="xref" title="DB_ENV-&gt;memp_fcreate()">DB_ENV-&gt;memp_fcreate()</a> | Create a memory pool file handle |
| <a href="mempregister.md" class="xref" title="DB_ENV-&gt;memp_register()">DB_ENV-&gt;memp_register()</a> | Register a custom file type |
| <a href="mempstat.md" class="xref" title="DB_ENV-&gt;memp_stat()">DB_ENV-&gt;memp_stat()</a> | Return cache statistics |
| <a href="mempstat_print.md" class="xref" title="DB_ENV-&gt;memp_stat_print()">DB_ENV-&gt;memp_stat_print()</a> | Print cache statistics |
| <a href="mempsync.md" class="xref" title="DB_ENV-&gt;memp_sync()">DB_ENV-&gt;memp_sync()</a> | Flush all pages from the cache |
| <a href="memptrickle.md" class="xref" title="DB_ENV-&gt;memp_trickle()">DB_ENV-&gt;memp_trickle()</a> | Flush some pages from the cache |
| <a href="envset_cache_max.md" class="xref" title="DB_ENV-&gt;set_cache_max()">DB_ENV-&gt;set_cache_max()</a>, <a href="envget_cache_max.md" class="xref" title="DB_ENV-&gt;get_cache_max()">DB_ENV-&gt;get_cache_max()</a> | Set/get the maximum cache size |
| <a href="envset_cachesize.md" class="xref" title="DB_ENV-&gt;set_cachesize()">DB_ENV-&gt;set_cachesize()</a>, <a href="envget_cachesize.md" class="xref" title="DB_ENV-&gt;get_cachesize()">DB_ENV-&gt;get_cachesize()</a> | Set/get the environment cache size |
| <a href="mempset_mp_max_openfd.md" class="xref" title="DB_ENV-&gt;set_mp_max_openfd()">DB_ENV-&gt;set_mp_max_openfd()</a>, <a href="mempget_mp_max_openfd.md" class="xref" title="DB_ENV-&gt;get_mp_max_openfd()">DB_ENV-&gt;get_mp_max_openfd()</a> | Set/get the maximum number of open file descriptors |
| <a href="mempset_mp_max_write.md" class="xref" title="DB_ENV-&gt;set_mp_max_write()">DB_ENV-&gt;set_mp_max_write()</a>, <a href="mempget_mp_max_write.md" class="xref" title="DB_ENV-&gt;get_mp_max_write()">DB_ENV-&gt;get_mp_max_write()</a> | Set/get the maximum number of sequential disk writes |
| <a href="envset_mp_mmapsize.md" class="xref" title="DB_ENV-&gt;set_mp_mmapsize()">DB_ENV-&gt;set_mp_mmapsize()</a>, <a href="envget_mp_mmapsize.md" class="xref" title="DB_ENV-&gt;get_mp_mmapsize()">DB_ENV-&gt;get_mp_mmapsize()</a> | Set/get maximum file size to memory map when opened read-only |
| <a href="envset_mp_mtxcount.md" class="xref" title="DB_ENV-&gt;set_mp_mtxcount()">DB_ENV-&gt;set_mp_mtxcount()</a>, <a href="envget_mp_mtxcount.md" class="xref" title="DB_ENV-&gt;get_mp_mtxcount()">DB_ENV-&gt;get_mp_mtxcount()</a> | Set/get the number of mutexes allocated to the hash table |
| <a href="envset_mp_pagesize.md" class="xref" title="DB_ENV-&gt;set_mp_pagesize()">DB_ENV-&gt;set_mp_pagesize()</a>, <a href="envget_mp_pagesize.md" class="xref" title="DB_ENV-&gt;get_mp_pagesize()">DB_ENV-&gt;get_mp_pagesize()</a> | Set/get page size to configure the buffer pool |
| <a href="envset_mp_tablesize.md" class="xref" title="DB_ENV-&gt;set_mp_tablesize()">DB_ENV-&gt;set_mp_tablesize()</a>, <a href="envget_mp_tablesize.md" class="xref" title="DB_ENV-&gt;get_mp_tablesize()">DB_ENV-&gt;get_mp_tablesize()</a> | Set/get the hash table size |
| <a href="mempfclose.md" class="xref" title="DB_MPOOLFILE-&gt;close()">DB_MPOOLFILE-&gt;close()</a> | Close a file in the cache |
| <a href="mempfget.md" class="xref" title="DB_MPOOLFILE-&gt;get()">DB_MPOOLFILE-&gt;get()</a> | Get page from a file in the cache |
| <a href="mempfopen.md" class="xref" title="DB_MPOOLFILE-&gt;open()">DB_MPOOLFILE-&gt;open()</a> | Open a file in the cache |
| <a href="mempput.md" class="xref" title="DB_MPOOLFILE-&gt;put()">DB_MPOOLFILE-&gt;put()</a> | Return a page to the cache |
| <a href="mempfsync.md" class="xref" title="DB_MPOOLFILE-&gt;sync()">DB_MPOOLFILE-&gt;sync()</a> | Flush pages from a file from the cache |
| <a href="mempset_clear_len.md" class="xref" title="DB_MPOOLFILE-&gt;set_clear_len()">DB_MPOOLFILE-&gt;set_clear_len()</a>, <a href="mempget_clear_len.md" class="xref" title="DB_MPOOLFILE-&gt;get_clear_len()">DB_MPOOLFILE-&gt;get_clear_len()</a> | Set/get number of bytes to clear when creating a new page |
| <a href="mempset_fileid.md" class="xref" title="DB_MPOOLFILE-&gt;set_fileid()">DB_MPOOLFILE-&gt;set_fileid()</a>, <a href="mempget_fileid.md" class="xref" title="DB_MPOOLFILE-&gt;get_fileid()">DB_MPOOLFILE-&gt;get_fileid()</a> | Set/get file unique identifier |
| <a href="mempset_flags.md" class="xref" title="DB_MPOOLFILE-&gt;set_flags()">DB_MPOOLFILE-&gt;set_flags()</a>, <a href="mempget_flags.md" class="xref" title="DB_MPOOLFILE-&gt;get_flags()">DB_MPOOLFILE-&gt;get_flags()</a> | Set/get file options |
| <a href="mempset_ftype.md" class="xref" title="DB_MPOOLFILE-&gt;set_ftype()">DB_MPOOLFILE-&gt;set_ftype()</a>, <a href="mempget_ftype.md" class="xref" title="DB_MPOOLFILE-&gt;get_ftype()">DB_MPOOLFILE-&gt;get_ftype()</a> | Set/get file type |
| <a href="mempset_lsn_offset.md" class="xref" title="DB_MPOOLFILE-&gt;set_lsn_offset()">DB_MPOOLFILE-&gt;set_lsn_offset()</a>, <a href="mempget_lsn_offset.md" class="xref" title="DB_MPOOLFILE-&gt;get_lsn_offset()">DB_MPOOLFILE-&gt;get_lsn_offset()</a> | Set/get file log-sequence-number offset |
| <a href="mempset_maxsize.md" class="xref" title="DB_MPOOLFILE-&gt;set_maxsize()">DB_MPOOLFILE-&gt;set_maxsize()</a>, <a href="mempget_maxsize.md" class="xref" title="DB_MPOOLFILE-&gt;get_maxsize()">DB_MPOOLFILE-&gt;get_maxsize()</a> | Set/get maximum file size |
| <a href="mempset_pgcookie.md" class="xref" title="DB_MPOOLFILE-&gt;set_pgcookie()">DB_MPOOLFILE-&gt;set_pgcookie()</a>, <a href="mempget_pgcookie.md" class="xref" title="DB_MPOOLFILE-&gt;get_pgcookie()">DB_MPOOLFILE-&gt;get_pgcookie()</a> | Set/get file cookie for pgin/pgout |
| <a href="mempset_priority.md" class="xref" title="DB_MPOOLFILE-&gt;set_priority()">DB_MPOOLFILE-&gt;set_priority()</a>, <a href="mempget_priority.md" class="xref" title="DB_MPOOLFILE-&gt;get_priority()">DB_MPOOLFILE-&gt;get_priority()</a> | Set/get cache file priority |
| <a href="mutexalloc.md" class="xref" title="DB_ENV-&gt;mutex_alloc()">DB_ENV-&gt;mutex_alloc()</a> | Allocate a mutex |
| <a href="mutexfree.md" class="xref" title="DB_ENV-&gt;mutex_free()">DB_ENV-&gt;mutex_free()</a> | Free a mutex |
| <a href="mutexlock.md" class="xref" title="DB_ENV-&gt;mutex_lock()">DB_ENV-&gt;mutex_lock()</a> | Lock a mutex |
| <a href="mutexset_align.md" class="xref" title="DB_ENV-&gt;mutex_set_align()">DB_ENV-&gt;mutex_set_align()</a>, <a href="mutexget_align.md" class="xref" title="DB_ENV-&gt;mutex_get_align()">DB_ENV-&gt;mutex_get_align()</a> | Configure mutex alignment |
| <a href="mutexset_increment.md" class="xref" title="DB_ENV-&gt;mutex_set_increment()">DB_ENV-&gt;mutex_set_increment()</a>, <a href="mutexget_increment.md" class="xref" title="DB_ENV-&gt;mutex_get_increment()">DB_ENV-&gt;mutex_get_increment()</a> | Configure number of additional mutexes |
| <a href="mutexset_init.md" class="xref" title="DB_ENV-&gt;mutex_set_init()">DB_ENV-&gt;mutex_set_init()</a>, <a href="mutexget_init.md" class="xref" title="DB_ENV-&gt;mutex_get_init()">DB_ENV-&gt;mutex_get_init()</a> | Configure initial number of mutexes |
| <a href="mutexset_max.md" class="xref" title="DB_ENV-&gt;mutex_set_max()">DB_ENV-&gt;mutex_set_max()</a>, <a href="mutexget_max.md" class="xref" title="DB_ENV-&gt;mutex_get_max()">DB_ENV-&gt;mutex_get_max()</a> | Configure total number of mutexes |
| <a href="mutexset_tas_spins.md" class="xref" title="DB_ENV-&gt;mutex_set_tas_spins()">DB_ENV-&gt;mutex_set_tas_spins()</a>, <a href="mutexget_tas_spins.md" class="xref" title="DB_ENV-&gt;mutex_get_tas_spins()">DB_ENV-&gt;mutex_get_tas_spins()</a> | Configure test-and-set mutex spin count |
| <a href="mutexstat.md" class="xref" title="DB_ENV-&gt;mutex_stat()">DB_ENV-&gt;mutex_stat()</a> | Mutex statistics |
| <a href="mutexstat_print.md" class="xref" title="DB_ENV-&gt;mutex_stat_print()">DB_ENV-&gt;mutex_stat_print()</a> | Print mutex statistics |
| <a href="mutexunlock.md" class="xref" title="DB_ENV-&gt;mutex_unlock()">DB_ENV-&gt;mutex_unlock()</a> | Unlock a mutex |
| <a href="dbchannel_close.md" class="xref" title="DB_CHANNEL-&gt;close()">DB_CHANNEL-&gt;close()</a> | Closes a DB_CHANNEL handle |
| <a href="dbchannel_send_msg.md" class="xref" title="DB_CHANNEL-&gt;send_msg()">DB_CHANNEL-&gt;send_msg()</a> | Sends an asynchronous message on a DB_CHANNEL |
| <a href="dbchannel_send_request.md" class="xref" title="DB_CHANNEL-&gt;send_request()">DB_CHANNEL-&gt;send_request()</a> | Sends a synchronous message on a DB_CHANNEL |
| <a href="dbchannel_set_timeout.md" class="xref" title="DB_CHANNEL-&gt;set_timeout()">DB_CHANNEL-&gt;set_timeout()</a> | Sets the default timeout for the DB_CHANNEL |
| <a href="dbsite_close.md" class="xref" title="DB_SITE-&gt;close()">DB_SITE-&gt;close()</a> | Closes the DB_SITE handle |
| <a href="dbsite_get_address.md" class="xref" title="DB_SITE-&gt;get_address()">DB_SITE-&gt;get_address()</a> | Returns a site's network address |
| <a href="dbsite_get_eid.md" class="xref" title="DB_SITE-&gt;get_eid()">DB_SITE-&gt;get_eid()</a> | Returns a site's Environment ID |
| <a href="dbsite_remove.md" class="xref" title="DB_SITE-&gt;remove()">DB_SITE-&gt;remove()</a> | Removes the site from the replication group |
| <a href="dbsite_set_config.md" class="xref" title="DB_SITE-&gt;set_config()">DB_SITE-&gt;set_config()</a>, <a href="dbsite_get_config.md" class="xref" title="DB_SITE-&gt;get_config()">DB_SITE-&gt;get_config()</a> | Configure a DB_SITE handle |
| <a href="repelect.md" class="xref" title="DB_ENV-&gt;rep_elect()">DB_ENV-&gt;rep_elect()</a> | Hold a replication election |
| <a href="repmessage.md" class="xref" title="DB_ENV-&gt;rep_process_message()">DB_ENV-&gt;rep_process_message()</a> | Process a replication message |
| <a href="repclockskew.md" class="xref" title="DB_ENV-&gt;rep_set_clockskew()">DB_ENV-&gt;rep_set_clockskew()</a>, <a href="repget_clockskew.md" class="xref" title="DB_ENV-&gt;rep_get_clockskew()">DB_ENV-&gt;rep_get_clockskew()</a> | Configure master lease clock adjustment |
| <a href="repconfig.md" class="xref" title="DB_ENV-&gt;rep_set_config()">DB_ENV-&gt;rep_set_config()</a>, <a href="repget_config.md" class="xref" title="DB_ENV-&gt;rep_get_config()">DB_ENV-&gt;rep_get_config()</a> | Configure the replication subsystem |
| <a href="repset_limit.md" class="xref" title="DB_ENV-&gt;rep_set_limit()">DB_ENV-&gt;rep_set_limit()</a>, <a href="repget_limit.md" class="xref" title="DB_ENV-&gt;rep_get_limit()">DB_ENV-&gt;rep_get_limit()</a> | Limit data sent in response to a single message |
| <a href="repnsites.md" class="xref" title="DB_ENV-&gt;rep_set_nsites()">DB_ENV-&gt;rep_set_nsites()</a>, <a href="repget_nsites.md" class="xref" title="DB_ENV-&gt;rep_get_nsites()">DB_ENV-&gt;rep_get_nsites()</a> | Configure replication group site count |
| <a href="reppriority.md" class="xref" title="DB_ENV-&gt;rep_set_priority()">DB_ENV-&gt;rep_set_priority()</a>, <a href="repget_priority.md" class="xref" title="DB_ENV-&gt;rep_get_priority()">DB_ENV-&gt;rep_get_priority()</a> | Configure replication site priority |
| <a href="repset_request.md" class="xref" title="DB_ENV-&gt;rep_set_request()">DB_ENV-&gt;rep_set_request()</a>, <a href="repget_request.md" class="xref" title="DB_ENV-&gt;rep_get_request()">DB_ENV-&gt;rep_get_request()</a> | Configure replication client retransmission requests |
| <a href="repset_timeout.md" class="xref" title="DB_ENV-&gt;rep_set_timeout()">DB_ENV-&gt;rep_set_timeout()</a>, <a href="repget_timeout.md" class="xref" title="DB_ENV-&gt;rep_get_timeout()">DB_ENV-&gt;rep_get_timeout()</a> | Configure replication timeouts |
| <a href="reptransport.md" class="xref" title="DB_ENV-&gt;rep_set_transport()">DB_ENV-&gt;rep_set_transport()</a> | Configure replication transport callback |
| <a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a> | Start replication |
| <a href="repstat.md" class="xref" title="DB_ENV-&gt;rep_stat()">DB_ENV-&gt;rep_stat()</a> | Replication statistics |
| <a href="repstat_print.md" class="xref" title="DB_ENV-&gt;rep_stat_print()">DB_ENV-&gt;rep_stat_print()</a> | Print replication statistics |
| <a href="repsync.md" class="xref" title="DB_ENV-&gt;rep_sync()">DB_ENV-&gt;rep_sync()</a> | Replication synchronization |
| <a href="repmgr_channel.md" class="xref" title="DB_ENV-&gt;repmgr_channel()">DB_ENV-&gt;repmgr_channel()</a> | Creates a DB_CHANNEL handle |
| <a href="repmgr_local_site.md" class="xref" title="DB_ENV-&gt;repmgr_local_site()">DB_ENV-&gt;repmgr_local_site()</a> | Returns a DB_SITE handle for the local site |
| <a href="repmgr_msg_dispatch.md" class="xref" title="DB_ENV-&gt;repmgr_msg_dispatch()">DB_ENV-&gt;repmgr_msg_dispatch()</a> | Creates a DB_CHANNEL handle |
| <a href="repmgrset_ack_policy.md" class="xref" title="DB_ENV-&gt;repmgr_set_ack_policy()">DB_ENV-&gt;repmgr_set_ack_policy()</a>, <a href="repmgrget_ack_policy.md" class="xref" title="DB_ENV-&gt;repmgr_get_ack_policy()">DB_ENV-&gt;repmgr_get_ack_policy()</a> | Specify the Replication Manager's client acknowledgement policy |
| <a href="repmgr_site.md" class="xref" title="DB_ENV-&gt;repmgr_site()">DB_ENV-&gt;repmgr_site()</a> | Creates a DB_SITE handle |
| <a href="repmgr_site_by_eid.md" class="xref" title="DB_ENV-&gt;repmgr_site_by_eid()">DB_ENV-&gt;repmgr_site_by_eid()</a> | Creates a DB_SITE handle given an EID value |
| <a href="repmgrsite_list.md" class="xref" title="DB_ENV-&gt;repmgr_site_list()">DB_ENV-&gt;repmgr_site_list()</a> | List the sites and their status |
| <a href="repmgrstart.md" class="xref" title="DB_ENV-&gt;repmgr_start()">DB_ENV-&gt;repmgr_start()</a> | Start the Replication Manager |
| <a href="repmgrstat.md" class="xref" title="DB_ENV-&gt;repmgr_stat()">DB_ENV-&gt;repmgr_stat()</a> | Replication Manager statistics |
| <a href="repmgrstat_print.md" class="xref" title="DB_ENV-&gt;repmgr_stat_print()">DB_ENV-&gt;repmgr_stat_print()</a> | Print Replication Manager statistics |
| <a href="envtxn_applied.md" class="xref" title="DB_ENV-&gt;txn_applied()">DB_ENV-&gt;txn_applied()</a> | Check if a transaction has been replicated |
| <a href="txnset_commit_token.md" class="xref" title="DB_TXN-&gt;set_commit_token()">DB_TXN-&gt;set_commit_token()</a> | Set a commit token |
| <a href="seqcreate.md" class="xref" title="db_sequence_create">db_sequence_create</a> | Create a sequence handle |
| <a href="seqclose.md" class="xref" title="DB_SEQUENCE-&gt;close()">DB_SEQUENCE-&gt;close()</a> | Close a sequence |
| <a href="seqget.md" class="xref" title="DB_SEQUENCE-&gt;get()">DB_SEQUENCE-&gt;get()</a> | Get the next sequence element(s) |
| <a href="seqget_dbp.md" class="xref" title="DB_SEQUENCE-&gt;get_dbp()">DB_SEQUENCE-&gt;get_dbp()</a> | Return a handle for the underlying sequence database |
| <a href="seqget_key.md" class="xref" title="DB_SEQUENCE-&gt;get_key()">DB_SEQUENCE-&gt;get_key()</a> | Return the key for a sequence |
| <a href="seqinitial_value.md" class="xref" title="DB_SEQUENCE-&gt;initial_value()">DB_SEQUENCE-&gt;initial_value()</a> | Set the initial value of a sequence |
| <a href="seqopen.md" class="xref" title="DB_SEQUENCE-&gt;open()">DB_SEQUENCE-&gt;open()</a> | Open a sequence |
| <a href="seqremove.md" class="xref" title="DB_SEQUENCE-&gt;remove()">DB_SEQUENCE-&gt;remove()</a> | Remove a sequence |
| <a href="seqset_cachesize.md" class="xref" title="DB_SEQUENCE-&gt;set_cachesize()">DB_SEQUENCE-&gt;set_cachesize()</a>, <a href="seqget_cachesize.md" class="xref" title="DB_SEQUENCE-&gt;get_cachesize()">DB_SEQUENCE-&gt;get_cachesize()</a> | Set/get the cache size of a sequence |
| <a href="seqset_flags.md" class="xref" title="DB_SEQUENCE-&gt;set_flags()">DB_SEQUENCE-&gt;set_flags()</a>, <a href="seqget_flags.md" class="xref" title="DB_SEQUENCE-&gt;get_flags()">DB_SEQUENCE-&gt;get_flags()</a> | Set/get the flags for a sequence |
| <a href="seqset_range.md" class="xref" title="DB_SEQUENCE-&gt;set_range()">DB_SEQUENCE-&gt;set_range()</a>, <a href="seqget_range.md" class="xref" title="DB_SEQUENCE-&gt;get_range()">DB_SEQUENCE-&gt;get_range()</a> | Set/get the range for a sequence |
| <a href="seqstat.md" class="xref" title="DB_SEQUENCE-&gt;stat()">DB_SEQUENCE-&gt;stat()</a> | Return sequence statistics |
| <a href="seqstat_print.md" class="xref" title="DB_SEQUENCE-&gt;stat_print()">DB_SEQUENCE-&gt;stat_print()</a> | Print sequence statistics |
| <a href="dbget_transactional.md" class="xref" title="DB-&gt;get_transactional()">DB-&gt;get_transactional()</a> | Does the DB have transaction support |
| <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a> | Get a locker ID in Berkeley DB Concurrent Data Store |
| <a href="envset_tx_max.md" class="xref" title="DB_ENV-&gt;set_tx_max()">DB_ENV-&gt;set_tx_max()</a>, <a href="envget_tx_max.md" class="xref" title="DB_ENV-&gt;get_tx_max()">DB_ENV-&gt;get_tx_max()</a> | Set/get maximum number of transactions |
| <a href="envset_tx_timestamp.md" class="xref" title="DB_ENV-&gt;set_tx_timestamp()">DB_ENV-&gt;set_tx_timestamp()</a>, <a href="envget_tx_timestamp.md" class="xref" title="DB_ENV-&gt;get_tx_timestamp()">DB_ENV-&gt;get_tx_timestamp()</a> | Set/get recovery timestamp |
| <a href="txnrecover.md" class="xref" title="DB_ENV-&gt;txn_recover()">DB_ENV-&gt;txn_recover()</a> | Distributed transaction recovery |
| <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a> | Begin a transaction |
| <a href="txncheckpoint.md" class="xref" title="DB_ENV-&gt;txn_checkpoint()">DB_ENV-&gt;txn_checkpoint()</a> | Checkpoint the transaction subsystem |
| <a href="txnstat.md" class="xref" title="DB_ENV-&gt;txn_stat()">DB_ENV-&gt;txn_stat()</a> | Return transaction subsystem statistics |
| <a href="txnstat_print.md" class="xref" title="DB_ENV-&gt;txn_stat_print()">DB_ENV-&gt;txn_stat_print()</a> | Print transaction subsystem statistics |
| <a href="txnabort.md" class="xref" title="DB_TXN-&gt;abort()">DB_TXN-&gt;abort()</a> | Abort a transaction |
| <a href="txncommit.md" class="xref" title="DB_TXN-&gt;commit()">DB_TXN-&gt;commit()</a> | Commit a transaction |
| <a href="txndiscard.md" class="xref" title="DB_TXN-&gt;discard()">DB_TXN-&gt;discard()</a> | Discard a prepared but not resolved transaction handle |
| <a href="txnid.md" class="xref" title="DB_TXN-&gt;id()">DB_TXN-&gt;id()</a> | Return a transaction's ID |
| <a href="txnprepare.md" class="xref" title="DB_TXN-&gt;prepare()">DB_TXN-&gt;prepare()</a> | Prepare a transaction for commit |
| <a href="txnset_name.md" class="xref" title="DB_TXN-&gt;set_name()">DB_TXN-&gt;set_name()</a>, <a href="txnget_name.md" class="xref" title="DB_TXN-&gt;get_name()">DB_TXN-&gt;get_name()</a> | Associate a string with a transaction |
| <a href="txnset_priority.md" class="xref" title="DB_TXN-&gt;set_priority()">DB_TXN-&gt;set_priority()</a>, <a href="txnget_priority.md" class="xref" title="DB_TXN-&gt;get_priority()">DB_TXN-&gt;get_priority()</a> | Set/get transaction's priority |
| <a href="txnset_timeout.md" class="xref" title="DB_TXN-&gt;set_timeout()">DB_TXN-&gt;set_timeout()</a> | Set transaction timeout |
| <a href="db_archive.md" class="xref" title="db_archive">db_archive</a> | Archival utility |
| <a href="db_checkpoint.md" class="xref" title="db_checkpoint">db_checkpoint</a> | Transaction checkpoint utility |
| <a href="db_deadlock.md" class="xref" title="db_deadlock">db_deadlock</a> | Deadlock detection utility |
| <a href="db_dump.md" class="xref" title="db_dump">db_dump</a> | Database dump utility |
| <a href="db_hotbackup.md" class="xref" title="db_hotbackup">db_hotbackup</a> | Hot backup utility |
| <a href="db_load.md" class="xref" title="db_load">db_load</a> | Database load utility |
| <a href="db_log_verify.md" class="xref" title="db_log_verify">db_log_verify</a> | Log verification utility |
| <a href="db_printlog.md" class="xref" title="db_printlog">db_printlog</a> | Transaction log display utility |
| <a href="db_recover.md" class="xref" title="db_recover">db_recover</a> | Recovery utility |
| <a href="db_replicate.md" class="xref" title="db_replicate">db_replicate</a> | Replication utility |
| <a href="db_sql_codegen.md" class="xref" title="db_sql_codegen">db_sql_codegen</a> | SQL schema to Berkeley DB code in C |
| <a href="dbsql.md" class="xref" title="dbsql">dbsql</a> | Command line interface to libdb_sql |
| <a href="db_stat.md" class="xref" title="db_stat">db_stat</a> | Statistics utility |
| <a href="db_tuner.md" class="xref" title="db_tuner">db_tuner</a> | Suggest a page size for optimal operation in a btree database |
| <a href="db_upgrade.md" class="xref" title="db_upgrade">db_upgrade</a> | Database upgrade utility |
| <a href="db_verify.md" class="xref" title="db_verify">db_verify</a> | Verification utility |
| <a href="sqlite3.md" class="xref" title="sqlite3">sqlite3</a> | Command line tool for wrapper library libsqlite3 |
| <a href="dbm.md" class="xref" title="dbm/ndbm">dbm/ndbm</a> | Compatibility for applications written to the historic dbm or hdbm interfaces |
| <a href="hsearch.md" class="xref" title="hsearch">hsearch</a> | Compatibility for applications written to the historic hsearch interface |
| <a href="db_env_set_func_close.md" class="xref" title="db_env_set_func_close">db_env_set_func_close</a> | Replace Berkeley DB calls to close() with the identified function. |
| <a href="db_env_set_func_dirfree.md" class="xref" title="db_env_set_func_dirfree">db_env_set_func_dirfree</a> | Specify function used to free memory obtained due to a directory walk. |
| <a href="db_env_set_func_dirlist.md" class="xref" title="db_env_set_func_dirlist">db_env_set_func_dirlist</a> | Specify function used to free memory obtained due to a directory list. |
| <a href="db_env_set_func_exists.md" class="xref" title="db_env_set_func_exists">db_env_set_func_exists</a> | Specify function used to determine whether a file exists. |
| <a href="db_env_set_func_file_map.md" class="xref" title="db_env_set_func_file_map">db_env_set_func_file_map</a> | Specify function used to map a file into memory. |
| <a href="db_env_set_func_free.md" class="xref" title="db_env_set_func_free">db_env_set_func_free</a> | Specify function used to free memory. |
| <a href="db_env_set_func_fsync.md" class="xref" title="db_env_set_func_fsync">db_env_set_func_fsync</a> | Specify function used to sync a file to disk. |
| <a href="db_env_set_func_ftruncate.md" class="xref" title="db_env_set_func_ftruncate">db_env_set_func_ftruncate</a> | Specify function used to truncate a file. |
| <a href="db_env_set_func_ioinfo.md" class="xref" title="db_env_set_func_ioinfo">db_env_set_func_ioinfo</a> | Specify function used to determine file characteristics. |
| <a href="db_env_set_func_malloc.md" class="xref" title="db_env_set_func_malloc">db_env_set_func_malloc</a> | Specify function used to allocate memory. |
| <a href="db_env_set_func_open.md" class="xref" title="db_env_set_func_open">db_env_set_func_open</a> | Specify function used to open a file. |
| <a href="db_env_set_func_pread.md" class="xref" title="db_env_set_func_pread">db_env_set_func_pread</a> | Specify function used to read data from an object. |
| <a href="db_env_set_func_pwrite.md" class="xref" title="db_env_set_func_pwrite">db_env_set_func_pwrite</a> | Specify function used to write data to an object. |
| <a href="db_env_set_func_read.md" class="xref" title="db_env_set_func_read">db_env_set_func_read</a> | Specify function used to read data from an object. |
| <a href="db_env_set_func_realloc.md" class="xref" title="db_env_set_func_realloc">db_env_set_func_realloc</a> | Specify function used to change the size of memory pointed to by a pointer. |
| <a href="db_env_set_func_region_map.md" class="xref" title="db_env_set_func_region_map">db_env_set_func_region_map</a> | Specify function used to created shared memory regions. |
| <a href="db_env_set_func_rename.md" class="xref" title="db_env_set_func_rename">db_env_set_func_rename</a> | Specify function used to change the name of a file. |
| <a href="db_env_set_func_seek.md" class="xref" title="db_env_set_func_seek">db_env_set_func_seek</a> | Specify function used to specify a location in a file. |
| <a href="db_env_set_func_unlink.md" class="xref" title="db_env_set_func_unlink">db_env_set_func_unlink</a> | Specify function used to delete a file. |
| <a href="db_env_set_func_write.md" class="xref" title="db_env_set_func_write">db_env_set_func_write</a> | Specify function used to write data to an object. |
| <a href="db_env_set_func_yield.md" class="xref" title="db_env_set_func_yield">db_env_set_func_yield</a> | Specify function used to yield the processor to another thread of control. |
| <a href="add_data_dir_parameter.md" class="xref" title="add_data_dir">add_data_dir</a> | Sets the mutex alignment. |
| <a href="mutex_set_align_parameter.md" class="xref" title="mutex_set_align">mutex_set_align</a> | Sets the mutex alignment. |
| <a href="mutex_set_increment_parameter.md" class="xref" title="mutex_set_increment">mutex_set_increment</a> | Configures the number of additional mutexes to allocate. |
| <a href="mutex_set_max_parameter.md" class="xref" title="mutex_set_max">mutex_set_max</a> | Configures the total number of mutexes to allocate. |
| <a href="mutex_set_tas_spins_parameter.md" class="xref" title="mutex_set_tas_spins">mutex_set_tas_spins</a> | Specifies the number of times the test-and-set mutexes should spin without blocking. |
| <a href="rep_set_clockskew_parameter.md" class="xref" title="rep_set_clockskew">rep_set_clockskew</a> | Sets the clock skew ratio. |
| <a href="rep_set_config_parameter.md" class="xref" title="rep_set_config">rep_set_config</a> | Configures the Berkeley DB replication subsystem. |
| <a href="rep_set_limit_parameter.md" class="xref" title="rep_set_limit">rep_set_limit</a> | Sets record transmission throttling. |
| <a href="rep_set_nsites_parameter.md" class="xref" title="rep_set_nsites">rep_set_nsites</a> | Specifies the total number of sites in a replication group. |
| <a href="rep_set_priority_parameter.md" class="xref" title="rep_set_priority">rep_set_priority</a> | Specifies the database environment's priority. |
| <a href="rep_set_request_parameter.md" class="xref" title="rep_set_request">rep_set_request</a> | Sets a threshold before requesting retransmission of a missing message. |
| <a href="rep_set_timeout_parameter.md" class="xref" title="rep_set_timeout">rep_set_timeout</a> | Specifies a variety of replication timeout values. |
| <a href="repmgr_set_ack_policy_parameter.md" class="xref" title="repmgr_set_ack_policy">repmgr_set_ack_policy</a> | Specifies how master and client sites will handle acknowledgment. |
| <a href="repmgr_site_parameter.md" class="xref" title="repmgr_site">repmgr_site</a> | Identifies a Replication Manager host. |
| <a href="set_cachesize_parameter.md" class="xref" title="set_cachesize">set_cachesize</a> | Sets the size of the shared memory buffer pool. |
| <a href="set_cache_max_parameter.md" class="xref" title="set_cache_max">set_cache_max</a> | Sets the maximum size for set_cachesize parameter. |
| <a href="set_create_dir_parameter.md" class="xref" title="set_create_dir">set_create_dir</a> | Sets the directory path to create the access method database files. |
| <a href="set_data_len_parameter.md" class="xref" title="set_data_len">set_data_len</a> | Sets the maximum number of bytes displayed by some utilities. |
| <a href="set_flags_parameter.md" class="xref" title="set_flags">set_flags</a> | Configures a database environment. |
| <a href="set_intermediate_dir_mode_parameter.md" class="xref" title="set_intermediate_dir_mode">set_intermediate_dir_mode</a> | Configures the directory permissions. |
| <a href="set_lg_bsize_parameter.md" class="xref" title="set_lg_bsize">set_lg_bsize</a> | Sets the size of the in-memory log buffer. |
| <a href="set_lg_dir_parameter.md" class="xref" title="set_lg_dir">set_lg_dir</a> | Sets the path of the directory for logging files. |
| <a href="set_lg_filemode_parameter.md" class="xref" title="set_lg_filemode">set_lg_filemode</a> | Sets the absolute file mode for created log files. |
| <a href="set_lg_max_parameter.md" class="xref" title="set_lg_max">set_lg_max</a> | Sets the maximum size of a single file in the log. |
| <a href="set_lg_regionmax_parameter.md" class="xref" title="set_lg_regionmax">set_lg_regionmax</a> | Sets the size of the underlying logging area. |
| <a href="set_lk_detect_parameter.md" class="xref" title="set_lk_detect">set_lk_detect</a> | Sets the maximum number of locking entities. |
| <a href="set_lk_max_lockers_parameter.md" class="xref" title="set_lk_max_lockers">set_lk_max_lockers</a> | Sets the maximum number of locking entities. |
| <a href="set_lk_max_locks_parameter.md" class="xref" title="set_lk_max_locks">set_lk_max_locks</a> | Sets the maximum number of locks supported by the Berkeley DB environment. |
| <a href="set_lk_max_objects_parameter.md" class="xref" title="set_lk_max_objects">set_lk_max_objects</a> | Sets the maximum number of locked objects. |
| <a href="set_lk_partitions_parameter.md" class="xref" title="set_lk_partitions">set_lk_partitions</a> | Sets the number of lock table partitions in the Berkeley DB environment. |
| <a href="log_set_config_parameter.md" class="xref" title="log_set_config">log_set_config</a> | Configures the Berkeley DB logging subsystem. |
| <a href="set_mp_max_openfd_parameter.md" class="xref" title="set_mp_max_openfd">set_mp_max_openfd</a> | Limits the number of file descriptors the library will open concurrently when flushing dirty pages from the cache. |
| <a href="set_mp_max_write_parameter.md" class="xref" title="set_mp_max_write">set_mp_max_write</a> | Limits the number of sequential write operations |
| <a href="set_mp_mmapsize_parameter.md" class="xref" title="set_mp_mmapsize">set_mp_mmapsize</a> | Sets the maximum file size. |
| <a href="set_open_flags_parameter.md" class="xref" title="set_open_flags">set_open_flags</a> | Initializes specific subsystems of the Berkeley DB environment. |
| <a href="set_shm_key_parameter.md" class="xref" title="set_shm_key">set_shm_key</a> | Configures the database environment's base segment ID. |
| <a href="set_thread_count_parameter.md" class="xref" title="set_thread_count">set_thread_count</a> | Declares an approximate number of threads in the database environment. |
| <a href="set_timeout_parameter.md" class="xref" title="set_timeout">set_timeout</a> | Sets timeout values for locks or transactions. |
| <a href="set_tmp_dir_parameter.md" class="xref" title="set_tmp_dir">set_tmp_dir</a> | Specifies the directory path of temporary files. |
| <a href="set_tx_max_parameter.md" class="xref" title="set_tx_max">set_tx_max</a> | Configures support of simultaneously active transactions. |
| <a href="set_verbose_parameter.md" class="xref" title="set_verbose">set_verbose</a> | Enables/disables the Berkeley DB message output. |

**Legal Notice**

This documentation is distributed under an open source license. You may review the terms of this license at: <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html" class="ulink" target="_top">http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html</a>

Oracle, Berkeley DB, and Sleepycat are trademarks or registered trademarks of Oracle. All rights to these marks are reserved. No third-party use is permitted without the express prior written consent of Oracle.

Other names may be trademarks of their respective owners.

To obtain a copy of this document's original source code, please submit a request to the Oracle Technology Network forum at: <a href="http://forums.oracle.com/forums/forum.jspa?forumID=271" class="ulink" target="_top">http://forums.oracle.com/forums/forum.jspa?forumID=271</a>

9/9/2013

------------------------------------------------------------------------
