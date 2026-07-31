---
title: "Berkeley DB Programmer's Reference Guide"
api-name: "Berkeley DB Programmer's Reference Guide"
source: docs/programmer_reference/index.html
---
# Berkeley DB Programmer's Reference Guide

**Legal Notice**

This documentation is distributed under an open source license. You may review the terms of this license at: <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html" class="ulink" target="_top">http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html</a>

Oracle, Berkeley DB, and Sleepycat are trademarks or registered trademarks of Oracle. All rights to these marks are reserved. No third-party use is permitted without the express prior written consent of Oracle.

Other names may be trademarks of their respective owners.

To obtain a copy of this document's original source code, please submit a request to the Oracle Technology Network forum at: <a href="http://forums.oracle.com/forums/forum.jspa?forumID=271" class="ulink" target="_top">http://forums.oracle.com/forums/forum.jspa?forumID=271</a>

9/9/2013

------------------------------------------------------------------------

**Table of Contents**

<span class="preface"> [Preface](preface.md) </span>

<span class="sect1"> [Conventions Used in this Book](preface.md#conventions) </span>

<span class="sect1"> [For More Information](moreinfo.md) </span>

<span class="sect2"> [Contact Us](moreinfo.md#contact_us) </span>

<span class="chapter"> [1. Introduction](intro.md) </span>

<span class="sect1"> [An introduction to data management](intro.md#intro_data) </span>

<span class="sect1"> [Mapping the terrain: theory and practice](intro_terrain.md) </span>

<span class="sect2"> [Data access and data management](intro_terrain.md#idp50584200) </span>

<span class="sect2"> [Relational databases](intro_terrain.md#idp50577368) </span>

<span class="sect2"> [Object-oriented databases](intro_terrain.md#idp50621160) </span>

<span class="sect2"> [Network databases](intro_terrain.md#idp50574144) </span>

<span class="sect2"> [Clients and servers](intro_terrain.md#idp50647776) </span>

<span class="sect1"> [What is Berkeley DB?](intro_dbis.md) </span>

<span class="sect2"> [Data Access Services](intro_dbis.md#idp50588152) </span>

<span class="sect2"> [Data management services](intro_dbis.md#idm1374112) </span>

<span class="sect2"> [Design](intro_dbis.md#idp50659944) </span>

<span class="sect1"> [What Berkeley DB is not](intro_dbisnot.md) </span>

<span class="sect2"> [Berkeley DB is not a relational database](intro_dbisnot.md#idp50596256) </span>

<span class="sect2"> [Berkeley DB is not an object-oriented database](intro_dbisnot.md#idp50675392) </span>

<span class="sect2"> [Berkeley DB is not a network database](intro_dbisnot.md#idp50621304) </span>

<span class="sect2"> [Berkeley DB is not a database server](intro_dbisnot.md#idp50657008) </span>

<span class="sect1"> [Do you need Berkeley DB?](intro_need.md) </span>

<span class="sect1"> [What other services does Berkeley DB provide?](intro_what.md) </span>

<span class="sect1"> [What does the Berkeley DB distribution include?](intro_distrib.md) </span>

<span class="sect1"> [Where does Berkeley DB run?](intro_where.md) </span>

<span class="sect1"> [The Berkeley DB products](intro_products.md) </span>

<span class="sect2"> [Berkeley DB Data Store](intro_products.md#idp50715960) </span>

<span class="sect2"> [Berkeley DB Concurrent Data Store](intro_products.md#idp50715552) </span>

<span class="sect2"> [Berkeley DB Transactional Data Store](intro_products.md#idp50708368) </span>

<span class="sect2"> [Berkeley DB High Availability](intro_products.md#idp50712672) </span>

<span class="chapter"> [2. Access Method Configuration](am_conf.md) </span>

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

<span class="chapter"> [3. Access Method Operations](am.md) </span>

<span class="sect1"> [Database open](am.md#am_open) </span>

<span class="sect1"> [Opening multiple databases in a single file](am_opensub.md) </span>

<span class="sect2"> [Configuring databases sharing a file](am_opensub.md#idp50943544) </span>

<span class="sect2"> [Caching databases sharing a file](am_opensub.md#idp50944288) </span>

<span class="sect2"> [Locking in databases based on sharing a file](am_opensub.md#idp50944984) </span>

<span class="sect1"> [Partitioning databases](am_partition.md) </span>

<span class="sect2"> [Specifying partition keys](am_partition.md#am_partition_keys) </span>

<span class="sect2"> [Partitioning callback](am_partition.md#am_partition_function) </span>

<span class="sect2"> [Placing partition files](am_partition.md#partition_file_placement) </span>

<span class="sect1"> [Retrieving records](am_get.md) </span>

<span class="sect1"> [Storing records](am_put.md) </span>

<span class="sect1"> [Deleting records](am_delete.md) </span>

<span class="sect1"> [Database statistics](am_stat.md) </span>

<span class="sect1"> [Database truncation](am_truncate.md) </span>

<span class="sect1"> [Database upgrade](am_upgrade.md) </span>

<span class="sect1"> [Database verification and salvage](am_verify.md) </span>

<span class="sect1"> [Flushing the database cache](am_sync.md) </span>

<span class="sect1"> [Database close](am_close.md) </span>

<span class="sect1"> [Secondary indexes](am_second.md) </span>

<span class="sect2"> [Error Handling With Secondary Indexes](am_second.md#idp51040080) </span>

<span class="sect1"> [Foreign key indexes](am_foreign.md) </span>

<span class="sect1"> [Cursor operations](am_cursor.md) </span>

<span class="sect2"> [Retrieving records with a cursor](am_cursor.md#am_curget) </span>

<span class="sect2"> [Storing records with a cursor](am_cursor.md#am_curput) </span>

<span class="sect2"> [Deleting records with a cursor](am_cursor.md#am_curdel) </span>

<span class="sect2"> [Duplicating a cursor](am_cursor.md#am_curdup) </span>

<span class="sect2"> [Equality Join](am_cursor.md#am_join) </span>

<span class="sect2"> [Data item count](am_cursor.md#am_count) </span>

<span class="sect2"> [Cursor close](am_cursor.md#am_curclose) </span>

<span class="chapter"> [4. Access Method Wrapup](am_misc.md) </span>

<span class="sect1"> [Data alignment](am_misc.md#am_misc_align) </span>

<span class="sect1"> [Retrieving and updating records in bulk](am_misc_bulk.md) </span>

<span class="sect2"> [Bulk retrieval](am_misc_bulk.md#am_misc_bulk_get) </span>

<span class="sect2"> [Bulk updates](am_misc_bulk.md#am_misc_bulk_put) </span>

<span class="sect2"> [Bulk deletes](am_misc_bulk.md#am_misc_bulk_del) </span>

<span class="sect1"> [Partial record storage and retrieval](am_misc_partial.md) </span>

<span class="sect1"> [Storing C/C++ structures/objects](am_misc_struct.md) </span>

<span class="sect1"> [Retrieved key/data permanence for C/C++](am_misc_perm.md) </span>

<span class="sect1"> [Error support](am_misc_error.md) </span>

<span class="sect1"> [Cursor stability](am_misc_stability.md) </span>

<span class="sect1"> [Database limits](am_misc_dbsizes.md) </span>

<span class="sect1"> [Disk space requirements](am_misc_diskspace.md) </span>

<span class="sect2"> [Btree](am_misc_diskspace.md#idp51253016) </span>

<span class="sect2"> [Hash](am_misc_diskspace.md#idp51253080) </span>

<span class="sect1"> [Specifying a Berkeley DB schema using SQL DDL](am_misc_db_sql.md) </span>

<span class="sect1"> [Access method tuning](am_misc_tune.md) </span>

<span class="sect1"> [Access method FAQ](am_misc_faq.md) </span>

<span class="chapter"> [5. Java API](java.md) </span>

<span class="sect1"> [Java configuration](java.md#java_conf) </span>

<span class="sect1"> [Compatibility](java_compat.md) </span>

<span class="sect1"> [Java programming notes](java_program.md) </span>

<span class="sect1"> [Java FAQ](java_faq.md) </span>

<span class="chapter"> [6. C# API](csharp.md) </span>

<span class="sect1"> [Compatibility](csharp.md#csharp_compat) </span>

<span class="chapter"> [7. Standard Template Library API](stl.md) </span>

<span class="sect1"> [Dbstl introduction](stl.md#stl_intro) </span>

<span class="sect2"> [Standards compatible](stl.md#stl_intro_stdcompat) </span>

<span class="sect2"> [Performance overhead](stl.md#stl_intro_performance) </span>

<span class="sect2"> [Portability](stl.md#stl_intro_portability) </span>

<span class="sect1"> [Dbstl typical use cases](stl_usecase.md) </span>

<span class="sect1"> [Dbstl examples](stl_examples.md) </span>

<span class="sect1"> [Berkeley DB configuration](stl_db_usage.md) </span>

<span class="sect2"> [Registering database and environment handles](stl_db_usage.md#idp51381760) </span>

<span class="sect2"> [Truncate requirements](stl_db_usage.md#idp51405208) </span>

<span class="sect2"> [Auto commit support](stl_db_usage.md#idp51416888) </span>

<span class="sect2"> [Database and environment identity checks](stl_db_usage.md#idp51379224) </span>

<span class="sect2"> [Products, constructors and configurations](stl_db_usage.md#idp51415360) </span>

<span class="sect1"> [Using advanced Berkeley DB features with dbstl](stl_db_advanced_usage.md) </span>

<span class="sect2"> [Using bulk retrieval iterators](stl_db_advanced_usage.md#idp51421384) </span>

<span class="sect2"> [Using the DB_RMW flag](stl_db_advanced_usage.md#idp51410312) </span>

<span class="sect2"> [Using secondary index database and secondary containers](stl_db_advanced_usage.md#idp51398048) </span>

<span class="sect1"> [Using transactions in dbstl](stl_txn_usage.md) </span>

<span class="sect1"> [Using dbstl in multithreaded applications](stl_mt_usage.md) </span>

<span class="sect1"> [Working with primitive types](stl_primitive_rw.md) </span>

<span class="sect2"> [Storing strings](stl_primitive_rw.md#idp51467888) </span>

<span class="sect1"> [Store and Retrieve data or objects of complex types](stl_complex_rw.md) </span>

<span class="sect2"> [Storing varying length objects](stl_complex_rw.md#idp51458752) </span>

<span class="sect2"> [Storing arbitrary sequences](stl_complex_rw.md#idp51477944) </span>

<span class="sect2"> [Notes](stl_complex_rw.md#idp51524696) </span>

<span class="sect1"> [Dbstl persistence](stl_persistence.md) </span>

<span class="sect2"> [Direct database get](stl_persistence.md#directdbget) </span>

<span class="sect2"> [Change persistence](stl_persistence.md#chg_persistence) </span>

<span class="sect2"> [Object life time and persistence](stl_persistence.md#obj_life_persistence) </span>

<span class="sect1"> [Dbstl container specific notes](stl_container_specific.md) </span>

<span class="sect2"> [db_vector specific notes](stl_container_specific.md#idp51492808) </span>

<span class="sect2"> [Associative container specific notes](stl_container_specific.md#idp51561456) </span>

<span class="sect1"> [Using dbstl efficiently](stl_efficienct_use.md) </span>

<span class="sect2"> [Using iterators efficiently](stl_efficienct_use.md#idp51530568) </span>

<span class="sect2"> [Using containers efficiently](stl_efficienct_use.md#idp51530352) </span>

<span class="sect1"> [Dbstl memory management](stl_memory_mgmt.md) </span>

<span class="sect2"> [Freeing memory](stl_memory_mgmt.md#idp51564672) </span>

<span class="sect2"> [Type specific notes](stl_memory_mgmt.md#idp51569240) </span>

<span class="sect1"> [Dbstl miscellaneous notes](stl_misc.md) </span>

<span class="sect2"> [Special notes about trivial methods](stl_misc.md#idp51587208) </span>

<span class="sect2"> [Using correct container and iterator public types](stl_misc.md#idp51603304) </span>

<span class="sect1"> [Dbstl known issues](stl_known_issues.md) </span>

<span class="chapter"> [8. Berkeley DB Architecture](arch.md) </span>

<span class="sect1"> [The big picture](arch.md#arch_bigpic) </span>

<span class="sect1"> [Programming model](arch_progmodel.md) </span>

<span class="sect1"> [Programmatic APIs](arch_apis.md) </span>

<span class="sect2"> [C](arch_apis.md#idp51640232) </span>

<span class="sect2"> [C++](arch_apis.md#idp51656168) </span>

<span class="sect2"> [STL](arch_apis.md#idp51646944) </span>

<span class="sect2"> [Java](arch_apis.md#idp51647760) </span>

<span class="sect2"> [Dbm/Ndbm, Hsearch](arch_apis.md#idp51664896) </span>

<span class="sect1"> [Scripting languages](arch_script.md) </span>

<span class="sect2"> [Perl](arch_script.md#idp51640920) </span>

<span class="sect2"> [PHP](arch_script.md#idp51639128) </span>

<span class="sect2"> [Tcl](arch_script.md#idp51657264) </span>

<span class="sect1"> [Supporting utilities](arch_utilities.md) </span>

<span class="chapter"> [9. The Berkeley DB Environment](env.md) </span>

<span class="sect1"> [Database environment introduction](env.md#env_intro) </span>

<span class="sect1"> [Creating a database environment](env_create.md) </span>

<span class="sect1"> [Sizing a database environment](env_size.md) </span>

<span class="sect1"> [Opening databases within the environment](env_open.md) </span>

<span class="sect1"> [Error support](env_error.md) </span>

<span class="sect1"> [DB_CONFIG configuration file](env_db_config.md) </span>

<span class="sect1"> [File naming](env_naming.md) </span>

<span class="sect2"> [Specifying file naming to Berkeley DB](env_naming.md#idp51749352) </span>

<span class="sect2"> [Filename resolution in Berkeley DB](env_naming.md#idp51763728) </span>

<span class="sect2"> [Examples](env_naming.md#idp51756464) </span>

<span class="sect1"> [Shared memory regions](env_region.md) </span>

<span class="sect1"> [Security](env_security.md) </span>

<span class="sect1"> [Encryption](env_encrypt.md) </span>

<span class="sect1"> [Remote filesystems](env_remote.md) </span>

<span class="sect1"> [Environment FAQ](env_faq.md) </span>

<span class="chapter"> [10. Berkeley DB Concurrent Data Store Applications](cam.md) </span>

<span class="sect1"> [Concurrent Data Store introduction](cam.md#cam_intro) </span>

<span class="sect1"> [Handling failure in Data Store and Concurrent Data Store applications](cam_fail.md) </span>

<span class="sect1"> [Architecting Data Store and Concurrent Data Store applications](cam_app.md) </span>

<span class="chapter"> [11. Berkeley DB Transactional Data Store Applications](transapp.md) </span>

<span class="sect1"> [Transactional Data Store introduction](transapp.md#transapp_intro) </span>

<span class="sect1"> [Why transactions?](transapp_why.md) </span>

<span class="sect1"> [Terminology](transapp_term.md) </span>

<span class="sect1"> [Handling failure in Transactional Data Store applications](transapp_fail.md) </span>

<span class="sect1"> [Architecting Transactional Data Store applications](transapp_app.md) </span>

<span class="sect1"> [Opening the environment](transapp_env_open.md) </span>

<span class="sect1"> [Opening the databases](transapp_data_open.md) </span>

<span class="sect1"> [Recoverability and deadlock handling](transapp_put.md) </span>

<span class="sect1"> [Atomicity](transapp_atomicity.md) </span>

<span class="sect1"> [Isolation](transapp_inc.md) </span>

<span class="sect1"> [Degrees of isolation](transapp_read.md) </span>

<span class="sect2"> [Snapshot Isolation](transapp_read.md#snapshot_isolation) </span>

<span class="sect1"> [Transactional cursors](transapp_cursor.md) </span>

<span class="sect1"> [Nested transactions](transapp_nested.md) </span>

<span class="sect1"> [Environment infrastructure](transapp_admin.md) </span>

<span class="sect1"> [Deadlock detection](transapp_deadlock.md) </span>

<span class="sect1"> [Checkpoints](transapp_checkpoint.md) </span>

<span class="sect1"> [Database and log file archival](transapp_archival.md) </span>

<span class="sect1"> [Log file removal](transapp_logfile.md) </span>

<span class="sect1"> [Recovery procedures](transapp_recovery.md) </span>

<span class="sect1"> [Hot failover](transapp_hotfail.md) </span>

<span class="sect1"> [Using Recovery on Journaling Filesystems](transapp_journal.md) </span>

<span class="sect1"> [Recovery and filesystem operations](transapp_filesys.md) </span>

<span class="sect1"> [Berkeley DB recoverability](transapp_reclimit.md) </span>

<span class="sect1"> [Transaction tuning](transapp_tune.md) </span>

<span class="sect1"> [Transaction throughput](transapp_throughput.md) </span>

<span class="sect1"> [Transaction FAQ](transapp_faq.md) </span>

<span class="chapter"> [12. Berkeley DB Replication](rep.md) </span>

<span class="sect1"> [Replication introduction](rep.md#rep_intro) </span>

<span class="sect1"> [Replication environment IDs](rep_id.md) </span>

<span class="sect1"> [Replication environment priorities](rep_pri.md) </span>

<span class="sect1"> [Building replicated applications](rep_app.md) </span>

<span class="sect1"> [Replication Manager methods](rep_mgr_meth.md) </span>

<span class="sect1"> [Base API Methods](rep_base_meth.md) </span>

<span class="sect1"> [Building the communications infrastructure](rep_comm.md) </span>

<span class="sect1"> [Connecting to a new site](rep_newsite.md) </span>

<span class="sect1"> [Managing Replication Manager Group Membership](group_membership.md) </span>

<span class="sect2"> [Adding Sites to a Replication Group](group_membership.md#group_mem_add) </span>

<span class="sect2"> [Removing Sites from a Replication Group](group_membership.md#group_mem_remove) </span>

<span class="sect2"> [Primordial Startups](group_membership.md#group_mem_primordialstartup) </span>

<span class="sect2"> [Upgrading Groups](group_membership.md#group_mem_upgrade) </span>

<span class="sect1"> [Managing Replication Files](rep_filename.md) </span>

<span class="sect1"> [Running Replication Manager in multiple processes](rep_mgrmulti.md) </span>

<span class="sect2"> [One replication process and multiple subordinate processes](rep_mgrmulti.md#idp52420616) </span>

<span class="sect2"> [Persistence of local site network address configuration](rep_mgrmulti.md#idp52417008) </span>

<span class="sect2"> [Programming considerations](rep_mgrmulti.md#idp52400144) </span>

<span class="sect2"> [Handling failure](rep_mgrmulti.md#idp52414488) </span>

<span class="sect2"> [Other miscellaneous rules](rep_mgrmulti.md#idp52412256) </span>

<span class="sect1"> [Running Replication using the db_replicate Utility](rep_replicate.md) </span>

<span class="sect2"> [One Replication Process and Multiple Subordinate Processes](rep_replicate.md#idp52430544) </span>

<span class="sect2"> [Common Use Case](rep_replicate.md#idp52447760) </span>

<span class="sect2"> [Avoiding Rollback](rep_replicate.md#idp52457840) </span>

<span class="sect2"> [When to Consider an Integrated HA Application](rep_replicate.md#idp52462952) </span>

<span class="sect1"> [Choosing a Replication Manager Ack Policy](rep_mgr_ack.md) </span>

<span class="sect1"> [Elections](rep_elect.md) </span>

<span class="sect1"> [Synchronizing with a master](rep_mastersync.md) </span>

<span class="sect2"> [Delaying client synchronization](rep_mastersync.md#rep_delay_sync) </span>

<span class="sect2"> [Client-to-client synchronization](rep_mastersync.md#rep_c2c_sync) </span>

<span class="sect2"> [Blocked client operations](rep_mastersync.md#idp52488504) </span>

<span class="sect2"> [Clients too far out-of-date to synchronize](rep_mastersync.md#idp52510624) </span>

<span class="sect1"> [Initializing a new site](rep_init.md) </span>

<span class="sect1"> [Bulk transfer](rep_bulk.md) </span>

<span class="sect1"> [Transactional guarantees](rep_trans.md) </span>

<span class="sect1"> [Master Leases](rep_lease.md) </span>

<span class="sect2"> [Changing Group Size](rep_lease.md#masterlease_change_groupsize) </span>

<span class="sect1"> [Read your writes consistency](rep_ryw.md) </span>

<span class="sect2"> [Getting a token](rep_ryw.md#gettoken) </span>

<span class="sect2"> [Token handling](rep_ryw.md#tokenhandling) </span>

<span class="sect2"> [Using a token to check or wait for a transaction](rep_ryw.md#usingtoken) </span>

<span class="sect1"> [Clock Skew](rep_clock_skew.md) </span>

<span class="sect1"> [Using Replication Manager message channels](repmgr_channels.md) </span>

<span class="sect2"> [DB_CHANNEL](repmgr_channels.md#dbchannel_class) </span>

<span class="sect2"> [Sending messages over a message channel](repmgr_channels.md#dbchannel_send) </span>

<span class="sect2"> [Receiving messages](repmgr_channels.md#dbchannel_receive) </span>

<span class="sect1"> [Special considerations for two-site replication groups](rep_twosite.md) </span>

<span class="sect1"> [Network partitions](rep_partition.md) </span>

<span class="sect1"> [Replication FAQ](rep_faq.md) </span>

<span class="sect1"> [Ex_rep: a replication example](rep_ex.md) </span>

<span class="sect1"> [Ex_rep_base: a TCP/IP based communication infrastructure](rep_ex_comm.md) </span>

<span class="sect1"> [Ex_rep_base: putting it all together](rep_ex_rq.md) </span>

<span class="sect1"> [Ex_rep_chan: a Replication Manager channel example](rep_ex_chan.md) </span>

<span class="chapter"> [13. Distributed Transactions](xa.md) </span>

<span class="sect1"> [Introduction](xa.md#xa_intro) </span>

<span class="sect1"> [Berkeley DB XA Implementation](ch13s02.md) </span>

<span class="sect1"> [Building a Global Transaction Manager](xa_build.md) </span>

<span class="sect2"> [Communicating with multiple Berkeley DB environments](xa_build.md#idp52778488) </span>

<span class="sect2"> [Recovering from GTM failure](xa_build.md#idp52779432) </span>

<span class="sect2"> [Managing the Global Transaction ID (GID) name space](xa_build.md#idp52703176) </span>

<span class="sect2"> [Maintaining state for each distributed transaction.](xa_build.md#idp52758336) </span>

<span class="sect2"> [Recovering from the failure of a single environment](xa_build.md#idp52777008) </span>

<span class="sect2"> [Recovering from GTM failure](xa_build.md#idp52779896) </span>

<span class="sect1"> [XA Introduction](xa_xa_intro.md) </span>

<span class="sect1"> [Configuring Berkeley DB with the Tuxedo System](xa_xa_config.md) </span>

<span class="sect2"> [Update the Resource Manager File in Tuxedo](xa_xa_config.md#idp52786896) </span>

<span class="sect2"> [Build the Transaction Manager Server](xa_xa_config.md#idp52812512) </span>

<span class="sect2"> [Update the UBBCONFIG File](xa_xa_config.md#idp52759288) </span>

<span class="sect1"> [Restrictions on XA Transactions](xa_xa_restrict.md) </span>

<span class="sect1"> [XA: Frequently Asked Questions](xa_faq.md) </span>

<span class="chapter"> [14. Application Specific Logging and Recovery](apprec.md) </span>

<span class="sect1"> [Introduction to application specific logging and recovery](apprec.md#apprec_intro) </span>

<span class="sect1"> [Defining application-specific log records](apprec_def.md) </span>

<span class="sect1"> [Automatically generated functions](apprec_auto.md) </span>

<span class="sect1"> [Application configuration](apprec_config.md) </span>

<span class="chapter"> [15. Programmer Notes](program.md) </span>

<span class="sect1"> [Signal handling](program.md#program_appsignals) </span>

<span class="sect1"> [Error returns to applications](program_errorret.md) </span>

<span class="sect1"> [Environment variables](program_environ.md) </span>

<span class="sect1"> [Multithreaded applications](program_mt.md) </span>

<span class="sect1"> [Berkeley DB handles](program_scope.md) </span>

<span class="sect1"> [Name spaces](program_namespace.md) </span>

<span class="sect2"> [C Language Name Space](program_namespace.md#idp52962960) </span>

<span class="sect2"> [Filesystem Name Space](program_namespace.md#idp53001824) </span>

<span class="sect1"> [Memory-only or Flash configurations](program_ram.md) </span>

<span class="sect1"> [Disk drive caches](program_cache.md) </span>

<span class="sect1"> [Copying or moving databases](program_copy.md) </span>

<span class="sect1"> [Compatibility with historic UNIX interfaces](program_compatible.md) </span>

<span class="sect1"> [Run-time configuration](program_runtime.md) </span>

<span class="sect1"> [Performance Event Monitoring](program_perfmon.md) </span>

<span class="sect2"> [Using the DTrace Provider](program_perfmon.md#program_perfmon_dtrace) </span>

<span class="sect2"> [Using SystemTap](program_perfmon.md#program_perfmon_stap) </span>

<span class="sect2"> [Example Scripts](program_perfmon.md#program_perfmon_examples) </span>

<span class="sect2"> [Performance Events Reference](program_perfmon.md#program_perfmon_probes) </span>

<span class="sect1"> [Programmer notes FAQ](program_faq.md) </span>

<span class="chapter"> [16. The Locking Subsystem](lock.md) </span>

<span class="sect1"> [Introduction to the locking subsystem](lock.md#lock_intro) </span>

<span class="sect1"> [Configuring locking](lock_config.md) </span>

<span class="sect1"> [Configuring locking: sizing the system](lock_max.md) </span>

<span class="sect1"> [Standard lock modes](lock_stdmode.md) </span>

<span class="sect1"> [Deadlock detection](lock_dead.md) </span>

<span class="sect1"> [Deadlock detection using timers](lock_timeout.md) </span>

<span class="sect1"> [Deadlock debugging](lock_deaddbg.md) </span>

<span class="sect1"> [Locking granularity](lock_page.md) </span>

<span class="sect1"> [Locking without transactions](lock_notxn.md) </span>

<span class="sect1"> [Locking with transactions: two-phase locking](lock_twopl.md) </span>

<span class="sect1"> [Berkeley DB Concurrent Data Store locking conventions](lock_cam_conv.md) </span>

<span class="sect1"> [Berkeley DB Transactional Data Store locking conventions](lock_am_conv.md) </span>

<span class="sect1"> [Locking and non-Berkeley DB applications](lock_nondb.md) </span>

<span class="chapter"> [17. The Logging Subsystem](log.md) </span>

<span class="sect1"> [Introduction to the logging subsystem](log.md#log_intro) </span>

<span class="sect1"> [Configuring logging](log_config.md) </span>

<span class="sect1"> [Log file limits](log_limits.md) </span>

<span class="chapter"> [18. The Memory Pool Subsystem](mp.md) </span>

<span class="sect1"> [Introduction to the memory pool subsystem](mp.md#mp_intro) </span>

<span class="sect1"> [Configuring the memory pool](mp_config.md) </span>

<span class="sect1"> [Warming the memory pool](mp_warm.md) </span>

<span class="sect2"> [The warm_cache() function](mp_warm.md#warm_cache) </span>

<span class="chapter"> [19. The Transaction Subsystem](txn.md) </span>

<span class="sect1"> [Introduction to the transaction subsystem](txn.md#txn_intro) </span>

<span class="sect1"> [Configuring transactions](txn_config.md) </span>

<span class="sect1"> [Transaction limits](txn_limits.md) </span>

<span class="sect2"> [Transaction IDs](txn_limits.md#idp53352320) </span>

<span class="sect2"> [Cursors](txn_limits.md#idp53275624) </span>

<span class="sect2"> [Multiple Threads of Control](txn_limits.md#idp53223368) </span>

<span class="chapter"> [20. Sequences](sequence.md) </span>

<span class="chapter"> [21. Berkeley DB Extensions: Tcl](tcl.md) </span>

<span class="sect1"> [Loading Berkeley DB with Tcl](tcl.md#tcl_intro) </span>

<span class="sect2"> [Installing as a Tcl Package](tcl.md#idp53366464) </span>

<span class="sect2"> [Loading Berkeley DB with Tcl](tcl.md#idp53356912) </span>

<span class="sect1"> [Using Berkeley DB with Tcl](tcl_using.md) </span>

<span class="sect1"> [Tcl API programming notes](tcl_program.md) </span>

<span class="sect1"> [Tcl error handling](tcl_error.md) </span>

<span class="sect1"> [Tcl FAQ](tcl_faq.md) </span>

<span class="chapter"> [22. Berkeley DB Extensions](ext.md) </span>

<span class="sect1"> [Using Berkeley DB with Apache](ext.md#ext_mod) </span>

<span class="sect1"> [Using Berkeley DB with Perl](ext_perl.md) </span>

<span class="sect1"> [Using Berkeley DB with PHP](ext_php.md) </span>

<span class="chapter"> [23. Dumping and Reloading Databases](dumpload.md) </span>

<span class="sect1"> [The db_dump and db_load utilities](dumpload.md#dumpload_utility) </span>

<span class="sect1"> [Dump output formats](dumpload_format.md) </span>

<span class="sect1"> [Loading text into databases](dumpload_text.md) </span>

<span class="chapter"> [24. Additional References](refs.md) </span>

<span class="sect1"> [Additional references](refs.md#refs_refs) </span>

<span class="sect2"> [Technical Papers on Berkeley DB](refs.md#idp53369464) </span>

<span class="sect2"> [Background on Berkeley DB Features](refs.md#idp53449960) </span>

<span class="sect2"> [Database Systems Theory](refs.md#idp53443200) </span>
