---
title: "Berkeley DB Installation and Build Guide"
api-name: "Berkeley DB Installation and Build Guide"
source: docs/installation/index.html
---
# Berkeley DB Installation and Build Guide

**Legal Notice**

9/9/2013

------------------------------------------------------------------------

**Table of Contents**

<span class="preface"> [Preface](preface.md) </span>

<span class="sect1"> [Conventions Used in this Book](preface.md#conventions) </span>

<span class="sect1"> [For More Information](moreinfo.md) </span>

<span class="sect2"> [Contact Us](moreinfo.md#contact_us) </span>

<span class="chapter"> [1. Introduction](introduction.md) </span>

<span class="sect1"> [Installation Overview](introduction.md#install-overview) </span>

<span class="sect1"> [Supported Platforms](ch01s02.md) </span>

<span class="chapter"> [2. System Installation Notes](install.md) </span>

<span class="sect1"> [File utility /etc/magic information](install.md#install_file) </span>

<span class="sect2"> [Magic information](install.md#magic) </span>

<span class="sect2"> [Big-endian magic information](install.md#big-endian) </span>

<span class="sect2"> [Little-endian magic information](install.md#little-endian) </span>

<span class="sect1"> [Building with multiple versions of Berkeley DB](install_multiple.md) </span>

<span class="chapter"> [3. Debugging Applications](debug.md) </span>

<span class="sect1"> [Introduction to debugging](debug.md#debug_intro) </span>

<span class="sect1"> [Compile-time configuration](debug_compile.md) </span>

<span class="sect1"> [Run-time error information](debug_runtime.md) </span>

<span class="sect1"> [Reviewing Berkeley DB log files](debug_printlog.md) </span>

<span class="sect2"> [Augmenting the Log for Debugging](debug_printlog.md#idp121880) </span>

<span class="sect2"> [Extracting Committed Transactions and Transaction Status](debug_printlog.md#idp53840) </span>

<span class="sect2"> [Extracting Transaction Histories](debug_printlog.md#idp41744) </span>

<span class="sect2"> [Extracting File Histories](debug_printlog.md#idp154152) </span>

<span class="sect2"> [Extracting Page Histories](debug_printlog.md#idp158032) </span>

<span class="sect2"> [Other log processing tools](debug_printlog.md#idp124648) </span>

<span class="chapter"> [4. Building Berkeley DB for Android](build_android_intro.md) </span>

<span class="sect1"> [Building the Drop-In Replacement for Android](build_android_intro.md#build_android) </span>

<span class="sect2"> [Migrating from SQLite to Berkeley DB](build_android_intro.md#build_android_migrate) </span>

<span class="sect1"> [Building the Android JDBC Driver](build_android_jdbc.md) </span>

<span class="sect1"> [Android Configuration Options](build_android_config.md) </span>

<span class="chapter"> [5. Building Berkeley DB for Windows](build_win.md) </span>

<span class="sect1"> [Building Berkeley DB for 32 bit Windows](build_win.md#win_build32) </span>

<span class="sect2"> [Visual C++ .NET 2010](build_win.md#idp242512) </span>

<span class="sect2"> [Visual C++ .NET 2008](build_win.md#idp249264) </span>

<span class="sect2"> [Visual C++ .NET 2005](build_win.md#idp220616) </span>

<span class="sect2"> [Build results](build_win.md#idp205672) </span>

<span class="sect1"> [Building Berkeley DB for 64-bit Windows](win_build64.md) </span>

<span class="sect2"> [x64 build with Visual Studio 2005 or newer](win_build64.md#idp259672) </span>

<span class="sect1"> [Building Berkeley DB with Cygwin](win_build_cygwin.md) </span>

<span class="sect1"> [Building the C++ API](win_build_cxx.md) </span>

<span class="sect1"> [Building the C++ STL API](win_build_stl.md) </span>

<span class="sect1"> [Building the Java API](build_win_java.md) </span>

<span class="sect1"> [Building the SQL API](build_win_sql.md) </span>

<span class="sect2"> [Binary Compatibility With SQLite](build_win_sql.md#idp290248) </span>

<span class="sect2"> [Setting Preprocessor Flags](build_win_sql.md#idp276576) </span>

<span class="sect2"> [Enabling Extensions](build_win_sql.md#idp288280) </span>

<span class="sect2"> [Disabling Log Checksums](build_win_sql.md#win-disablechecksums) </span>

<span class="sect2"> [Building the JDBC Driver](build_win_sql.md#build_jdbc) </span>

<span class="sect2"> [Using the JDBC Driver](build_win_sql.md#idp266616) </span>

<span class="sect2"> [Building the ODBC Driver](build_win_sql.md#idp305704) </span>

<span class="sect2"> [Using the ADO.NET Driver](build_win_sql.md#idp320888) </span>

<span class="sect1"> [Building the Tcl API](build_win_tcl.md) </span>

<span class="sect1"> [Distributing DLLs](win_build_dist_dll.md) </span>

<span class="sect1"> [Additional build options](win_additional_options.md) </span>

<span class="sect1"> [Building a small memory footprint library](build_win_small.md) </span>

<span class="sect1"> [Running the test suite under Windows](build_win_test.md) </span>

<span class="sect2"> [Building the software needed by the tests](build_win_test.md#idp368040) </span>

<span class="sect2"> [Running the test suite under Windows](build_win_test.md#idp379184) </span>

<span class="sect2"> [Building the software needed by the SQL tests](build_win_test.md#build_win_test_sql) </span>

<span class="sect1"> [Windows notes](build_win_notes.md) </span>

<span class="sect1"> [Windows FAQ](build_win_faq.md) </span>

<span class="chapter"> [7. Building Berkeley DB for UNIX/POSIX](build_unix.md) </span>

<span class="sect1"> [Building for UNIX/POSIX](build_unix.md#build_unix_intro) </span>

<span class="sect2"> [Building the Berkeley DB SQL Interface](build_unix.md#build_unix_sqlinter) </span>

<span class="sect1"> [Configuring Berkeley DB](build_unix_conf.md) </span>

<span class="sect1"> [Configuring the SQL Interface](build_unix_sql.md) </span>

<span class="sect2"> [Changing Compile Options](build_unix_sql.md#config_sql) </span>

<span class="sect2"> [Enabling Extensions](build_unix_sql.md#idp500824) </span>

<span class="sect2"> [Building the JDBC Driver](build_unix_sql.md#build_unix_jdbc) </span>

<span class="sect2"> [Using the JDBC Driver](build_unix_sql.md#idp571856) </span>

<span class="sect2"> [Building the ODBC Driver](build_unix_sql.md#idp593744) </span>

<span class="sect2"> [Building the BFILE extension](build_unix_sql.md#bfile) </span>

<span class="sect1"> [Building a small memory footprint library](build_unix_small.md) </span>

<span class="sect1"> [Changing compile or load options](build_unix_flags.md) </span>

<span class="sect1"> [Cross-Compiling on Unix](cross_compile_unix.md) </span>

<span class="sect1"> [Installing Berkeley DB](build_unix_install.md) </span>

<span class="sect1"> [Dynamic shared libraries](build_unix_shlib.md) </span>

<span class="sect1"> [Running the test suite under UNIX](build_unix_test.md) </span>

<span class="sect2"> [Building SQL Test Suite on Unix](build_unix_test.md#build_unix_test_sql) </span>

<span class="sect1"> [Architecture independent FAQ](build_unix_notes.md) </span>

<span class="sect1"> [AIX](build_unix_aix.md) </span>

<span class="sect1"> [FreeBSD](build_unix_freebsd.md) </span>

<span class="sect1"> [Apple iOS (iPhone OS)](build_unix_iphone.md) </span>

<span class="sect1"> [IRIX](build_unix_irix.md) </span>

<span class="sect1"> [Linux](build_unix_linux.md) </span>

<span class="sect1"> [Mac OS X](build_unix_macosx.md) </span>

<span class="sect1"> [QNX](build_unix_qnx.md) </span>

<span class="sect1"> [SCO](build_unix_sco.md) </span>

<span class="sect1"> [Solaris](build_unix_solaris.md) </span>

<span class="sect1"> [SunOS](build_unix_sunos.md) </span>

<span class="chapter"> [9. Upgrading Berkeley DB 11.2.5.2 applications to Berkeley DB 11.2.5.3](upgrade_53_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_53_toc.md#upgrade_53_intro) </span>

<span class="sect1"> [Changes to the build_windows Folder](upgrade_11gr2_53_build_windows.md) </span>

<span class="sect1"> [Replication Connection Status in the Java API](upgrade_11gr2_53_conn_status.md) </span>

<span class="sect2"> [New Function](upgrade_11gr2_53_conn_status.md#idp804776) </span>

<span class="sect2"> [New Class](upgrade_11gr2_53_conn_status.md#idp771568) </span>

<span class="sect2"> [Deprecated Function](upgrade_11gr2_53_conn_status.md#idp809200) </span>

<span class="sect1"> [Exclusive Database Handles](upgrade_11gr2_53_excl.md) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_53_excl.md#idp811424) </span>

<span class="sect1"> [Configure the Region Size of Heap Databases](upgrade_11gr2_53_heap_regionsize.md) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_53_heap_regionsize.md#idp775064) </span>

<span class="sect1"> [New Hotbackup Interface](upgrade_11gr2_53_hotbackup.md) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_53_hotbackup.md#idp815256) </span>

<span class="sect2"> [Flags Accepted by DB_ENV-\>backup()](upgrade_11gr2_53_hotbackup.md#idp805032) </span>

<span class="sect2"> [Flags Accepted by DB_ENV-\>dbbackup()](upgrade_11gr2_53_hotbackup.md#idp822632) </span>

<span class="sect2"> [Enumerations Accepted by DB_ENV-\>set_backup_config()](upgrade_11gr2_53_hotbackup.md#idp828456) </span>

<span class="sect1"> [Updated JDBC Version](upgrade_11gr2_53_jdbc.md) </span>

<span class="sect1"> [Configure Directory to Store Metadata Files](upgrade_11gr2_53_meta_dir.md) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_53_meta_dir.md#idp837576) </span>

<span class="sect1"> [Changes in the SQL API Build](upgrade_11gr2_53_sql_build.md) </span>

<span class="sect1"> [New Berkeley DB SQL API PRAGMAs](upgrade_11gr2_53_sql_pragma.md) </span>

<span class="sect2"> [New PRAGMAs](upgrade_11gr2_53_sql_pragma.md#idp843792) </span>

<span class="sect1"> [Replication for Existing Databases in the SQL API](upgrade_11gr2_53_sql_rep.md) </span>

<span class="sect2"> [PRAGMAs With Permanent Effects](upgrade_11gr2_53_sql_rep.md#idp837896) </span>

<span class="sect2"> [PRAGMAs That Can Now Operate on Existing Databases](upgrade_11gr2_53_sql_rep.md#idp844568) </span>

<span class="sect1"> [Berkeley DB X/Open Compliant XA Resource Manager and Transaction Snapshots](upgrade_11gr2_53_xa_mvcc.md) </span>

<span class="sect1"> [Berkeley DB Library Version 11.2.5.3 Change Log](changelog_5_3.md) </span>

<span class="sect2"> [Changes between 11.2.5.3.21 and 11.2.5.3.28](changelog_5_3.md#idp839120) </span>

<span class="sect2"> [Changes between 11.2.5.3.15 and 11.2.5.3.21](changelog_5_3.md#idp845408) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes](changelog_5_3.md#idp636088) </span>

<span class="sect2"> [New Features](changelog_5_3.md#idp856040) </span>

<span class="sect2"> [Database Environment Changes](changelog_5_3.md#idp853696) </span>

<span class="sect2"> [Access Method Changes](changelog_5_3.md#idp844240) </span>

<span class="sect2"> [SQL API Changes](changelog_5_3.md#idp838728) </span>

<span class="sect2"> [Java-specific API changes](changelog_5_3.md#idp863240) </span>

<span class="sect2"> [Replication Changes](changelog_5_3.md#idp867984) </span>

<span class="sect2"> [Locking Subsystem Changes](changelog_5_3.md#idp853912) </span>

<span class="sect2"> [Logging Subsystem Changes](changelog_5_3.md#idp844888) </span>

<span class="sect2"> [Memory Pool Subsystem Changes](changelog_5_3.md#idp868368) </span>

<span class="sect2"> [Mutex Subsystem Changes](changelog_5_3.md#idp883216) </span>

<span class="sect2"> [Transaction Subsystem Changes](changelog_5_3.md#idp875448) </span>

<span class="sect2"> [Utility Changes](changelog_5_3.md#idp889064) </span>

<span class="sect2"> [Configuration, Documentation, Sample Apps, Portability and Build Changes](changelog_5_3.md#idp892136) </span>

<span class="sect2"> [Known Bugs](changelog_5_3.md#idp892656) </span>

<span class="chapter"> [10. Upgrading Berkeley DB 11.2.5.1 applications to Berkeley DB 11.2.5.2](upgrade_52_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_52_toc.md#upgrade_52_intro) </span>

<span class="sect1"> [SQLite Interface Upgrade](upgrade_11gr2_52_sqlite_ver.md) </span>

<span class="sect1"> [32bit/64bit Compatibility on Windows](upgrade_11gr2_52_bit_cmp_win.md) </span>

<span class="sect1"> [Read Only flag for DBT](upgrade_11gr2_52_rep_dbt_readonly.md) </span>

<span class="sect2"> [New Flag](upgrade_11gr2_52_rep_dbt_readonly.md#idp907000) </span>

<span class="sect1"> [Dynamic Environment Configuration](upgrade_11gr2_52_dyn_env.md) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_52_dyn_env.md#idp902144) </span>

<span class="sect2"> [Deprecated Functions](upgrade_11gr2_52_dyn_env.md#idp912000) </span>

<span class="sect1"> [Exclusive Transactions in the SQL Layer](upgrade_11gr2_52_excl_txn_sql.md) </span>

<span class="sect1"> [Group Membership in Repmgr](upgrade_11gr2_52_grp_mbr.md) </span>

<span class="sect2"> [Upgrading](upgrade_11gr2_52_grp_mbr.md#idp929720) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_52_grp_mbr.md#idp910056) </span>

<span class="sect2"> [Modified Functions](upgrade_11gr2_52_grp_mbr.md#idp901088) </span>

<span class="sect2"> [New Events](upgrade_11gr2_52_grp_mbr.md#idp924520) </span>

<span class="sect2"> [Removed Functions](upgrade_11gr2_52_grp_mbr.md#idp937928) </span>

<span class="sect2"> [New Parameters](upgrade_11gr2_52_grp_mbr.md#idp909344) </span>

<span class="sect2"> [New Structure](upgrade_11gr2_52_grp_mbr.md#idp924776) </span>

<span class="sect1"> [Heap Access Method](upgrade_11gr2_52_heap.md) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_52_heap.md#idp936848) </span>

<span class="sect2"> [Modified Functions](upgrade_11gr2_52_heap.md#idp930424) </span>

<span class="sect2"> [New Definition](upgrade_11gr2_52_heap.md#idp931776) </span>

<span class="sect1"> [Enabling Transaction Snapshots in the SQL Layer](upgrade_11gr2_52_mvcc_sql.md) </span>

<span class="sect2"> [New Pragmas](upgrade_11gr2_52_mvcc_sql.md#idp951464) </span>

<span class="sect1"> [2SITE_STRICT Enabled by Default in Replication](upgrade_11gr2_52_rep_2site_strict.md) </span>

<span class="sect1"> [Enabling Replication in the SQL Layer](upgrade_11gr2_52_rep_sql.md) </span>

<span class="sect2"> [New Pragmas](upgrade_11gr2_52_rep_sql.md#idp962696) </span>

<span class="sect1"> [Repmgr Message Channels](upgrade_11gr2_52_repmgr_channels.md) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_52_repmgr_channels.md#idp919280) </span>

<span class="sect1"> [Sequence Support in the SQL Layer](upgrade_11gr2_52_seq_sql.md) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_52_seq_sql.md#idp963480) </span>

<span class="sect1"> [Berkeley DB X/Open Compliant XA Resource Manager](upgrade_11gr2_52_xa.md) </span>

<span class="sect2"> [Constraints](upgrade_11gr2_52_xa.md#idp973264) </span>

<span class="sect2"> [New Flag](upgrade_11gr2_52_xa.md#idp978200) </span>

<span class="sect2"> [Modified Function](upgrade_11gr2_52_xa.md#idp982256) </span>

<span class="sect1"> [Hot Backup Changes](upgrade_11gr2_52_hot_backup.md) </span>

<span class="sect1"> [Berkeley DB Library Version 11.2.5.2 Change Log](changelog_5_2.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes](changelog_5_2.md#idp972456) </span>

<span class="sect2"> [New Features](changelog_5_2.md#idp978720) </span>

<span class="sect2"> [Database Environment Changes](changelog_5_2.md#idp984720) </span>

<span class="sect2"> [Concurrent Data Store Changes](changelog_5_2.md#idp995752) </span>

<span class="sect2"> [Access Method Changes](changelog_5_2.md#idp989160) </span>

<span class="sect2"> [SQL API Changes](changelog_5_2.md#idp989544) </span>

<span class="sect2"> [C API Changes](changelog_5_2.md#idp971912) </span>

<span class="sect2"> [Tcl-specific API Changes](changelog_5_2.md#idp996528) </span>

<span class="sect2"> [C#-specific API Changes](changelog_5_2.md#idp972000) </span>

<span class="sect2"> [Replication Changes](changelog_5_2.md#idp994456) </span>

<span class="sect2"> [Locking Subsystem Changes](changelog_5_2.md#idp996912) </span>

<span class="sect2"> [Logging Subsystem Changes](changelog_5_2.md#idp1010640) </span>

<span class="sect2"> [Memory Pool Subsystem Changes](changelog_5_2.md#idp992728) </span>

<span class="sect2"> [Mutex Subsystem Changes](changelog_5_2.md#idp1018872) </span>

<span class="sect2"> [Transaction Subsystem Changes](changelog_5_2.md#idp1011056) </span>

<span class="sect2"> [Test Suite Changes](changelog_5_2.md#idp1003424) </span>

<span class="sect2"> [Utility Changes](changelog_5_2.md#idp1029752) </span>

<span class="sect2"> [Configuration, Documentation, Sample Apps, Portability and Build Changes](changelog_5_2.md#idp1031368) </span>

<span class="sect2"> [Example Changes](changelog_5_2.md#idp1003200) </span>

<span class="sect2"> [Miscellaneous Bug Fixes](changelog_5_2.md#idp1034280) </span>

<span class="sect2"> [Deprecated Features](changelog_5_2.md#idp1035816) </span>

<span class="sect2"> [Known Bugs](changelog_5_2.md#idp1037736) </span>

<span class="chapter"> [11. Upgrading Berkeley DB 11.2.5.0 applications to Berkeley DB 11.2.5.1](upgrade_51_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_51_toc.md#upgrade_51_intro) </span>

<span class="sect1"> [DPL Applications must be recompiled](upgrade_11gr2_51_dpl_recompile.md) </span>

<span class="sect1"> [Source Tree Rearranged](upgrade_11gr2_51_src_reorg.md) </span>

<span class="sect1"> [SQLite Interface Upgrade](upgrade_11gr2_51_sqlite_ver.md) </span>

<span class="sect1"> [Mod_db4 Support Discontinued](upgrade_11gr2_51_mod_db4_unsupp.md) </span>

<span class="sect1"> [Berkeley DB Library Version 11.2.5.1 Change Log](changelog_5_1.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes](changelog_5_1.md#idp1052992) </span>

<span class="sect2"> [New Features](changelog_5_1.md#idp953176) </span>

<span class="sect2"> [Database Environment Changes](changelog_5_1.md#idp1045336) </span>

<span class="sect2"> [Concurrent Data Store Changes](changelog_5_1.md#idp1059760) </span>

<span class="sect2"> [Access Method Changes](changelog_5_1.md#idp981016) </span>

<span class="sect2"> [API Changes](changelog_5_1.md#idp1049008) </span>

<span class="sect2"> [SQL-Specific API Changes](changelog_5_1.md#idp1055592) </span>

<span class="sect2"> [Tcl-Specific API Changes](changelog_5_1.md#idp1056952) </span>

<span class="sect2"> [Java-Specific API Changes](changelog_5_1.md#idp1052280) </span>

<span class="sect2"> [C#-Specific API Changes](changelog_5_1.md#idp987592) </span>

<span class="sect2"> [Direct Persistence Layer (DPL), Bindings and Collections API](changelog_5_1.md#idp1060648) </span>

<span class="sect2"> [Replication Changes](changelog_5_1.md#idp1070000) </span>

<span class="sect2"> [Locking Subsystem Changes](changelog_5_1.md#idp1080936) </span>

<span class="sect2"> [Logging Subsystem Changes](changelog_5_1.md#idp1092608) </span>

<span class="sect2"> [Memory Pool Subsystem Changes](changelog_5_1.md#idp1076376) </span>

<span class="sect2"> [Mutex Subsystem Changes](changelog_5_1.md#idp1080752) </span>

<span class="sect2"> [Transaction Subsystem Changes](changelog_5_1.md#idp1089584) </span>

<span class="sect2"> [Test Suite Changes](changelog_5_1.md#idp1067160) </span>

<span class="sect2"> [Utility Changes](changelog_5_1.md#idp1088000) </span>

<span class="sect2"> [Configuration, Documentation, Sample Apps, Portability, and Build Changes](changelog_5_1.md#idp1091312) </span>

<span class="sect2"> [Example Changes](changelog_5_1.md#idp1081576) </span>

<span class="sect2"> [Miscellaneous Bug Fixes](changelog_5_1.md#idp1102152) </span>

<span class="sect2"> [Deprecated Features](changelog_5_1.md#idp1100024) </span>

<span class="sect2"> [Known Bugs](changelog_5_1.md#idp1100672) </span>

<span class="chapter"> [12. Upgrading Berkeley DB 4.8 applications to Berkeley DB 11.2.5.0](upgrade_11gr2_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_11gr2_toc.md#upgrade_11gr2_intro) </span>

<span class="sect1"> [db_sql Renamed to db_sql_codegen](upgrade_11gr2_dbsqlcodegen.md) </span>

<span class="sect1"> [DB_REP_CONF_NOAUTOINIT Replaced](upgrade_11gr2_autoinit.md) </span>

<span class="sect1"> [Support for Multiple Client-to-Client Peers](upgrade_11gr2_repmgr.md) </span>

<span class="sect1"> [Cryptography Support](build_unix_encrypt.md) </span>

<span class="sect1"> [DB_NOSYNC Flag to Flush Files](build_unix_db_nosync.md) </span>

<span class="sect1"> [Dropped Support](upgrade_11gr2_remsupp.md) </span>

<span class="sect1"> [Changing Stack Size](build_unix_stacksize.md) </span>

<span class="sect1"> [Berkeley DB 11g Release 2 Change Log](changelog_5_0.md) </span>

<span class="sect2"> [Changes between 11.2.5.0.26 and 11.2.5.0.32](changelog_5_0.md#idp1125968) </span>

<span class="sect2"> [Changes between 11.2.5.0.21 and 11.2.5.0.26](changelog_5_0.md#idp1126872) </span>

<span class="sect2"> [Changes between 4.8 and 11.2.5.0.21](changelog_5_0.md#idp1125192) </span>

<span class="sect2"> [Known Bugs](changelog_5_0.md#idp1131672) </span>

<span class="chapter"> [13. Upgrading Berkeley DB 4.7 applications to Berkeley DB 4.8](upgrade_4_8_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_4_8_toc.md#upgrade_4_8_intro) </span>

<span class="sect1"> [Registering DPL Secondary Keys](upgrade_4_8_dpl.md) </span>

<span class="sect1"> [Minor Change in Behavior of DB_MPOOLFILE-\>get](upgrade_4_8_mpool.md) </span>

<span class="sect1"> [Dropped Support for fcntl System Calls](upgrade_4_8_fcntl.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_8_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.8.28 Change Log](changelog_4_8.md) </span>

<span class="sect2"> [Changes between 4.8.26 and 4.8.28:](changelog_4_8.md#idp1162104) </span>

<span class="sect2"> [Known bugs in 4.8](changelog_4_8.md#idp1184264) </span>

<span class="sect2"> [Changes between 4.8.24 and 4.8.26:](changelog_4_8.md#idp1139288) </span>

<span class="sect2"> [Changes between 4.8.21 and 4.8.24:](changelog_4_8.md#idp1091200) </span>

<span class="sect2"> [Changes between 4.7 and 4.8.21:](changelog_4_8.md#idp1199520) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_8.md#idp1200208) </span>

<span class="sect2"> [New Features:](changelog_4_8.md#idp981712) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_8.md#idp1130224) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_8.md#idp1209320) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_8.md#idp1209720) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_8.md#idp1218064) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_8.md#idp1215560) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_8.md#idp1226120) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_8.md#idp1163928) </span>

<span class="sect2"> [C-specific API Changes:](changelog_4_8.md#idp1138904) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_8.md#idp1218344) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_8.md#idp1238856) </span>

<span class="sect2"> [Direct Persistence Layer (DPL), Bindings and Collections API:](changelog_4_8.md#idp1232112) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_8.md#idp1232384) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_8.md#idp1244368) </span>

<span class="sect2"> [Replication Changes:](changelog_4_8.md#idp1245896) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_8.md#idp1242240) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_8.md#idp1247728) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_8.md#idp1241128) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_8.md#idp1258328) </span>

<span class="sect2"> [Mutex Subsystem Changes:](changelog_4_8.md#idp1258720) </span>

<span class="sect2"> [Test Suite Changes](changelog_4_8.md#idp1240832) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_8.md#idp1249776) </span>

<span class="sect2"> [Utility Changes:](changelog_4_8.md#idp1271664) </span>

<span class="sect2"> [Configuration, Documentation, Sample Application, Portability and Build Changes:](changelog_4_8.md#idp1274104) </span>

<span class="chapter"> [14. Test Suite](test.md) </span>

<span class="sect1"> [Running the test suite](test.md#test_run) </span>

<span class="sect2"> [Running SQL Test Suite on Unix](test.md#idp1298736) </span>

<span class="sect2"> [Running SQL Test Suite on Windows](test.md#idp1289848) </span>

<span class="sect1"> [Test suite FAQ](test_faq.md) </span>
