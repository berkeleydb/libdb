---
title: "Berkeley DB Upgrade Guide"
api-name: "Berkeley DB Upgrade Guide"
source: docs/upgrading/index.html
---
# Berkeley DB Upgrade Guide

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

<span class="chapter"> [1. Introduction](introduction.md) </span>

<span class="sect1"> [Library version information](introduction.md#upgrade_version) </span>

<span class="chapter"> [2. Upgrading from previous versions of Berkeley DB](upgrade_process.md) </span>

<span class="chapter"> [3. Upgrading Berkeley DB 4.6 applications to Berkeley DB 4.7](upgrade_4_7_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_4_7_toc.md#upgrade_4_7_intro) </span>

<span class="sect1"> [Run-time configuration](upgrade_4_7_rtc.md) </span>

<span class="sect1"> [Replication API](upgrade_4_7_repapi.md) </span>

<span class="sect1"> [Tcl API](upgrade_4_7_tcl.md) </span>

<span class="sect1"> [DB_ENV-\>set_intermediate_dir](upgrade_4_7_interdir.md) </span>

<span class="sect1"> [Log configuration](upgrade_4_7_log.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_7_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.7.25 Change Log](changelog_4_7.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_7.md#idp50357648) </span>

<span class="sect2"> [New Features:](changelog_4_7.md#idp50378912) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_7.md#idp50380752) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_7.md#idp50382592) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_7.md#idp50381120) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_7.md#idp50391248) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_7.md#idp50365280) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_7.md#idp50355136) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_7.md#idp50346288) </span>

<span class="sect2"> [C-specific API Changes:](changelog_4_7.md#idp50346816) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_7.md#idp50347072) </span>

<span class="sect2"> [Direct Persistence Layer (DPL), Bindings and Collections API:](changelog_4_7.md#idp50347296) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_7.md#idp50386200) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_7.md#idp50395072) </span>

<span class="sect2"> [Replication Changes:](changelog_4_7.md#idp50395328) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_7.md#idp50391504) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_7.md#idp50357904) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_7.md#idp50397528) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_7.md#idp50385512) </span>

<span class="sect2"> [Mutex Subsystem Changes:](changelog_4_7.md#idp50386616) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_7.md#idp50391632) </span>

<span class="sect2"> [Utility Changes:](changelog_4_7.md#idp50396200) </span>

<span class="sect2"> [Configuration, Documentation, Sample Application, Portability and Build Changes:](changelog_4_7.md#idp50412288) </span>

<span class="chapter"> [4. Upgrading Berkeley DB 4.5 applications to Berkeley DB 4.6](upgrade_4_6_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_4_6_toc.md#upgrade_4_6_intro) </span>

<span class="sect1"> [C API cursor handle method names](upgrade_4_6_cursor.md) </span>

<span class="sect1"> [DB_MPOOLFILE-\>put](upgrade_4_6_memp_fput.md) </span>

<span class="sect1"> [B_MPOOLFILE-\>set](upgrade_4_6_memp_fset.md) </span>

<span class="sect1"> [Replication Events](upgrade_4_6_event.md) </span>

<span class="sect1"> [DB_REP_FULL_ELECTION](upgrade_4_6_full_election.md) </span>

<span class="sect1"> [Verbose Output](upgrade_4_6_verbose.md) </span>

<span class="sect1"> [DB_VERB_REPLICATION](upgrade_4_6_verb.md) </span>

<span class="sect1"> [Windows 9X](upgrade_4_6_win.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_6_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.6.21 Change Log](changelog_4_6.md) </span>

<span class="sect2"> [4.6.21 Patches:](changelog_4_6.md#idp50449856) </span>

<span class="sect2"> [4.6.19 Patches](changelog_4_6.md#idp50370888) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_6.md#idp50361912) </span>

<span class="sect2"> [New Features:](changelog_4_6.md#idp50454856) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_6.md#idp50457960) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_6.md#idp50459800) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_6.md#idp50458344) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_6.md#idp50475672) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_6.md#idp50460536) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_6.md#idp50444272) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_6.md#idp50463616) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_6.md#idp50463872) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_6.md#idp50481800) </span>

<span class="sect2"> [Java collections and bind API Changes:](changelog_4_6.md#idp50464456) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_6.md#idp50464944) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_6.md#idp50465232) </span>

<span class="sect2"> [Replication Changes:](changelog_4_6.md#idp50486584) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_6.md#idp50466136) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_6.md#idp50465496) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_6.md#idp50451848) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_6.md#idp50452712) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_6.md#idp50468064) </span>

<span class="sect2"> [Utility Changes:](changelog_4_6.md#idp50475736) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_6.md#idp50479800) </span>

<span class="chapter"> [5. Upgrading Berkeley DB 4.4 applications to Berkeley DB 4.5](upgrade_4_5_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_4_5_toc.md#upgrade_4_5_intro) </span>

<span class="sect1"> [deprecated interfaces](upgrade_4_5_deprecate.md) </span>

<span class="sect1"> [DB-\>set_isalive](upgrade_4_5_alive.md) </span>

<span class="sect1"> [DB_ENV-\>rep_elect](upgrade_4_5_elect.md) </span>

<span class="sect1"> [Replication method naming](upgrade_4_5_rep_set.md) </span>

<span class="sect1"> [Replication events](upgrade_4_5_rep_event.md) </span>

<span class="sect1"> [Memory Pool API](upgrade_4_5_memp.md) </span>

<span class="sect1"> [DB_ENV-\>set_paniccall](upgrade_4_5_paniccall.md) </span>

<span class="sect1"> [DB-\>set_pagesize](upgrade_4_5_pagesize.md) </span>

<span class="sect1"> [Collections API](upgrade_4_5_collect.md) </span>

<span class="sect1"> [--enable-pthread_self](upgrade_4_5_config.md) </span>

<span class="sect1"> [Recno backing text source files](upgrade_4_5_source.md) </span>

<span class="sect1"> [Application-specific logging](upgrade_4_5_applog.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_5_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.5.20 Change Log](changelog_4_5_20.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_5_20.md#idp50532016) </span>

<span class="sect2"> [New Features:](changelog_4_5_20.md#idp50511048) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_5_20.md#idp50513672) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_5_20.md#idp50516704) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_5_20.md#idp50520456) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_5_20.md#idp50542544) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_5_20.md#idp50536608) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_5_20.md#idp50533200) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_5_20.md#idp50539504) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_5_20.md#idp50539760) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_5_20.md#idp50541672) </span>

<span class="sect2"> [Java collections and bind API Changes:](changelog_4_5_20.md#idp50542632) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_5_20.md#idp50546176) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_5_20.md#idp50548752) </span>

<span class="sect2"> [Replication Changes:](changelog_4_5_20.md#idp50547824) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_5_20.md#idp50557816) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_5_20.md#idp50534496) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_5_20.md#idp50532216) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_5_20.md#idp50542056) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_5_20.md#idp50543016) </span>

<span class="sect2"> [Utility Changes:](changelog_4_5_20.md#idp50556608) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_5_20.md#idp50557880) </span>

<span class="chapter"> [6. Upgrading Berkeley DB 4.3 applications to Berkeley DB 4.4](upgrade_4_4_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_4_4_toc.md#upgrade_4_4_intro) </span>

<span class="sect1"> [DB_AUTO_COMMIT](upgrade_4_4_autocommit.md) </span>

<span class="sect1"> [DB_DEGREE_2, DB_DIRTY_READ](upgrade_4_4_isolation.md) </span>

<span class="sect1"> [DB_JOINENV](upgrade_4_4_joinenv.md) </span>

<span class="sect1"> [mutexes](upgrade_4_4_mutex.md) </span>

<span class="sect1"> [DB_MPOOLFILE-\>set_clear_len](upgrade_4_4_clear.md) </span>

<span class="sect1"> [lock statistics](upgrade_4_4_lockstat.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_4_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.4.16 Change Log](changelog_4_4_16.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_4_16.md#idp50595920) </span>

<span class="sect2"> [New Features:](changelog_4_4_16.md#idp50583264) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_4_16.md#idp50583648) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_4_16.md#idp50567656) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_4_16.md#idp50591960) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_4_16.md#idp50592384) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_4_16.md#idp50595984) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_4_16.md#idp50597856) </span>

<span class="sect2"> [Recno Access Method Changes](changelog_4_4_16.md#idp50598936) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_4_16.md#idp50598072) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_4_16.md#idp50600424) </span>

<span class="sect2"> [Java collections and bind API Changes:](changelog_4_4_16.md#idp50621112) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_4_16.md#idp50604672) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_4_16.md#idp50589536) </span>

<span class="sect2"> [Replication Changes:](changelog_4_4_16.md#idp50610200) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_4_16.md#idp50594920) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_4_16.md#idp50614600) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_4_16.md#idp50614888) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_4_16.md#idp50635800) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_4_16.md#idp50617400) </span>

<span class="sect2"> [Utility Changes:](changelog_4_4_16.md#idp50617824) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_4_16.md#idp50621200) </span>

<span class="sect1"> [Berkeley DB 4.4.20 Change Log](changelog_4_4_20.md) </span>

<span class="sect2"> [Changes since Berkeley DB 4.4.16:](changelog_4_4_20.md#idp50624312) </span>

<span class="chapter"> [7. Upgrading Berkeley DB 4.2 applications to Berkeley DB 4.3](upgrade_4_3_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_4_3_toc.md#upgrade_4_3_intro) </span>

<span class="sect1"> [Java](upgrade_4_3_java.md) </span>

<span class="sect1"> [DB_ENV-\>set_errcall, DB-\>set_errcall](upgrade_4_3_err.md) </span>

<span class="sect1"> [DBcursor-\>c_put](upgrade_4_3_cput.md) </span>

<span class="sect1"> [DB-\>stat](upgrade_4_3_stat.md) </span>

<span class="sect1"> [DB_ENV-\>set_verbose](upgrade_4_3_verb.md) </span>

<span class="sect1"> [Logging](upgrade_4_3_log.md) </span>

<span class="sect1"> [DB_FILEOPEN](upgrade_4_3_fileopen.md) </span>

<span class="sect1"> [ENOMEM and DbMemoryException](upgrade_4_3_enomem.md) </span>

<span class="sect1"> [Replication](upgrade_4_3_repl.md) </span>

<span class="sect1"> [Run-time configuration](upgrade_4_3_rtc.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_3_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.3.29 Change Log](changelog_4_3_29.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_3_29.md#idp50694880) </span>

<span class="sect2"> [New Features:](changelog_4_3_29.md#idp50670248) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_3_29.md#idp50673968) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_3_29.md#idp50703424) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_3_29.md#idp50690848) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_3_29.md#idp50691272) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_3_29.md#idp50694944) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_3_29.md#idp50697104) </span>

<span class="sect2"> [Recno Access Method Changes](changelog_4_3_29.md#idp50720352) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_3_29.md#idp50700784) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_3_29.md#idp50670632) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_3_29.md#idp50702384) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_3_29.md#idp50703784) </span>

<span class="sect2"> [Replication Changes:](changelog_4_3_29.md#idp50685776) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_3_29.md#idp50733112) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_3_29.md#idp50712384) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_3_29.md#idp50740760) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_3_29.md#idp50695328) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_3_29.md#idp50720440) </span>

<span class="sect2"> [Utility Changes:](changelog_4_3_29.md#idp50724480) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_3_29.md#idp50724864) </span>

<span class="chapter"> [8. Upgrading Berkeley DB 4.1 applications to Berkeley DB 4.2](upgrade_4_2_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_4_2_toc.md#upgrade_4_2_intro) </span>

<span class="sect1"> [Java](upgrade_4_2_java.md) </span>

<span class="sect1"> [Queue access method](upgrade_4_2_queue.md) </span>

<span class="sect1"> [DB_CHKSUM_SHA1](upgrade_4_2_cksum.md) </span>

<span class="sect1"> [DB_CLIENT](upgrade_4_2_client.md) </span>

<span class="sect1"> [DB-\>del](upgrade_4_2_del.md) </span>

<span class="sect1"> [DB-\>set_cache_priority](upgrade_4_2_priority.md) </span>

<span class="sect1"> [DB-\>verify](upgrade_4_2_verify.md) </span>

<span class="sect1"> [DB_LOCK_NOTGRANTED](upgrade_4_2_lockng.md) </span>

<span class="sect1"> [Replication](upgrade_4_2_repinit.md) </span>

<span class="sect2"> [Replication initialization](upgrade_4_2_repinit.md#idp50804696) </span>

<span class="sect2"> [Database methods and replication clients](upgrade_4_2_repinit.md#idp50772032) </span>

<span class="sect2"> [DB_ENV-\>rep_process_message()](upgrade_4_2_repinit.md#idp50779672) </span>

<span class="sect1"> [Client replication environments](upgrade_4_2_nosync.md) </span>

<span class="sect1"> [Tcl API](upgrade_4_2_tcl.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_2_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.2.52 Change Log](changelog_4_2_52.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_2_52.md#idp50822856) </span>

<span class="sect2"> [New Features:](changelog_4_2_52.md#idp50784344) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_2_52.md#idp50809288) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_2_52.md#idp50822104) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_2_52.md#idp50824288) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_2_52.md#idp50825368) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_2_52.md#idp50844704) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_2_52.md#idp50828568) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_2_52.md#idp50858440) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_2_52.md#idp50832248) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_2_52.md#idp50815840) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_2_52.md#idp50867864) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_2_52.md#idp50852544) </span>

<span class="sect2"> [Replication Changes:](changelog_4_2_52.md#idp50858528) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_2_52.md#idp50877816) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_2_52.md#idp50865088) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_2_52.md#idp50868008) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_2_52.md#idp50865504) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_2_52.md#idp50845064) </span>

<span class="sect2"> [Utility Changes:](changelog_4_2_52.md#idp50858944) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_2_52.md#idp50892568) </span>

<span class="chapter"> [9. Upgrading Berkeley DB 4.0 applications to Berkeley DB 4.1](upgrade_4_1_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_4_1_toc.md#upgrade_4_1_intro) </span>

<span class="sect1"> [DB_EXCL](upgrade_4_1_excl.md) </span>

<span class="sect1"> [DB-\>associate, DB-\>open, DB-\>remove, DB-\>rename](upgrade_4_1_fop.md) </span>

<span class="sect1"> [DB_ENV-\>log_register](upgrade_4_1_log_register.md) </span>

<span class="sect1"> [st_flushcommit](upgrade_4_1_log_stat.md) </span>

<span class="sect1"> [DB_CHECKPOINT, DB_CURLSN](upgrade_4_1_checkpoint.md) </span>

<span class="sect1"> [DB_INCOMPLETE](upgrade_4_1_incomplete.md) </span>

<span class="sect1"> [DB_ENV-\>memp_sync](upgrade_4_1_memp_sync.md) </span>

<span class="sect1"> [DB-\>stat.hash_nelem](upgrade_4_1_hash_nelem.md) </span>

<span class="sect1"> [Java exceptions](upgrade_4_1_java.md) </span>

<span class="sect1"> [C++ exceptions](upgrade_4_1_cxx.md) </span>

<span class="sect1"> [Application-specific logging and recovery](upgrade_4_1_app_dispatch.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_1_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.1.24 and 4.1.25 Change Log](changelog_4_1_24.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_1_24.md#idp50963888) </span>

<span class="sect2"> [Major New Features:](changelog_4_1_24.md#idp50959088) </span>

<span class="sect2"> [General Environment Changes:](changelog_4_1_24.md#idp50962280) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_1_24.md#idp50961984) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_1_24.md#idp50964272) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_1_24.md#idp50967400) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_1_24.md#idp50969240) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_1_24.md#idp50972088) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_1_24.md#idp50973928) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_1_24.md#idp50975768) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_1_24.md#idp50950328) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_1_24.md#idp50958680) </span>

<span class="sect2"> [Replication Changes:](changelog_4_1_24.md#idp50977144) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_1_24.md#idp50964336) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_1_24.md#idp50987264) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_1_24.md#idp50989192) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_1_24.md#idp50992072) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_1_24.md#idp50993160) </span>

<span class="sect2"> [Utility Changes:](changelog_4_1_24.md#idp50994744) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_1_24.md#idp50997648) </span>

<span class="sect1"> [Berkeley DB 4.1.25 Change Log](changelog_4_1_25.md) </span>

<span class="chapter"> [10. Upgrading Berkeley DB 3.3 applications to Berkeley DB 4.0](upgrade_4_0_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_4_0_toc.md#upgrade_4_0_intro) </span>

<span class="sect1"> [db_deadlock](upgrade_4_0_deadlock.md) </span>

<span class="sect1"> [lock_XXX](upgrade_4_0_lock.md) </span>

<span class="sect1"> [log_XXX](upgrade_4_0_log.md) </span>

<span class="sect1"> [memp_XXX](upgrade_4_0_mp.md) </span>

<span class="sect1"> [txn_XXX](upgrade_4_0_txn.md) </span>

<span class="sect1"> [db_env_set_XXX](upgrade_4_0_env.md) </span>

<span class="sect1"> [DB_ENV-\>set_server](upgrade_4_0_rpc.md) </span>

<span class="sect1"> [DB_ENV-\>set_lk_max](upgrade_4_0_set_lk_max.md) </span>

<span class="sect1"> [DB_ENV-\>lock_id_free](upgrade_4_0_lock_id_free.md) </span>

<span class="sect1"> [Java CLASSPATH environment variable](upgrade_4_0_java.md) </span>

<span class="sect1"> [C++ ostream objects](upgrade_4_0_cxx.md) </span>

<span class="sect1"> [application-specific recovery](upgrade_4_0_asr.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_0_disk.md) </span>

<span class="sect1"> [4.0.14 Change Log](changelog_4_0_14.md) </span>

<span class="sect2"> [Major New Features:](changelog_4_0_14.md#idp51113768) </span>

<span class="sect2"> [General Environment Changes:](changelog_4_0_14.md#idp51101344) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_0_14.md#idp51103296) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_0_14.md#idp51105152) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_0_14.md#idp51109416) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_0_14.md#idp51112664) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_0_14.md#idp51113832) </span>

<span class="sect2"> [C++ API Changes:](changelog_4_0_14.md#idp51115760) </span>

<span class="sect2"> [Java API Changes:](changelog_4_0_14.md#idp51126328) </span>

<span class="sect2"> [Tcl API Changes:](changelog_4_0_14.md#idp51116840) </span>

<span class="sect2"> [RPC Client/Server Changes:](changelog_4_0_14.md#idp51117920) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_0_14.md#idp51118608) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_0_14.md#idp51118928) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_0_14.md#idp51103680) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_0_14.md#idp51122816) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_0_14.md#idp51109800) </span>

<span class="sect2"> [Utility Changes:](changelog_4_0_14.md#idp51113048) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_0_14.md#idp51125248) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_0_14.md#idp51126712) </span>

<span class="chapter"> [11. Upgrading Berkeley DB 3.2 applications to Berkeley DB 3.3](upgrade_3_3_toc.md) </span>

<span class="sect1"> [introduction](upgrade_3_3_toc.md#upgrade_3_3_intro) </span>

<span class="sect1"> [DB_ENV-\>set_server](upgrade_3_3_rpc.md) </span>

<span class="sect1"> [DB-\>get_type](upgrade_3_3_gettype.md) </span>

<span class="sect1"> [DB-\>get_byteswapped](upgrade_3_3_getswap.md) </span>

<span class="sect1"> [DB-\>set_malloc, DB-\>set_realloc](upgrade_3_3_alloc.md) </span>

<span class="sect1"> [DB_LOCK_CONFLICT](upgrade_3_3_conflict.md) </span>

<span class="sect1"> [memp_fget, EIO](upgrade_3_3_memp_fget.md) </span>

<span class="sect1"> [txn_prepare](upgrade_3_3_txn_prepare.md) </span>

<span class="sect1"> [--enable-dynamic, --enable-shared](upgrade_3_3_shared.md) </span>

<span class="sect1"> [--disable-bigfile](upgrade_3_3_bigfile.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_3_3_disk.md) </span>

<span class="chapter"> [12. Upgrading Berkeley DB 3.1 applications to Berkeley DB 3.2](upgrade_3_2_toc.md) </span>

<span class="sect1"> [introduction](upgrade_3_2_toc.md#upgrade_3_2_intro) </span>

<span class="sect1"> [DB_ENV-\>set_flags](upgrade_3_2_set_flags.md) </span>

<span class="sect1"> [DB callback functions, app_private field](upgrade_3_2_callback.md) </span>

<span class="sect1"> [Logically renumbering records](upgrade_3_2_renumber.md) </span>

<span class="sect1"> [DB_INCOMPLETE](upgrade_3_2_incomplete.md) </span>

<span class="sect1"> [DB_ENV-\>set_tx_recover](upgrade_3_2_tx_recover.md) </span>

<span class="sect1"> [DB_ENV-\>set_mutexlocks](upgrade_3_2_mutexlock.md) </span>

<span class="sect1"> [Java and C++ object reuse](upgrade_3_2_handle.md) </span>

<span class="sect1"> [Java java.io.FileNotFoundException](upgrade_3_2_notfound.md) </span>

<span class="sect1"> [db_dump](upgrade_3_2_db_dump.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_3_2_disk.md) </span>

<span class="chapter"> [13. Upgrading Berkeley DB 3.0 applications to Berkeley DB 3.1](upgrade_3_1_toc.md) </span>

<span class="sect1"> [introduction](upgrade_3_1_toc.md#upgrade_3_1_intro) </span>

<span class="sect1"> [DB_ENV-\>open, DB_ENV-\>remove](upgrade_3_1_config.md) </span>

<span class="sect1"> [DB_ENV-\>set_tx_recover](upgrade_3_1_set_tx_recover.md) </span>

<span class="sect1"> [DB_ENV-\>set_feedback, DB-\>set_feedback](upgrade_3_1_set_feedback.md) </span>

<span class="sect1"> [DB_ENV-\>set_paniccall, DB-\>set_paniccall](upgrade_3_1_set_paniccall.md) </span>

<span class="sect1"> [DB-\>put](upgrade_3_1_put.md) </span>

<span class="sect1"> [identical duplicate data items](upgrade_3_1_dup.md) </span>

<span class="sect1"> [DB-\>stat](upgrade_3_1_btstat.md) </span>

<span class="sect1"> [DB_SYSTEM_MEM](upgrade_3_1_sysmem.md) </span>

<span class="sect1"> [log_register](upgrade_3_1_log_register.md) </span>

<span class="sect1"> [memp_register](upgrade_3_1_memp_register.md) </span>

<span class="sect1"> [txn_checkpoint](upgrade_3_1_txn_check.md) </span>

<span class="sect1"> [environment configuration](upgrade_3_1_env.md) </span>

<span class="sect1"> [Tcl API](upgrade_3_1_tcl.md) </span>

<span class="sect1"> [DB_TMP_DIR](upgrade_3_1_tmp.md) </span>

<span class="sect1"> [log file pre-allocation](upgrade_3_1_logalloc.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_3_1_disk.md) </span>

<span class="chapter"> [14. Upgrading Berkeley DB 2.X applications to Berkeley DB 3.0](upgrade_3_0_toc.md) </span>

<span class="sect1"> [introduction](upgrade_3_0_toc.md#upgrade_3_0_intro) </span>

<span class="sect1"> [environment open/close/unlink](upgrade_3_0_envopen.md) </span>

<span class="sect1"> [function arguments](upgrade_3_0_func.md) </span>

<span class="sect1"> [DB_ENV structure](upgrade_3_0_dbenv.md) </span>

<span class="sect1"> [database open/close](upgrade_3_0_open.md) </span>

<span class="sect1"> [db_xa_open](upgrade_3_0_xa.md) </span>

<span class="sect1"> [DB structure](upgrade_3_0_db.md) </span>

<span class="sect1"> [DBINFO structure](upgrade_3_0_dbinfo.md) </span>

<span class="sect1"> [DB-\>join](upgrade_3_0_join.md) </span>

<span class="sect1"> [DB-\>stat](upgrade_3_0_stat.md) </span>

<span class="sect1"> [DB-\>sync and DB-\>close](upgrade_3_0_close.md) </span>

<span class="sect1"> [lock_put](upgrade_3_0_lock_put.md) </span>

<span class="sect1"> [lock_detect](upgrade_3_0_lock_detect.md) </span>

<span class="sect1"> [lock_stat](upgrade_3_0_lock_stat.md) </span>

<span class="sect1"> [log_register](upgrade_3_0_log_register.md) </span>

<span class="sect1"> [log_stat](upgrade_3_0_log_stat.md) </span>

<span class="sect1"> [memp_stat](upgrade_3_0_memp_stat.md) </span>

<span class="sect1"> [txn_begin](upgrade_3_0_txn_begin.md) </span>

<span class="sect1"> [txn_commit](upgrade_3_0_txn_commit.md) </span>

<span class="sect1"> [txn_stat](upgrade_3_0_txn_stat.md) </span>

<span class="sect1"> [DB_RMW](upgrade_3_0_rmw.md) </span>

<span class="sect1"> [DB_LOCK_NOTHELD](upgrade_3_0_lock_notheld.md) </span>

<span class="sect1"> [EAGAIN](upgrade_3_0_eagain.md) </span>

<span class="sect1"> [EACCES](upgrade_3_0_eacces.md) </span>

<span class="sect1"> [db_jump_set](upgrade_3_0_jump_set.md) </span>

<span class="sect1"> [db_value_set](upgrade_3_0_value_set.md) </span>

<span class="sect1"> [DbEnv class for C++ and Java](upgrade_3_0_dbenv_cxx.md) </span>

<span class="sect1"> [Db class for C++ and Java](upgrade_3_0_db_cxx.md) </span>

<span class="sect1"> [additional C++ changes](upgrade_3_0_cxx.md) </span>

<span class="sect1"> [additional Java changes](upgrade_3_0_java.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_3_0_disk.md) </span>

<span class="chapter"> [15. Upgrading Berkeley DB 1.85 or 1.86 applications to Berkeley DB 2.0](upgrade_2_0_toc.md) </span>

<span class="sect1"> [Introduction](upgrade_2_0_toc.md#upgrade_2_0_intro) </span>

<span class="sect1"> [System Integration](upgrade_2_0_system.md) </span>

<span class="sect1"> [Converting Applications](upgrade_2_0_convert.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_2_0_disk.md) </span>
