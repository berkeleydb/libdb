---
title: "Chapter 9.  Upgrading Berkeley DB 11.2.5.2 applications to Berkeley DB 11.2.5.3"
api-name: "Chapter 9.  Upgrading Berkeley DB 11.2.5.2 applications to Berkeley DB 11.2.5.3"
source: docs/installation/upgrade_53_toc.html
---
## Chapter 9.  Upgrading Berkeley DB 11.2.5.2 applications to Berkeley DB 11.2.5.3

**Table of Contents**

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

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 11.2.5.2 library to the Berkeley DB 11.2.5.3 library (both of which belong to Berkeley DB 11<span class="emphasis">*g*</span> Release 2). This information does not describe how to upgrade Berkeley DB 1.85 release applications.

For information on the general process of upgrading Berkeley DB installations and upgrade instructions related to historical releases, see the <a href="http://download.oracle.com/docs/cd/E17076_02/html/upgrading/index.html" class="ulink" target="_top">Berkeley DB Upgrade Guide.</a>
