---
title: "Chapter 12.  Upgrading Berkeley DB 4.8 applications to Berkeley DB 11.2.5.0"
api-name: "Chapter 12.  Upgrading Berkeley DB 4.8 applications to Berkeley DB 11.2.5.0"
source: docs/installation/upgrade_11gr2_toc.html
---
## Chapter 12.  Upgrading Berkeley DB 4.8 applications to Berkeley DB 11.2.5.0

**Table of Contents**

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

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 4.8 release interfaces to the Berkeley DB 11<span class="emphasis">*g*</span> Release 2 interfaces. (Library version 11.2.5.0). This information does not describe how to upgrade Berkeley DB 1.85 release applications.
