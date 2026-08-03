---
title: "Getting Started with Berkeley DB"
api-name: "Getting Started with Berkeley DB"
source: docs/gsg/C/index.html
---
# Getting Started with Berkeley DB

**Language:** C (this page) · [C++](cxx/index.md) · [Java](java/index.md)

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

<span class="chapter"> [1. Introduction to Berkeley DB](introduction.md) </span>

<span class="sect1"> [About This Manual](introduction.md#aboutthismanual) </span>

<span class="sect1"> [Berkeley DB Concepts](concepts.md) </span>

<span class="sect1"> [Access Methods](accessmethods.md) </span>

<span class="sect2"> [Selecting Access Methods](accessmethods.md#selectAM) </span>

<span class="sect2"> [Choosing between BTree and Hash](accessmethods.md#BTreeVSHash) </span>

<span class="sect2"> [Choosing between Queue and Recno](accessmethods.md#QueueVSRecno) </span>

<span class="sect1"> [Database Limits and Portability](databaseLimits.md) </span>

<span class="sect1"> [Environments](environments.md) </span>

<span class="sect1"> [Error Returns](returns.md) </span>

<span class="sect1"> [Getting and Using DB](gettingit.md) </span>

<span class="chapter"> [2. Databases](databases.md) </span>

<span class="sect1"> [Opening Databases](databases.md#DBOpen) </span>

<span class="sect1"> [Closing Databases](coredbclose.md) </span>

<span class="sect1"> [Database Open Flags](DBOpenFlags.md) </span>

<span class="sect1"> [Administrative Methods](CoreDBAdmin.md) </span>

<span class="sect1"> [Error Reporting Functions](dbErrorReporting.md) </span>

<span class="sect1"> [Managing Databases in Environments](CoreEnvUsage.md) </span>

<span class="sect1"> [Database Example](CoreDbUsage.md) </span>

<span class="chapter"> [3. Database Records](DBEntry.md) </span>

<span class="sect1"> [Using Database Records](DBEntry.md#usingDbEntry) </span>

<span class="sect1"> [Reading and Writing Database Records](usingDbt.md) </span>

<span class="sect2"> [Writing Records to the Database](usingDbt.md#databaseWrite) </span>

<span class="sect2"> [Getting Records from the Database](usingDbt.md#CoreDatabaseRead) </span>

<span class="sect2"> [Deleting Records](usingDbt.md#recordDelete) </span>

<span class="sect2"> [Data Persistence](usingDbt.md#datapersist) </span>

<span class="sect1"> [Using C Structures with DB](cstructs.md) </span>

<span class="sect2"> [C Structures with Pointers](cstructs.md#cstructdynamic) </span>

<span class="sect1"> [Database Usage Example](DbUsage.md) </span>

<span class="chapter"> [4. Using Cursors](Cursors.md) </span>

<span class="sect1"> [Opening and Closing Cursors](Cursors.md#openCursor) </span>

<span class="sect1"> [Getting Records Using the Cursor](Positioning.md) </span>

<span class="sect2"> [Searching for Records](Positioning.md#cursorsearch) </span>

<span class="sect2"> [Working with Duplicate Records](Positioning.md#getdups) </span>

<span class="sect1"> [Putting Records Using Cursors](PutEntryWCursor.md) </span>

<span class="sect1"> [Deleting Records Using Cursors](DeleteEntryWCursor.md) </span>

<span class="sect1"> [Replacing Records Using Cursors](ReplacingEntryWCursor.md) </span>

<span class="sect1"> [Cursor Example](CoreCursorUsage.md) </span>

<span class="chapter"> [5. Secondary Databases](indexes.md) </span>

<span class="sect1"> [Opening and Closing Secondary Databases](indexes.md#CoreDbAssociate) </span>

<span class="sect1"> [Implementing Key Extractors](keyCreator.md) </span>

<span class="sect2"> [Working with Multiple Keys](keyCreator.md#multikeys) </span>

<span class="sect1"> [Reading Secondary Databases](readSecondary.md) </span>

<span class="sect1"> [Deleting Secondary Database Records](secondaryDelete.md) </span>

<span class="sect1"> [Using Cursors with Secondary Databases](secondaryCursor.md) </span>

<span class="sect1"> [Database Joins](joins.md) </span>

<span class="sect2"> [Using Join Cursors](joins.md#joinUsage) </span>

<span class="sect1"> [Secondary Database Example](coreindexusage.md) </span>

<span class="sect2"> [Secondary Databases with example_database_load](coreindexusage.md#edlWIndexes) </span>

<span class="sect2"> [Secondary Databases with example_database_read](coreindexusage.md#edrWIndexes) </span>

<span class="chapter"> [6. Database Configuration](dbconfig.md) </span>

<span class="sect1"> [Setting the Page Size](dbconfig.md#pagesize) </span>

<span class="sect2"> [Overflow Pages](dbconfig.md#overflowpages) </span>

<span class="sect2"> [Locking](dbconfig.md#Locking) </span>

<span class="sect2"> [IO Efficiency](dbconfig.md#IOEfficiency) </span>

<span class="sect2"> [Page Sizing Advice](dbconfig.md#pagesizeAdvice) </span>

<span class="sect1"> [Selecting the Cache Size](cachesize.md) </span>

<span class="sect1"> [BTree Configuration](btree.md) </span>

<span class="sect2"> [Allowing Duplicate Records](btree.md#duplicateRecords) </span>

<span class="sect2"> [Setting Comparison Functions](btree.md#comparators) </span>

**List of Examples**

2.1. [The stock_db Structure](CoreDbUsage.md#stock-db)

2.2. [The stock_db Utility Functions](CoreDbUsage.md#stock-db-functions)

2.3. [open_database() Function](CoreDbUsage.md#open-db)

2.4. [The databases_setup() Function](CoreDbUsage.md#databasesetup)

2.5. [The databases_close() Function](CoreDbUsage.md#database_close)

3.1. [VENDOR Structure](DbUsage.md#VENDORStruct)

3.2. [example_database_load](DbUsage.md#exampledbload)

4.1. [example_database_read](CoreCursorUsage.md#CoreEIR)
