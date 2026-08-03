---
title: "Getting Started with Berkeley DB"
api-name: "Getting Started with Berkeley DB"
source: docs/gsg/JAVA/index.html
---
# Getting Started with Berkeley DB

**Language:** [C](../index.md) · [C++](../cxx/index.md) · Java (this page)

**Legal Notice**

This documentation is distributed under an open source license. You may review the terms of this license at: <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html" class="ulink" target="_top">http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html</a>

Oracle, Berkeley DB, and Sleepycat are trademarks or registered trademarks of Oracle. All rights to these marks are reserved. No third-party use is permitted without the express prior written consent of Oracle.

<span class="trademark">Java</span>™ and all Java-based marks are a trademark or registered trademark of Sun Microsystems, Inc, in the United States and other countries.

Other names may be trademarks of their respective owners.

To obtain a copy of this document's original source code, please submit a request to the Oracle Technology Network forum at: <a href="http://forums.oracle.com/forums/forum.jspa?forumID=271" class="ulink" target="_top">http://forums.oracle.com/forums/forum.jspa?forumID=271</a>

9/9/2013

------------------------------------------------------------------------

**Table of Contents**

<span class="preface">[Preface](preface.md)</span>

<span class="sect1">[Conventions Used in this Book](preface.md#conventions)</span>

<span class="sect1">[For More Information](moreinfo.md)</span>

<span class="sect2">[Contact Us](moreinfo.md#contact_us)</span>

<span class="chapter">[1. Introduction to Berkeley DB](introduction.md) </span>

<span class="sect1">[About This Manual](introduction.md#aboutthismanual)</span>

<span class="sect1">[Berkeley DB Concepts](javadplconcepts.md)</span>

<span class="sect2">[Environments](javadplconcepts.md#dplenvconcepts)</span>

<span class="sect2">[Key-Data Pairs](javadplconcepts.md#key-data)</span>

<span class="sect2">[Storing Data](javadplconcepts.md#storing-intro)</span>

<span class="sect2">[Duplicate Data](javadplconcepts.md#duplicatesintro)</span>

<span class="sect2">[Replacing and Deleting Entries](javadplconcepts.md#replacedeleteIntro)</span>

<span class="sect2">[Secondary Keys](javadplconcepts.md#secondary)</span>

<span class="sect2">[Which API Should You Use?](javadplconcepts.md#whichapi)</span>

<span class="sect1">[Access Methods](accessmethods.md)</span>

<span class="sect2">[Selecting Access Methods](accessmethods.md#selectAM)</span>

<span class="sect2">[Choosing between BTree and Hash](accessmethods.md#BTreeVSHash)</span>

<span class="sect2">[Choosing between Queue and Recno](accessmethods.md#QueueVSRecno)</span>

<span class="sect1">[Database Limits and Portability](databaseLimits.md)</span>

<span class="sect1">[Exception Handling](coreExceptions.md)</span>

<span class="sect1">[Error Returns](returns.md)</span>

<span class="sect1">[Getting and Using DB](gettingit.md) </span>

<span class="chapter">[2. Database Environments](Env.md)</span>

<span class="sect1">[Opening Database Environments](Env.md#EnvOpen)</span>

<span class="sect1">[Closing Database Environments](EnvClose.md)</span>

<span class="sect1">[Environment Properties](EnvProps.md)</span>

<span class="sect2">[The EnvironmentConfig Class](EnvProps.md#envconfig)</span>

<span class="sect2">[EnvironmentMutableConfig](EnvProps.md#envhandleconfig)</span>

<span class="part">[I. Programming with the Direct Persistence Layer](dpl.md)</span>

<span class="chapter">[3. Direct Persistence Layer First Steps](persist_first.md)</span>

<span class="sect1">[Entity Stores](persist_first.md#entitystore)</span>

<span class="sect2">[Opening and Closing Environments and Stores](persist_first.md#persist-open)</span>

<span class="sect1">[Persistent Objects](persistobject.md)</span>

<span class="sect1">[Saving and Retrieving Data](saveret.md)</span>

<span class="chapter">[4. Working with Indices](persist_index.md)</span>

<span class="sect1">[Accessing Indexes](persist_index.md#dplindexaccess)</span>

<span class="sect2">[Accessing Primary Indices](persist_index.md#primaryindexaccess)</span>

<span class="sect2">[Accessing Secondary Indices](persist_index.md#secondaryindexaccess)</span>

<span class="sect1">[Creating Indexes](dplindexcreate.md)</span>

<span class="sect2">[Declaring a Primary Indexes](dplindexcreate.md#dplprimaryidxdecl)</span>

<span class="sect2">[Declaring Secondary Indexes](dplindexcreate.md#dplsecondaryidxdecl)</span>

<span class="sect2">[Foreign Key Constraints](dplindexcreate.md#foreignkey)</span>

<span class="chapter">[5. Saving and Retrieving Objects](persist_access.md)</span>

<span class="sect1">[A Simple Entity Class](persist_access.md#simpleentity)</span>

<span class="sect1">[SimpleDA.class](simpleda.md)</span>

<span class="sect1">[Placing Objects in an Entity Store](simpleput.md)</span>

<span class="sect1">[Retrieving Objects from an Entity Store](simpleget.md)</span>

<span class="sect1">[Retrieving Multiple Objects](getmultiple.md)</span>

<span class="sect2">[Cursor Initialization](getmultiple.md#dpl_cursor_initialize)</span>

<span class="sect2">[Working with Duplicate Keys](getmultiple.md#dpl_dups)</span>

<span class="sect2">[Key Ranges](getmultiple.md#dpl_cursor_range)</span>

<span class="sect1">[Join Cursors](dpl_entityjoin.md)</span>

<span class="sect1">[Deleting Entity Objects](dpl_delete.md)</span>

<span class="sect1">[Replacing Entity Objects](dpl_replace.md)</span>

<span class="chapter">[6. A DPL Example](dpl_example.md)</span>

<span class="sect1">[Vendor.java](dpl_example.md#vendorclass)</span>

<span class="sect1">[Inventory.java](inventoryclass.md)</span>

<span class="sect1">[MyDbEnv](mydbenv-persist.md)</span>

<span class="sect1">[DataAccessor.java](dataaccessorclass.md)</span>

<span class="sect1">[ExampleDatabasePut.java](dpl_exampledatabaseput.md)</span>

<span class="sect1">[ExampleInventoryRead.java](dpl_exampleinventoryread.md)</span>

<span class="part">[II. Programming with the Base API](baseapi.md)</span>

<span class="chapter">[7. Databases](databases.md)</span>

<span class="sect1">[Opening Databases](databases.md#DBOpen)</span>

<span class="sect1">[Closing Databases](coredbclose.md)</span>

<span class="sect1">[Database Properties](dbprops.md)</span>

<span class="sect1">[Administrative Methods](DBAdmin.md)</span>

<span class="sect1">[Error Reporting Functions](dbErrorReporting.md)</span>

<span class="sect1">[Managing Databases in Environments](CoreEnvUsage.md)</span>

<span class="sect1">[Database Example](CoreJavaUsage.md)</span>

<span class="chapter">[8. Database Records](DBEntry.md)</span>

<span class="sect1">[Using Database Records](DBEntry.md#usingDbEntry)</span>

<span class="sect1">[Reading and Writing Database Records](usingDbt.md)</span>

<span class="sect2">[Writing Records to the Database](usingDbt.md#databaseWrite)</span>

<span class="sect2">[Getting Records from the Database](usingDbt.md#databaseRead)</span>

<span class="sect2">[Deleting Records](usingDbt.md#recordDelete)</span>

<span class="sect2">[Data Persistence](usingDbt.md#datapersist)</span>

<span class="sect1">[Using the BIND APIs](bindAPI.md)</span>

<span class="sect2">[Numerical and String Objects](bindAPI.md#bindPrimitive)</span>

<span class="sect2">[Serializable Complex Objects](bindAPI.md#object2dbt)</span>

<span class="sect2">[Custom Tuple Bindings](bindAPI.md#customTuple)</span>

<span class="sect1">[Database Usage Example](dbtJavaUsage.md)</span>

<span class="chapter">[9. Using Cursors](Cursors.md)</span>

<span class="sect1">[Opening and Closing Cursors](Cursors.md#openCursor)</span>

<span class="sect1">[Getting Records Using the Cursor](Positioning.md)</span>

<span class="sect2">[Searching for Records](Positioning.md#cursorsearch)</span>

<span class="sect2">[Working with Duplicate Records](Positioning.md#getdups)</span>

<span class="sect1">[Putting Records Using Cursors](PutEntryWCursor.md)</span>

<span class="sect1">[Deleting Records Using Cursors](DeleteEntryWCursor.md)</span>

<span class="sect1">[Replacing Records Using Cursors](ReplacingEntryWCursor.md)</span>

<span class="sect1">[Cursor Example](cursorJavaUsage.md)</span>

<span class="chapter">[10. Secondary Databases](indexes.md)</span>

<span class="sect1">[Opening and Closing Secondary Databases](indexes.md#DbAssociate)</span>

<span class="sect1">[Implementing Key Creators](keyCreator.md) </span>

<span class="sect2">[Working with Multiple Keys](keyCreator.md#multikeys)</span>

<span class="sect1">[Secondary Database Properties](secondaryProps.md)</span>

<span class="sect1">[Reading Secondary Databases](readSecondary.md)</span>

<span class="sect1">[Deleting Secondary Database Records](secondaryDelete.md)</span>

<span class="sect1"> [Using Secondary Cursors](secondaryCursor.md) </span>

<span class="sect1">[Database Joins](joins.md)</span>

<span class="sect2">[Using Join Cursors](joins.md#joinUsage)</span>

<span class="sect2">[JoinCursor Properties](joins.md#joinconfig)</span>

<span class="sect1">[Secondary Database Example](javaindexusage.md)</span>

<span class="sect2">[Opening Secondary Databases with MyDbs](javaindexusage.md#secondaryMyDbs)</span>

<span class="sect2">[Using Secondary Databases with ExampleDatabaseRead](javaindexusage.md#exampleReadJavaSecondaries)</span>

<span class="chapter">[11. Database Configuration](dbconfig.md)</span>

<span class="sect1">[Setting the Page Size](dbconfig.md#pagesize)</span>

<span class="sect2">[Overflow Pages](dbconfig.md#overflowpages)</span>

<span class="sect2">[Locking](dbconfig.md#Locking)</span>

<span class="sect2">[IO Efficiency](dbconfig.md#IOEfficiency)</span>

<span class="sect2">[Page Sizing Advice](dbconfig.md#pagesizeAdvice)</span>

<span class="sect1">[Selecting the Cache Size](cachesize.md)</span>

<span class="sect1">[BTree Configuration](btree.md)</span>

<span class="sect2">[Allowing Duplicate Records](btree.md#duplicateRecords)</span>

<span class="sect2">[Setting Comparison Functions](btree.md#comparators)</span>

**List of Examples**

7.1. [MyDbs Class](CoreJavaUsage.md#MyDb)

8.1. [Inventory.java](dbtJavaUsage.md#inventoryjava)

8.2. [Vendor.java](dbtJavaUsage.md#vendorjava)

8.3. [InventoryBinding.java](dbtJavaUsage.md#InventoryJavaBinding)

8.4. [Stored Class Catalog Management with MyDbs](dbtJavaUsage.md#dbsStoredClass)

8.5. [ExampleDatabaseLoad.java](dbtJavaUsage.md#EDL)

9.1. [ExampleDatabaseRead.java](cursorJavaUsage.md#EDR)

10.1. [ItemNameKeyCreator.java](javaindexusage.md#ItemNameKeyCreator-Java)

10.2. [SecondaryDatabase Management with MyDbs](javaindexusage.md#mydbsSecondary)

10.3. [SecondaryDatabase usage with ExampleDatabaseRead](javaindexusage.md#secondaryWithEDR)

\>
