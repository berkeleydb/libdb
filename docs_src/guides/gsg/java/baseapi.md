---
title: "Part II. Programming with the Base API"
api-name: "Part II. Programming with the Base API"
source: docs/gsg/JAVA/baseapi.html
---
# Part II. Programming with the Base API

This section discusses application that are built using the DB base API. Note that most DB applications can probably be written using the DPL (see <a href="dpl.md" class="xref" title="Part I. Programming with the Direct Persistence Layer">Programming with the Direct Persistence Layer</a> for more information). However, if you want to use Java 1.4 for your DB application, or if you are porting an application from the Berkeley DB API, then the base API is right for you.

**Table of Contents**

<span class="chapter"> [7. Databases](databases.md) </span>

<span class="sect1"> [Opening Databases](databases.md#DBOpen) </span>

<span class="sect1"> [Closing Databases](coredbclose.md) </span>

<span class="sect1"> [Database Properties](dbprops.md) </span>

<span class="sect1"> [Administrative Methods](DBAdmin.md) </span>

<span class="sect1"> [Error Reporting Functions](dbErrorReporting.md) </span>

<span class="sect1"> [Managing Databases in Environments](CoreEnvUsage.md) </span>

<span class="sect1"> [Database Example](CoreJavaUsage.md) </span>

<span class="chapter"> [8. Database Records](DBEntry.md) </span>

<span class="sect1"> [Using Database Records](DBEntry.md#usingDbEntry) </span>

<span class="sect1"> [Reading and Writing Database Records](usingDbt.md) </span>

<span class="sect2"> [Writing Records to the Database](usingDbt.md#databaseWrite) </span>

<span class="sect2"> [Getting Records from the Database](usingDbt.md#databaseRead) </span>

<span class="sect2"> [Deleting Records](usingDbt.md#recordDelete) </span>

<span class="sect2"> [Data Persistence](usingDbt.md#datapersist) </span>

<span class="sect1"> [Using the BIND APIs](bindAPI.md) </span>

<span class="sect2"> [Numerical and String Objects](bindAPI.md#bindPrimitive) </span>

<span class="sect2"> [Serializable Complex Objects](bindAPI.md#object2dbt) </span>

<span class="sect2"> [Custom Tuple Bindings](bindAPI.md#customTuple) </span>

<span class="sect1"> [Database Usage Example](dbtJavaUsage.md) </span>

<span class="chapter"> [9. Using Cursors](Cursors.md) </span>

<span class="sect1"> [Opening and Closing Cursors](Cursors.md#openCursor) </span>

<span class="sect1"> [Getting Records Using the Cursor](Positioning.md) </span>

<span class="sect2"> [Searching for Records](Positioning.md#cursorsearch) </span>

<span class="sect2"> [Working with Duplicate Records](Positioning.md#getdups) </span>

<span class="sect1"> [Putting Records Using Cursors](PutEntryWCursor.md) </span>

<span class="sect1"> [Deleting Records Using Cursors](DeleteEntryWCursor.md) </span>

<span class="sect1"> [Replacing Records Using Cursors](ReplacingEntryWCursor.md) </span>

<span class="sect1"> [Cursor Example](cursorJavaUsage.md) </span>

<span class="chapter"> [10. Secondary Databases](indexes.md) </span>

<span class="sect1"> [Opening and Closing Secondary Databases](indexes.md#DbAssociate) </span>

<span class="sect1"> [Implementing Key Creators](keyCreator.md) </span>

<span class="sect2"> [Working with Multiple Keys](keyCreator.md#multikeys) </span>

<span class="sect1"> [Secondary Database Properties](secondaryProps.md) </span>

<span class="sect1"> [Reading Secondary Databases](readSecondary.md) </span>

<span class="sect1"> [Deleting Secondary Database Records](secondaryDelete.md) </span>

<span class="sect1"> [Using Secondary Cursors](secondaryCursor.md) </span>

<span class="sect1"> [Database Joins](joins.md) </span>

<span class="sect2"> [Using Join Cursors](joins.md#joinUsage) </span>

<span class="sect2"> [JoinCursor Properties](joins.md#joinconfig) </span>

<span class="sect1"> [Secondary Database Example](javaindexusage.md) </span>

<span class="sect2"> [Opening Secondary Databases with MyDbs](javaindexusage.md#secondaryMyDbs) </span>

<span class="sect2"> [Using Secondary Databases with ExampleDatabaseRead](javaindexusage.md#exampleReadJavaSecondaries) </span>

<span class="chapter"> [11. Database Configuration](dbconfig.md) </span>

<span class="sect1"> [Setting the Page Size](dbconfig.md#pagesize) </span>

<span class="sect2"> [Overflow Pages](dbconfig.md#overflowpages) </span>

<span class="sect2"> [Locking](dbconfig.md#Locking) </span>

<span class="sect2"> [IO Efficiency](dbconfig.md#IOEfficiency) </span>

<span class="sect2"> [Page Sizing Advice](dbconfig.md#pagesizeAdvice) </span>

<span class="sect1"> [Selecting the Cache Size](cachesize.md) </span>

<span class="sect1"> [BTree Configuration](btree.md) </span>

<span class="sect2"> [Allowing Duplicate Records](btree.md#duplicateRecords) </span>

<span class="sect2"> [Setting Comparison Functions](btree.md#comparators) </span>
