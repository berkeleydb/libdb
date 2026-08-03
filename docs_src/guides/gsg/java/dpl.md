---
title: "Part I. Programming with the Direct Persistence Layer"
api-name: "Part I. Programming with the Direct Persistence Layer"
source: docs/gsg/JAVA/dpl.html
---
# Part I. Programming with the Direct Persistence Layer

This section discusses how to build an application using the DPL. The DPL is ideally suited for those applications that want a mechanism for storing and managing Java class objects in a DB database. Note that the DPL is best suited for applications that work with classes with a relatively static schema.

Also, the DPL requires Java 1.5.

If you want to use Java 1.4 for your DB application, or if you are porting an application from the Berkeley DB API, then you probably want to use the base API instead of the DPL. For information on using the base API, see <a href="baseapi.md" class="xref" title="Part II. Programming with the Base API">Programming with the Base API</a>.

**Table of Contents**

<span class="chapter"> [3. Direct Persistence Layer First Steps](persist_first.md) </span>

<span class="sect1"> [Entity Stores](persist_first.md#entitystore) </span>

<span class="sect2"> [Opening and Closing Environments and Stores](persist_first.md#persist-open) </span>

<span class="sect1"> [Persistent Objects](persistobject.md) </span>

<span class="sect1"> [Saving and Retrieving Data](saveret.md) </span>

<span class="chapter"> [4. Working with Indices](persist_index.md) </span>

<span class="sect1"> [Accessing Indexes](persist_index.md#dplindexaccess) </span>

<span class="sect2"> [Accessing Primary Indices](persist_index.md#primaryindexaccess) </span>

<span class="sect2"> [Accessing Secondary Indices](persist_index.md#secondaryindexaccess) </span>

<span class="sect1"> [Creating Indexes](dplindexcreate.md) </span>

<span class="sect2"> [Declaring a Primary Indexes](dplindexcreate.md#dplprimaryidxdecl) </span>

<span class="sect2"> [Declaring Secondary Indexes](dplindexcreate.md#dplsecondaryidxdecl) </span>

<span class="sect2"> [Foreign Key Constraints](dplindexcreate.md#foreignkey) </span>

<span class="chapter"> [5. Saving and Retrieving Objects](persist_access.md) </span>

<span class="sect1"> [A Simple Entity Class](persist_access.md#simpleentity) </span>

<span class="sect1"> [SimpleDA.class](simpleda.md) </span>

<span class="sect1"> [Placing Objects in an Entity Store](simpleput.md) </span>

<span class="sect1"> [Retrieving Objects from an Entity Store](simpleget.md) </span>

<span class="sect1"> [Retrieving Multiple Objects](getmultiple.md) </span>

<span class="sect2"> [Cursor Initialization](getmultiple.md#dpl_cursor_initialize) </span>

<span class="sect2"> [Working with Duplicate Keys](getmultiple.md#dpl_dups) </span>

<span class="sect2"> [Key Ranges](getmultiple.md#dpl_cursor_range) </span>

<span class="sect1"> [Join Cursors](dpl_entityjoin.md) </span>

<span class="sect1"> [Deleting Entity Objects](dpl_delete.md) </span>

<span class="sect1"> [Replacing Entity Objects](dpl_replace.md) </span>

<span class="chapter"> [6. A DPL Example](dpl_example.md) </span>

<span class="sect1"> [Vendor.java](dpl_example.md#vendorclass) </span>

<span class="sect1"> [Inventory.java](inventoryclass.md) </span>

<span class="sect1"> [MyDbEnv](mydbenv-persist.md) </span>

<span class="sect1"> [DataAccessor.java](dataaccessorclass.md) </span>

<span class="sect1"> [ExampleDatabasePut.java](dpl_exampledatabaseput.md) </span>

<span class="sect1"> [ExampleInventoryRead.java](dpl_exampleinventoryread.md) </span>
