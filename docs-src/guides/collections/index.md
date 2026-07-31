---
title: "Berkeley DB Collections Tutorial"
api-name: "Berkeley DB Collections Tutorial"
source: docs/collections/tutorial/index.html
---
# Berkeley DB Collections Tutorial

**Legal Notice**

This documentation is distributed under an open source license. You may review the terms of this license at: <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html" class="ulink" target="_top">http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html</a>

Oracle, Berkeley DB, and Sleepycat are trademarks or registered trademarks of Oracle. All rights to these marks are reserved. No third-party use is permitted without the express prior written consent of Oracle.

<span class="trademark">Java</span>™ and all Java-based marks are a trademark or registered trademark of Sun Microsystems, Inc, in the United States and other countries.

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

<span class="sect1"> [Features](intro.md#features) </span>

<span class="sect1"> [Developing a DB Collections Application](developing.md) </span>

<span class="sect1"> [Tutorial Introduction](tutorialintroduction.md) </span>

<span class="chapter"> [2. The Basic Program](BasicProgram.md) </span>

<span class="sect1"> [Defining Serialized Key and Value Classes](BasicProgram.md#keyandvalueclasses) </span>

<span class="sect1"> [Opening and Closing the Database Environment](opendbenvironment.md) </span>

<span class="sect1"> [Opening and Closing the Class Catalog](openclasscatalog.md) </span>

<span class="sect1"> [Opening and Closing Databases](opendatabases.md) </span>

<span class="sect1"> [Creating Bindings and Collections](createbindingscollections.md) </span>

<span class="sect1"> [Implementing the Main Program](implementingmain.md) </span>

<span class="sect1"> [Using Transactions](usingtransactions.md) </span>

<span class="sect1"> [Adding Database Items](addingdatabaseitems.md) </span>

<span class="sect1"> [Retrieving Database Items](retrievingdatabaseitems.md) </span>

<span class="sect1"> [Handling Exceptions](handlingexceptions.md) </span>

<span class="chapter"> [3. Using Secondary Indices](UsingSecondaries.md) </span>

<span class="sect1"> [Opening Secondary Key Indices](UsingSecondaries.md#opensecondaryindices) </span>

<span class="sect1"> [More Secondary Key Indices](openingforeignkeys.md) </span>

<span class="sect1"> [Creating Indexed Collections](indexedcollections.md) </span>

<span class="sect1"> [Retrieving Items by Index Key](retrievingbyindexkey.md) </span>

<span class="chapter"> [4. Using Entity Classes](Entity.md) </span>

<span class="sect1"> [Defining Entity Classes](Entity.md#definingentityclasses) </span>

<span class="sect1"> [Creating Entity Bindings](creatingentitybindings.md) </span>

<span class="sect1"> [Creating Collections with Entity Bindings](collectionswithentities.md) </span>

<span class="sect1"> [Using Entities with Collections](entitieswithcollections.md) </span>

<span class="chapter"> [5. Using Tuples](Tuple.md) </span>

<span class="sect1"> [Using the Tuple Format](Tuple.md#tupleformat) </span>

<span class="sect1"> [Using Tuples with Key Creators](tupleswithkeycreators.md) </span>

<span class="sect1"> [Creating Tuple Key Bindings](tuplekeybindings.md) </span>

<span class="sect1"> [Creating Tuple-Serial Entity Bindings](tuple-serialentitybindings.md) </span>

<span class="sect1"> [Using Sorted Collections](sortedcollections.md) </span>

<span class="chapter"> [6. Using Serializable Entities](SerializableEntity.md) </span>

<span class="sect1"> [Using Transient Fields in an Entity Class](SerializableEntity.md#transientfieldsinclass) </span>

<span class="sect1"> [Using Transient Fields in an Entity Binding](transientfieldsinbinding.md) </span>

<span class="sect1"> [Removing the Redundant Value Classes](removingredundantvalueclasses.md) </span>

<span class="chapter"> [7. Summary](Summary.md) </span>

<span class="appendix"> [A. API Notes and Details](collectionOverview.md) </span>

<span class="sect1"> [Using Data Bindings](collectionOverview.md#UsingDataBindings) </span>

<span class="sect2"> [Selecting Binding Formats](collectionOverview.md#SelectingBindingFormats) </span>

<span class="sect2"> [Record Number Bindings](collectionOverview.md#RecordNumberBindings) </span>

<span class="sect2"> [Selecting Data Bindings](collectionOverview.md#SelectingDataBindings) </span>

<span class="sect2"> [Implementing Bindings](collectionOverview.md#ImplementingBindings) </span>

<span class="sect2"> [Using Bindings](collectionOverview.md#UsingBindings) </span>

<span class="sect2"> [Secondary Key Creators](collectionOverview.md#SecondaryKeyCreators) </span>

<span class="sect1"> [Using the DB Java Collections API](UsingCollectionsAPI.md) </span>

<span class="sect2"> [Using Transactions](UsingCollectionsAPI.md#UsingTransactions) </span>

<span class="sect2"> [Transaction Rollback](UsingCollectionsAPI.md#TransactionRollback) </span>

<span class="sect2"> [Selecting Access Methods](UsingCollectionsAPI.md#SelectingAccessMethods) </span>

<span class="sect2"> [Access Method Restrictions](UsingCollectionsAPI.md#AccessMethodRestrictions) </span>

<span class="sect1"> [Using Stored Collections](UsingStoredCollections.md) </span>

<span class="sect2"> [Stored Collection and Access Methods](UsingStoredCollections.md#StoredCollectionAccessMethods) </span>

<span class="sect2"> [Stored Collections Versus Standard Java Collections](UsingStoredCollections.md#StoredVersusStandardCollections) </span>

<span class="sect2"> [Other Stored Collection Characteristics](UsingStoredCollections.md#StoredCollectionCharacteristics) </span>

<span class="sect2"> [Why Java Collections for Berkeley DB](UsingStoredCollections.md#WhyJavaCollections) </span>

<span class="sect1"> [Serialized Object Storage](SerializedObjectStorage.md) </span>
