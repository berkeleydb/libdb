---
title: "Developing a DB Collections Application"
api-name: "Developing a DB Collections Application"
source: docs/collections/tutorial/developing.html
---
## Developing a DB Collections Application

There are several important choices to make when developing an application using the DB Java Collections API.

1.  Choose the Berkeley DB Environment

    Depending on your application's concurrency and transactional requirements, you may choose one of the three Berkeley DB Environments: Data Store, Concurrent Data Store, or Transactional Data Store. For details on creating and configuring the environment, see the *Berkeley DB Programmer's Reference Guide*

2.  Choose the Berkeley DB Access Method

    For each Berkeley DB datastore, you may choose from any of the four Berkeley DB access methods — BTREE, HASH, RECNO, or QUEUE <span class="html"> (<a href="../../java/com/sleepycat/db/DatabaseType.html#BTREE" class="ulink" target="_top">DatabaseType.BTREE</a>, <a href="../../java/com/sleepycat/db/DatabaseType.html#HASH" class="ulink" target="_top">DatabaseType.HASH</a>, <a href="../../java/com/sleepycat/db/DatabaseType.html#RECNO" class="ulink" target="_top">DatabaseType.RECNO</a>, or <a href="../../java/com/sleepycat/db/DatabaseType.html#QUEUE" class="ulink" target="_top">DatabaseType.QUEUE</a>.) </span> — and a number of other database options. Your choice depends on several factors such as whether you need ordered keys, unique keys, record number access, and so forth. For more information on access methods, see the *Berkeley DB Programmer's Reference Guide*.

3.  Choose the Format for Keys and Values

    For each database you may choose a binding format for the keys and values. For example, the tuple format is useful for keys because it has a deterministic sort order. The serial format is useful for values if you want to store arbitrary Java objects. In some cases a custom format may be appropriate. For details on choosing a binding format see <a href="collectionOverview.md#UsingDataBindings" class="xref" title="Using Data Bindings">Using Data Bindings</a> .

4.  Choose the Binding for Keys and Values

    With the serial data format you do not have to create a binding for each Java class that is stored since Java serialization is used. But for other formats a binding must be defined that translates between stored byte arrays and Java objects. For details see <a href="collectionOverview.md#UsingDataBindings" class="xref" title="Using Data Bindings">Using Data Bindings</a> .

5.  Choose Secondary Indices

    Any database that has unique keys may have any number of secondary indices. A secondary index has keys that are derived from data values in the primary database. This allows lookup and iteration of objects in the database by its index keys. For each index you must define how the index keys are derived from the data values using a <a href="../../java/com/sleepycat/db/SecondaryKeyCreator.html" class="ulink" target="_top">SecondaryKeyCreator</a>. For details see the <a href="../../java/com/sleepycat/db/SecondaryDatabase.html" class="ulink" target="_top">SecondaryDatabase</a>, <a href="../../java/com/sleepycat/db/SecondaryConfig.html" class="ulink" target="_top">SecondaryConfig</a> and <a href="../../java/com/sleepycat/db/SecondaryKeyCreator.html" class="ulink" target="_top">SecondaryKeyCreator</a> classes.

6.  Choose the Collection Interface for each Database

    The standard Java Collection interfaces are used for accessing databases and secondary indices. The Map and Set interfaces may be used for any type of database. The Iterator interface is used through the Set interfaces. For more information on the collection interfaces see <a href="UsingStoredCollections.md" class="xref" title="Using Stored Collections">Using Stored Collections</a> .

Any number of bindings and collections may be created for the same database. This allows multiple views of the same stored data. For example, a data store may be viewed as a Map of keys to values, a Set of keys, or a Collection of values. String values, for example, may be used with the built-in binding to the String class, or with a custom binding to another class that represents the string values differently.

It is sometimes desirable to use a Java class that encapsulates both a data key and a data value. For example, a Part object might contain both the part number (key) and the part name (value). Using the DB Java Collections API this type of object is called an "entity". An entity binding is used to translate between the Java object and the stored data key and value. Entity bindings may be used with all Collection types.

Please be aware that the provided DB Java Collections API collection classes do not conform completely to the interface contracts defined in the `java.util` package. For example, all iterators must be explicitly closed and the `size()` method is not available. The differences between the DB Java Collections API collections and the standard Java collections are documented in <a href="UsingStoredCollections.md#StoredVersusStandardCollections" class="xref" title="Stored Collections Versus Standard Java Collections">Stored Collections Versus Standard Java Collections</a> .
