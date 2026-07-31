---
title: "Appendix A.  API Notes and Details"
api-name: "Appendix A.  API Notes and Details"
source: docs/collections/tutorial/collectionOverview.html
---
## Appendix A.  API Notes and Details

This appendix contains information useful to the collections programmer that is too detailed to easily fit into the format of a tutorial. Specifically, this appendix contains the following information:

- <a href="collectionOverview.md#UsingDataBindings" class="xref" title="Using Data Bindings">Using Data Bindings</a>

- <a href="UsingCollectionsAPI.md" class="xref" title="Using the DB Java Collections API">Using the DB Java Collections API</a>

- <a href="UsingStoredCollections.md" class="xref" title="Using Stored Collections">Using Stored Collections</a>

- <a href="SerializedObjectStorage.md" class="xref" title="Serialized Object Storage">Serialized Object Storage</a>

## Using Data Bindings

<span class="sect2"> [Selecting Binding Formats](collectionOverview.md#SelectingBindingFormats) </span>

<span class="sect2"> [Record Number Bindings](collectionOverview.md#RecordNumberBindings) </span>

<span class="sect2"> [Selecting Data Bindings](collectionOverview.md#SelectingDataBindings) </span>

<span class="sect2"> [Implementing Bindings](collectionOverview.md#ImplementingBindings) </span>

<span class="sect2"> [Using Bindings](collectionOverview.md#UsingBindings) </span>

<span class="sect2"> [Secondary Key Creators](collectionOverview.md#SecondaryKeyCreators) </span>

Data bindings determine how keys and values are represented as stored data (byte arrays) in the database, and how stored data is converted to and from Java objects.

The selection of data bindings is, in general, independent of the selection of access methods and collection views. In other words, any binding can be used with any access method or collection. One exception to this rule is described under <a href="collectionOverview.md#RecordNumberBindings" class="xref" title="Record Number Bindings">Record Number Bindings</a> below.

### Note

In this document, bindings are described in the context of their use for stored data in a database. However, bindings may also be used independently of a database to operate on an arbitrary byte array. This allows using bindings when data is to be written to a file or sent over a network, for example.

### Selecting Binding Formats

For the key and value of each stored collection, you may select one of the following types of bindings.

| Binding Format | Ordered | Description |
|----|----|----|
| <a href="../../java/com/sleepycat/bind/serial/SerialBinding.html" class="ulink" target="_top">SerialBinding</a> | No | The data is stored using a compact form of Java serialization, where the class descriptions are stored separately in a catalog database. Arbitrary Java objects are supported. |
| <a href="../../java/com/sleepycat/bind/tuple/TupleBinding.html" class="ulink" target="_top">TupleBinding</a> | Yes | The data is stored using a series of fixed length primitive values or zero terminated character arrays (strings). Class/type evolution is not supported. |
| <a href="../../java/com/sleepycat/bind/RecordNumberBinding.html" class="ulink" target="_top">RecordNumberBinding</a> | Yes | The data is a 32-bit integer stored in a platform-dependent format. |
| Custom binding format | User-defined | The data storage format and ordering is determined by the custom binding implementation. |

As shown in the table above, the tuple format supports built-in ordering (without specifying a custom comparator), while the serial format does not. This means that when a specific key order is needed, tuples should be used instead of serial data. Alternatively, a custom Btree comparator should be specified using `DatabaseConfig.setBtreeComparator()`. Note that a custom Btree comparator will usually execute more slowly than the default byte-by-byte comparison. This makes using tuples an attractive option, since they provide ordering along with optimal performance.

The tuple binding uses less space and executes faster than the serial binding. But once a tuple is written to a database, the order of fields in the tuple may not be changed and fields may not be deleted. The only type evolution allowed is the addition of fields at the end of the tuple, and this must be explicitly supported by the custom binding implementation.

The serial binding supports the full generality of Java serialization including type evolution. But serialized data can only be accessed by Java applications, its size is larger, and its bindings are slower to execute.

### Record Number Bindings

Any use of an access method with record number keys, and therefore any use of a stored list view, requires using <a href="../../java/com/sleepycat/bind/RecordNumberBinding.html" class="ulink" target="_top">RecordNumberBinding</a> as the key binding. Since Berkeley DB stores record number keys using a platform-dependent byte order, <a href="../../java/com/sleepycat/bind/RecordNumberBinding.html" class="ulink" target="_top">RecordNumberBinding</a> is needed to store record numbers properly. See <span class="html"><a href="../../guides/programmer_reference/am_conf_logrec.md" class="ulink" target="_top">logical record numbers</a> in</span> the *Berkeley DB Programmer's Reference Guide* for more information on storing DB record numbers.

### Note

You may not use <a href="../../java/com/sleepycat/bind/RecordNumberBinding.html" class="ulink" target="_top">RecordNumberBinding</a> except with record number keys, as determined by the access method. Using <a href="../../java/com/sleepycat/bind/RecordNumberBinding.html" class="ulink" target="_top">RecordNumberBinding</a> in other cases will create a database that is not portable between platforms. When constructing the stored collection, the DB Java Collections API will throw an <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/IllegalArgumentException.html" class="ulink" target="_top">IllegalArgumentException</a> in such cases.

### Selecting Data Bindings

There are two types of binding interfaces. Simple entry bindings implement the <a href="../../java/com/sleepycat/bind/EntryBinding.html" class="ulink" target="_top">EntryBinding</a> interface and can be used for key or value objects. Entity bindings implement the <a href="../../java/com/sleepycat/bind/EntityBinding.html" class="ulink" target="_top">EntityBinding</a> interface and are used for combined key and value objects called entities.

Simple entry bindings map between the key or value data stored by Berkeley DB and a key or value object. This is a simple one-to-one mapping.

Simple entry bindings are easy to implement and in some cases require no coding. For example, a <a href="../../java/com/sleepycat/bind/serial/SerialBinding.html" class="ulink" target="_top">SerialBinding</a> can be used for keys or values without writing any additional code. A tuple binding for a single-item tuple can also be used without writing any code; see the <a href="../../java/com/sleepycat/bind/tuple/TupleBinding.html#getPrimitiveBinding(java.lang.Class)" class="ulink" target="_top">TupleBinding.getPrimitiveBinding</a> method.

Entity bindings must divide an entity object into its key and value data, and then combine the key and value data to re-create the entity object. This is a two-to-one mapping.

Entity bindings are useful when a stored application object naturally has its primary key as a property, which is very common. For example, an Employee object would naturally have an EmployeeNumber property (its primary key) and an entity binding would then be needed. Of course, entity bindings are more complex to implement, especially if their key and data formats are different.

Note that even when an entity binding is used a key binding is also usually needed. For example, a key binding is used to create key objects that are passed to the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#get" class="ulink" target="_top">Map.get()</a> method. A key object is passed to this method even though it may return an entity that also contains the key.

### Implementing Bindings

There are two ways to implement bindings. The first way is to create a binding class that implements one of the two binding interfaces, <a href="../../java/com/sleepycat/bind/EntryBinding.html" class="ulink" target="_top">EntryBinding</a> or <a href="../../java/com/sleepycat/bind/EntityBinding.html" class="ulink" target="_top">EntityBinding</a>. For tuple bindings and serial bindings there are a number of abstract classes that make this easier. For example, you can extend <a href="../../java/com/sleepycat/bind/tuple/TupleBinding.html" class="ulink" target="_top">TupleBinding</a> to implement a simple binding for a tuple key or value. Abstract classes are also provided for entity bindings and are named after the format names of the key and value. For example, you can extend <a href="../../java/com/sleepycat/bind/serial/TupleSerialBinding.html" class="ulink" target="_top">TupleSerialBinding</a> to implement an entity binding with a tuple key and serial value.

Another way to implement bindings is with marshalling interfaces. These are interfaces which perform the binding operations and are implemented by the key, value or entity classes themselves. With marshalling you use a binding which calls the marshalling interface and you implement the marshalling interface for each key, value or entity class. For example, you can use <a href="../../java/com/sleepycat/bind/tuple/TupleMarshalledBinding.html" class="ulink" target="_top">TupleMarshalledBinding</a> along with key or value classes that implement the <a href="../../java/com/sleepycat/bind/tuple/MarshalledTupleEntry.html" class="ulink" target="_top">MarshalledTupleEntry</a> interface.

### Using Bindings

Bindings are specified whenever a stored collection is created. A key binding must be specified for map, key set and entry set views. A value binding or entity binding must be specified for map, value set and entry set views.

Any number of bindings may be created for the same stored data. This allows multiple views over the same data. For example, a tuple might be bound to an array of values or to a class with properties for each object.

It is important to be careful of bindings that only use a subset of the stored data. This can be useful to simplify a view or to hide information that should not be accessible. However, if you write records using these bindings you may create stored data that is invalid from the application's point of view. It is up to the application to guard against this by creating a read-only collection when such bindings are used.

### Secondary Key Creators

Secondary Key Creators are needed whenever database indices are used. For each secondary index (<a href="../../java/com/sleepycat/db/SecondaryDatabase.html" class="ulink" target="_top">SecondaryDatabase</a>) a key creator is used to derive index key data from key/value data. Key creators are objects whose classes implement the <a href="../../java/com/sleepycat/db/SecondaryKeyCreator.html" class="ulink" target="_top">SecondaryKeyCreator</a> interface.

Like bindings, key creators may be implemented using a separate key creator class or using a marshalling interface. Abstract key creator classes and marshalling interfaces are provided in the com.sleepycat.bind.tuple and com.sleepycat.bind.serial packages.

Unlike bindings, key creators fundamentally operate on key and value data, not necessarily on the objects derived from the data by bindings. In this sense key creators are a part of a database definition, and may be independent of the various bindings that may be used to view data in a database. However, key creators are not prohibited from using higher level objects produced by bindings, and doing so may be convenient for some applications. For example, marshalling interfaces, which are defined for objects produced by bindings, are a convenient way to define key creators.
