---
title: "Using Stored Collections"
api-name: "Using Stored Collections"
source: docs/collections/tutorial/UsingStoredCollections.html
---
## Using Stored Collections

<span class="sect2"> [Stored Collection and Access Methods](UsingStoredCollections.md#StoredCollectionAccessMethods) </span>

<span class="sect2"> [Stored Collections Versus Standard Java Collections](UsingStoredCollections.md#StoredVersusStandardCollections) </span>

<span class="sect2"> [Other Stored Collection Characteristics](UsingStoredCollections.md#StoredCollectionCharacteristics) </span>

<span class="sect2"> [Why Java Collections for Berkeley DB](UsingStoredCollections.md#WhyJavaCollections) </span>

When a stored collection is created it is based on either a <a href="../../java/com/sleepycat/db/Database.html" class="ulink" target="_top">Database</a> or a <a href="../../java/com/sleepycat/db/SecondaryDatabase.html" class="ulink" target="_top">SecondaryDatabase</a>. When a database is used, the primary key of the database is used as the collection key. When a secondary database is used, the index key is used as the collection key. Indexed collections can be used for reading elements and removing elements but not for adding or updating elements.

### Stored Collection and Access Methods

The use of stored collections is constrained in certain respects as described below. Most of these restrictions have to do with <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/List.html" class="ulink" target="_top">List</a> interfaces; for <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html" class="ulink" target="_top">Map</a> interfaces, most all access modes are fully supported since the Berkeley DB model is map-like.

- <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedSet.html" class="ulink" target="_top">SortedSet</a> and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedMap.html" class="ulink" target="_top">SortedMap</a> interfaces may only be used if keys are ordered. This means ordered keys are required for creating a <a href="../../java/com/sleepycat/collections/StoredSortedEntrySet.html" class="ulink" target="_top">StoredSortedEntrySet</a>, <a href="../../java/com/sleepycat/collections/StoredSortedKeySet.html" class="ulink" target="_top">StoredSortedKeySet</a>, <a href="../../java/com/sleepycat/collections/StoredSortedMap.html" class="ulink" target="_top">StoredSortedMap</a>, or <a href="../../java/com/sleepycat/collections/StoredSortedValueSet.html" class="ulink" target="_top">StoredSortedValueSet</a>.

- All iterators for stored collections implement the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html" class="ulink" target="_top">ListIterator</a> interface as well as the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Iterator.html" class="ulink" target="_top">Iterator</a> interface. <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#hasPrevious()" class="ulink" target="_top">ListIterator.hasPrevious()</a> and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#previous()" class="ulink" target="_top">ListIterator.previous()</a> work in all cases. However, the following <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html" class="ulink" target="_top">ListIterator</a> method behavior is dependent on the access method.

  - <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#nextIndex()" class="ulink" target="_top">ListIterator.nextIndex()</a> and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#previousIndex()" class="ulink" target="_top">ListIterator.previousIndex()</a> only work when record number keys are used, and throw <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/UnsupportedOperationException.html" class="ulink" target="_top">UnsupportedOperationException</a> otherwise.

  - <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#add()" class="ulink" target="_top">ListIterator.add()</a> inserts before the current position and renumbers following keys if the RECNO-RENUMBER access method is used.

  - For all access methods other than RECNO-RENUMBER:

    - <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#add()" class="ulink" target="_top">ListIterator.add()</a> throws <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/UnsupportedOperationException.html" class="ulink" target="_top">UnsupportedOperationException</a> if duplicates are not allowed.

    - <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#add()" class="ulink" target="_top">ListIterator.add()</a> inserts a duplicate before the current position if duplicates are unsorted.

    - <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#add()" class="ulink" target="_top">ListIterator.add()</a> inserts a duplicate in sorted order if duplicates are sorted.

  - <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#set()" class="ulink" target="_top">ListIterator.set()</a> throws <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/UnsupportedOperationException.html" class="ulink" target="_top">UnsupportedOperationException</a> if sorted duplicates are configured, since updating with sorted duplicates would change the iterator position.

- <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.Entry.html#setValue()" class="ulink" target="_top">Map.Entry.setValue()</a> throws <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/UnsupportedOperationException.html" class="ulink" target="_top">UnsupportedOperationException</a> if duplicates are sorted.

- Only the access methods that use a record number key may be used with a <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/List.html" class="ulink" target="_top">List</a> `List` view.

- To create a stored List that supports the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/List.html#add()" class="ulink" target="_top">List.add()</a> `List.add()` method, only the RECNO-RENUMBER access method may be used.

- For List access methods that do not support <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/List.html#add()" class="ulink" target="_top">List.add()</a> `List.add()` (RECNO, QUEUE, and BTREE-RECNUM):

  - <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/List.html#add()" class="ulink" target="_top">List.add()</a> `List.add()` and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#add()" class="ulink" target="_top">ListIterator.add()</a> `ListIterator.add()` always throw <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/UnsupportedOperationException.html" class="ulink" target="_top">UnsupportedOperationException</a> .

  - <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/List.html#remove()" class="ulink" target="_top">List.remove()</a> `List.remove()` and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#remove()" class="ulink" target="_top">ListIterator.remove()</a> `ListIterator.remove()` do not cause list indices to be renumbered. However, iterators will skip the removed values.

  For these access methods, stored Lists are most useful as read-only collections where indices are not required to be sequential.

- When duplicates are allowed the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Collection.html" class="ulink" target="_top">Collection</a> interfaces are modified in several ways as described in the next section.

### Stored Collections Versus Standard Java Collections

Stored collections have the following differences with the standard Java collection interfaces. Some of these are interface contract violations.

The Java collections interface does not support duplicate keys (multi-maps or multi-sets). When the access method allows duplicate keys, the collection interfaces are defined as follows.

- <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#entrySet()" class="ulink" target="_top">Map.entrySet()</a> may contain multiple <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.Entry.html" class="ulink" target="_top">Map.Entry</a> objects with the same key.

- <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#keySet()" class="ulink" target="_top">Map.keySet()</a> always contains unique keys, it does not contain duplicates.

- <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#values()" class="ulink" target="_top">Map.values()</a> contains all values including the values associated with duplicate keys.

- <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#put()" class="ulink" target="_top">Map.put()</a> appends a duplicate if the key already exists rather than replacing the existing value, and always returns null.

- <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#remove()" class="ulink" target="_top">Map.remove()</a> removes all duplicates for the specified key.

- <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#get()" class="ulink" target="_top">Map.get()</a> returns the first duplicate for the specified key.

- <a href="../../java/com/sleepycat/collections/StoredMap.html#duplicates(java.lang.Object)" class="ulink" target="_top">StoredMap.duplicates()</a> is an additional method for returning the values for a given key as a <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Collection.html" class="ulink" target="_top">Collection</a>.

Other differences are:

- Collection.size() and Map.size() always throws <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/UnsupportedOperationException.html" class="ulink" target="_top">UnsupportedOperationException</a>. This is because the number of records in a database cannot be determined reliably or cheaply.

- Because the size() method cannot be used, the bulk operation methods of standard Java collections cannot be passed stored collections as parameters, since the implementations rely on size(). However, the bulk operation methods of stored collections can be passed standard Java collections as parameters. `storedCollection.addAll(standardCollection)` is allowed while `standardCollection.addAll(storedCollection)` is <span class="emphasis">*not*</span> allowed. This restriction applies to the standard collection constructors that take a Collection parameter (copy constructors), the Map.putAll() method, and the following Collection methods: addAll(), containsAll(), removeAll() and retainAll().

- The `ListIterator.nextIndex()` method returns `Integer.MAX_VALUE` for stored lists when positioned at the end of the list, rather than returning the list size as specified by the ListIterator interface. Again, this is because the database size is not available.

- <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Comparator.html" class="ulink" target="_top">Comparator</a> objects cannot be used and the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedMap.html#comparator()" class="ulink" target="_top">SortedMap.comparator()</a> and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedSet.html#comparator()" class="ulink" target="_top">SortedSet.comparator()</a> methods always return null. The <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/Comparable.html" class="ulink" target="_top">Comparable</a> interface is not supported. However, Comparators that operate on byte arrays may be specified using <a href="../../java/com/sleepycat/db/DatabaseConfig.html#setBtreeComparator(java.util.Comparator)" class="ulink" target="_top">DatabaseConfig.setBtreeComparator</a>.

- The <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/Object.html#equals()" class="ulink" target="_top">Object.equals()</a> method is not used to determine whether a key or value is contained in a collection, to locate a value by key, etc. Instead the byte array representation of the keys and values are used. However, the equals() method <span class="emphasis">*is*</span> called for each key and value when comparing two collections for equality. It is the responsibility of the application to make sure that the equals() method returns true if and only if the byte array representations of the two objects are equal. Normally this occurs naturally since the byte array representation is derived from the object's fields.

### Other Stored Collection Characteristics

The following characteristics of stored collections are extensions of the definitions in the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/package-summary.html" class="ulink" target="_top">java.util</a> package. These differences do not violate the Java collections interface contract.

- All stored collections are thread safe (can be used by multiple threads concurrently) whenever the Berkeley DB Concurrent Data Store or Transactional Data Store environment is used. Locking is handled by the Berkeley DB environment. To access a collection from multiple threads, creation of synchronized collections using the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Collections.html" class="ulink" target="_top">Collections</a> class is not necessary except when using the Data Store environment. Iterators, however, should always be used only by a single thread.

- All stored collections may be read-only if desired by passing false for the writeAllowed parameter of their constructor. Creation of immutable collections using the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Collections.html" class="ulink" target="_top">Collections</a> class is not necessary.

- A stored collection is partially read-only if a secondary index is used. Specifically, values may be removed but may not be added or updated. The following methods will throw <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/UnsupportedOperationException.html" class="ulink" target="_top">UnsupportedOperationException</a> when an index is used: <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Collection.html#add()" class="ulink" target="_top">Collection.add()</a>, <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/List.html#set()" class="ulink" target="_top">List.set()</a>, <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#set()" class="ulink" target="_top">ListIterator.set()</a> and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.Entry.html#setValue()" class="ulink" target="_top">Map.Entry.setValue()</a>.

- <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedMap.html#entrySet()" class="ulink" target="_top">SortedMap.entrySet()</a> and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedMap.html#keySet()" class="ulink" target="_top">SortedMap.keySet()</a> return a <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedSet.html" class="ulink" target="_top">SortedSet</a>, not just a <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Set.html" class="ulink" target="_top">Set</a> as specified in Java collections interface. This allows using the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedSet.html" class="ulink" target="_top">SortedSet</a> methods on the returned collection.

- <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedMap.html#values()" class="ulink" target="_top">SortedMap.values()</a> returns a <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedSet.html" class="ulink" target="_top">SortedSet</a>, not just a <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Collection.html" class="ulink" target="_top">Collection</a>, whenever the keys of the map can be derived from the values using an entity binding. Note that the sorted set returned is not really a set if duplicates are allowed, since it is technically a collection; however, the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedSet.html" class="ulink" target="_top">SortedSet</a> methods (for example, subSet()), can still be used.

- For <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedSet.html" class="ulink" target="_top">SortedSet</a> and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedMap.html" class="ulink" target="_top">SortedMap</a> views, additional subSet() and subMap() methods are provided that allow control over whether keys are treated as inclusive or exclusive values in the key range.

- Keys and values are stored by value, not by reference. This is because objects that are added to collections are converted to byte arrays (by bindings) and stored in the database. When they are retrieved from the collection they are read from the database and converted from byte arrays to objects. Therefore, the object reference added to a collection will not be the same as the reference later retrieved from the collection.

- A runtime exception, <a href="../../java/com/sleepycat/util/RuntimeExceptionWrapper.html" class="ulink" target="_top">RuntimeExceptionWrapper</a>, is thrown whenever database exceptions occur which are not runtime exceptions. The <a href="../../java/com/sleepycat/util/RuntimeExceptionWrapper.html#getCause()" class="ulink" target="_top">RuntimeExceptionWrapper.getCause()</a> method can be called to get the underlying exception.

- All iterators for stored collections implement the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html" class="ulink" target="_top">ListIterator</a> interface as well as the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Iterator.html" class="ulink" target="_top">Iterator</a> interface. This is to allow use of the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#hasPrevious()" class="ulink" target="_top">ListIterator.hasPrevious()</a> and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#previous()" class="ulink" target="_top">ListIterator.previous()</a> methods, which work for all collections since Berkeley DB provides bidirectional cursors.

- All stored collections have a <a href="../../java/com/sleepycat/collections/StoredCollection.html#iterator(boolean)" class="ulink" target="_top">StoredCollection.iterator(boolean)</a> method that allows creating a read-only iterator for a writable collection. For the standard <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Collection.html#iterator()" class="ulink" target="_top">Collection.iterator()</a> method, the iterator is read-only only when the collection is read-only. Read-only iterators are important for using the Berkeley DB Concurrent Data Store environment, since only one write cursors may be open at one time.

- Iterator stability for stored collections is greater than the iterator stability defined by the Java collections interfaces. Stored iterator stability is the same as the cursor stability defined by Berkeley DB.

- When an entity binding is used, updating (setting) a value is not allowed if the key in the entity is not equal to the original key. For example, calling <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#put()" class="ulink" target="_top">Map.put()</a> is not allowed when the key parameter is not equal to the key of the entity parameter. <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#put()" class="ulink" target="_top">Map.put()</a>, <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/List.html#set()" class="ulink" target="_top">List.set()</a>, <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#set()" class="ulink" target="_top">ListIterator.set()</a>, and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.Entry.html#setValue()" class="ulink" target="_top">Map.Entry.setValue()</a> will throw <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/IllegalArgumentException.html" class="ulink" target="_top">IllegalArgumentException</a> in this situation.

- Adding and removing items from stored lists is not allowed for sublists. This is simply an unimplemented feature and may be changed in the future. Currently for sublists the following methods throw <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/UnsupportedOperationException.html" class="ulink" target="_top">UnsupportedOperationException</a>: <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/List.html#add()" class="ulink" target="_top">List.add()</a>`List.add()`, <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/List.html#remove()" class="ulink" target="_top">List.remove()</a>`List.remove()`, <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#add()" class="ulink" target="_top">ListIterator.add()</a> and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/ListIterator.html#remove()" class="ulink" target="_top">ListIterator.remove()</a>`ListIterator.remove()`.

- The <a href="../../java/com/sleepycat/collections/StoredList.html#append(java.lang.Object)" class="ulink" target="_top">StoredList.append(java.lang.Object)</a> and <a href="../../java/com/sleepycat/collections/StoredMap.html#append(java.lang.Object)" class="ulink" target="_top">StoredMap.append(java.lang.Object)</a> extension methods allows adding a new record with an automatically assigned key. Record number assignment by the database itself is supported for QUEUE, RECNO and RECNO-RENUMBER databases. An application-defined <a href="../../java/com/sleepycat/collections/PrimaryKeyAssigner.html" class="ulink" target="_top">PrimaryKeyAssigner</a> is used to assign the key value.

### Why Java Collections for Berkeley DB

The Java collections interface was chosen as the best Java API for DB given these requirements:

1.  Provide the Java developer with an API that is as familiar and easy to use as possible.

2.  Provide access to all, or a large majority, of the features of the underlying Berkeley DB storage system.

3.  Compared to the DB API, provide a higher-level API that is oriented toward Java developers.

4.  For ease of use, support object-to-data bindings, per-thread transactions, and some traditional database features such as foreign keys.

5.  Provide a thin layer that can be thoroughly tested and which does not significantly impact the reliability and performance of DB.

Admittedly there are several things about the Java Collections API that don't quite fit with DB or with any transactional database, and therefore there are some new rules for applying the Java Collections API. However, these disadvantages are considered to be smaller than the disadvantages of the alternatives:

- A new API not based on the Java Collections API could have been designed that maps well to DB but is higher-level. However, this would require designing an entirely new model. The exceptions for using the Java Collections API are considered easier to learn than a whole new model. A new model would also require a long design stabilization period before being as complete and understandable as either the Java Collections API or the DB API.

- The ODMG API or another object persistence API could have been implemented on top of DB. However, an object persistence implementation would add much code and require a long stabilization period. And while it may work well for applications that require object persistence, it would probably never perform well enough for many other applications.
