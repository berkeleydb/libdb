---
title: "Chapter 4. Working with Indices"
api-name: "Chapter 4. Working with Indices"
source: docs/gsg/JAVA/persist_index.html
---
## Chapter 4. Working with Indices

**Table of Contents**

<span class="sect1"> [Accessing Indexes](persist_index.md#dplindexaccess) </span>

<span class="sect2"> [Accessing Primary Indices](persist_index.md#primaryindexaccess) </span>

<span class="sect2"> [Accessing Secondary Indices](persist_index.md#secondaryindexaccess) </span>

<span class="sect1"> [Creating Indexes](dplindexcreate.md) </span>

<span class="sect2"> [Declaring a Primary Indexes](dplindexcreate.md#dplprimaryidxdecl) </span>

<span class="sect2"> [Declaring Secondary Indexes](dplindexcreate.md#dplsecondaryidxdecl) </span>

<span class="sect2"> [Foreign Key Constraints](dplindexcreate.md#foreignkey) </span>

All entity classes stored in DB using the DPL must have a primary index, or key, identified for them. All such classes may also have one or more secondary keys declared for them. This chapter describes primary and secondary indexes in detail, and shows how to access the indexes created for a given entity class.

One way to organize access to your primary and secondary indexes is to create a <span class="emphasis">*data accessor*</span> class. We show an implementation of a data accessor class in <a href="simpleda.md" class="xref" title="SimpleDA.class">SimpleDA.class</a>.

## Accessing Indexes

<span class="sect2"> [Accessing Primary Indices](persist_index.md#primaryindexaccess) </span>

<span class="sect2"> [Accessing Secondary Indices](persist_index.md#secondaryindexaccess) </span>

In order to retrieve any object from an entity store, you must access at least the primary index for that object. Different entity classes stored in an entity store can have different primary indexes, but all entity classes must have a primary index declared for it. The primary index is just the default index used for the class. (That is, it is the data's primary <span class="emphasis">*key*</span> for the underlying database.)

Entity classes can optionally have secondary indexes declared for them. In order to access these secondary indexes, you must first access the primary index.

### Accessing Primary Indices

You retrieve a primary index using the `EntityStore.getPrimaryIndex()` method. To do this, you indicate the index key type (that is, whether it is a String, Integer, and so forth) and the class of the entities stored in the index.

For example, the following retrieves the primary index for an `Inventory` class (we provide an implementation of this class in <a href="inventoryclass.md" class="xref" title="Inventory.java">Inventory.java</a>). These index keys are of type `String`.

``` c
PrimaryIndex<String,Inventory> inventoryBySku = 
    store.getPrimaryIndex(String.class, Inventory.class); 
```

### Accessing Secondary Indices

You retrieve a secondary index using the `EntityStore.getSecondaryIndex()` method. Because secondary indices actually refer to a primary index somewhere in your data store, to access a secondary index you:

1.  Provide the primary index as returned by `EntityStore.getPrimaryIndex()`.

2.  Identify the key data type used by the secondary index (`String`, `Long`, and so forth).

3.  Identify the name of the secondary key field. When you declare the `SecondaryIndex` object, you identify the entity class to which the secondary index must refer.

For example, the following first retrieves the primary index, and then uses that to retrieve a secondary index. The secondary key is held by the `itemName` field of the `Inventory` class.

``` c
PrimaryIndex<String,Inventory> inventoryBySku = 
store.getPrimaryIndex(String.class, Inventory.class); 

SecondaryIndex<String,String,Inventory> inventoryByName = 
    store.getSecondaryIndex(inventoryBySku, String.class, "itemName"); 
```
