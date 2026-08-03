---
title: "Retrieving Multiple Objects"
api-name: "Retrieving Multiple Objects"
source: docs/gsg/JAVA/getmultiple.html
---
## Retrieving Multiple Objects

<span class="sect2"> [Cursor Initialization](getmultiple.md#dpl_cursor_initialize) </span>

<span class="sect2"> [Working with Duplicate Keys](getmultiple.md#dpl_dups) </span>

<span class="sect2"> [Key Ranges](getmultiple.md#dpl_cursor_range) </span>

It is possible to iterate over every object referenced by a specific index. You may want to do this if, for example, you want to examine or modify every object accessible by a specific primary index.

In addition, some indexes result in the retrieval of multiple objects. For example, `MANY_TO_ONE` secondary indexes can result in more than one object for any given key (also known as <span class="emphasis">*duplicate keys*</span>). When this is the case, you must iterate over the resulting set of objects in order to examine each object in turn.

There are two ways to iterate over a collection of objects as returned by an index. One is to use a standard Java `Iterator`, which you obtain using an `EntityCursor`, which in turn you can obtain from a `PrimaryIndex`:

``` c
PrimaryIndex<String,SimpleEntityClass> pi =
    store.getPrimaryIndex(String.class, SimpleEntityClass.class);
EntityCursor<SimpleEntityClass> pi_cursor = pi.entities();
try {
    Iterator<SimpleEntityClass> i = pi_cursor.iterator();
    while (i.hasNext()) {
        // Do something here
    }
} finally {
    // Always close the cursor
    pi_cursor.close();
} 
```

Alternatively, you can use a Java "foreach" statement to iterate over object set:

``` c
PrimaryIndex<String,SimpleEntityClass> pi =
    store.getPrimaryIndex(String.class, SimpleEntityClass.class);
EntityCursor<SimpleEntityClass> pi_cursor = pi.entities();
try {
    for (SimpleEntityClass seci : pi_cursor) {
        // do something with each object "seci"
    }
// Always make sure the cursor is closed when we are done with it.
} finally {
    pi_cursor.close();
} 
```

### Cursor Initialization

When a cursor is first opened, it is not positioned to any value; that is, it is not <span class="emphasis">*initialized*</span>. Most of the `EntityCursor` methods that move a cursor will initialize it to either the first or last object, depending on whether the operation is moving the cursor forward (all `next...` methods) or backwards (all `prev...`) methods.

You can also force a cursor, whether it is initialized or not, to return the first object by calling `EntityCursor.first()`. Similarly, you can force a return of the last object using `EntityCursor.last()`.

Operations that do not move the cursor (such as `EntityCursor.current()` or `EntityCursor.delete()` will throw an `IllegalStateException` when used on an uninitialized cursor.

### Working with Duplicate Keys

If you have duplicate secondary keys, you can return an `EntityIndex` class object for them using `SecondaryIndex.subIndex()` Then, use that object's `entities()` method to obtain an `EntityCursor` instance.

For example:

``` c
PrimaryIndex<String,SimpleEntityClass> pi =
    store.getPrimaryIndex(String.class, SimpleEntityClass.class);

SecondaryIndex<String,String,SimpleEntityClass> si = 
    store.getSecondaryIndex(pi, String.class, "sKey");

EntityCursor<SimpleEntityClass> sec_cursor = 
    si.subIndex("skeyone").entities(); 

try {
for (SimpleEntityClass seci : sec_cursor) {
        // do something with each object "seci"
    }
// Always make sure the cursor is closed when we are done with it.
} finally {
    sec_cursor.close(); } 
```

Note that if you are working with duplicate keys, you can control how cursor iteration works by using the following `EntityCursor` methods:

- `nextDup()`

  Moves the cursor to the next object with the same key as the cursor is currently referencing. That is, this method returns the next duplicate object. If no such object exists, this method returns `null`.

- `prevDup()`

  Moves the cursor to the previous object with the same key as the cursor is currently referencing. That is, this method returns the previous duplicate object in the cursor's set of objects. If no such object exists, this method returns `null`.

- `nextNoDup()`

  Moves the cursor to the next object in the cursor's set that has a key which is different than the key that the cursor is currently referencing. That is, this method skips all duplicate objects and returns the next non-duplicate object in the cursor's set of objects. If no such object exists, this method returns `null`.

- `prevNoDup()`

  Moves the cursor to the previous object in the cursor's set that has a key which is different than the key that the cursor is currently referencing. That is, this method skips all duplicate objects and returns the previous non-duplicate object in the cursor's set of objects. If no such object exists, this method returns `null`.

For example:

``` c
PrimaryIndex<String,SimpleEntityClass> pi =
    store.getPrimaryIndex(String.class, SimpleEntityClass.class);

SecondaryIndex<String,String,SimpleEntityClass> si = 
    store.getSecondaryIndex(pi, String.class, "sKey");

EntityCursor<SimpleEntityClass> sec_cursor = 
    si.subIndex("skeyone").entities(); 

try {
    SimpleEntityClass sec;
    Iterator<SimpleEntityClass> i = sec_cursor.iterator();
    while (sec = i.nextNoDup() != null) {
        // Do something here
    }
// Always make sure the cursor is closed when we are done with it.
} finally {
    sec_cursor.close(); } 
```

### Key Ranges

You can restrict the scope of a cursor's movement by specifying a <span class="emphasis">*range*</span> when you create the cursor. The cursor can then never be positioned outside of the specified range.

When specifying a range, you indicate whether a range bound is <span class="emphasis">*inclusive*</span> or <span class="emphasis">*exclusive*</span> by providing a boolean value for each range. `true` indicates that the provided bound is inclusive, while `false` indicates that it is exclusive.

You provide this information when you call `PrimaryIndex.entities()` or `SecondaryIndex.entities()`. For example, suppose you had a class indexed by numerical information. Suppose further that you wanted to examine only those objects with indexed values of 100 - 199. Then (assuming the numerical information is the primary index), you can bound your cursor as follows:

``` c
EntityCursor<SomeEntityClass> cursor = 
    primaryIndex.entities(100, true, 200, false);

try {
    for (SomeEntityClass sec : cursor {
        // Do something here to objects ranged from 100 to 199
    }
// Always make sure the cursor is closed when we are done with it.
} finally {
    cursor.close(); } 
```
