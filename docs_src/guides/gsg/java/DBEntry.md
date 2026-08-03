---
title: "Chapter 8. Database Records"
api-name: "Chapter 8. Database Records"
source: docs/gsg/JAVA/DBEntry.html
---
## Chapter 8. Database Records

**Table of Contents**

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

DB records contain two parts — a key and some data. Both the key and its corresponding data are encapsulated in `DatabaseEntry` class objects. Therefore, to access a DB record, you need two such objects, one for the key and one for the data.

`DatabaseEntry` can hold any kind of data from simple Java primitive types to complex Java objects so long as that data can be represented as a Java `byte` array. Note that due to performance considerations, you should not use Java serialization to convert a Java object to a `byte` array. Instead, use the Bind APIs to perform this conversion (see <a href="bindAPI.md" class="xref" title="Using the BIND APIs">Using the BIND APIs</a> for more information).

This chapter describes how you can convert both Java primitives and Java class objects into and out of `byte` arrays. It also introduces storing and retrieving key/value pairs from a database. In addition, this chapter describes how you can use comparators to influence how DB sorts its database records.

## Using Database Records

Each database record is comprised of two `DatabaseEntry` objects — one for the key and another for the data. The key and data information are passed to- and returned from DB using `DatabaseEntry` objects as `byte` arrays. Using `DatabaseEntry`s allows DB to change the underlying byte array as well as return multiple values (that is, key and data). Therefore, using `DatabaseEntry` instances is mostly an exercise in efficiently moving your keys and your data in and out of `byte` arrays.

For example, to store a database record where both the key and the data are Java `String` objects, you instantiate a pair of `DatabaseEntry` objects:

``` c
package db.GettingStarted;

import com.sleepycat.db.DatabaseEntry;

...

String aKey = "key";
String aData = "data";

try {
    DatabaseEntry theKey = new DatabaseEntry(aKey.getBytes("UTF-8"));
    DatabaseEntry theData = new DatabaseEntry(aData.getBytes("UTF-8"));
} catch (Exception e) {
    // Exception handling goes here
}

    // Storing the record is described later in this chapter 
```

### Note

Notice that we specify `UTF-8` when we retrieve the `byte` array from our `String` object. Without parameters, `String.getBytes()` uses the Java system's default encoding. You should never use a system's default encoding when storing data in a database because the encoding can change.

When the record is retrieved from the database, the method that you use to perform this operation populates two `DatabaseEntry` instances for you, one for the key and another for the data. Assuming Java `String` objects, you retrieve your data from the `DatabaseEntry` as follows:

``` c
package db.GettingStarted;

import com.sleepycat.db.DatabaseEntry;

...

// theKey and theData are DatabaseEntry objects. Database
// retrieval is described later in this chapter. For now, 
// we assume some database get method has populated these
// objects for us.

// Use DatabaseEntry.getData() to retrieve the encapsulated Java
// byte array.

byte[] myKey = theKey.getData();
byte[] myData = theData.getData();

String key = new String(myKey, "UTF-8");
String data = new String(myData, "UTF-8"); 
```

There are a large number of mechanisms that you can use to move data in and out of `byte` arrays. To help you with this activity, DB provides the bind APIs. These APIs allow you to efficiently store both primitive data types and complex objects in `byte` arrays.

The next section describes basic database put and get operations. A basic understanding of database access is useful when describing database storage of more complex data such as is supported by the bind APIs. Basic bind API usage is then described in <a href="bindAPI.md" class="xref" title="Using the BIND APIs">Using the BIND APIs</a>.
