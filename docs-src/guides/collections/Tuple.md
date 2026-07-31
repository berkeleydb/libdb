---
title: "Chapter 5.  Using Tuples"
api-name: "Chapter 5.  Using Tuples"
source: docs/collections/tutorial/Tuple.html
---
## Chapter 5.  Using Tuples

**Table of Contents**

<span class="sect1"> [Using the Tuple Format](Tuple.md#tupleformat) </span>

<span class="sect1"> [Using Tuples with Key Creators](tupleswithkeycreators.md) </span>

<span class="sect1"> [Creating Tuple Key Bindings](tuplekeybindings.md) </span>

<span class="sect1"> [Creating Tuple-Serial Entity Bindings](tuple-serialentitybindings.md) </span>

<span class="sect1"> [Using Sorted Collections](sortedcollections.md) </span>

DB Java Collections API <span class="emphasis">*tuples*</span> are sequences of primitive Java data types, for example, integers and strings. The <span class="emphasis">*tuple format*</span> is a binary format for tuples that can be used to store keys and/or values.

Tuples are useful as keys because they have a meaningful sort order, while serialized objects do not. This is because the binary data for a tuple is written in such a way that its raw byte ordering provides a useful sort order. For example, strings in tuples are written with a null terminator rather than with a leading length.

Tuples are useful as keys <span class="emphasis">*or*</span> values when reducing the record size to a minimum is important. A tuple is significantly smaller than an equivalent serialized object. However, unlike serialized objects, tuples cannot contain complex data types and are not easily extended except by adding fields at the end of the tuple.

Whenever a tuple format is used, except when the key or value class is a Java primitive wrapper class, a <span class="emphasis">*tuple binding*</span> class must be implemented to map between the Java object and the tuple fields. Because of this extra requirement, and because tuples are not easily extended, a useful technique shown in this example is to use tuples for keys and serialized objects for values. This provides compact ordered keys but still allows arbitrary Java objects as values, and avoids implementing a tuple binding for each value class.

Compare this example to the prior Entity example and you'll see that the `Sample` class has not changed. When changing a database format, while new bindings are needed to map key and value objects to the new format, the application using the objects often does not need to be modified.

The complete source of the final version of the example program is included in the Berkeley DB distribution.

## Using the Tuple Format

Tuples are sequences of primitive Java values that can be written to, and read from, the raw data bytes of a stored record. The primitive values are written or read one at a time in sequence, using the DB Java Collections API <a href="../../java/com/sleepycat/bind/tuple/TupleInput.html" class="ulink" target="_top">TupleInput</a> and <a href="../../java/com/sleepycat/bind/tuple/TupleOutput.html" class="ulink" target="_top">TupleOutput</a> classes. These classes are very similar to the standard Java <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/io/DataInput.html" class="ulink" target="_top">DataInput</a> and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/io/DataOutput.html" class="ulink" target="_top">DataOutput</a> interfaces. The primary difference is the binary format of the data, which is designed for sorting in the case of tuples.

For example, to read and write a tuple containing two string values, the following code snippets could be used.

``` c
import com.sleepycat.bind.tuple.TupleInput;
import com.sleepycat.bind.tuple.TupleOutput;
...
TupleInput input;
TupleOutput output;
...
String partNumber = input.readString();
String supplierNumber = input.readString();
...
output.writeString(partNumber);
output.writeString(supplierNumber);  
```

Since a tuple is defined as an ordered sequence, reading and writing order must match. If the wrong data type is read (an integer instead of string, for example), an exception may be thrown or at minimum invalid data will be read.

When the tuple format is used, bindings and key creators must read and write tuples using the tuple API as shown above. This will be illustrated in the next two sections.
