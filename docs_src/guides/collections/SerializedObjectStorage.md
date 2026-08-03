---
title: "Serialized Object Storage"
api-name: "Serialized Object Storage"
source: docs/collections/tutorial/SerializedObjectStorage.html
---
## Serialized Object Storage

Serialization of an object graph includes class information as well as instance information. If more than one instance of the same class is serialized as separate serialization operations then the class information exists more than once. To eliminate this inefficiency the <a href="../../java/com/sleepycat/bind/serial/StoredClassCatalog.html" class="ulink" target="_top">StoredClassCatalog</a> class will store the class format for all database records stored using a <a href="../../java/com/sleepycat/bind/serial/SerialBinding.html" class="ulink" target="_top">SerialBinding</a>. Refer to the `ship` sample code for examples (the class `SampleDatabase` in `examples_java/src/com/sleepycat/examples/collections/ship/basic/SampleDatabase.java` is a good place to start).
