---
title: "Using Sorted Collections"
api-name: "Using Sorted Collections"
source: docs/collections/tutorial/sortedcollections.html
---
## Using Sorted Collections

In general, no changes to the prior example are necessary to use collections having tuple keys. Iteration of elements in a stored collection will be ordered by the sort order of the tuples.

In addition to using the tuple format, the <a href="../../java/com/sleepycat/db/DatabaseType.html#BTREE" class="ulink" target="_top">DatabaseType.BTREE</a> access method must be used when creating the database. <a href="../../java/com/sleepycat/db/DatabaseType.html#BTREE" class="ulink" target="_top">DatabaseType.BTREE</a> is used for the databases in all examples. The <a href="../../java/com/sleepycat/db/DatabaseType.html#HASH" class="ulink" target="_top">DatabaseType.HASH</a> access method does not support sorted keys.

Although not shown in the example, all methods of the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedMap.html" class="ulink" target="_top">SortedMap</a> and <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/SortedSet.html" class="ulink" target="_top">SortedSet</a> interfaces may be used with sorted collections. For example, submaps and subsets may be created.

The output of the example program shows that records are sorted by key value.

``` c
Adding Suppliers
Adding Parts
Adding Shipments

--- Parts ---
Part: number=P1 name=Nut color=Red weight=[12.0 grams] city=London
Part: number=P2 name=Bolt color=Green weight=[17.0 grams] city=Paris
Part: number=P3 name=Screw color=Blue weight=[17.0 grams] city=Rome
Part: number=P4 name=Screw color=Red weight=[14.0 grams] city=London
Part: number=P5 name=Cam color=Blue weight=[12.0 grams] city=Paris
Part: number=P6 name=Cog color=Red weight=[19.0 grams] city=London

--- Suppliers ---
Supplier: number=S1 name=Smith status=20 city=London
Supplier: number=S2 name=Jones status=10 city=Paris
Supplier: number=S3 name=Blake status=30 city=Paris
Supplier: number=S4 name=Clark status=20 city=London
Supplier: number=S5 name=Adams status=30 city=Athens

--- Suppliers for City Paris ---
Supplier: number=S2 name=Jones status=10 city=Paris
Supplier: number=S3 name=Blake status=30 city=Paris

--- Shipments ---
Shipment: part=P1 supplier=S1 quantity=300
Shipment: part=P1 supplier=S2 quantity=300
Shipment: part=P2 supplier=S1 quantity=200
Shipment: part=P2 supplier=S2 quantity=400
Shipment: part=P2 supplier=S3 quantity=200
Shipment: part=P2 supplier=S4 quantity=200
Shipment: part=P3 supplier=S1 quantity=400
Shipment: part=P4 supplier=S1 quantity=200
Shipment: part=P4 supplier=S4 quantity=300
Shipment: part=P5 supplier=S1 quantity=100
Shipment: part=P5 supplier=S4 quantity=400
Shipment: part=P6 supplier=S1 quantity=100

--- Shipments for Part P1 ---
Shipment: part=P1 supplier=S1 quantity=300
Shipment: part=P1 supplier=S2 quantity=300

--- Shipments for Supplier S1 ---
Shipment: part=P1 supplier=S1 quantity=300
Shipment: part=P2 supplier=S1 quantity=200
Shipment: part=P3 supplier=S1 quantity=400
Shipment: part=P4 supplier=S1 quantity=200
Shipment: part=P5 supplier=S1 quantity=100
Shipment: part=P6 supplier=S1 quantity=100 
```
