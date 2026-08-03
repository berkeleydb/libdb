---
title: "Retrieving Items by Index Key"
api-name: "Retrieving Items by Index Key"
source: docs/collections/tutorial/retrievingbyindexkey.html
---
## Retrieving Items by Index Key

Retrieving information via database index keys can be accomplished using the standard Java collections API, using a collection created from a <a href="../../java/com/sleepycat/db/SecondaryDatabase.html" class="ulink" target="_top">SecondaryDatabase</a> rather than a <a href="../../java/com/sleepycat/db/Database.html" class="ulink" target="_top">Database</a>. However, the standard Java API does not support <span class="emphasis">*duplicate keys*</span>: more than one element in a collection having the same key. All three indices created in the prior section have duplicate keys because of the nature of the city, part number and supplier number index keys. More than one supplier may be in the same city, and more than one shipment may have the same part number or supplier number. This section describes how to use extended methods for stored collections to return all values for a given key.

Using the standard Java collections API, the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#get(java.lang.Object)" class="ulink" target="_top">Map.get</a> method for a stored collection with duplicate keys will return only the first value for a given key. To obtain all values for a given key, the <a href="../../java/com/sleepycat/collections/StoredMap.html#duplicates(java.lang.Object)" class="ulink" target="_top">StoredMap.duplicates</a> method may be called. This returns a <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Collection.html" class="ulink" target="_top">Collection</a> of values for the given key. If duplicate keys are not allowed, the returned collection will have at most one value. If the key is not present in the map, an empty collection is returned.

The `Sample` class is extended to retrieve duplicates for specific index keys that are present in the database.

``` c
import java.util.Iterator;
...
public class Sample
{
    ...
    private SampleViews views;
    ...
    private class PrintDatabase implements TransactionWorker
    {
        public void doWork()
            throws Exception
        {
            printEntries("Parts",
                          views.getPartEntrySet().iterator());
            printEntries("Suppliers",
                          views.getSupplierEntrySet().iterator());
            printValues("Suppliers for City Paris",
                         views.getSupplierByCityMap().duplicates(
                                            "Paris").iterator());
            printEntries("Shipments",
                          views.getShipmentEntrySet().iterator());
            printValues("Shipments for Part P1",
                         views.getShipmentByPartMap().duplicates(
                                            new PartKey("P1")).iterator());
            printValues("Shipments for Supplier S1",
                         views.getShipmentBySupplierMap().duplicates(
                                            new
                                            SupplierKey("S1")).iterator());
        }
    }

    private void printValues(String label, Iterator iterator)
    {
        System.out.println("\n--- " + label + " ---");
        while (iterator.hasNext())
        {
                System.out.println(iterator.next().toString());
        }
     } 
    ...
} 
```

The <a href="../../java/com/sleepycat/collections/StoredMap.html#duplicates(java.lang.Object)" class="ulink" target="_top">StoredMap.duplicates</a> method is called passing the desired key. The returned value is a standard Java <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Collection.html" class="ulink" target="_top">Collection</a> containing the values for the specified key. A standard Java <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Iterator.html" class="ulink" target="_top">Iterator</a> is then obtained for this collection and all values returned by that iterator are printed.

Another technique for retrieving duplicates is to use the collection returned by <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#entrySet()" class="ulink" target="_top">Map.entrySet</a>. When duplicate keys are present, a <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.Entry.html" class="ulink" target="_top">Map.Entry</a> object will be present in this collection for each duplicate. This collection can then be iterated or a subset can be created from it, all using the standard Java collection API.

Note that we did not discuss how duplicates keys can be explicitly added or removed in a collection. For index keys, the addition and deletion of duplicate keys happens automatically when records containing the index key are added, updated, or removed.

While not shown in the example program, it is also possible to create a store with duplicate keys in the same way as an index with duplicate keys — by calling `DatabaseConfig.setSortedDuplicates()` method. In that case, calling <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#put(K,%20V)" class="ulink" target="_top">Map.put</a> will add duplicate keys. To remove all duplicate keys, call <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#remove(java.lang.Object)" class="ulink" target="_top">Map.remove</a>. To remove a specific duplicate key, call <a href="../../java/com/sleepycat/collections/StoredMap.html#duplicates(java.lang.Object)" class="ulink" target="_top">StoredMap.duplicates</a> and then call <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Collection.html#remove(java.lang.Object)" class="ulink" target="_top">Collection.remove</a> using the returned collection. Duplicate values may also be added to this collection using <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Collection.html#add(E)" class="ulink" target="_top">Collection.add</a>.

The output of the example program is shown below.

``` c
Adding Suppliers
Adding Parts
Adding Shipments

--- Parts ---
PartKey: number=P1
PartData: name=Nut color=Red weight=[12.0 grams] city=London
PartKey: number=P2
PartData: name=Bolt color=Green weight=[17.0 grams] city=Paris
PartKey: number=P3
PartData: name=Screw color=Blue weight=[17.0 grams] city=Rome
PartKey: number=P4
PartData: name=Screw color=Red weight=[14.0 grams] city=London
PartKey: number=P5
PartData: name=Cam color=Blue weight=[12.0 grams] city=Paris
PartKey: number=P6
PartData: name=Cog color=Red weight=[19.0 grams] city=London

--- Suppliers ---
SupplierKey: number=S1
SupplierData: name=Smith status=20 city=London
SupplierKey: number=S2
SupplierData: name=Jones status=10 city=Paris
SupplierKey: number=S3
SupplierData: name=Blake status=30 city=Paris
SupplierKey: number=S4
SupplierData: name=Clark status=20 city=London
SupplierKey: number=S5
SupplierData: name=Adams status=30 city=Athens

--- Suppliers for City Paris ---
SupplierData: name=Jones status=10 city=Paris
SupplierData: name=Blake status=30 city=Paris

--- Shipments ---
ShipmentKey: supplier=S1 part=P1
ShipmentData: quantity=300
ShipmentKey: supplier=S2 part=P1
ShipmentData: quantity=300
ShipmentKey: supplier=S1 part=P2
ShipmentData: quantity=200
ShipmentKey: supplier=S2 part=P2
ShipmentData: quantity=400
ShipmentKey: supplier=S3 part=P2
ShipmentData: quantity=200
ShipmentKey: supplier=S4 part=P2
ShipmentData: quantity=200
ShipmentKey: supplier=S1 part=P3
ShipmentData: quantity=400
ShipmentKey: supplier=S1 part=P4
ShipmentData: quantity=200
ShipmentKey: supplier=S4 part=P4
ShipmentData: quantity=300
ShipmentKey: supplier=S1 part=P5
ShipmentData: quantity=100
ShipmentKey: supplier=S4 part=P5
ShipmentData: quantity=400
ShipmentKey: supplier=S1 part=P6
ShipmentData: quantity=100 

--- Shipments for Part P1 ---
ShipmentData: quantity=300
ShipmentData: quantity=300

--- Shipments for Supplier S1 ---
ShipmentData: quantity=300
ShipmentData: quantity=200
ShipmentData: quantity=400
ShipmentData: quantity=200
ShipmentData: quantity=100
ShipmentData: quantity=100 
```
