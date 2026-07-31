---
title: "Retrieving Database Items"
api-name: "Retrieving Database Items"
source: docs/collections/tutorial/retrievingdatabaseitems.html
---
## Retrieving Database Items

Retrieving information from the database is accomplished via the standard Java collections API. In the example, the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Set.html#iterator()" class="ulink" target="_top">Set.iterator</a> method is used to iterate all <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.Entry.html" class="ulink" target="_top">Map.Entry</a> objects for each store. All standard Java methods for retrieving objects from a collection may be used with the DB Java Collections API.

The `PrintDatabase.doWork()` method calls `printEntries()` to print the map entries for each database store. It is called via the <a href="../../java/com/sleepycat/collections/TransactionRunner.html" class="ulink" target="_top">TransactionRunner</a> class and was outlined in the previous section.

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
            printEntries("Shipments",
                          views.getShipmentEntrySet().iterator());
        }
    }
    ...

    private void printEntries(String label, Iterator iterator)
    {
    }
    ...
} 
```

The <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Set.html" class="ulink" target="_top">Set</a> of <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.Entry.html" class="ulink" target="_top">Map.Entry</a> objects for each store is obtained from the `SampleViews` object. This set can also be obtained by calling the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#entrySet()" class="ulink" target="_top">Map.entrySet</a> method of a stored map.

The `printEntries()` prints the map entries for any stored map. The <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/Object.html#toString()" class="ulink" target="_top">Object.toString</a> method of each key and value is called to obtain a printable representation of each object.

``` c
    private void printEntries(String label, Iterator iterator)
    {
        System.out.println("\n--- " + label + " ---");
        while (iterator.hasNext())
        {
            Map.Entry entry = (Map.Entry) iterator.next();
            System.out.println(entry.getKey().toString());
            System.out.println(entry.getValue().toString());
        }
    } 
```

This is one of a small number of behavioral differences between standard Java collections and stored collections. For a complete list see <a href="UsingStoredCollections.md" class="xref" title="Using Stored Collections">Using Stored Collections</a> .

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
```
