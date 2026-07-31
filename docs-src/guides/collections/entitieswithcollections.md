---
title: "Using Entities with Collections"
api-name: "Using Entities with Collections"
source: docs/collections/tutorial/entitieswithcollections.html
---
## Using Entities with Collections

In this example entity objects, rather than key and value objects, are used for adding and enumerating the records in a collection. Because fewer classes and objects are involved, adding and enumerating is done more conveniently and more simply than in the prior examples.

For adding and iterating entities, the collection of entities returned by <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#values()" class="ulink" target="_top">Map.values</a> is used. In general, when using an entity binding, all Java collection methods that are passed or returned a value object will be passed or returned an entity object instead.

The `Sample` class has been changed in this example to add objects using the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Set.html#add(E)" class="ulink" target="_top">Set.add</a> method rather than the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#put(K,%20V)" class="ulink" target="_top">Map.put</a> method that was used in the prior examples. Entity objects are constructed and passed to <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Set.html#add(E)" class="ulink" target="_top">Set.add</a>.

``` c
import java.util.Set;
...
public class Sample
{
    ...
    private void addSuppliers()
    {
        Set suppliers = views.getSupplierSet();
        if (suppliers.isEmpty())
        {
            System.out.println("Adding Suppliers");
            suppliers.add(new Supplier("S1", "Smith", 20, "London"));
            suppliers.add(new Supplier("S2", "Jones", 10, "Paris"));
            suppliers.add(new Supplier("S3", "Blake", 30, "Paris"));
            suppliers.add(new Supplier("S4", "Clark", 20, "London"));
            suppliers.add(new Supplier("S5", "Adams", 30, "Athens"));
        }
    }

    private void addParts()
    {
        Set parts = views.getPartSet();
        if (parts.isEmpty())
        {
            System.out.println("Adding Parts");
            parts.add(new Part("P1", "Nut", "Red",
                      new Weight(12.0, Weight.GRAMS), "London"));
            parts.add(new Part("P2", "Bolt", "Green",
                      new Weight(17.0, Weight.GRAMS), "Paris"));
            parts.add(new Part("P3", "Screw", "Blue",
                      new Weight(17.0, Weight.GRAMS), "Rome"));
            parts.add(new Part("P4", "Screw", "Red",
                      new Weight(14.0, Weight.GRAMS), "London"));
            parts.add(new Part("P5", "Cam", "Blue",
                      new Weight(12.0, Weight.GRAMS), "Paris"));
            parts.add(new Part("P6", "Cog", "Red",
                      new Weight(19.0, Weight.GRAMS), "London"));
        }
    }

    private void addShipments()
    {
        Set shipments = views.getShipmentSet();
        if (shipments.isEmpty())
        {
            System.out.println("Adding Shipments");
            shipments.add(new Shipment("P1", "S1", 300));
            shipments.add(new Shipment("P2", "S1", 200));
            shipments.add(new Shipment("P3", "S1", 400));
            shipments.add(new Shipment("P4", "S1", 200));
            shipments.add(new Shipment("P5", "S1", 100));
            shipments.add(new Shipment("P6", "S1", 100));
            shipments.add(new Shipment("P1", "S2", 300));
            shipments.add(new Shipment("P2", "S2", 400));
            shipments.add(new Shipment("P2", "S3", 200));
            shipments.add(new Shipment("P2", "S4", 200));
            shipments.add(new Shipment("P4", "S4", 300));
            shipments.add(new Shipment("P5", "S4", 400));
        }
    } 
```

Instead of printing the key/value pairs by iterating over the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#entrySet()" class="ulink" target="_top">Map.entrySet</a> as done in the prior example, this example iterates over the entities in the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#values()" class="ulink" target="_top">Map.values</a> collection.

``` c
import java.util.Iterator;
import java.util.Set;
...
public class Sample
{
    ...
    private class PrintDatabase implements TransactionWorker
    {
        public void doWork()
            throws Exception
        {
            printValues("Parts",
                         views.getPartSet().iterator());
            printValues("Suppliers",
                         views.getSupplierSet().iterator());
            printValues("Suppliers for City Paris",
                         views.getSupplierByCityMap().duplicates(
                                            "Paris").iterator());
            printValues("Shipments",
                         views.getShipmentSet().iterator());
            printValues("Shipments for Part P1",
                         views.getShipmentByPartMap().duplicates(
                                            new PartKey("P1")).iterator());
            printValues("Shipments for Supplier S1",
                         views.getShipmentBySupplierMap().duplicates(
                                      new SupplierKey("S1")).iterator());
        }
    }
    ...
} 
```

The output of the example program is shown below.

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
