---
title: "Adding Database Items"
api-name: "Adding Database Items"
source: docs/collections/tutorial/addingdatabaseitems.html
---
## Adding Database Items

Adding (as well as updating, removing, and deleting) information in the database is accomplished via the standard Java collections API. In the example, the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#put(K,%20V)" class="ulink" target="_top">Map.put</a> method is used to add objects. All standard Java methods for modifying a collection may be used with the DB Java Collections API.

The `PopulateDatabase.doWork()` method calls private methods for adding objects to each of the three database stores. It is called via the <a href="../../java/com/sleepycat/collections/TransactionRunner.html" class="ulink" target="_top">TransactionRunner</a> class and was outlined in the previous section.

``` c
import java.util.Map;
import com.sleepycat.collections.TransactionWorker;
...
public class Sample
{
    ...
    private SampleViews views;
    ...
    private class PopulateDatabase implements TransactionWorker
    {
        public void doWork()
            throws Exception
        {
            addSuppliers();
            addParts();
            addShipments();
        }
    }
    ...

    private void addSuppliers()
    {
    }

    private void addParts()
    {
    }

    private void addShipments()
    {
    }
} 
```

The `addSuppliers()`, `addParts()` and `addShipments()` methods add objects to the Suppliers, Parts and Shipments stores. The <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html" class="ulink" target="_top">Map</a> for each store is obtained from the `SampleViews` object.

``` c
    private void addSuppliers()
    {
        Map suppliers = views.getSupplierMap();
        if (suppliers.isEmpty())
        {
            System.out.println("Adding Suppliers");
            suppliers.put(new SupplierKey("S1"),
                          new SupplierData("Smith", 20, "London"));
            suppliers.put(new SupplierKey("S2"),
                          new SupplierData("Jones", 10, "Paris"));
            suppliers.put(new SupplierKey("S3"),
                          new SupplierData("Blake", 30, "Paris"));
            suppliers.put(new SupplierKey("S4"),
                          new SupplierData("Clark", 20, "London"));
            suppliers.put(new SupplierKey("S5"),
                          new SupplierData("Adams", 30, "Athens"));
        }
    }

    private void addParts()
    {
        Map parts = views.getPartMap();
        if (parts.isEmpty())
        {
            System.out.println("Adding Parts");
            parts.put(new PartKey("P1"),
                      new PartData("Nut", "Red",
                                    new Weight(12.0, Weight.GRAMS),
                                    "London"));
            parts.put(new PartKey("P2"),
                      new PartData("Bolt", "Green",
                                    new Weight(17.0, Weight.GRAMS),
                                    "Paris"));
            parts.put(new PartKey("P3"),
                      new PartData("Screw", "Blue",
                                    new Weight(17.0, Weight.GRAMS),
                                    "Rome"));
            parts.put(new PartKey("P4"),
                      new PartData("Screw", "Red",
                                    new Weight(14.0, Weight.GRAMS),
                                    "London"));
            parts.put(new PartKey("P5"),
                      new PartData("Cam", "Blue",
                                    new Weight(12.0, Weight.GRAMS),
                                    "Paris"));
            parts.put(new PartKey("P6"),
                      new PartData("Cog", "Red",
                                    new Weight(19.0, Weight.GRAMS),
                                    "London"));
        }
    }

    private void addShipments()
    {
        Map shipments = views.getShipmentMap();
        if (shipments.isEmpty())
        {
            System.out.println("Adding Shipments");
            shipments.put(new ShipmentKey("P1", "S1"),
                          new ShipmentData(300));
            shipments.put(new ShipmentKey("P2", "S1"),
                          new ShipmentData(200));
            shipments.put(new ShipmentKey("P3", "S1"),
                          new ShipmentData(400));
            shipments.put(new ShipmentKey("P4", "S1"),
                          new ShipmentData(200));
            shipments.put(new ShipmentKey("P5", "S1"),
                          new ShipmentData(100));
            shipments.put(new ShipmentKey("P6", "S1"),
                          new ShipmentData(100));
            shipments.put(new ShipmentKey("P1", "S2"),
                          new ShipmentData(300));
            shipments.put(new ShipmentKey("P2", "S2"),
                          new ShipmentData(400));
            shipments.put(new ShipmentKey("P2", "S3"),
                          new ShipmentData(200));
            shipments.put(new ShipmentKey("P2", "S4"),
                          new ShipmentData(200));
            shipments.put(new ShipmentKey("P4", "S4"),
                          new ShipmentData(300));
            shipments.put(new ShipmentKey("P5", "S4"),
                          new ShipmentData(400));
        }
    } 
}
```

The key and value classes used above were defined in the <a href="BasicProgram.md#keyandvalueclasses" class="xref" title="Defining Serialized Key and Value Classes">Defining Serialized Key and Value Classes</a> .

In each method above, objects are added only if the map is not empty. This is a simple way of allowing the example program to be run repeatedly. In real-life applications another technique — checking the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#containsKey(java.lang.Object)" class="ulink" target="_top">Map.containsKey</a> method, for example — might be used.
