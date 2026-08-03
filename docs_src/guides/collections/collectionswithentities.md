---
title: "Creating Collections with Entity Bindings"
api-name: "Creating Collections with Entity Bindings"
source: docs/collections/tutorial/collectionswithentities.html
---
## Creating Collections with Entity Bindings

Stored map objects are created in this example in the same way as in prior examples, but using entity bindings in place of value bindings. All value objects passed and returned to the Java collections API are then actually entity objects (`Part`, `Supplier` and `Shipment`). The application no longer deals directly with plain value objects (`PartData`, `SupplierData` and `ShipmentData`).

Since the `partValueBinding`, `supplierValueBinding` and `shipmentValueBinding` were defined as entity bindings in the prior section, there are no source code changes necessary for creating the stored map objects.

``` c
public class SampleViews
{
    ...
    public SampleViews(SampleDatabase db)
    {
        ...
        partMap =
            new StoredMap(db.getPartDatabase(),
                          partKeyBinding, partValueBinding, true);
        supplierMap =
            new StoredMap(db.getSupplierDatabase(),
                          supplierKeyBinding, supplierValueBinding, true);
        shipmentMap =
            new StoredMap(db.getShipmentDatabase(),
                          shipmentKeyBinding, shipmentValueBinding, true);
      ...
    } 
```

Specifying an <a href="../../java/com/sleepycat/bind/EntityBinding.html" class="ulink" target="_top">EntityBinding</a> will select a different <a href="../../java/com/sleepycat/collections/StoredMap.html" class="ulink" target="_top">StoredMap</a> constructor, but the syntax is the same. In general, an entity binding may be used anywhere that a value binding is used.

The following getter methods are defined for use by other classes in the example program. Instead of returning the map's entry set (<a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#entrySet()" class="ulink" target="_top">Map.entrySet</a>), the map's value set (<a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#values()" class="ulink" target="_top">Map.values</a>) is returned. The entry set was convenient in prior examples because it allowed enumerating all key/value pairs in the collection. Since an entity contains the key and the value, enumerating the value set can now be used more conveniently for the same purpose.

``` c
import com.sleepycat.collections.StoredValueSet;
...
public class SampleViews
{
    ...
    public StoredValueSet getPartSet()
    {
        return (StoredValueSet) partMap.values();
    }

    public StoredValueSet getSupplierSet()
    {
        return (StoredValueSet) supplierMap.values();
    }

    public StoredValueSet getShipmentSet()
    {
        return (StoredValueSet) shipmentMap.values();
    }
    ...
} 
```

Notice that the collection returned by the <a href="../../java/com/sleepycat/collections/StoredMap.html#values()" class="ulink" target="_top">StoredMap.values</a> method is actually a <a href="../../java/com/sleepycat/collections/StoredValueSet.html" class="ulink" target="_top">StoredValueSet</a> and not just a <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Collection.html" class="ulink" target="_top">Collection</a> as defined by the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#values()" class="ulink" target="_top">Map.values</a> interface. As long as duplicate keys are not allowed, this collection will behave as a true set and will disallow the addition of duplicates, etc.
