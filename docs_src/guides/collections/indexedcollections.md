---
title: "Creating Indexed Collections"
api-name: "Creating Indexed Collections"
source: docs/collections/tutorial/indexedcollections.html
---
## Creating Indexed Collections

In the prior Basic example, bindings and Java collections were created for accessing databases via their primary keys. In this example, bindings and collections are added for accessing the same databases via their index keys. As in the prior example, serial bindings and the Java <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html" class="ulink" target="_top">Map</a> class are used.

When a map is created from a <a href="../../java/com/sleepycat/db/SecondaryDatabase.html" class="ulink" target="_top">SecondaryDatabase</a>, the keys of the map will be the index keys. However, the values of the map will be the values of the primary database associated with the index. This is how index keys can be used to access the values in a primary database.

For example, the Supplier's City field is an index key that can be used to access the Supplier database. When a map is created using the `supplierByCityDb()` method, the key to the map will be the City field, a <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/String.html" class="ulink" target="_top">String</a> object. When <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#get(java.lang.Object)" class="ulink" target="_top">Map.get</a> is called passing the City as the key parameter, a `SupplierData` object will be returned.

The `SampleViews` class is extended to create an index key binding for the Supplier's City field and three Java maps based on the three indices created in the prior section.

``` c
import com.sleepycat.bind.EntryBinding;
import com.sleepycat.bind.serial.SerialBinding;
import com.sleepycat.collections.StoredEntrySet;
import com.sleepycat.collections.StoredMap;
...

public class SampleViews
{
    ...
    private StoredMap supplierByCityMap;
    private StoredMap shipmentByPartMap;
    private StoredMap shipmentBySupplierMap;
    ...

    public SampleViews(SampleDatabase db)
    {
        ClassCatalog catalog = db.getClassCatalog();
        ...
        EntryBinding cityKeyBinding =
            new SerialBinding(catalog, String.class);
        ...
        supplierByCityMap =
            new StoredMap(db.getSupplierByCityDatabase(),
                          cityKeyBinding, supplierValueBinding, true);
        shipmentByPartMap =
            new StoredMap(db.getShipmentByPartDatabase(),
                          partKeyBinding, shipmentValueBinding, true);
        shipmentBySupplierMap =
            new StoredMap(db.getShipmentBySupplierDatabase(),
                          supplierKeyBinding, shipmentValueBinding, true); 
    ...
    }
} 
```

In general, the indexed maps are created here in the same way as the unindexed maps were created in the Basic example. The differences are:

- The first parameter of the <a href="../../java/com/sleepycat/collections/StoredMap.html" class="ulink" target="_top">StoredMap</a> constructor is a <a href="../../java/com/sleepycat/db/SecondaryDatabase.html" class="ulink" target="_top">SecondaryDatabase</a> rather than a <a href="../../java/com/sleepycat/db/Database.html" class="ulink" target="_top">Database</a>.

- The second parameter is the index key binding rather than the primary key binding.

For the `supplierByCityMap`, the `cityKeyBinding` must first be created. This binding was not created in the Basic example because the City field is not a primary key.

Like the bindings created earlier for keys and values, the `cityKeyBinding` is a <a href="../../java/com/sleepycat/bind/serial/SerialBinding.html" class="ulink" target="_top">SerialBinding</a>. Unlike the bindings created earlier, it is an example of creating a binding for a built-in Java class, <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/String.html" class="ulink" target="_top">String</a>, instead of an application-defined class. Any serializable class may be used.

For the `shipmentByPartMap` and `shipmentBySupplierMap`, the `partKeyBinding` and `supplierKeyBinding` are used. These were created in the Basic example and used as the primary key bindings for the `partMap` and `supplierMap`.

The value bindings — `supplierValueBinding` and `shipmentValueBinding` — were also created in the Basic example.

This illustrates that bindings and formats may and should be reused where appropriate for creating maps and other collections.

The following getter methods return the stored maps for use by other classes in the example program. Convenience methods for returning entry sets are also included.

``` c
public class SampleViews
{
    ...
    public final StoredMap getShipmentByPartMap()
    {
        return shipmentByPartMap;
    }

    public final StoredMap getShipmentBySupplierMap()
    {
        return shipmentBySupplierMap;
    }

    public final StoredMap getSupplierByCityMap()
    {
        return supplierByCityMap;
    }

    public final StoredEntrySet getShipmentByPartEntrySet()
    {
        return (StoredEntrySet) shipmentByPartMap.entrySet();
    }

    public final StoredEntrySet getShipmentBySupplierEntrySet()
    {
        return (StoredEntrySet) shipmentBySupplierMap.entrySet();
    }

    public final StoredEntrySet getSupplierByCityEntrySet()
    {
        return (StoredEntrySet) supplierByCityMap.entrySet();
    }
    ...
} 
```
