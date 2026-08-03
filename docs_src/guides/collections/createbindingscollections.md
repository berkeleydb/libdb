---
title: "Creating Bindings and Collections"
api-name: "Creating Bindings and Collections"
source: docs/collections/tutorial/createbindingscollections.html
---
## Creating Bindings and Collections

<span class="emphasis">*Bindings*</span> translate between stored records and Java objects. In this example, Java serialization bindings are used. Serial bindings are the simplest type of bindings because no mapping of fields or type conversion is needed. Tuple bindings — which are more difficult to create than serial bindings but have some advantages — will be introduced later in the Tuple example program.

Standard Java <span class="emphasis">*collections*</span> are used to access records in a database. Stored collections use bindings transparently to convert the records to objects when they are retrieved from the collection, and to convert the objects to records when they are stored in the collection.

An important characteristic of stored collections is that they do <span class="emphasis">*not*</span> perform object caching. Every time an object is accessed via a collection it will be added to or retrieved from the database, and the bindings will be invoked to convert the data. Objects are therefore always passed and returned by value, not by reference. Because Berkeley DB is an embedded database, efficient caching of stored raw record data is performed by the database library.

The `SampleViews` class is used to create the bindings and collections. This class is separate from the `SampleDatabase` class to illustrate the idea that a single set of stored data can be accessed via multiple bindings and collections, or <span class="emphasis">*views*</span>. The skeleton for the `SampleViews` class follows.

``` c
import com.sleepycat.bind.EntryBinding;
import com.sleepycat.bind.serial.ClassCatalog;
import com.sleepycat.bind.serial.SerialBinding;
import com.sleepycat.collections.StoredEntrySet;
import com.sleepycat.collections.StoredMap;
...

public class SampleViews
{
    private StoredMap partMap;
    private StoredMap supplierMap;
    private StoredMap shipmentMap;

    ...
    public SampleViews(SampleDatabase db)
    {
    }
} 
```

A <a href="../../java/com/sleepycat/collections/StoredMap.html" class="ulink" target="_top">StoredMap</a> field is used for each database. The StoredMap class implements the standard Java <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html" class="ulink" target="_top">Map</a> interface, which has methods for obtaining a <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Set.html" class="ulink" target="_top">Set</a> of keys, a <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Collection.html" class="ulink" target="_top">Collection</a> of values, or a <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Set.html" class="ulink" target="_top">Set</a> of <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.Entry.html" class="ulink" target="_top">Map.Entry</a> key/value pairs. Because databases contain key/value pairs, any Berkeley DB database may be represented as a Java map.

The following statements create the key and data bindings using the <a href="../../java/com/sleepycat/bind/serial/SerialBinding.html" class="ulink" target="_top">SerialBinding</a> class.

``` c
    public SampleViews(SampleDatabase db)
    {
        ClassCatalog catalog = db.getClassCatalog();
        EntryBinding partKeyBinding =
            new SerialBinding(catalog, PartKey.class);
        EntryBinding partValueBinding =
            new SerialBinding(catalog, PartData.class);
        EntryBinding supplierKeyBinding =
            new SerialBinding(catalog, SupplierKey.class);
        EntryBinding supplierValueBinding =
            new SerialBinding(catalog, SupplierData.class);
        EntryBinding shipmentKeyBinding =
            new SerialBinding(catalog, ShipmentKey.class);
        EntryBinding shipmentValueBinding =
            new SerialBinding(catalog, ShipmentData.class);
        ...
    } 
```

The first parameter of the <a href="../../java/com/sleepycat/bind/serial/SerialBinding.html" class="ulink" target="_top">SerialBinding</a> constructor is the class catalog, and is used to store the class descriptions of the serialized objects.

The second parameter is the base class for the serialized objects and is used for type checking of the stored objects. If `null` or `Object.class` is specified, then any Java class is allowed. Otherwise, all objects stored in that format must be instances of the specified class or derived from the specified class. In the example, specific classes are used to enable strong type checking.

The following statements create standard Java maps using the <a href="../../java/com/sleepycat/collections/StoredMap.html" class="ulink" target="_top">StoredMap</a> class.

``` c
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

The first parameter of the <a href="../../java/com/sleepycat/collections/StoredMap.html" class="ulink" target="_top">StoredMap</a> constructor is the database. In a StoredMap, the database keys (the primary keys) are used as the map keys. The Index example shows how to use secondary index keys as map keys.

The second and third parameters are the key and value bindings to use when storing and retrieving objects via the map.

The fourth and last parameter specifies whether changes will be allowed via the collection. If false is passed, the collection will be read-only.

The following getter methods return the stored maps for use by other classes in the example program. Convenience methods for returning entry sets are also included.

``` c
public class SampleViews
{
    ...
    public final StoredMap getPartMap()
    {
        return partMap;
    }

    public final StoredMap getSupplierMap()
    {
        return supplierMap;
    }

    public final StoredMap getShipmentMap()
    {
        return shipmentMap;
    }

    public final StoredEntrySet getPartEntrySet()
    {
        return (StoredEntrySet) partMap.entrySet();
    }

    public final StoredEntrySet getSupplierEntrySet()
    {
        return (StoredEntrySet) supplierMap.entrySet();
    }

    public final StoredEntrySet getShipmentEntrySet()
    {
        return (StoredEntrySet) shipmentMap.entrySet();
    }
    ...
} 
```

Note that StoredMap and StoredEntrySet are returned rather than just returning Map and Set. Since StoredMap implements the Map interface and StoredEntrySet implements the Set interface, you may ask why Map and Set were not returned directly.

`StoredMap`, `StoredEntrySet`, and other stored collection classes have a small number of extra methods beyond those in the Java collection interfaces. The stored collection types are therefore returned to avoid casting when using the extended methods. Normally, however, only a Map or Set is needed, and may be used as follows.

``` c
    SampleDatabase sd = new SampleDatabase(new String("/home"));
    SampleViews views = new SampleViews(sd);
    Map partMap = views.getPartMap();
    Set supplierEntries = views.getSupplierEntrySet(); 
```
