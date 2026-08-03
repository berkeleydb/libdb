---
title: "Creating Entity Bindings"
api-name: "Creating Entity Bindings"
source: docs/collections/tutorial/creatingentitybindings.html
---
## Creating Entity Bindings

<span class="emphasis">*Entity bindings*</span> are similar to ordinary bindings in that they convert between Java objects and the stored data format of keys and values. In addition, entity bindings map between key/value pairs and entity objects. An ordinary binding is a one-to-one mapping, while an entity binding is a two-to-one mapping.

The `partValueBinding`, `supplierValueBinding` and `shipmentValueBinding` bindings are created below as entity bindings rather than (in the prior examples) serial bindings.

``` c
import com.sleepycat.bind.EntryBinding;
import com.sleepycat.bind.EntityBinding;
import com.sleepycat.bind.serial.SerialBinding;
import com.sleepycat.bind.serial.SerialSerialBinding;
...

public class SampleViews
{
    ...
    public SampleViews(SampleDatabase db)
    {
        ClassCatalog catalog = db.getClassCatalog();
        SerialBinding partKeyBinding =
            new SerialBinding(catalog, PartKey.class);
        EntityBinding partValueBinding =
            new PartBinding(catalog, PartKey.class, PartData.class);
        SerialBinding supplierKeyBinding =
            new SerialBinding(catalog, SupplierKey.class);
        EntityBinding supplierValueBinding =
            new SupplierBinding(catalog, SupplierKey.class,
                                SupplierData.class);
        SerialBinding shipmentKeyBinding =
            new SerialBinding(catalog, ShipmentKey.class);
        EntityBinding shipmentValueBinding =
            new ShipmentBinding(catalog, ShipmentKey.class,
                                ShipmentData.class);
        SerialBinding cityKeyBinding =
            new SerialBinding(catalog, String.class);
        ...
    }
} 
```

The entity bindings will be used in the next section to construct stored map objects.

The `PartBinding` class is defined below.

``` c
public class SampleViews
{
    ...
    private static class PartBinding extends SerialSerialBinding {
        private PartBinding(ClassCatalog classCatalog,
                            Class keyClass,
                            Class dataClass)
        {
            super(classCatalog, keyClass, dataClass);
        }

        public Object entryToObject(Object keyInput, Object dataInput)
        {
            PartKey key = (PartKey) keyInput;
            PartData data = (PartData) dataInput;
            return new Part(key.getNumber(), data.getName(),
                            data.getColor(), data.getWeight(), 
                            data.getCity());
        }

        public Object objectToKey(Object object)
        {
            Part part = (Part) object;
            return new PartKey(part.getNumber());
        }

        public Object objectToData(Object object)
        {
            Part part = (Part) object;
            return new PartData(part.getName(), part.getColor(),
                                part.getWeight(), part.getCity());
        }
    }
    ...
} 
```

In general, an entity binding is any class that implements the <a href="../../java/com/sleepycat/bind/EntityBinding.html" class="ulink" target="_top">EntityBinding</a> interface, just as an ordinary binding is any class that implements the <a href="../../java/com/sleepycat/bind/EntryBinding.html" class="ulink" target="_top">EntryBinding</a> interface. In the prior examples the built-in <a href="../../java/com/sleepycat/bind/serial/SerialBinding.html" class="ulink" target="_top">SerialBinding</a> class (which implements <a href="../../java/com/sleepycat/bind/EntryBinding.html" class="ulink" target="_top">EntryBinding</a>) was used and no application-defined binding classes were needed.

In this example, application-defined binding classes are used that extend the <a href="../../java/com/sleepycat/bind/serial/SerialSerialBinding.html" class="ulink" target="_top">SerialSerialBinding</a> abstract base class. This base class implements <a href="../../java/com/sleepycat/bind/EntityBinding.html" class="ulink" target="_top">EntityBinding</a> and provides the conversions between key/value bytes and key/value objects, just as the <a href="../../java/com/sleepycat/bind/serial/SerialBinding.html" class="ulink" target="_top">SerialBinding</a> class does. The application-defined entity class implements the abstract methods defined in the base class that map between key/value objects and entity objects.

Three abstract methods are implemented for each entity binding. The `entryToObject()` method takes as input the key and data objects, which have been deserialized automatically by the base class. As output, it returns the combined `Part` entity.

The `objectToKey()` and `objectToData()` methods take an entity object as input. As output they return the part key or data object that is extracted from the entity object. The key or data will then be serialized automatically by the base class.

The `SupplierBinding` and `ShipmentBinding` classes are very similar to the `PartBinding` class.

``` c
public class SampleViews
{
    ...
    private static class SupplierBinding extends SerialSerialBinding {
        private SupplierBinding(ClassCatalog classCatalog,
                                Class keyClass,
                                Class dataClass)
        {
            super(classCatalog, keyClass, dataClass);
        }

        public Object entryToObject(Object keyInput, Object dataInput)
        {
            SupplierKey key = (SupplierKey) keyInput;
            SupplierData data = (SupplierData) dataInput;
            return new Supplier(key.getNumber(), data.getName(),
                                data.getStatus(), data.getCity());
        }

        public Object objectToKey(Object object)
        {
            Supplier supplier = (Supplier) object;
            return new SupplierKey(supplier.getNumber());
        }

        public Object objectToData(Object object)
        {
            Supplier supplier = (Supplier) object;
            return new SupplierData(supplier.getName(),
                                    supplier.getStatus(), 
                                    supplier.getCity());
        }
    }

    private static class ShipmentBinding extends SerialSerialBinding {
        private ShipmentBinding(ClassCatalog classCatalog,
                                Class keyClass,
                                Class dataClass)
        {
            super(classCatalog, keyClass, dataClass);
        }

        public Object entryToObject(Object keyInput, Object dataInput)
        {
            ShipmentKey key = (ShipmentKey) keyInput;
            ShipmentData data = (ShipmentData) dataInput;
            return new Shipment(key.getPartNumber(), 
                                key.getSupplierNumber(),
                                data.getQuantity());
        }

        public Object objectToKey(Object object)
        {
            Shipment shipment = (Shipment) object;
            return new ShipmentKey(shipment.getPartNumber(),
                                   shipment.getSupplierNumber());
        }

        public Object objectToData(Object object)
        {
            Shipment shipment = (Shipment) object;
            return new ShipmentData(shipment.getQuantity());
        }
    }
    ...
} 
```
