---
title: "Creating Tuple Key Bindings"
api-name: "Creating Tuple Key Bindings"
source: docs/collections/tutorial/tuplekeybindings.html
---
## Creating Tuple Key Bindings

Serial bindings were used in prior examples as key bindings, and keys were stored as serialized objects. In this example, a tuple binding is used for each key since keys will be stored as tuples. Because keys are no longer stored as serialized objects, the `PartKey`, `SupplierKey` and `ShipmentKey` classes no longer implement the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/io/Serializable.html" class="ulink" target="_top">Serializable</a> interface (this is the only change to these classes and is not shown below).

For the `Part` key, `Supplier` key, and `Shipment` key, the `SampleViews` class was changed in this example to create a custom <a href="../../java/com/sleepycat/bind/tuple/TupleBinding.html" class="ulink" target="_top">TupleBinding</a> instead of a <a href="../../java/com/sleepycat/bind/serial/SerialBinding.html" class="ulink" target="_top">SerialBinding</a>. The custom tuple key binding classes are defined further below.

``` c
import com.sleepycat.bind.tuple.TupleBinding;
...
public class SampleViews
{
    ...
    public SampleViews(SampleDatabase db)
    {
        ...
        ClassCatalog catalog = db.getClassCatalog();
        EntryBinding partKeyBinding =
            new PartKeyBinding();
        EntityBinding partDataBinding =
            new PartBinding(catalog, PartData.class);
        EntryBinding supplierKeyBinding =
            new SupplierKeyBinding();
        EntityBinding supplierDataBinding =
            new SupplierBinding(catalog, SupplierData.class);
        EntryBinding shipmentKeyBinding =
            new ShipmentKeyBinding();
        EntityBinding shipmentDataBinding =
            new ShipmentBinding(catalog, ShipmentData.class);
        EntryBinding cityKeyBinding =
            TupleBinding.getPrimitiveBinding(String.class);
        ...
    }
} 
```

For the City key, however, a custom binding class is not needed because the key class is a primitive Java type, <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/String.html" class="ulink" target="_top">String</a>. For any primitive Java type, a tuple binding may be created using the <a href="../../java/com/sleepycat/bind/tuple/TupleBinding.html#getPrimitiveBinding(java.lang.Class)" class="ulink" target="_top">TupleBinding.getPrimitiveBinding</a> static method.

The custom key binding classes, `PartKeyBinding`, `SupplierKeyBinding` and `ShipmentKeyBinding`, are defined by extending the <a href="../../java/com/sleepycat/bind/tuple/TupleBinding.html" class="ulink" target="_top">TupleBinding</a> class. The <a href="../../java/com/sleepycat/bind/tuple/TupleBinding.html" class="ulink" target="_top">TupleBinding</a> abstract class implements the <a href="../../java/com/sleepycat/bind/EntryBinding.html" class="ulink" target="_top">EntryBinding</a> interface, and is used for one-to-one bindings between tuples and objects. Each binding class implements two methods for converting between tuples and objects. Tuple fields are read using the <a href="../../java/com/sleepycat/bind/tuple/TupleInput.html" class="ulink" target="_top">TupleInput</a> parameter and written using the <a href="../../java/com/sleepycat/bind/tuple/TupleOutput.html" class="ulink" target="_top">TupleOutput</a> parameter.

``` c
import com.sleepycat.bind.tuple.TupleBinding;
import com.sleepycat.bind.tuple.TupleInput;
import com.sleepycat.bind.tuple.TupleOutput;
...
public class SampleViews
{
...

    private static class PartKeyBinding extends TupleBinding
    {
        private PartKeyBinding()
        {
        }

        public Object entryToObject(TupleInput input)
        {
            String number = input.readString();
            return new PartKey(number);
        }

        public void objectToEntry(Object object, TupleOutput output)
        {
            PartKey key = (PartKey) object;
            output.writeString(key.getNumber());
        }
    }
    ...
    private static class SupplierKeyBinding extends TupleBinding
    {
        private SupplierKeyBinding()
        {
        }

        public Object entryToObject(TupleInput input)
        {
            String number = input.readString();
            return new SupplierKey(number);
        }

        public void objectToEntry(Object object, TupleOutput output)
        {
            SupplierKey key = (SupplierKey) object;
            output.writeString(key.getNumber());
        }
    }
    ...
    private static class ShipmentKeyBinding extends TupleBinding
    {
        private ShipmentKeyBinding()
        {
        }

        public Object entryToObject(TupleInput input)
        {
            String partNumber = input.readString();
            String supplierNumber = input.readString();
            return new ShipmentKey(partNumber, supplierNumber);
        }

        public void objectToEntry(Object object, TupleOutput output)
        {
            ShipmentKey key = (ShipmentKey) object;
            output.writeString(key.getPartNumber());
            output.writeString(key.getSupplierNumber());
        }
    }
    ...
} 
```
