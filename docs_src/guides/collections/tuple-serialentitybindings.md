---
title: "Creating Tuple-Serial Entity Bindings"
api-name: "Creating Tuple-Serial Entity Bindings"
source: docs/collections/tutorial/tuple-serialentitybindings.html
---
## Creating Tuple-Serial Entity Bindings

In the prior example serial keys and serial values were used, and the <a href="../../java/com/sleepycat/bind/serial/SerialSerialBinding.html" class="ulink" target="_top">SerialSerialBinding</a> base class was used for entity bindings. In this example, tuple keys and serial values are used and therefore the <a href="../../java/com/sleepycat/bind/serial/TupleSerialBinding.html" class="ulink" target="_top">TupleSerialBinding</a> base class is used for entity bindings.

As with any entity binding, a key and value is converted to an entity in the <a href="../../java/com/sleepycat/bind/serial/TupleSerialBinding.html#entryToObject(com.sleepycat.bind.tuple.TupleInput,%20java.lang.Object)" class="ulink" target="_top">TupleSerialBinding.entryToObject</a> method, and from an entity to a key and value in the <a href="../../java/com/sleepycat/bind/serial/TupleSerialBinding.html#objectToKey(java.lang.Object,%20com.sleepycat.db.DatabaseEntry)" class="ulink" target="_top">TupleSerialBinding.objectToKey</a> and <a href="../../java/com/sleepycat/bind/serial/TupleSerialBinding.html#objectToData(java.lang.Object)" class="ulink" target="_top">TupleSerialBinding.objectToData</a> methods. But since keys are stored as tuples, not as serialized objects, key fields are read and written using the <a href="../../java/com/sleepycat/bind/tuple/TupleInput.html" class="ulink" target="_top">TupleInput</a> and <a href="../../java/com/sleepycat/bind/tuple/TupleOutput.html" class="ulink" target="_top">TupleOutput</a> parameters.

The `SampleViews` class contains the modified entity binding classes that were defined in the prior example: `PartBinding`, `SupplierBinding` and `ShipmentBinding`.

``` c
import com.sleepycat.bind.serial.TupleSerialBinding;
import com.sleepycat.bind.tuple.TupleInput;
import com.sleepycat.bind.tuple.TupleOutput;
...
public class SampleViews
{
    ...
    private static class PartBinding extends TupleSerialBinding 
    {
        private PartBinding(ClassCatalog classCatalog, Class dataClass)
        {
            super(classCatalog, dataClass);
        }
        public Object entryToObject(TupleInput keyInput, Object dataInput)
        {
            String number = keyInput.readString();
            PartData data = (PartData) dataInput;
            return new Part(number, data.getName(), data.getColor(),
                            data.getWeight(), data.getCity());
        }
        public void objectToKey(Object object, TupleOutput output)
        {
            Part part = (Part) object;
            output.writeString(part.getNumber());
        }
        public Object objectToData(Object object)
        {
            Part part = (Part) object;
            return new PartData(part.getName(), part.getColor(),
                                 part.getWeight(), part.getCity());
        }
    }
    ...
    private static class SupplierBinding extends TupleSerialBinding
    {
        private SupplierBinding(ClassCatalog classCatalog, Class dataClass)
        {
            super(classCatalog, dataClass);
        }
        public Object entryToObject(TupleInput keyInput, Object dataInput)
        {
            String number = keyInput.readString();
            SupplierData data = (SupplierData) dataInput;
            return new Supplier(number, data.getName(),
                                data.getStatus(), data.getCity());
        }
        public void objectToKey(Object object, TupleOutput output)
        {
            Supplier supplier = (Supplier) object;
            output.writeString(supplier.getNumber());
        }
        public Object objectToData(Object object)
        {
            Supplier supplier = (Supplier) object;
            return new SupplierData(supplier.getName(), 
                                    supplier.getStatus(),
                                    supplier.getCity());
        }
    }
    ...
    private static class ShipmentBinding extends TupleSerialBinding
    {
        private ShipmentBinding(ClassCatalog classCatalog, Class dataClass)
        {
            super(classCatalog, dataClass);
        }
        public Object entryToObject(TupleInput keyInput, Object dataInput)
        {
            String partNumber = keyInput.readString();
            String supplierNumber = keyInput.readString();
            ShipmentData data = (ShipmentData) dataInput;
            return new Shipment(partNumber, supplierNumber,
                                data.getQuantity());
        }
        public void objectToKey(Object object, TupleOutput output)
        {
            Shipment shipment = (Shipment) object;
            output.writeString(shipment.getPartNumber());
            output.writeString(shipment.getSupplierNumber());
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
