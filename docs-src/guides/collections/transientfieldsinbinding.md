---
title: "Using Transient Fields in an Entity Binding"
api-name: "Using Transient Fields in an Entity Binding"
source: docs/collections/tutorial/transientfieldsinbinding.html
---
## Using Transient Fields in an Entity Binding

The entity bindings from the prior example have been changed in this example to use the entity object both as a value object and an entity object.

Before, the `entryToObject()` method combined the deserialized value object with the key fields to create a new entity object. Now, this method uses the deserialized object directly as an entity, and initializes its key using the fields read from the key tuple.

Before, the `objectToData()` method constructed a new value object using information in the entity. Now it simply returns the entity. Nothing needs to be changed in the entity, since the transient key fields won't be serialized.

``` c
import com.sleepycat.bind.serial.ClassCatalog;
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
            Part part = (Part) dataInput;
            part.setKey(number);
            return part;
        }

        public void objectToKey(Object object, TupleOutput output)
        {
            Part part = (Part) object;
            output.writeString(part.getNumber());
        }

        public Object objectToData(Object object)
        {
            return object;
        }
    }

    private static class SupplierBinding extends TupleSerialBinding
    {
        private SupplierBinding(ClassCatalog classCatalog, Class dataClass)
        {
            super(classCatalog, dataClass);
        }

        public Object entryToObject(TupleInput keyInput, Object dataInput)
        {
            String number = keyInput.readString();
            Supplier supplier = (Supplier) dataInput;
            supplier.setKey(number);
            return supplier;
        }

        public void objectToKey(Object object, TupleOutput output)
        {
            Supplier supplier = (Supplier) object;
            output.writeString(supplier.getNumber());
        }

        public Object objectToData(Object object)
        {
            return object;
        }
    }

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
            Shipment shipment = (Shipment) dataInput;
            shipment.setKey(partNumber, supplierNumber);
            return shipment;
        }

        public void objectToKey(Object object, TupleOutput output)
        {
            Shipment shipment = (Shipment) object;
            output.writeString(shipment.getPartNumber());
            output.writeString(shipment.getSupplierNumber());
        }

        public Object objectToData(Object object)
        {
            return object;
        }
    }
} 
```
