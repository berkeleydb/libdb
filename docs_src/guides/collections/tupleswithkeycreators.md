---
title: "Using Tuples with Key Creators"
api-name: "Using Tuples with Key Creators"
source: docs/collections/tutorial/tupleswithkeycreators.html
---
## Using Tuples with Key Creators

Key creators were used in prior examples to extract index keys from value objects. The keys were returned as deserialized key objects, since the serial format was used for keys. In this example, the tuple format is used for keys and the key creators return keys by writing information to a tuple. The differences between this example and the prior example are:

- The <a href="../../java/com/sleepycat/bind/serial/TupleSerialKeyCreator.html" class="ulink" target="_top">TupleSerialKeyCreator</a> base class is used instead of the <a href="../../java/com/sleepycat/bind/serial/SerialSerialKeyCreator.html" class="ulink" target="_top">SerialSerialKeyCreator</a> base class.

- For all key input and output parameters, the <a href="../../java/com/sleepycat/bind/tuple/TupleInput.html" class="ulink" target="_top">TupleInput</a> and <a href="../../java/com/sleepycat/bind/tuple/TupleOutput.html" class="ulink" target="_top">TupleOutput</a> classes are used instead of <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/Object.html" class="ulink" target="_top">Object</a> (representing a deserialized object).

- Instead of returning a key output object, these methods call tuple write methods such as <a href="../../java/com/sleepycat/bind/tuple/TupleOutput.html#writeString(java.lang.String)" class="ulink" target="_top">TupleOutput.writeString</a>.

In addition to writing key tuples, the `ShipmentByPartKeyCreator` and `ShipmentBySupplierKeyCreator` classes also read the key tuple of the primary key. This is because they extract the index key from fields in the Shipment's primary key. Instead of calling getter methods on the `ShipmentKey` object, as in prior examples, these methods call <a href="../../java/com/sleepycat/bind/tuple/TupleInput.html#readString()" class="ulink" target="_top">TupleInput.readString</a>. The `ShipmentKey` consists of two string fields that are read in sequence.

The modified key creators are shown below: `SupplierByCityKeyCreator`, `ShipmentByPartKeyCreator` and `ShipmentBySupplierKeyCreator`.

``` c
import com.sleepycat.bind.serial.TupleSerialKeyCreator;
import com.sleepycat.bind.tuple.TupleInput;
import com.sleepycat.bind.tuple.TupleOutput;
...
public class SampleDatabase
{
    ...
    private static class SupplierByCityKeyCreator
        extends TupleSerialKeyCreator
    {
        private SupplierByCityKeyCreator(ClassCatalog catalog,
                                         Class valueClass)
        {
            super(catalog, valueClass);
        }

        public boolean createSecondaryKey(TupleInput primaryKeyInput,
                                          Object valueInput,
                                          TupleOutput indexKeyOutput)
        {
            SupplierData supplierData = (SupplierData) valueInput;
            String city = supplierData.getCity();
            if (city != null) {
                indexKeyOutput.writeString(supplierData.getCity());
                return true;
            } else {
                return false;
            }
        }
    }

    private static class ShipmentByPartKeyCreator
        extends TupleSerialKeyCreator
    {
        private ShipmentByPartKeyCreator(ClassCatalog catalog,
                                         Class valueClass)
        {
            super(catalog, valueClass);
        }

        public boolean createSecondaryKey(TupleInput primaryKeyInput,
                                          Object valueInput,
                                          TupleOutput indexKeyOutput)
        {
            String partNumber = primaryKeyInput.readString();
            // don't bother reading the supplierNumber
            indexKeyOutput.writeString(partNumber);
            return true;
        }
    }

    private static class ShipmentBySupplierKeyCreator
        extends TupleSerialKeyCreator
    {
        private ShipmentBySupplierKeyCreator(ClassCatalog catalog,
                                             Class valueClass)
        {
            super(catalog, valueClass);
        }

        public boolean createSecondaryKey(TupleInput primaryKeyInput,
                                          Object valueInput,
                                          TupleOutput indexKeyOutput)
        {
            primaryKeyInput.readString(); // skip the partNumber
            String supplierNumber = primaryKeyInput.readString();
            indexKeyOutput.writeString(supplierNumber);
            return true;
        }
    }
    ...
}
    
```
