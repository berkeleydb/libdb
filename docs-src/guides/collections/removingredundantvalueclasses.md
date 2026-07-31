---
title: "Removing the Redundant Value Classes"
api-name: "Removing the Redundant Value Classes"
source: docs/collections/tutorial/removingredundantvalueclasses.html
---
## Removing the Redundant Value Classes

The `PartData`, `SupplierData` and `ShipmentData` classes have been removed in this example, and the `Part`, `Supplier` and `Shipment` entity classes are used in their place.

The serial formats are created with the entity classes.

``` c
public class SampleDatabase
{
    ...
    public SampleDatabase(String homeDirectory)
        throws DatabaseException, FileNotFoundException
    {
        ...
        secConfig.setKeyCreator(new SupplierByCityKeyCreator(javaCatalog,
                                                     Supplier.class));
        ...
        secConfig.setKeyCreator(new ShipmentByPartKeyCreator(javaCatalog,
                                                     Shipment.class));
        ...
        secConfig.setKeyCreator(new 
                                ShipmentBySupplierKeyCreator(javaCatalog,
                                                     Shipment.class));
        ...
    }
} 
```

The index key creator uses the entity class as well.

``` c
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
            Supplier supplier = (Supplier) valueInput;
            String city = supplier.getCity();
            if (city != null) {
                indexKeyOutput.writeString(supplier.getCity());
                return true;
            } else {
                return false;
            }
        }
    }
} 
```
