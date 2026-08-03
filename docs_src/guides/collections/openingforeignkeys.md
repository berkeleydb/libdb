---
title: "More Secondary Key Indices"
api-name: "More Secondary Key Indices"
source: docs/collections/tutorial/openingforeignkeys.html
---
## More Secondary Key Indices

This section builds on the prior section describing secondary key indices. Two more secondary key indices are defined for indexing the Shipment record by PartNumber and by SupplierNumber.

The `SampleDatabase` class is extended to open the Shipment-by-Part and Shipment-by-Supplier secondary key indices.

``` c
import com.sleepycat.bind.serial.SerialSerialKeyCreator;
import com.sleepycat.db.SecondaryConfig;
import com.sleepycat.db.SecondaryDatabase;
...
public class SampleDatabase
{
    ...
    private static final String SHIPMENT_PART_INDEX = 
        "shipment_part_index";
    private static final String SHIPMENT_SUPPLIER_INDEX = 
        "shipment_supplier_index";
    ...
    private SecondaryDatabase shipmentByPartDb;
    private SecondaryDatabase shipmentBySupplierDb;
    ...
    public SampleDatabase(String homeDirectory)
        throws DatabaseException, FileNotFoundException
    {
        ...
        SecondaryConfig secConfig = new SecondaryConfig();
        secConfig.setTransactional(true);
        secConfig.setAllowCreate(true);
        secConfig.setType(DatabaseType.BTREE);
        secConfig.setSortedDuplicates(true);
        ...
        secConfig.setKeyCreator(
            new ShipmentByPartKeyCreator(javaCatalog,
                                         ShipmentKey.class,
                                         ShipmentData.class,
                                         PartKey.class));
        shipmentByPartDb = env.openSecondaryDatabase(null, 
                                         SHIPMENT_PART_INDEX,
                                         null,
                                         shipmentDb,
                                         secConfig);

        secConfig.setKeyCreator(
            new ShipmentBySupplierKeyCreator(javaCatalog,
                                             ShipmentKey.class,
                                             ShipmentData.class,
                                             SupplierKey.class));
        shipmentBySupplierDb = env.openSecondaryDatabase(null,
                                                  SHIPMENT_SUPPLIER_INDEX,
                                                  null,
                                                  shipmentDb,
                                                  secConfig);
    ...
    }
} 
```

The statements in this example are very similar to the statements used in the previous section for opening a secondary index.

The application-defined `ShipmentByPartKeyCreator` and `ShipmentBySupplierKeyCreator` classes are shown below. They were used above to configure the secondary database objects.

``` c
public class SampleDatabase
{
...
    private static class ShipmentByPartKeyCreator
        extends SerialSerialKeyCreator
    {
        private ShipmentByPartKeyCreator(ClassCatalog catalog,
                                         Class primaryKeyClass,
                                         Class valueClass,
                                         Class indexKeyClass)
        {
            super(catalog, primaryKeyClass, valueClass, indexKeyClass);
        }

        public Object createSecondaryKey(Object primaryKeyInput,
                                         Object valueInput)
        {
            ShipmentKey shipmentKey = (ShipmentKey) primaryKeyInput;
            return new PartKey(shipmentKey.getPartNumber());
        }
    }

    private static class ShipmentBySupplierKeyCreator
        extends SerialSerialKeyCreator
    {
        private ShipmentBySupplierKeyCreator(ClassCatalog catalog,
                                             Class primaryKeyClass,
                                             Class valueClass,
                                             Class indexKeyClass)
        {
            super(catalog, primaryKeyClass, valueClass, indexKeyClass);
        }

        public Object createSecondaryKey(Object primaryKeyInput,
                                         Object valueInput)
        {
            ShipmentKey shipmentKey = (ShipmentKey) primaryKeyInput;
            return new SupplierKey(shipmentKey.getSupplierNumber());
        }
    }
    ...
} 
```

The key creator classes above are almost identical to the one defined in the previous section for use with a secondary index. The index key fields are different, of course, but the interesting difference is that the index keys are extracted from the key, not the value, of the Shipment record. This illustrates that an index key may be derived from the primary database record key, value, or both.

The following getter methods return the secondary database objects for use by other classes in the example program.

``` c
public class SampleDatabase
{
    ...
    public final SecondaryDatabase getShipmentByPartDatabase()
    {
        return shipmentByPartDb;
    }

    public final SecondaryDatabase getShipmentBySupplierDatabase()
    {
        return shipmentBySupplierDb;
    }
    ...
} 
```

The following statements close the secondary databases.

``` c
public class SampleDatabase
{
    ...
    public void close()
        throws DatabaseException {

        supplierByCityDb.close();
        shipmentByPartDb.close();
        shipmentBySupplierDb.close();
        partDb.close();
        supplierDb.close();
        shipmentDb.close();
        javaCatalog.close();
        env.close();
    }
    ...
} 
```

Secondary databases must be closed before closing their associated primary database.
