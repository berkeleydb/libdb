---
title: "Opening and Closing Databases"
api-name: "Opening and Closing Databases"
source: docs/collections/tutorial/opendatabases.html
---
## Opening and Closing Databases

This section describes how to open and close the Part, Supplier and Shipment databases. A <span class="emphasis">*database*</span> is a collection of records, each of which has a key and a value. The keys and values are stored in a selected format, which defines the syntax of the stored data. Two examples of formats are Java serialization format and tuple format. In a given database, all keys have the same format and all values have the same format.

The `SampleDatabase` class is extended to open and close the three databases. The following additional class members are needed.

``` c
public class SampleDatabase
{
    ...
    private static final String SUPPLIER_STORE = "supplier_store";
    private static final String PART_STORE = "part_store";
    private static final String SHIPMENT_STORE = "shipment_store";
    ...
    private Database supplierDb;
    private Database partDb;
    private Database shipmentDb;
    ...
} 
```

For each database there is a database name constant and a `Database` object.

The following statements open the three databases by constructing a `Database` object.

``` c
    public SampleDatabase(String homeDirectory)
        throws DatabaseException, FileNotFoundException
    {
        ...
        DatabaseConfig dbConfig = new DatabaseConfig();
        dbConfig.setTransactional(true);
        dbConfig.setAllowCreate(true);
        dbConfig.setType(DatabaseType.BTREE);
        ...
        partDb = env.openDatabase(null, PART_STORE, null, dbConfig);
        supplierDb = env.openDatabase(null, SUPPLIER_STORE, null, 
                                      dbConfig);
        shipmentDb = env.openDatabase(null, SHIPMENT_STORE, null,
                                      dbConfig); 
        ...
    } 
```

The database configuration object that was used previously for opening the catalog database is reused for opening the three databases above. The databases are created if they don't already exist. The parameters of the `openDatabase()` method were described earlier when the class catalog database was opened.

The following statements close the three databases.

``` c
    public void close()
        throws DatabaseException
    {
        partDb.close();
        supplierDb.close();
        shipmentDb.close();
        javaCatalog.close();
        env.close();
    } 
```

It is recommended that all databases, including the catalog database, is closed before closing the environment.

The following getter methods return the databases for use by other classes in the example program.

``` c
public class SampleDatabase
{
    ...
    public final Database getPartDatabase()
    {
        return partDb;
    }

    public final Database getSupplierDatabase()
    {
        return supplierDb;
    }

    public final Database getShipmentDatabase()
    {
        return shipmentDb;
    }
    ...
}
```
