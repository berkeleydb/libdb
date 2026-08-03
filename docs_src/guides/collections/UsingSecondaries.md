---
title: "Chapter 3.  Using Secondary Indices"
api-name: "Chapter 3.  Using Secondary Indices"
source: docs/collections/tutorial/UsingSecondaries.html
---
## Chapter 3.  Using Secondary Indices

**Table of Contents**

<span class="sect1"> [Opening Secondary Key Indices](UsingSecondaries.md#opensecondaryindices) </span>

<span class="sect1"> [More Secondary Key Indices](openingforeignkeys.md) </span>

<span class="sect1"> [Creating Indexed Collections](indexedcollections.md) </span>

<span class="sect1"> [Retrieving Items by Index Key](retrievingbyindexkey.md) </span>

In the Basic example, each store has a single <span class="emphasis">*primary key*</span>. The Index example extends the Basic example to add the use of <span class="emphasis">*secondary keys*</span>.

The complete source of the final version of the example program is included in the Berkeley DB distribution.

## Opening Secondary Key Indices

<span class="emphasis">*Secondary indices*</span> or <span class="emphasis">*secondary databases*</span> are used to access a primary database by a key other than the primary key. Recall that the Supplier Number field is the primary key of the Supplier database. In this section, the Supplier City field will be used as a secondary lookup key. Given a city value, we would like to be able to find the Suppliers in that city. Note that more than one Supplier may be in the same city.

Both primary and secondary databases contain key-value records. The key of an index record is the secondary key, and its value is the key of the associated record in the primary database. When lookups by secondary key are performed, the associated record in the primary database is transparently retrieved by its primary key and returned to the caller.

Secondary indices are maintained automatically when index key fields (the City field in this case) are added, modified or removed in the records of the primary database. However, the application must implement a <a href="../../java/com/sleepycat/db/SecondaryKeyCreator.html" class="ulink" target="_top">SecondaryKeyCreator</a> that extracts the index key from the database record.

It is useful to contrast opening an secondary index with opening a primary database (as described earlier in <a href="opendatabases.md" class="xref" title="Opening and Closing Databases">Opening and Closing Databases</a> .

- A primary database may be associated with one or more secondary indices. A secondary index is always associated with exactly one primary database.

- For a secondary index, a <a href="../../java/com/sleepycat/db/SecondaryKeyCreator.html" class="ulink" target="_top">SecondaryKeyCreator</a> must be implemented by the application to extract the index key from the record of its associated primary database.

- A primary database is represented by a <a href="../../java/com/sleepycat/db/Database.html" class="ulink" target="_top">Database</a> object and a secondary index is represented by a <a href="../../java/com/sleepycat/db/SecondaryDatabase.html" class="ulink" target="_top">SecondaryDatabase</a> object. The <a href="../../java/com/sleepycat/db/SecondaryDatabase.html" class="ulink" target="_top">SecondaryDatabase</a> class extends the <a href="../../java/com/sleepycat/db/Database.html" class="ulink" target="_top">Database</a> class.

- When a <a href="../../java/com/sleepycat/db/SecondaryDatabase.html" class="ulink" target="_top">SecondaryDatabase</a> is created it is associated with a primary <a href="../../java/com/sleepycat/db/Database.html" class="ulink" target="_top">Database</a> object and a <a href="../../java/com/sleepycat/db/SecondaryKeyCreator.html" class="ulink" target="_top">SecondaryKeyCreator</a>.

The `SampleDatabase` class is extended to open the Supplier-by-City secondary key index.

``` c
import com.sleepycat.bind.serial.SerialSerialKeyCreator;
import com.sleepycat.db.SecondaryConfig;
import com.sleepycat.db.SecondaryDatabase;
...
public class SampleDatabase
{
    ...
    private static final String SUPPLIER_CITY_INDEX = 
        "supplier_city_index";
    ...
    private SecondaryDatabase supplierByCityDb;
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

        secConfig.setKeyCreator(
            new SupplierByCityKeyCreator(javaCatalog,
                                         SupplierKey.class,
                                         SupplierData.class,
                                         String.class));

        supplierByCityDb = env.openSecondaryDatabase(null, 
                                                     SUPPLIER_CITY_INDEX,
                                                     null,
                                                     supplierDb,
                                                     secConfig);
    ...
    }
} 
```

A <a href="../../java/com/sleepycat/db/SecondaryConfig.html" class="ulink" target="_top">SecondaryConfig</a> object is used to configure the secondary database. The <a href="../../java/com/sleepycat/db/SecondaryConfig.html" class="ulink" target="_top">SecondaryConfig</a> class extends the <a href="../../java/com/sleepycat/db/DatabaseConfig.html" class="ulink" target="_top">DatabaseConfig</a> class, and most steps for configuring a secondary database are the same as for configuring a primary database. The main difference in the example above is that the `SecondaryConfig.setSortedDuplicates()` method is called to allow duplicate index keys. This is how more than one Supplier may be in the same City. If this property is not specified, the default is that the index keys of all records must be unique.

For a primary database, duplicate keys are not normally used since a primary database with duplicate keys may not have any associated secondary indices. If primary database keys are not unique, there is no way for a secondary key to reference a specific record in the primary database.

Note that `setSortedDuplicates()` and not `setUnsortedDuplicates()` was called. Sorted duplicates are always used for indices rather than unsorted duplicates, since sorting enables optimized equality joins.

Opening a secondary key index requires creating a <a href="../../java/com/sleepycat/db/SecondaryKeyCreator.html" class="ulink" target="_top">SecondaryKeyCreator</a>. The `SupplierByCityKeyCreator` class implements the <a href="../../java/com/sleepycat/db/SecondaryKeyCreator.html" class="ulink" target="_top">SecondaryKeyCreator</a> interface and will be defined below.

The <a href="../../java/com/sleepycat/db/SecondaryDatabase.html" class="ulink" target="_top">SecondaryDatabase</a> object is opened last. If you compare the `openSecondaryDatabase()` and `openDatabase()` methods you'll notice only two differences:

- `openSecondaryDatabase()` has an extra parameter for specifying the associated primary database. The primary database is `supplierDb` in this case.

- The last parameter of `openSecondaryDatabase()` is a `SecondaryConfig` instead of a `DatabaseConfig`.

How to use the secondary index to access records will be shown in a later section.

The application-defined `SupplierByCityKeyCreator` class is shown below. It was used above to configure the secondary database.

``` c
public class SampleDatabase
{
...
    private static class SupplierByCityKeyCreator
        extends SerialSerialKeyCreator
    {
        private SupplierByCityKeyCreator(ClassCatalog catalog,
                                      Class primaryKeyClass,
                                      Class valueClass,
                                      Class indexKeyClass)
        {
            super(catalog, primaryKeyClass, valueClass, indexKeyClass);
        }

        public Object createSecondaryKey(Object primaryKeyInput,
                                         Object valueInput)
        {
            SupplierData supplierData = (SupplierData) valueInput;
            return supplierData.getCity();
        }
    }
...
} 
```

In general, a key creator class must implement the <a href="../../java/com/sleepycat/db/SecondaryKeyCreator.html" class="ulink" target="_top">SecondaryKeyCreator</a> interface. This interface has methods that operate on the record data as raw bytes. In practice, it is easiest to use an abstract base class that performs the conversion of record data to and from the format defined for the database's key and value. The base class implements the <a href="../../java/com/sleepycat/db/SecondaryKeyCreator.html" class="ulink" target="_top">SecondaryKeyCreator</a> interface and has abstract methods that must be implemented in turn by the application.

In this example the <a href="../../java/com/sleepycat/bind/serial/SerialSerialKeyCreator.html" class="ulink" target="_top">SerialSerialKeyCreator</a> base class is used because the database record uses the serial format for both its key and its value. The abstract methods of this class have key and value parameters of type <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/Object.html" class="ulink" target="_top">Object</a> which are automatically converted to and from the raw record data by the base class.

To perform the conversions properly, the key creator must be aware of all three formats involved: the key format of the primary database record, the value format of the primary database record, and the key format of the index record. The <a href="../../java/com/sleepycat/bind/serial/SerialSerialKeyCreator.html" class="ulink" target="_top">SerialSerialKeyCreator</a> constructor is given the base classes for these three formats as parameters.

The `SerialSerialKeyCreator.createSecondaryKey` method is given the key and value of the primary database record as parameters, and it returns the key of the index record. In this example, the index key is a field in the primary database record value. Since the record value is known to be a `SupplierData` object, it is cast to that class and the city field is returned.

Note that the `primaryKeyInput` parameter is not used in the example. This parameter is needed only when an index key is derived from the key of the primary database record. Normally an index key is derived only from the primary database record value, but it may be derived from the key, value or both.

The following getter methods return the secondary database object for use by other classes in the example program. The secondary database object is used to create Java collections for accessing records via their secondary keys.

``` c
public class SampleDatabase
{
    ...
    public final SecondaryDatabase getSupplierByCityDatabase()
    {
        return supplierByCityDb;
    }
    ...
} 
```

The following statement closes the secondary database.

``` c
public class SampleDatabase
{
    ...
    public void close()
        throws DatabaseException {

        supplierByCityDb.close();
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
