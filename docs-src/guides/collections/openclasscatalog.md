---
title: "Opening and Closing the Class Catalog"
api-name: "Opening and Closing the Class Catalog"
source: docs/collections/tutorial/openclasscatalog.html
---
## Opening and Closing the Class Catalog

This section describes how to open and close the Java class catalog. The class catalog is a specialized database store that contains the Java class descriptions of the serialized objects that are stored in the database. The class descriptions are stored in the catalog rather than storing them redundantly in each database record. A single class catalog per environment must be opened whenever serialized objects will be stored in the database.

The `SampleDatabase` class is extended to open and close the class catalog. The following additional imports and class members are needed.

``` c
import com.sleepycat.bind.serial.StoredClassCatalog;
import com.sleepycat.bind.serial.ClassCatalog;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import java.io.File;
import java.io.FileNotFoundException;

...

public class SampleDatabase
{
    private Environment env;
    private static final String CLASS_CATALOG = "java_class_catalog";
    ...
    private StoredClassCatalog javaCatalog;
    ...
} 
```

While the class catalog is itself a database, it contains metadata for other databases and is therefore treated specially by the DB Java Collections API. The <a href="../../java/com/sleepycat/bind/serial/StoredClassCatalog.html" class="ulink" target="_top">StoredClassCatalog</a> class encapsulates the catalog store and implements this special behavior.

The following statements open the class catalog by creating a `Database` and a `StoredClassCatalog` object. The catalog database is created if it does not already exist.

``` c
    public SampleDatabase(String homeDirectory)
        throws DatabaseException, FileNotFoundException
    {
        ...
        DatabaseConfig dbConfig = new DatabaseConfig();
        dbConfig.setTransactional(true);
        dbConfig.setAllowCreate(true);
        dbConfig.setType(DatabaseType.BTREE);

        Database catalogDb = env.openDatabase(null, CLASS_CATALOG, null, 
                                              dbConfig);

        javaCatalog = new StoredClassCatalog(catalogDb);
        ...
    }
    ...
    public final StoredClassCatalog getClassCatalog() {
        return javaCatalog;
    } 
```

The <a href="../../java/com/sleepycat/db/DatabaseConfig.html" class="ulink" target="_top">DatabaseConfig</a> class is used to specify configuration parameters when opening a database. The first configuration option specified — `setTransactional()` — is set to true to create a transactional database. While non-transactional databases can also be created, the examples in this tutorial use transactional databases.

`setAllowCreate()` is set to true to specify that the database will be created if it does not already exist. If this parameter is not specified, an exception will be thrown if the database does not already exist.

`setDatabaseType()` identifies the database storage type or access method. For opening a catalog database, the `BTREE` type is required. `BTREE` is the most commonly used database type and in this tutorial is used for all databases.

The first parameter of the `openDatabase()` method is an optional transaction that is used for creating a new database. If null is passed, auto-commit is used when creating a database.

The second and third parameters of `openDatabase()` specify the filename and database (sub-file) name of the database. The database name is optional and is `null` in this example.

The last parameter of `openDatabase()` specifies the database configuration object.

Lastly, the `StoredClassCatalog` object is created to manage the information in the class catalog database. The `StoredClassCatalog` object will be used in the sections following for creating serial bindings.

The `getClassCatalog` method returns the catalog object for use by other classes in the example program.

When the environment is closed, the class catalog is closed also.

``` c
    public void close()
        throws DatabaseException
    {
        javaCatalog.close();
        env.close();
    } 
```

The `StoredClassCatalog.close()` method simply closes the underlying class catalog database and in fact the <a href="../../java/com/sleepycat/db/Database.html#close()" class="ulink" target="_top">Database.close()</a> method may be called instead, if desired. It is recommended that you close the catalog database and all other databases, before closing the environment.
