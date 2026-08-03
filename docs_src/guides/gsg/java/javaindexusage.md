---
title: "Secondary Database Example"
api-name: "Secondary Database Example"
source: docs/gsg/JAVA/javaindexusage.html
---
## Secondary Database Example

<span class="sect2"> [Opening Secondary Databases with MyDbs](javaindexusage.md#secondaryMyDbs) </span>

<span class="sect2"> [Using Secondary Databases with ExampleDatabaseRead](javaindexusage.md#exampleReadJavaSecondaries) </span>

In previous chapters in this book, we built applications that load and display several DB databases. In this example, we will extend those examples to use secondary databases. Specifically:

- In <a href="dbtJavaUsage.md#dbsStoredClass" class="xref" title="Example 8.4 Stored Class Catalog Management with MyDbs">Stored Class Catalog Management with MyDbs</a> we built a class that we can use to open several `Database` objects. In <a href="javaindexusage.md#secondaryMyDbs" class="xref" title="Opening Secondary Databases with MyDbs">Opening Secondary Databases with MyDbs</a> we will extend that class to also open and manage a `SecondaryDatabase`.

- In <a href="cursorJavaUsage.md" class="xref" title="Cursor Example">Cursor Example</a> we built an application to display our inventory database (and related vendor information). In <a href="javaindexusage.md#exampleReadJavaSecondaries" class="xref" title="Using Secondary Databases with ExampleDatabaseRead">Using Secondary Databases with ExampleDatabaseRead</a> we will extend that application to show inventory records based on the index we cause to be loaded using `ExampleDatabaseLoad`.

Before we can use a secondary database, we must implement a class to extract secondary keys for us. We use `ItemNameKeyCreator` for this purpose.

**Example 10.1 ItemNameKeyCreator.java**

This class assumes the primary database uses `Inventory` objects for the record data. The `Inventory` class is described in <a href="dbtJavaUsage.md#inventoryjava" class="xref" title="Example 8.1 Inventory.java">Inventory.java</a>.

In our key creator class, we make use of a custom tuple binding called `InventoryBinding`. This class is described in <a href="dbtJavaUsage.md#InventoryJavaBinding" class="xref" title="Example 8.3 InventoryBinding.java">InventoryBinding.java</a>.

You can find `InventoryBinding.java` in:

``` c
DB_INSTALL/examples_java/db/GettingStarted
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

``` c
package db.GettingStarted;

import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.SecondaryDatabase;
import com.sleepycat.db.SecondaryKeyCreator;
import com.sleepycat.bind.tuple.TupleBinding;

import java.io.IOException;

public class ItemNameKeyCreator implements SecondaryKeyCreator {

    private TupleBinding theBinding;

    // Use the constructor to set the tuple binding
    ItemNameKeyCreator(TupleBinding binding) {
        theBinding = binding;
    }

    // Abstract method that we must implement
    public boolean createSecondaryKey(SecondaryDatabase secDb,
        DatabaseEntry keyEntry,    // From the primary
        DatabaseEntry dataEntry,   // From the primary
        DatabaseEntry resultEntry) // set the key data on this.
        throws DatabaseException {

        try {
            // Convert dataEntry to an Inventory object
            Inventory inventoryItem = 
                (Inventory) theBinding.entryToObject(dataEntry);
            // Get the item name and use that as the key
            String theItem = inventoryItem.getItemName();
            resultEntry.setData(theItem.getBytes("UTF-8"));
        } catch (IOException willNeverOccur) {}

        return true;
    }
} 
```

  

Now that we have a key creator, we can use it to generate keys for a secondary database. We will now extend `MyDbs` to manage a secondary database, and to use `ItemNameKeyCreator` to generate keys for that secondary database.

### Opening Secondary Databases with MyDbs

In <a href="dbtJavaUsage.md#dbsStoredClass" class="xref" title="Example 8.4 Stored Class Catalog Management with MyDbs">Stored Class Catalog Management with MyDbs</a> we built `MyDbs` as an example of a class that encapsulates `Database` opens and closes. We will now extend that class to manage a `SecondaryDatabase`.

**Example 10.2 SecondaryDatabase Management with MyDbs**

We start by importing two additional classes needed to support secondary databases. We also add a global variable to use as a handle for our secondary database.

``` c
// File MyDbs.java
package db.GettingStarted;

import java.io.FileNotFoundException;

import com.sleepycat.bind.serial.StoredClassCatalog;
import com.sleepycat.bind.tuple.TupleBinding;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.SecondaryConfig;
import com.sleepycat.db.SecondaryDatabase;

public class MyDbs {

    // The databases that our application uses
    private Database vendorDb = null;
    private Database inventoryDb = null;
    private Database classCatalogDb = null;
    private SecondaryDatabase itemNameIndexDb = null;

    private String vendordb = "VendorDB.db";
    private String inventorydb = "InventoryDB.db";
    private String classcatalogdb = "ClassCatalogDB.db";
    private String itemnameindexdb = "ItemNameIndexDB.db";

    // Needed for object serialization
    private StoredClassCatalog classCatalog;

    // Our constructor does nothing
    public MyDbs() {} 
```

Next we update the `MyDbs.setup()` method to open the secondary database. As a part of this, we have to pass an `ItemNameKeyCreator` object on the call to open the secondary database. Also, in order to instantiate `ItemNameKeyCreator`, we need an `InventoryBinding` object (we described this class in <a href="dbtJavaUsage.md#InventoryJavaBinding" class="xref" title="Example 8.3 InventoryBinding.java">InventoryBinding.java</a>). We do all this work together inside of `MyDbs.setup()`.

``` c
    public void setup(String databasesHome)
        throws DatabaseException {
        DatabaseConfig myDbConfig = new DatabaseConfig();
        SecondaryConfig mySecConfig = new SecondaryConfig();

        myDbConfig.setErrorStream(System.err);
        mySecConfig.setErrorStream(System.err);
        myDbConfig.setErrorPrefix("MyDbs");
        mySecConfig.setErrorPrefix("MyDbs");
        myDbConfig.setType(DatabaseType.BTREE);
        mySecConfig.setType(DatabaseType.BTREE);
        myDbConfig.setAllowCreate(true);
        mySecConfig.setAllowCreate(true);

        // Now open, or create and open, our databases
        // Open the vendors and inventory databases
        try {
            vendordb = databasesHome + "/" + vendordb;
            vendorDb = new Database(vendordb,
                                    null,
                                    myDbConfig);

            inventorydb = databasesHome + "/" + inventorydb;
            inventoryDb = new Database(inventorydb,
                                        null,
                                        myDbConfig);

            // Open the class catalog db. This is used to
            // optimize class serialization.
            classcatalogdb = databasesHome + "/" + classcatalogdb;
            classCatalogDb = new Database(classcatalogdb,
                                          null,
                                          myDbConfig);
        } catch(FileNotFoundException fnfe) {
            System.err.println("MyDbs: " + fnfe.toString());
            System.exit(-1);
        }

        // Create our class catalog
        classCatalog = new StoredClassCatalog(classCatalogDb);

        // Need a tuple binding for the Inventory class.
        // We use the InventoryBinding class
        // that we implemented for this purpose.
        TupleBinding inventoryBinding = new InventoryBinding();

        // Open the secondary database. We use this to create a
        // secondary index for the inventory database

        // We want to maintain an index for the inventory entries based
        // on the item name. So, instantiate the appropriate key creator
        // and open a secondary database.
        ItemNameKeyCreator keyCreator =
            new ItemNameKeyCreator(new InventoryBinding());

        // Set up additional secondary properties
        // Need to allow duplicates for our secondary database
        mySecConfig.setSortedDuplicates(true);
        mySecConfig.setAllowPopulate(true); // Allow autopopulate
        mySecConfig.setKeyCreator(keyCreator);
        // Now open it
        try {
            itemnameindexdb = databasesHome + "/" + itemnameindexdb;
            itemNameIndexDb = new SecondaryDatabase(itemnameindexdb,
                                                    null,
                                                    inventoryDb,
                                                    mySecConfig);
        } catch(FileNotFoundException fnfe) {
            System.err.println("MyDbs: " + fnfe.toString());
            System.exit(-1);
        }
    }
    
```

Next we need an additional getter method for returning the secondary database.

``` c
    public SecondaryDatabase getNameIndexDB() {
        return itemNameIndexDb;
    } 
```

Finally, we need to update the `MyDbs.close()` method to close the new secondary database. We want to make sure that the secondary is closed before the primaries. While this is not necessary for this example because our closes are single-threaded, it is still a good habit to adopt.

``` c
    public void close() {
        try {
            if (itemNameIndexDb != null) {
                itemNameIndexDb.close();
            }

            if (vendorDb != null) {
                vendorDb.close();
            }

            if (inventoryDb != null) {
                inventoryDb.close();
            }

            if (classCatalogDb != null) {
                classCatalogDb.close();
            }

        } catch(DatabaseException dbe) {
            System.err.println("Error closing MyDbs: " +
                                dbe.toString());
            System.exit(-1);
        }
    }
} 
```

That completes our update to `MyDbs`. You can find the complete class implementation in:

``` c
DB_INSTALL/examples_java/db/GettingStarted
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

  

### Using Secondary Databases with ExampleDatabaseRead

Because we performed all our secondary database configuration management in `MyDbs`, we do not need to modify `ExampleDatabaseLoad` at all in order to create our secondary indices. When `ExampleDatabaseLoad` calls `MyDbs.setup()`, all of the necessary work is performed for us.

However, we still need to take advantage of the new secondary indices. We do this by updating `ExampleDatabaseRead` to allow us to query for an inventory record based on its name. Remember that the primary key for an inventory record is the item's SKU. The item's name is contained in the `Inventory` object that is stored as each record's data in the inventory database. But our new secondary index now allows us to easily query based on the item's name.

For this update, we modify `ExampleDatabaseRead` to accept a new command line switch, `-s`, whose argument is the name of an inventory item. If the switch is present on the command line call to `ExampleDatabaseRead`, then the application will use the secondary database to look up and display all the inventory records with that item name. Note that we use a `SecondaryCursor` to seek to the item name key and then display all matching records.

Remember that you can find `ExampleDatabaseRead.java` in:

``` c
DB_INSTALL/examples_java/db/GettingStarted
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

**Example 10.3 SecondaryDatabase usage with ExampleDatabaseRead**

First we need to import an additional class in order to use the secondary cursor:

``` c
package db.GettingStarted;

import java.io.IOException;

import com.sleepycat.bind.EntryBinding;
import com.sleepycat.bind.serial.SerialBinding;
import com.sleepycat.bind.tuple.TupleBinding;
import com.sleepycat.db.Cursor;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.OperationStatus;
import com.sleepycat.db.SecondaryCursor; 
```

Next we add a single global variable:

``` c
    public class ExampleDatabaseRead {

    private static String myDbsPath = "./";

    // Encapsulates the database environment and databases.
    private static MyDbs myDbs = new MyDbs();

    private static TupleBinding inventoryBinding;
    private static EntryBinding vendorBinding;

    // The item to locate if the -s switch is used
    private static String locateItem; 
```

Next we update `ExampleDatabaseRead.run()` to check to see if the `locateItem` global variable has a value. If it does, then we show just those records related to the item name passed on the `-s` switch.

``` c
    private void run(String args[]) 
        throws DatabaseException {
        // Parse the arguments list
        parseArgs(args);

        myDbs.setup(myDbsPath);

        // Setup our bindings.
        inventoryBinding = new InventoryBinding();
        vendorBinding =
             new SerialBinding(myDbs.getClassCatalog(),
                               Vendor.class);

            if (locateItem != null) {
                showItem();
            } else {
                showAllInventory();
            }
    } 
```

Finally, we need to implement `ExampleDatabaseRead.showItem()`. This is a fairly simple method that opens a secondary cursor, and then displays every primary record that is related to the secondary key identified by the `locateItem` global variable.

``` c
    private void showItem() throws DatabaseException {
        SecondaryCursor secCursor = null;
        try {
            // searchKey is the key that we want to find in the
            // secondary db.
            DatabaseEntry searchKey =
                new DatabaseEntry(locateItem.getBytes("UTF-8"));

            // foundKey and foundData are populated from the primary
            // entry that is associated with the secondary db key.
            DatabaseEntry foundKey = new DatabaseEntry();
            DatabaseEntry foundData = new DatabaseEntry();

            // open a secondary cursor
            secCursor =
                myDbs.getNameIndexDB().openSecondaryCursor(null, null);

            // Search for the secondary database entry.
            OperationStatus retVal =
                secCursor.getSearchKey(searchKey, foundKey,
                    foundData, LockMode.DEFAULT);

            // Display the entry, if one is found. Repeat until no more
            // secondary duplicate entries are found
            while(retVal == OperationStatus.SUCCESS) {
                Inventory theInventory =
                    (Inventory)inventoryBinding.entryToObject(foundData);
                displayInventoryRecord(foundKey, theInventory);
                retVal = secCursor.getNextDup(searchKey, foundKey,
                    foundData, LockMode.DEFAULT);
            }
        } catch (Exception e) {
            System.err.println("Error on inventory secondary cursor:");
            System.err.println(e.toString());
            e.printStackTrace();
        } finally {
            if (secCursor != null) {
                secCursor.close();
            }
        }

    }
```

The only other thing left to do is to update `ExampleDatabaseRead.parseArgs()` to support the `-s` command line switch. To see how this is done, see `ExampleDatabaseRead.java` in:

``` c
DB_INSTALL/examples_java/db/GettingStarted
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.
