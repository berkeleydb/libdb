---
title: "Cursor Example"
api-name: "Cursor Example"
source: docs/gsg/JAVA/cursorJavaUsage.html
---
## Cursor Example

In <a href="dbtJavaUsage.md" class="xref" title="Database Usage Example">Database Usage Example</a> we wrote an application that loaded two `Database` objects with vendor and inventory information. In this example, we will use those databases to display all of the items in the inventory database. As a part of showing any given inventory item, we will look up the vendor who can provide the item and show the vendor's contact information.

To do this, we create the `ExampleDatabaseRead` application. This application reads and displays all inventory records by:

1.  Opening the inventory, vendor, and class catalog `Database` objects. We do this using the `MyDbs` class. See <a href="dbtJavaUsage.md#dbsStoredClass" class="xref" title="Example 8.4 Stored Class Catalog Management with MyDbs">Stored Class Catalog Management with MyDbs</a> for a description of this class.

2.  Obtaining a cursor from the inventory `Database`.

3.  Steps through the `Database`, displaying each record as it goes.

4.  To display the Inventory record, the custom tuple binding that we created in <a href="dbtJavaUsage.md#InventoryJavaBinding" class="xref" title="Example 8.3 InventoryBinding.java">InventoryBinding.java</a> is used.

5.  `Database.get()` is used to obtain the vendor that corresponds to the inventory item.

6.  A serial binding is used to convert the `DatabaseEntry` returned by the `get()` to a Vendor object.

7.  The contents of the Vendor object are displayed.

We implemented the `Vendor` class in <a href="dbtJavaUsage.md#vendorjava" class="xref" title="Example 8.2 Vendor.java">Vendor.java</a>. We implemented the `Inventory` class in <a href="dbtJavaUsage.md#inventoryjava" class="xref" title="Example 8.1 Inventory.java">Inventory.java</a>.

The full implementation of `ExampleDatabaseRead` can be found in:

``` c
DB_INSTALL/examples_java/db/GettingStarted
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

**Example 9.1 ExampleDatabaseRead.java**

To begin, we import the necessary classes:

``` c
// file ExampleDatabaseRead.java
package db.GettingStarted;

import java.io.File;
import java.io.IOException;

import com.sleepycat.bind.EntryBinding;
import com.sleepycat.bind.serial.SerialBinding;
import com.sleepycat.bind.tuple.TupleBinding;
import com.sleepycat.db.Cursor;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.OperationStatus;
```

Next we declare our class and set up some global variables. Note a `MyDbs` object is instantiated here. We can do this because its constructor never throws an exception. See <a href="CoreJavaUsage.md" class="xref" title="Database Example">Database Example</a> for its implementation details.

``` c
public class ExampleDatabaseRead {

    private static String myDbsPath = "./";

    // Encapsulates the database environment and databases.
    private static MyDbs myDbs = new MyDbs();

    private static TupleBinding inventoryBinding;
    private static EntryBinding vendorBinding; 
```

Next we create the `ExampleDatabaseRead.usage()` and `ExampleDatabaseRead.main()` methods. We perform almost all of our exception handling from `ExampleDatabaseRead.main()`, and so we must catch `DatabaseException` because the `com.sleepycat.db.*` APIs throw them.

``` c
   private static void usage() {
        System.out.println("ExampleDatabaseRead [-h <env directory>]" +
                           "[-s <item to locate>]");
        System.exit(-1);
    }

    public static void main(String args[]) {
        ExampleDatabaseRead edr = new ExampleDatabaseRead();
        try {
            edr.run(args);
        } catch (DatabaseException dbe) {
            System.err.println("ExampleDatabaseRead: " + dbe.toString());
            dbe.printStackTrace();
        } finally {
            myDbs.close();
        }
        System.out.println("All done.");
    }
```

In `ExampleDatabaseRead.run()`, we call `MyDbs.setup()` to open our databases. Then we create the bindings that we need for using our data objects with `DatabaseEntry` objects.

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

        showAllInventory();
    }
```

Now we write the loop that displays the `Inventory` records. We do this by opening a cursor on the inventory database and iterating over all its contents, displaying each as we go.

``` c
    private void showAllInventory() 
        throws DatabaseException {
        // Get a cursor
        Cursor cursor = myDbs.getInventoryDB().openCursor(null, null);

        // DatabaseEntry objects used for reading records
        DatabaseEntry foundKey = new DatabaseEntry();
        DatabaseEntry foundData = new DatabaseEntry();

        try { // always want to make sure the cursor gets closed
            while (cursor.getNext(foundKey, foundData,
                        LockMode.DEFAULT) == OperationStatus.SUCCESS) {
                Inventory theInventory =
                    (Inventory)inventoryBinding.entryToObject(foundData);
                displayInventoryRecord(foundKey, theInventory);
            }
        } catch (Exception e) {
            System.err.println("Error on inventory cursor:");
            System.err.println(e.toString());
            e.printStackTrace();
        } finally {
            cursor.close();
        }
    } 
```

We use `ExampleDatabaseRead.displayInventoryRecord()` to actually show the record. This method first displays all the relevant information from the retrieved Inventory object. It then uses the vendor database to retrieve and display the vendor. Because the vendor database is keyed by vendor name, and because each inventory object contains this key, it is trivial to retrieve the appropriate vendor record.

``` c
   private void displayInventoryRecord(DatabaseEntry theKey,
                                        Inventory theInventory)
        throws DatabaseException {
        String theSKU = null;
        try {
            theSKU = new String(theKey.getData(), "UTF-8");
        } catch(java.io.UnsupportedEncodingException e) {
            /* Handle the exception here. */
        }
        System.out.println(theSKU + ":");
        System.out.println("\t " + theInventory.getItemName());
        System.out.println("\t " + theInventory.getCategory());
        System.out.println("\t " + theInventory.getVendor());
        System.out.println("\t\tNumber in stock: " +
            theInventory.getVendorInventory());
        System.out.println("\t\tPrice per unit:  " +
            theInventory.getVendorPrice());
        System.out.println("\t\tContact: ");

        DatabaseEntry searchKey = null;
        try {
            searchKey =
             new DatabaseEntry(theInventory.getVendor().getBytes("UTF-8"));
        } catch (IOException willNeverOccur) {}
        DatabaseEntry foundVendor = new DatabaseEntry();

        if (myDbs.getVendorDB().get(null, searchKey, foundVendor,
                LockMode.DEFAULT) != OperationStatus.SUCCESS) {
            System.out.println("Could not find vendor: " +
                theInventory.getVendor() + ".");
            System.exit(-1);
        } else {
            Vendor theVendor =
                (Vendor)vendorBinding.entryToObject(foundVendor);
            System.out.println("\t\t " + theVendor.getAddress());
            System.out.println("\t\t " + theVendor.getCity() + ", " +
                theVendor.getState() + " " + theVendor.getZipcode());
            System.out.println("\t\t Business Phone: " +
                theVendor.getBusinessPhoneNumber());
            System.out.println("\t\t Sales Rep: " +
                                theVendor.getRepName());
            System.out.println("\t\t            " +
                theVendor.getRepPhoneNumber());
       }
    }
```

The remainder of this application provides a utility method used to parse the command line options. From the perspective of this document, this is relatively uninteresting. You can see how this is implemented by looking at:

``` c
DB_INSTALL/examples_java/db/GettingStarted
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.
