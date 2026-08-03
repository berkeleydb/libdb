---
title: "Database Usage Example"
api-name: "Database Usage Example"
source: docs/gsg/JAVA/dbtJavaUsage.html
---
## Database Usage Example

In <a href="CoreJavaUsage.md#MyDb" class="xref" title="Example 7.1 MyDbs Class">MyDbs Class</a> we created a class that opens and closes databases for us. We now make use of that class to load inventory data into two databases that we will use for our inventory system.

Again, remember that you can find the complete implementation for these functions in:

``` c
DB_INSTALL/examples_java/db/GettingStarted
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

Note that in this example, we are going to save two types of information. First there are a series of inventory records that identify information about some food items (fruits, vegetables, and desserts). These records identify particulars about each item such as the vendor that the item can be obtained from, how much the vendor has in stock, the price per unit, and so forth.

We also want to manage vendor contact information, such as the vendor's address and phone number, the sales representative's name and his phone number, and so forth.

**Example 8.1 Inventory.java**

All Inventory data is encapsulated in an instance of the following class. Note that because this class is not serializable, we need a custom tuple binding in order to place it on a `DatabaseEntry` object. Because the `TupleInput` and `TupleOutput` classes used by custom tuple bindings support Java numerical types and not Java numerical classes, we use `int` and `float` here instead of the corresponding `Integer` and `Float` classes.

``` c
// File Inventory.java
package db.GettingStarted;

public class Inventory {

    private String sku;
    private String itemName;
    private String category;
    private String vendor;
    private int vendorInventory;
    private float vendorPrice;

    public void setSku(String data) {
            sku = data;
    }

    public void setItemName(String data) {
            itemName = data;
    }

    public void setCategory(String data) {
            category = data;
    }

    public void setVendorInventory(int data) {
            vendorInventory = data;
    }

    public void setVendor(String data) {
            vendor = data;
    }

    public void setVendorPrice(float data) {
            vendorPrice = data;
    }

    public String getSku() { return sku; }
    public String getItemName() { return itemName; }
    public String getCategory() { return category; }
    public int getVendorInventory() { return vendorInventory; }
    public String getVendor() { return vendor; }
    public float getVendorPrice() { return vendorPrice; }

} 
```

  
**Example 8.2 Vendor.java**

The data for vendor records are stored in instances of the following class. Notice that we are using serialization with this class for no other reason than to demonstrate serializing a class instance.

``` c
// File Vendor.java
package db.GettingStarted;

import java.io.Serializable;

public class Vendor implements Serializable {

    private String repName;
    private String address;
    private String city;
    private String state;
    private String zipcode;
    private String bizPhoneNumber;
    private String repPhoneNumber;
    private String vendor;

    public void setRepName(String data) {
        repName = data;
    }

    public void setAddress(String data) {
        address = data;
    }

    public void setCity(String data) {
        city = data;
    }

    public void setState(String data) {
        state = data;
    }

    public void setZipcode(String data) {
        zipcode = data;
    }

    public void setBusinessPhoneNumber(String data) {
        bizPhoneNumber = data;
    }

    public void setRepPhoneNumber(String data) {
        repPhoneNumber = data;
    }

    public void setVendorName(String data) {
        vendor = data;
    }

    ...
    // Corresponding getter methods omitted for brevity.
    // See examples/je/gettingStarted/Vendor.java
    // for a complete implementation of this class.

} 
```

  

Because we will not be using serialization to convert our `Inventory` objects to a `DatabaseEntry` object, we need a custom tuple binding:

**Example 8.3 InventoryBinding.java**

``` c
// File InventoryBinding.java
package db.GettingStarted;

import com.sleepycat.bind.tuple.TupleBinding;
import com.sleepycat.bind.tuple.TupleInput;
import com.sleepycat.bind.tuple.TupleOutput;

public class InventoryBinding extends TupleBinding {

    // Implement this abstract method. Used to convert
    // a DatabaseEntry to an Inventory object.
    public Object entryToObject(TupleInput ti) {

        String sku = ti.readString();
        String itemName = ti.readString();
        String category = ti.readString();
        String vendor = ti.readString();
        int vendorInventory = ti.readInt();
        float vendorPrice = ti.readFloat();

        Inventory inventory = new Inventory();
        inventory.setSku(sku);
        inventory.setItemName(itemName);
        inventory.setCategory(category);
        inventory.setVendor(vendor);
        inventory.setVendorInventory(vendorInventory);
        inventory.setVendorPrice(vendorPrice);

        return inventory;
    }

    // Implement this abstract method. Used to convert a
    // Inventory object to a DatabaseEntry object.
    public void objectToEntry(Object object, TupleOutput to) {

        Inventory inventory = (Inventory)object;

        to.writeString(inventory.getSku());
        to.writeString(inventory.getItemName());
        to.writeString(inventory.getCategory());
        to.writeString(inventory.getVendor());
        to.writeInt(inventory.getVendorInventory());
        to.writeFloat(inventory.getVendorPrice());
    }
} 
```

  

In order to store the data identified above, we write the `ExampleDatabaseLoad` application. This application loads the inventory and vendor databases for you.

Inventory information is stored in a `Database` dedicated for that purpose. The key for each such record is a product SKU. The inventory data stored in this database are objects of the `Inventory` class (see <a href="dbtJavaUsage.md#inventoryjava" class="xref" title="Example 8.1 Inventory.java">Inventory.java</a> for more information). `ExampleDatabaseLoad` loads the inventory database as follows:

1.  Reads the inventory data from a flat text file prepared in advance for this purpose.

2.  Uses `java.lang.String` to create a key based on the item's SKU.

3.  Uses an `Inventory` class instance for the record data. This object is stored on a `DatabaseEntry` object using `InventoryBinding`, a custom tuple binding that we implemented above.

4.  Saves each record to the inventory database.

Vendor information is also stored in a `Database` dedicated for that purpose. The vendor data stored in this database are objects of the `Vendor` class (see <a href="dbtJavaUsage.md#vendorjava" class="xref" title="Example 8.2 Vendor.java">Vendor.java</a> for more information). To load this `Database`, `ExampleDatabaseLoad` does the following:

1.  Reads the vendor data from a flat text file prepared in advance for this purpose.

2.  Uses the vendor's name as the record's key.

3.  Uses a `Vendor` class instance for the record data. This object is stored on a `DatabaseEntry` object using `com.sleepycat.bind.serial.SerialBinding`.

**Example 8.4 Stored Class Catalog Management with MyDbs**

Before we can write `ExampleDatabaseLoad`, we need to update `MyDbs.java` to support the class catalogs that we need for this application.

To do this, we start by importing an additional class to support stored class catalogs:

``` c
// File: MyDbs.java
package db.GettingStarted;

import com.sleepycat.bind.serial.StoredClassCatalog;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.DatabaseType;

import java.io.FileNotFoundException; 
```

We also need to add two additional private data members to this class. One supports the database used for the class catalog, and the other is used as a handle for the class catalog itself.

``` c
public class MyDbs {

    // The databases that our application uses
    private Database vendorDb = null;
    private Database inventoryDb = null;
    private Database classCatalogDb = null;

    // Needed for object serialization
    private StoredClassCatalog classCatalog;

    private String vendordb = "VendorDB.db";
    private String inventorydb = "InventoryDB.db";
    private String classcatalogdb = "ClassCatalogDB.db";

    // Our constructor does nothing
    public MyDbs() {} 
```

Next we need to update the `MyDbs.setup()` method to open the class catalog database and create the class catalog.

``` c
    // The setup() method opens all our databases
    // for us.
    public void setup(String databasesHome)
        throws DatabaseException {

        DatabaseConfig myDbConfig = new DatabaseConfig();

        ...
        // Database configuration omitted for brevity
        ...

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
    } 
```

Finally we need a getter method to return the class catalog. Note that we do not provide a getter for the catalog database itself – our application has no need for that.

We also update our `close()` to close our class catalog.

``` c
   // getter methods
    public Database getVendorDB() {
        return vendorDb;
    }

    public Database getInventoryDB() {
        return inventoryDb;
    }

    public StoredClassCatalog getClassCatalog() {
        return classCatalog;
    }
```

Finally, we need our `close()` method:

``` c

    // Close the databases
    public void close() {
        try {
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

  

So far we have identified the data that we want to store in our databases and how we will convert that data in and out of `DatabaseEntry` objects for database storage. We have also updated `MyDbs` to manage our databases for us. Now we write `ExampleDatabaseLoad` to actually put the inventory and vendor data into their respective databases. Because of the work that we have done so far, this application is actually fairly simple to write.

**Example 8.5 ExampleDatabaseLoad.java**

First we need the usual series of import statements:

``` c
// File: ExampleDatabaseLoad.java
package db.GettingStarted;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import com.sleepycat.bind.EntryBinding;
import com.sleepycat.bind.serial.SerialBinding;
import com.sleepycat.bind.tuple.TupleBinding;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException; 
```

Next comes the class declaration and the private data members that we need for this class. Most of these are setting up default values for the program.

Note that two `DatabaseEntry` objects are instantiated here. We will reuse these for every database operation that this program performs. Also a `MyDbEnv` object is instantiated here. We can do this because its constructor never throws an exception. See <a href="dbtJavaUsage.md#dbsStoredClass" class="xref" title="Example 8.4 Stored Class Catalog Management with MyDbs">Stored Class Catalog Management with MyDbs</a> for its implementation details.

Finally, the `inventory.txt` and `vendors.txt` file can be found in the GettingStarted examples directory along with the classes described in this extended example.

``` c
public class ExampleDatabaseLoad {

    private static String myDbsPath = "./";
    private static File inventoryFile = new File("./inventory.txt");
    private static File vendorsFile = new File("./vendors.txt");

    // DatabaseEntries used for loading records
    private static DatabaseEntry theKey = new DatabaseEntry();
    private static DatabaseEntry theData = new DatabaseEntry();

    // Encapsulates the databases.
    private static MyDbs myDbs = new MyDbs(); 
```

Next comes the `usage()` and `main()` methods. Notice the exception handling in the `main()` method. This is the only place in the application where we catch exceptions. For this reason, we must catch `DatabaseException` which is thrown by the `com.sleepycat.db.*` classes.

Also notice the call to `MyDbs.close()` in the `finally` block. This is the only place in the application where `MyDbs.close()` is called. `MyDbs.close()` is responsible for closing all open `Database` handles for you.

``` c
    private static void usage() {
        System.out.println("ExampleDatabaseLoad [-h <database home>]");
        System.out.println("      [-s <selections file>]");
        System.out.println("      [-v <vendors file>]");
        System.exit(-1);
    }

    public static void main(String args[]) {
        ExampleDatabaseLoad edl = new ExampleDatabaseLoad();
        try {
            edl.run(args);
        } catch (DatabaseException dbe) {
            System.err.println("ExampleDatabaseLoad: " + dbe.toString());
            dbe.printStackTrace();
        } catch (Exception e) {
            System.out.println("Exception: " + e.toString());
            e.printStackTrace();
        } finally {
            myDbs.close();
        }
        System.out.println("All done.");
    } 
```

Next we write the `ExampleDatabaseLoad.run()` method. This method is responsible for initializing all objects. Because our environment and databases are all opened using the `MyDbs.setup()` method, `ExampleDatabaseLoad.run()` method is only responsible for calling `MyDbs.setup()` and then calling the `ExampleDatabaseLoad` methods that actually load the databases.

``` c
    private void run(String args[]) throws DatabaseException {
        // Parse the arguments list
        parseArgs(args);

        myDbs.setup(myDbsPath); // path to the environment home

        System.out.println("loading vendors db.");
        loadVendorsDb();
        System.out.println("loading inventory db.");
        loadInventoryDb();
    } 
```

This next method loads the vendor database. This method uses serialization to convert the `Vendor` object to a `DatabaseEntry` object.

``` c
   private void loadVendorsDb() 
            throws DatabaseException {

        // loadFile opens a flat-text file that contains our data
        // and loads it into a list for us to work with. The integer
        // parameter represents the number of fields expected in the
        // file.
        List vendors = loadFile(vendorsFile, 8);

        // Now load the data into the database. The vendor's name is the
        // key, and the data is a Vendor class object.

        // Need a serial binding for the data
        EntryBinding dataBinding =
            new SerialBinding(myDbs.getClassCatalog(), Vendor.class);

        for (int i = 0; i < vendors.size(); i++) {
            String[] sArray = (String[])vendors.get(i);
            Vendor theVendor = new Vendor();
            theVendor.setVendorName(sArray[0]);
            theVendor.setAddress(sArray[1]);
            theVendor.setCity(sArray[2]);
            theVendor.setState(sArray[3]);
            theVendor.setZipcode(sArray[4]);
            theVendor.setBusinessPhoneNumber(sArray[5]);
            theVendor.setRepName(sArray[6]);
            theVendor.setRepPhoneNumber(sArray[7]);

            // The key is the vendor's name.
            // ASSUMES THE VENDOR'S NAME IS UNIQUE!
            String vendorName = theVendor.getVendorName();
            try {
                theKey = new DatabaseEntry(vendorName.getBytes("UTF-8"));
            } catch (IOException willNeverOccur) {}

            // Convert the Vendor object to a DatabaseEntry object
            // using our SerialBinding
            dataBinding.objectToEntry(theVendor, theData);

            // Put it in the database.
            myDbs.getVendorDB().put(null, theKey, theData);
        }
    } 
```

Now load the inventory database. This method uses our custom tuple binding (see <a href="dbtJavaUsage.md#InventoryJavaBinding" class="xref" title="Example 8.3 InventoryBinding.java">InventoryBinding.java</a>) to convert the `Inventory` object to a `DatabaseEntry` object.

``` c
    private void loadInventoryDb() 
        throws DatabaseException {

        // loadFile opens a flat-text file that contains our data
        // and loads it into a list for us to work with. The integer
        // parameter represents the number of fields expected in the
        // file.
        List inventoryArray = loadFile(inventoryFile, 6);

        // Now load the data into the database. The item's sku is the
        // key, and the data is an Inventory class object.

        // Need a tuple binding for the Inventory class.
        TupleBinding inventoryBinding = new InventoryBinding();

        for (int i = 0; i < inventoryArray.size(); i++) {
            String[] sArray = (String[])inventoryArray.get(i);
            String sku = sArray[1];
            try {
                theKey = new DatabaseEntry(sku.getBytes("UTF-8"));
            } catch (IOException willNeverOccur) {}

            Inventory theInventory = new Inventory();
            theInventory.setItemName(sArray[0]);
            theInventory.setSku(sArray[1]);
            Float price = new Float(sArray[2]);
            theInventory.setVendorPrice(price.floatValue());
            Integer vInventory = new Integer(sArray[3]);
            theInventory.setVendorInventory(vInventory.intValue());
            theInventory.setCategory(sArray[4]);
            theInventory.setVendor(sArray[5]);

            // Place the Vendor object on the DatabaseEntry object using 
            // our the tuple binding we implemented in 
            // InventoryBinding.java
            inventoryBinding.objectToEntry(theInventory, theData);

            // Put it in the database. Note that this causes our 
            // secondary database to be automatically updated for us.
            myDbs.getInventoryDB().put(null, theKey, theData);
        }
    }
```

The remainder of this application provides utility methods to read a flat text file into an array of strings and parse the command line options:

``` c
    private static void parseArgs(String args[]) {
        // Implementation omitted for brevity.
    }

    private List loadFile(File theFile, int numFields) {
        List records = new ArrayList();
        // Implementation omitted for brevity.
        return records;
    }

    protected ExampleDatabaseLoad() {}
} 
```

From the perspective of this document, these things are relatively uninteresting. You can see how they are implemented by looking at `ExampleDatabaseLoad.java` in:

``` c
DB_INSTALL/examples_java/db/GettingStarted
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.
