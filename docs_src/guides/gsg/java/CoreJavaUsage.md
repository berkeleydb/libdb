---
title: "Database Example"
api-name: "Database Example"
source: docs/gsg/JAVA/CoreJavaUsage.html
---
## Database Example

Throughout this book we will build a couple of applications that load and retrieve inventory data from DB databases. While we are not yet ready to begin reading from or writing to our databases, we can at least create the class that we will use to manage our databases.

Note that subsequent examples in this book will build on this code to perform the more interesting work of writing to and reading from the databases.

Note that you can find the complete implementation of these functions in:

``` c
DB_INSTALL/examples_java/db/GettingStarted
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

**Example 7.1 MyDbs Class**

To manage our database open and close activities, we encapsulate them in the `MyDbs` class. There are several good reasons to do this, the most important being that we can ensure our databases are closed by putting that activity in the `MyDbs` class destructor.

To begin, we import some needed classes:

``` c
// File: MyDbs.java
package db.GettingStarted;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.DatabaseType;

import java.io.FileNotFoundException; 
```

And then we write our class declaration and provided some necessary private data members:

``` c
public class MyDbs {

    // The databases that our application uses
    private Database vendorDb = null;
    private Database inventoryDb = null;

    private String vendordb = "VendorDB.db";
    private String inventorydb = "InventoryDB.db";

    // Our constructor does nothing
    public MyDbs() {} 
```

Next we need a `setup()` method. This is where we configure and open our databases.

``` c
    // The setup() method opens all our databases
    // for us.
    public void setup(String databasesHome)
        throws DatabaseException {

        DatabaseConfig myDbConfig = new DatabaseConfig();

        myDbConfig.setErrorStream(System.err);
        myDbConfig.setErrorPrefix("MyDbs");
        myDbConfig.setType(DatabaseType.BTREE);
        myDbConfig.setAllowCreate(true);

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
        } catch(FileNotFoundException fnfe) {
            System.err.println("MyDbs: " + fnfe.toString());
            System.exit(-1);
        }
    } 
```

Finally, we provide some getter methods, and our `close()` method.

``` c
   // getter methods
    public Database getVendorDB() {
        return vendorDb;
    }

    public Database getInventoryDB() {
        return inventoryDb;
    }

    // Close the databases
    public void close() {
        try {
            if (vendorDb != null) {
                vendorDb.close();
            }

            if (inventoryDb != null) {
                inventoryDb.close();
            }
        } catch(DatabaseException dbe) {
            System.err.println("Error closing MyDbs: " +
                                dbe.toString());
            System.exit(-1);
        }
    }
} 
```
