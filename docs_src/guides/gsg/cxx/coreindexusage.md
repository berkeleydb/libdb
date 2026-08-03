---
title: "Secondary Database Example"
api-name: "Secondary Database Example"
source: docs/gsg/CXX/coreindexusage.html
---
## Secondary Database Example

<span class="sect2"> [Secondary Databases with example_database_load](coreindexusage.md#edlWIndexes) </span>

<span class="sect2"> [Secondary Databases with example_database_read](coreindexusage.md#edrWIndexes) </span>

In previous chapters in this book, we built applications that load and display several DB databases. In this example, we will extend those examples to use secondary databases. Specifically:

- In <a href="DbCXXUsage.md" class="xref" title="Database Usage Example">Database Usage Example</a> we built an application that can open and load data into several databases. In <a href="coreindexusage.md#edlWIndexes" class="xref" title="Secondary Databases with example_database_load">Secondary Databases with example_database_load</a> we will extend that application to also open a secondary database for the purpose of indexing inventory item names.

- In <a href="CoreCursorUsage.md" class="xref" title="Cursor Example">Cursor Example</a> we built an application to display our inventory database (and related vendor information). In <a href="coreindexusage.md#edrWIndexes" class="xref" title="Secondary Databases with example_database_read">Secondary Databases with example_database_read</a> we will extend that application to show inventory records based on the index we cause to be loaded using `example_database_load`.

### Secondary Databases with example_database_load

In order to update `example_database_load` to maintain an index of inventory item names, all we really need to do is:

1.  Create a new database to be used as a secondary database.

2.  Associate our new database to the inventory primary database.

We also need a function that can create our secondary keys for us.

Because DB maintains secondary databases for us; once this work is done we need not make any other changes to `example_database_load`.

Remember that you can find the complete implementation of these functions in:

``` c
DB_INSTALL/examples_cxx/getting_started
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

To begin, we go to `gettingStartedCommon.hpp` and we write our secondary key extractor function. This is a fairly trivial function to write because we have already done most of the work when we wrote the `InventoryData` class. Recall that when we wrote that class, we provided a constructor that accepts a pointer to a buffer and unpacks the contents of the buffer for us (see <a href="DbCXXUsage.md#InventoryData" class="xref" title="Example 3.2 InventoryData Class">InventoryData Class</a> for the implementation). We now make use of that constructor.

``` c
// File: gettingStartedCommon.hpp
// Forward declarations
class Db;
class Dbt;

// Used to extract an inventory item's name from an
// inventory database record. This function is used to create
// keys for secondary database records.
int
get_item_name(Db *dbp, const Dbt *pkey, const Dbt *pdata, Dbt *skey)
{
    // Obtain the buffer location where the we placed the item's name. In
    // this example, the item's name is located in the primary data. It is
    // the first string in the buffer after the price (a double) and
    // the quantity (a long).
    size_t offset = sizeof(double) + sizeof(long);
    char * itemname = (char *)pdata->get_data() + offset;

    // unused
    (void)pkey;

    // If the offset is beyond the end of the data, then there is a
    // problem with the buffer contained in pdata, or there's a
    // programming error in how the buffer is marshalled/unmarshalled.
    // This should never happen!
    if (offset > pdata->get_size()) {
        dbp->errx("get_item_name: buffer sizes do not match!");
        // When we return non-zero, the index record is not
        // added/updated.
        return (-1);
    }
    // Now set the secondary key's data to be the item name 

    skey->set_data(itemname);
    skey->set_size(strlen(itemname) + 1);

    return (0);
}; 
```

Having written our key extractor callback, we now need to make a trivial update to our `MyDb` implementation. Because an item name is used by multiple inventory records, we need our secondary database to support sorted duplicates. We therefore must update `MyDb` to handle this detail.

The `MyDb` class definition changes to add a boolean to the constructor (remember that new code is in **`bold`**):

``` c
// File: MyDb.hpp
#include <db_cxx.h>

class MyDb
{
public:
    // Constructor requires a path to the database,
    // and a database name.
    MyDb(std::string &path, std::string &dbName,
         bool isSecondary = false);

    // Our destructor just calls our private close method.
    ~MyDb() { close(); }

    inline Db &getDb() {return db_;}

private:
    Db db_;
    std::string dbFileName_;
    u_int32_t cFlags_;

    // Make sure the default constructor is private
    // We don't want it used.
    MyDb() : db_(0, 0) {}

    // We put our database close activity here.
    // This is called from our destructor. In
    // a more complicated example, we might want
    // to make this method public, but a private
    // method is more appropriate for this example.
    void close();
}; 
```

And the implementation changes slightly to take advantage of the new boolean. Note that to save space, we just show the constructor where the code actually changes:

``` c
// File: MyDb.cpp
#include "MyDb.hpp"

// Class constructor. Requires a path to the location
// where the database is located, and a database name
MyDb::MyDb(std::string &path, std::string &dbName,
           bool isSecondary)
    : db_(NULL, 0),               // Instantiate Db object
      dbFileName_(path + dbName), // Database file name
      cFlags_(DB_CREATE)          // If the database doesn't yet exist,
                                  // allow it to be created.
{
    try
    {
        // Redirect debugging information to std::cerr
        db_.set_error_stream(&std::cerr);

        // If this is a secondary database, support
        // sorted duplicates
        if (isSecondary)
            db_.set_flags(DB_DUPSORT);

        // Open the database
        db_.open(NULL, dbFileName_.c_str(), NULL, DB_BTREE, cFlags_, 0);
    }
    // DbException is not a subclass of std::exception, so we
    // need to catch them both.
    catch(DbException &e)
    {
        std::cerr << "Error opening database: " << dbFileName_ << "\n";
        std::cerr << e.what() << std::endl;
    }
    catch(std::exception &e)
    {
        std::cerr << "Error opening database: " << dbFileName_ << "\n";
        std::cerr << e.what() << std::endl;
    }
} 
```

That done, we can now update `example_database_load` to open our new secondary database and associate it to the inventory database.

To save space, we do not show the entire implementation for this program here. Instead, we show just the `main()` function, which is where all our modifications occur. To see the rest of the implementation for this command, see <a href="DbCXXUsage.md#exampledbload-cxx" class="xref" title="Example 3.3 example_database_load">example_database_load</a>.

``` c
// Loads the contents of vendors.txt and inventory.txt into
// Berkeley DB databases.
int
main(int argc, char *argv[])
{
    // Initialize the path to the database files
    std::string basename("./");
    std::string databaseHome("./");

    // Database names
    std::string vDbName("vendordb.db");
    std::string iDbName("inventorydb.db");
    std::string itemSDbName("itemname.sdb");

    // Parse the command line arguments here and determine
    // the location of the flat text files containing the
    // inventory data here. This step is omitted for clarity.

    //  Identify the full name for our input files, which should
    //  also include some path information.
    std::string inventoryFile = basename + "inventory.txt";
    std::string vendorFile = basename + "vendors.txt";

    try
    {
        // Open all databases.
        MyDb inventoryDB(databaseHome, iDbName);
        MyDb vendorDB(databaseHome, vDbName);
        MyDb itemnameSDB(databaseHome, itemSDbName, true);

        // Associate the primary and the secondary
        inventoryDB.getDb().associate(NULL,
                                      &(itemnameSDB.getDb()),
                                      get_item_name,
                                      0);

        // Load the vendor database
        loadVendorDB(vendorDB, vendorFile);

        // Load the inventory database
        loadInventoryDB(inventoryDB, inventoryFile);
    } catch(DbException &e) {
        std::cerr << "Error loading databases. " << std::endl;
        std::cerr << e.what() << std::endl;
        return(e.get_errno());
    } catch(std::exception &e) {
        std::cerr << "Error loading databases. " << std::endl;
        std::cerr << e.what() << std::endl;
        return(-1);
    }

    return(0);
} // End main 
```

Note that the order in which we instantiate our `MyDb` class instances is important. In general you want to close a secondary database before closing the primary with which it is associated. This is particularly true for multi-threaded or multi-processed applications where the database closes are not single threaded. Even so, it is a good habit to adopt, even for simple applications such as this one. Here, we ensure that the databases are closed in the desired order by opening the secondary database last. This works because our `MyDb` objects are on the stack, and therefore the last one opened is the first one closed.

That completes our update to `example_database_load`. Now when this program is called, it will automatically index inventory items based on their names. We can then query for those items using the new index. We show how to do that in the next section.

### Secondary Databases with example_database_read

In <a href="CoreCursorUsage.md" class="xref" title="Cursor Example">Cursor Example</a> we wrote an application that displays every inventory item in the Inventory database. In this section, we will update that example to allow us to search for and display an inventory item given a specific name. To do this, we will make use of the secondary database that `example_database_load` now creates.

The update to `example_database_read` is relatively modest. We need to open the new secondary database in exactly the same way was we do for `example_database_load`. We also need to add a command line parameter on which we can specify the item name, and we will need a new function in which we will perform the query and display the results.

To begin, we add a single forward declaration to the application, and update our usage function slightly:

``` c
// File: example_database_read.cpp
#include <iostream>
#include <fstream>
#include <cstdlib>

#include "MyDb.hpp"
#include "gettingStartedCommon.hpp"

// Forward declarations
int show_all_records(MyDb &inventoryDB, MyDb &vendorDB);
int show_item(MyDb &itemnameSDB, MyDb &vendorDB, std::string &itemName);
int show_vendor(MyDb &vendorDB, const char *vendor); 
```

Next, we update `main()` to open the new secondary database and accept the new command line switch. We also need a new variable to contain the item's name.

The final update to the `main()` entails a little bit of logic to determine whether we want to display all available inventory items, or just the ones that match a name provided on the `-i` command line parameter.

``` c
// Displays all inventory items and the associated vendor record.
int
main (int argc, char *argv[])
{
    // Initialize the path to the database files
    std::string databaseHome("./");
    std::string itemName;

    // Database names
    std::string vDbName("vendordb.db");
    std::string iDbName("inventorydb.db");
    std::string itemSDbName("itemname.sdb");

    // Parse the command line arguments
    // Omitted for brevity

    try
    {
        // Open all databases.
        MyDb inventoryDB(databaseHome, iDbName);
        MyDb vendorDB(databaseHome, vDbName);
        MyDb itemnameSDB(databaseHome, itemSDbName, true);

        // Associate the secondary to the primary
        inventoryDB.getDb().associate(NULL,
                                      &(itemnameSDB.getDb()),
                                      get_item_name,
                                      0);

        if (itemName.empty())
        {
            show_all_records(inventoryDB, vendorDB);
        } else {
            show_item(itemnameSDB, vendorDB, itemName);
        }
    } catch(DbException &e) {
        std::cerr << "Error reading databases. " << std::endl;
        std::cerr << e.what() << std::endl;
        return(e.get_errno());
    } catch(std::exception &e) {
        std::cerr << "Error reading databases. " << std::endl;
        std::cerr << e.what() << std::endl;
        return(-1);
    }

    return(0);
} // End main 
```

The only other thing that we need to add to the application is the implementation of the `show_item()` function.

### Note

In the interest of space, we refrain from showing the other functions used by this application. For their implementation, please see <a href="CoreCursorUsage.md" class="xref" title="Cursor Example">Cursor Example</a>. Alternatively, you can see the entire implementation of this application in:

``` c
DB_INSTALL/examples_cxx/getting_started
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

``` c
// Shows the records in the inventory database that
// have a specific item name. For each inventory record
// shown, the appropriate vendor record is also displayed.
int
show_item(MyDb &itemnameSDB, MyDb &vendorDB, std::string &itemName)
{
    // Get a cursor to the itemname secondary db
    Dbc *cursorp;

    try {
        itemnameSDB.getDb().cursor(NULL, &cursorp, 0);

        // Get the search key. This is the name on the inventory
        // record that we want to examine.
        std::cout << "Looking for " << itemName << std::endl;
        Dbt key((void *)itemName.c_str(), itemName.length() + 1);
        Dbt data;

        // Position the cursor to the first record in the secondary
        // database that has the appropriate key.
        int ret = cursorp->get(&key, &data, DB_SET);
        if (!ret) {
            do {
                InventoryData inventoryItem(data.get_data());
                inventoryItem.show();

                show_vendor(vendorDB, inventoryItem.getVendor().c_str());

            } while(cursorp->get(&key, &data, DB_NEXT_DUP) == 0);
        } else {
            std::cerr << "No records found for '" << itemName
                      << "'" << std::endl;
        }
    } catch(DbException &e) {
        itemnameSDB.getDb().err(e.get_errno(), "Error in show_item");
        cursorp->close();
        throw e;
    } catch(std::exception &e) {
        itemnameSDB.getDb().errx("Error in show_item: %s", e.what());
        cursorp->close();
        throw e;
    }

    cursorp->close();
    return (0);
}
```

This completes our update to `example_inventory_read`. Using this update, you can now search for and show all inventory items that match a particular name. For example:

``` c
    example_inventory_read -i "Zulu Nut"
```
