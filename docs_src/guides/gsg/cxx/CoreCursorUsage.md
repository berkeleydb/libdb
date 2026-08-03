---
title: "Cursor Example"
api-name: "Cursor Example"
source: docs/gsg/CXX/CoreCursorUsage.html
---
## Cursor Example

In <a href="DbCXXUsage.md" class="xref" title="Database Usage Example">Database Usage Example</a> we wrote an application that loaded two databases with vendor and inventory information. In this example, we will write an application to display all of the items in the inventory database. As a part of showing any given inventory item, we will look up the vendor who can provide the item and show the vendor's contact information.

Specifically, the `example_database_read` application does the following:

1.  Opens the the inventory and vendor databases that were created by our `example_database_load` application. See <a href="DbCXXUsage.md#exampledbload-cxx" class="xref" title="Example 3.3 example_database_load">example_database_load</a> for information on how that application creates the databases and writes data to them.

2.  Obtains a cursor from the inventory database.

3.  Steps through the inventory database, displaying each record as it goes.

4.  Gets the name of the vendor for that inventory item from the inventory record.

5.  Uses the vendor name to look up the vendor record in the vendor database.

6.  Displays the vendor record.

Remember that you can find the complete implementation of this application in:

``` c
DB_INSTALL/examples_cxx/getting_started
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

**Example 4.1 example_database_read**

To begin, we include the necessary header files and perform our forward declarations. We also write our `usage()` function.

``` c
// File: example_database_read.cpp
#include <iostream>
#include <fstream>
#include <cstdlib>

#include "MyDb.hpp"
#include "gettingStartedCommon.hpp"

// Forward declarations
int show_all_records(MyDb &inventoryDB, MyDb &vendorDB);
int show_vendor(MyDb &vendorDB, const char *vendor); 
```

Next we write our `main()` function. Note that it is somewhat unnecessarily complicated here because we will be extending it in the next chapter to perform inventory item lookups.

``` c
// Displays all inventory items and the associated vendor record.
int
main (int argc, char *argv[])
{
    // Initialize the path to the database files
    std::string databaseHome("./");

    // Database names
    std::string vDbName("vendordb.db");
    std::string iDbName("inventorydb.db");

    // Parse the command line arguments
    // Omitted for brevity

    try
    {
        // Open all databases.
        MyDb inventoryDB(databaseHome, iDbName);
        MyDb vendorDB(databaseHome, vDbName);

        show_all_records(inventoryDB, vendorDB);
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

Next we need to write the `show_all_records()` function. This function displays all of the inventory records found in the inventory database. Once it shows the inventory record, it retrieves the vendor's name from that record and uses it to look up and display the appropriate vendor record:

``` c
// Shows all the records in the inventory database.
// For each inventory record shown, the appropriate
// vendor record is also displayed.
int
show_all_records(MyDb &inventoryDB, MyDb &vendorDB)
{
    // Get a cursor to the inventory db
    Dbc *cursorp;
    try {
        inventoryDB.getDb().cursor(NULL, &cursorp, 0);

        // Iterate over the inventory database, from the first record
        // to the last, displaying each in turn
        Dbt key, data;
        int ret;
        while ((ret = cursorp->get(&key, &data, DB_NEXT)) == 0 )
        {
            InventoryData inventoryItem(data.get_data());
            inventoryItem.show();

            show_vendor(vendorDB, inventoryItem.getVendor().c_str());
        }
    } catch(DbException &e) {
        inventoryDB.getDb().err(e.get_errno(), 
                                "Error in show_all_records");
        cursorp->close();
        throw e;
    } catch(std::exception &e) {
        cursorp->close();
        throw e;
    }

    cursorp->close();
    return (0);
} 
```

Note that the `InventoryData` class that we use here is described in <a href="DbCXXUsage.md#InventoryData" class="xref" title="Example 3.2 InventoryData Class">InventoryData Class</a>.

Having displayed the inventory record, we now want to display the vendor record corresponding to this record. In this case we do not need to use a cursor to display the vendor record. Using a cursor here complicates our code slightly for no good gain. Instead, we simply perform a `get()` directly against the vendor database.

``` c
// Shows a vendor record. Each vendor record is an instance of
// a vendor structure. See loadVendorDB() in
// example_database_load for how this structure was originally
// put into the database.
int
show_vendor(MyDb &vendorDB, const char *vendor)
{
    Dbt data;
    VENDOR my_vendor;

    try {
        // Set the search key to the vendor's name
        // vendor is explicitly cast to char * to stop a compiler
        // complaint.
        Dbt key((char *)vendor, strlen(vendor) + 1);

        // Make sure we use the memory we set aside for the VENDOR
        // structure rather than the memory that DB allocates.
        // Some systems may require structures to be aligned in memory
        // in a specific way, and DB may not get it right.

        data.set_data(&my_vendor);
        data.set_ulen(sizeof(VENDOR));
        data.set_flags(DB_DBT_USERMEM);

        // Get the record
        vendorDB.getDb().get(NULL, &key, &data, 0);
        std::cout << "        " << my_vendor.street << "\n"
                  << "        " << my_vendor.city << ", "
                  << my_vendor.state << "\n"
                  << "        " << my_vendor.zipcode << "\n"
                  << "        " << my_vendor.phone_number << "\n"
                  << "        Contact: " << my_vendor.sales_rep << "\n"
                  << "                 " << my_vendor.sales_rep_phone
                  << std::endl;

    } catch(DbException &e) {
        vendorDB.getDb().err(e.get_errno(), "Error in show_vendor");
        throw e;
    } catch(std::exception &e) {
        throw e;
    }
    return (0);
} 
```

  

That completes the implementation of `example_database_read()`. In the next chapter, we will extend this application to make use of a secondary database so that we can query the inventory database for a specific inventory item.
