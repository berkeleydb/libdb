---
title: "Database Usage Example"
api-name: "Database Usage Example"
source: docs/gsg/CXX/DbCXXUsage.html
---
## Database Usage Example

In <a href="CoreDbCXXUsage.md" class="xref" title="Database Example">Database Example</a> we created a class that opens and closes a database for us. We now make use of that class to load inventory data into two databases that we will use for our inventory system.

Again, remember that you can find the complete implementation for these functions in:

``` c
DB_INSTALL/examples_cxx/getting_started
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

**Example 3.1 VENDOR Structure**

We want to store data related to an inventory system. There are two types of information that we want to manage: inventory data and related vendor contact information. To manage this information, we could have created a structure for each type of data, but to illustrate storing mixed data without a structure we refrain from creating one for the inventory data.

We now show the definition of the VENDOR structure. Note that the VENDOR structure uses fixed-length fields. This is not necessary and in fact could represent a waste of resources if the number of vendors stored in our database scales to very large numbers. However, for simplicity we use fixed-length fields anyway, especially given that our sample data contains so few vendor records.

``` c
// File: gettingStartedCommon.hpp
#define MAXFIELD 20
typedef struct vendor {
    char name[MAXFIELD];             // Vendor name
    char street[MAXFIELD];           // Street name and number
    char city[MAXFIELD];             // City
    char state[3];                   // Two-digit US state code
    char zipcode[6];                 // US zipcode
    char phone_number[13];           // Vendor phone number
    char sales_rep[MAXFIELD];        // Name of sales representative
    char sales_rep_phone[MAXFIELD];  // Sales rep's phone number
} VENDOR;
```

  
**Example 3.2 InventoryData Class**

In order to manage our actual inventory data, we create a class that encapsulates the data that we want to store for each inventory record. Beyond simple data encapsulation, this class is also capable of marshaling the inventory data into a single contiguous buffer for the purposes of storing in that data in a DB database.

We also provide two constructors for this class. The default constructor simply initializes all our data members for us. A second constructor is also provided that is capable of populating our data members from a `void *`. This second constructor is not really needed until the next chapter where we show how to read data from the databases, but we include it here for the purpose of completeness anyway.

To simplify things a bit, we include the entire implementation for this class in `gettingStartedCommon.hpp` along with our `VENDOR` structure definition.

To begin, we create the public getter and setter methods that we use with our class' private members. We also show the implementation of the method that we use to initialize all our private members.

``` c
class InventoryData
{
public:
    inline void setPrice(double price) {price_ = price;}
    inline void setQuantity(long quantity) {quantity_ = quantity;}
    inline void setCategory(std::string &category) 
                    {category_ = category;}
    inline void setName(std::string &name) {name_ = name;}
    inline void setVendor(std::string &vendor) {vendor_ = vendor;}
    inline void setSKU(std::string &sku) {sku_ = sku;}

    inline double& getPrice() {return(price_);}
    inline long& getQuantity() {return(quantity_);}
    inline std::string& getCategory() {return(category_);}
    inline std::string& getName() {return(name_);}
    inline std::string& getVendor() {return(vendor_);}
    inline std::string& getSKU() {return(sku_);}

    // Initialize our data members
    void clear()
    {
        price_ = 0.0;
        quantity_ = 0;
        category_.clear();
        name_.clear();
        vendor_.clear();
        sku_.clear();
    } 
```

Next we implement our constructors. The default constructor simply calls the `clear()`. The second constructor takes a `void *` as an argument, which it then uses to initialize the data members. Note, again, that we will not actually use this second constructor in this chapter, but we show it here just to be complete anyway.

``` c
    // Default constructor
    InventoryData() { clear(); }

    // Constructor from a void *
    // For use with the data returned from a bdb get
    InventoryData(void *buffer)
    {
        char *buf = (char *)buffer;

        price_ = *((double *)buf);
        bufLen_ = sizeof(double);

        quantity_ = *((long *)(buf + bufLen_));
        bufLen_ += sizeof(long);

        name_ = buf + bufLen_;
        bufLen_ += name_.size() + 1;

        sku_ = buf + bufLen_;
        bufLen_ += sku_.size() + 1;

        category_ = buf + bufLen_;
        bufLen_ += category_.size() + 1;

        vendor_ = buf + bufLen_;
        bufLen_ += vendor_.size() + 1;
    } 
```

Next we provide a couple of methods for returning the class' buffer and the size of the buffer. These are used for actually storing the class' data in a DB database.

``` c
    // Marshalls this classes data members into a single
    // contiguous memory location for the purpose of storing
    // the data in a database.
    char *
    getBuffer()
    {
        // Zero out the buffer
        memset(databuf_, 0, 500);
        // Now pack the data into a single contiguous memory location
        // for storage.
        bufLen_ = 0;
        int dataLen = 0;

        dataLen = sizeof(double);
        memcpy(databuf_, &price_, dataLen);
        bufLen_ += dataLen;

        dataLen = sizeof(long);
        memcpy(databuf_ + bufLen_, &quantity_, dataLen);
        bufLen_ += dataLen;

        packString(databuf_, name_);
        packString(databuf_, sku_);
        packString(databuf_, category_);
        packString(databuf_, vendor_);

        return (databuf_);
    }

    // Returns the size of the buffer. Used for storing
    // the buffer in a database.
    inline int getBufferSize() { return (bufLen_); } 
```

Our last public method is a utility method that we use to get the class to show itself.

``` c
     // Utility function used to show the contents of this class
    void
    show() {
        std::cout << "\nName:         " << name_ << std::endl;
        std::cout << "    SKU:        " << sku_ << std::endl;
        std::cout << "    Price:      " << price_ << std::endl;
        std::cout << "    Quantity:   " << quantity_ << std::endl;
        std::cout << "    Category:   " << category_ << std::endl;
        std::cout << "    Vendor:     " << vendor_ << std::endl;
    } 
```

Finally, we provide a private method that is used to help us pack data into our buffer, and we declare our private data members.

``` c
private:

    // Utility function that appends a char * to the end of
    // the buffer.
    void
    packString(char *buffer, std::string &theString)
    {
        int string_size = theString.size() + 1;
        memcpy(buffer+bufLen_, theString.c_str(), string_size);
        bufLen_ += string_size;
    }

    // Data members
    std::string category_, name_, vendor_, sku_;
    double price_;
    long quantity_;
    int bufLen_;
    char databuf_[500];
}; 
```

  
**Example 3.3 example_database_load**

Our initial sample application loads database information from several flat files. To save space, we won't show all the details of this example program. However, as always you can find the complete implementation for this program here:

``` c
DB_INSTALL/examples_cxx/getting_started
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

We begin with the normal include directives and forward declarations:

``` c
// File: example_database_load.cpp
#include <iostream>
#include <fstream>
#include <cstdlib>

#include "MyDb.hpp"
#include "gettingStartedCommon.hpp" 

// Forward declarations
void loadVendorDB(MyDb&, std::string&);
void loadInventoryDB(MyDb&, std::string&);
```

Next we begin our `main()` function with the variable declarations and command line parsing that is normal for most command line applications:

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

Note that we do not explicitly close our databases here. This is because the databases are encapsulated in `MyDb` class objects, and those objects are on the stack. When they go out of scope, their destructors will cause the database close to occur.

Notice that there is not a lot to this function because we have pushed off all the database activity to other places.

Next we show the implementation of `loadVendorDB()`. We load this data by scanning (line by line) the contents of the `vendors.txt` file into a VENDOR structure. Once we have a line scanned into the structure, we can store that structure into our vendors database.

Note that we use the vendor's name as the key here. In doing so, we assume that the vendor's name is unique in our database. If it was not, we would either have to select a different key, or architect our application such that it could cope with multiple vendor records with the same name.

``` c
// Loads the contents of the vendors.txt file into a database
void
loadVendorDB(MyDb &vendorDB, std::string &vendorFile)
{
    std::ifstream inFile(vendorFile.c_str(), std::ios::in);
    if ( !inFile )
    {
        std::cerr << "Could not open file '" << vendorFile
                  << "'. Giving up." << std::endl;
        throw std::exception();
    }

    VENDOR my_vendor;
    while (!inFile.eof())
    {
        std::string stringBuf;
        std::getline(inFile, stringBuf);
        memset(&my_vendor, 0, sizeof(VENDOR));

        // Scan the line into the structure.
        // Convenient, but not particularly safe.
        // In a real program, there would be a lot more
        // defensive code here.
        sscanf(stringBuf.c_str(),
          "%20[^#]#%20[^#]#%20[^#]#%3[^#]#%6[^#]#%13[^#]#%20[^#]#%20[^\n]",
          my_vendor.name, my_vendor.street,
          my_vendor.city, my_vendor.state,
          my_vendor.zipcode, my_vendor.phone_number,
          my_vendor.sales_rep, my_vendor.sales_rep_phone);

        Dbt key(my_vendor.name, strlen(my_vendor.name) + 1);
        Dbt data(&my_vendor, sizeof(VENDOR));

        vendorDB.getDb().put(NULL, &key, &data, 0);
    }
    inFile.close();
} 
```

Finally, we need to write the `loadInventoryDB()` function. To load the inventory information, we read in each line of the inventory.txt file, obtain each field from it, then we load this data into an `InventoryData` instance.

To help us obtain the various fields from each line of input, we also create a simple helper function that locates the position of the first a field delimiter (a pound (#) sign) from a line of input.

Note that we could have simply decided to store our inventory data in a structure very much like the VENDOR structure that we use above. However, by storing this data in the `InventoryData` class, which identifies the size of the data that it contains, we can use the smallest amount of space possible for the data that we are storing. The result is that our cache can be smaller than it might otherwise be and our database will take less space on disk than if we used a structure with fixed-length fields.

For a trivial dataset such as what we use for these examples, these resource savings are negligible. But if we were storing hundreds of millions of records, then the cost savings may become significant.

``` c
// Used to locate the first pound sign (a field delimiter)
// in the input string.
int
getNextPound(std::string &theString, std::string &substring)
{
    int pos = theString.find("#");
    substring.assign(theString, 0, pos);
    theString.assign(theString, pos + 1, theString.size());
    return (pos);
}

// Loads the contents of the inventory.txt file into a database
void
loadInventoryDB(MyDb &inventoryDB, std::string &inventoryFile)
{
    InventoryData inventoryData;
    std::string substring;
    int nextPound;

    std::ifstream inFile(inventoryFile.c_str(), std::ios::in);
    if (!inFile)
    {
        std::cerr << "Could not open file '" << inventoryFile
                  << "'. Giving up." << std::endl;
        throw std::exception();
    }

    while (!inFile.eof())
    {
        inventoryData.clear();
        std::string stringBuf;
        std::getline(inFile, stringBuf);

        // Now parse the line
        if (!stringBuf.empty())
        {
            nextPound = getNextPound(stringBuf, substring);
            inventoryData.setName(substring);

            nextPound = getNextPound(stringBuf, substring);
            inventoryData.setSKU(substring);

            nextPound = getNextPound(stringBuf, substring);
            inventoryData.setPrice(strtod(substring.c_str(), 0));

            nextPound = getNextPound(stringBuf, substring);
            inventoryData.setQuantity(strtol(substring.c_str(), 0, 10));

            nextPound = getNextPound(stringBuf, substring);
            inventoryData.setCategory(substring);

            nextPound = getNextPound(stringBuf, substring);
            inventoryData.setVendor(substring);

            void *buff = (void *)inventoryData.getSKU().c_str();
            int size = inventoryData.getSKU().size()+1;
            Dbt key(buff, size);

            buff = inventoryData.getBuffer();
            size = inventoryData.getBufferSize();
            Dbt data(buff, size);

            inventoryDB.getDb().put(NULL, &key, &data, 0);
        }
    }
    inFile.close();
} 
```

In the next chapter we provide an example that shows how to read the inventory and vendor databases.
