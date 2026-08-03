---
title: "Cursor Example"
api-name: "Cursor Example"
source: docs/gsg/C/CoreCursorUsage.html
---
## Cursor Example

In <a href="DbUsage.md" class="xref" title="Database Usage Example">Database Usage Example</a> we wrote an application that loaded two databases with vendor and inventory information. In this example, we will write an application to display all of the items in the inventory database. As a part of showing any given inventory item, we will look up the vendor who can provide the item and show the vendor's contact information.

Specifically, the `example_database_read` application does the following:

1.  Opens the the inventory and vendor databases that were created by our `example_database_load` application. See <a href="DbUsage.md#exampledbload" class="xref" title="Example 3.2 example_database_load">example_database_load</a> for information on how that application creates the databases and writes data to them.

2.  Obtains a cursor from the inventory database.

3.  Steps through the inventory database, displaying each record as it goes.

4.  Gets the name of the vendor for that inventory item from the inventory record.

5.  Uses the vendor name to look up the vendor record in the vendor database.

6.  Displays the vendor record.

Remember that you can find the complete implementation of this application in:

``` c
DB_INSTALL/examples_c/getting_started
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

**Example 4.1 example_database_read**

To begin, we include the necessary header files and perform our forward declarations.

``` c
/* File: example_database_read.c */
/* gettingstarted_common.h includes db.h for us */
#include "gettingstarted_common.h"

/* Forward declarations */
char * show_inventory_item(void *);
int show_all_records(STOCK_DBS *);
int show_records(STOCK_DBS *, char *);
int show_vendor_record(char *, DB *); 
```

Next we write our `main()` function. Note that it is somewhat unnecessarily complicated here because we will be extending it in the next chapter to perform inventory item lookups.

``` c
/*
 * Displays all inventory items and the associated vendor record.
 */
int
main(int argc, char *argv[])
{
    STOCK_DBS my_stock;
    int ret;

    /* Initialize the STOCK_DBS struct */
    initialize_stockdbs(&my_stock);

    /*
     * Parse the command line arguments here and determine
     * the location of the database files. This step is
     * omitted for brevity.
     */

    /*
     * Identify the files that will hold our databases
     * This function uses information obtained from the
     * command line to identify the directory in which
     * the database files reside.
     */
    set_db_filenames(&my_stock);

    /* Open all databases */
    ret = databases_setup(&my_stock, "example_database_read", stderr);
    if (ret != 0) {
        fprintf(stderr, "Error opening databases\n");
        databases_close(&my_stock);
        return (ret);
    }

    ret = show_all_records(&my_stock);

    /* close our databases */
    databases_close(&my_stock);
    return (ret);
} 
```

Next we need to write the `show_all_records()` function. This function takes a `STOCK_DBS` structure and displays all of the inventory records found in the inventory database. Once it shows the inventory record, it retrieves the vendor's name from that record and uses it to look up and display the appropriate vendor record:

``` c
int show_all_records(STOCK_DBS *my_stock)
{
    DBC *cursorp;
    DBT key, data;
    char *the_vendor;
    int exit_value, ret;

    /* Initialize our DBTs. */
    memset(&key, 0, sizeof(DBT));
    memset(&data, 0, sizeof(DBT));

    /* Get a cursor to the itemname db */
    my_stock->inventory_dbp->cursor(my_stock->inventory_dbp, NULL,
      &cursorp, 0);

    /*
     * Iterate over the inventory database, from the first record
     * to the last, displaying each in turn.
     */
    exit_value = 0;
    while ((ret =
      cursorp->get(cursorp, &key, &data, DB_NEXT))
      == 0)
    {
        the_vendor = show_inventory_item(data.data);
        ret = show_vendor_record(the_vendor, my_stock->vendor_dbp);
        if (ret) {
            exit_value = ret;
            break;
        }
    }

    /* Close the cursor */
    cursorp->close(cursorp);
    return(exit_value);
} 
```

The `show_inventory_item()` simply extracts the inventory information from the record data and displays it. It then returns the vendor's name. Note that in order to extract the inventory information, we have to unpack it from the data buffer. How we do this is entirely dependent on how we packed the buffer in the first place. For more information, see the `load_inventory_database()` function implementation in <a href="DbUsage.md#exampledbload" class="xref" title="Example 3.2 example_database_load">example_database_load</a>.

``` c
/*
 * Shows an inventory item.
 */
char *
show_inventory_item(void *vBuf)
{
    float price;
    int buf_pos, quantity;
    char *category, *name, *sku, *vendor_name;
    char *buf = (char *)vBuf;

    /* Get the price.  */
    price = *((float *)buf);
    buf_pos = sizeof(float);

    /* Get the quantity. */
    quantity = *((int *)(buf + buf_pos));
    buf_pos += sizeof(int);

    /* Get the inventory item's name */
    name = buf + buf_pos;
    buf_pos += strlen(name) + 1;

    /* Get the inventory item's sku */
    sku = buf + buf_pos;
    buf_pos += strlen(sku) + 1;

    /* 
     * Get the category (fruits, vegetables, desserts) that this 
     * item belongs to.
     */
    category = buf + buf_pos;
    buf_pos += strlen(category) + 1;

    /* Get the vendor's name */
    vendor_name = buf + buf_pos;

    /* Display all this information */
    printf("name: %s\n", name);
    printf("\tSKU: %s\n", sku);
    printf("\tCategory: %s\n", category);
    printf("\tPrice: %.2f\n", price);
    printf("\tQuantity: %i\n", quantity);
    printf("\tVendor:\n");

    /* Return the vendor's name */
    return(vendor_name);
} 
```

Having returned the vendor's name, we can now use it to look up and display the appropriate vendor record. In this case we do not need to use a cursor to display the vendor record. Using a cursor here complicates our code slightly for no good gain. Instead, we simply perform a `get()` directly against the vendor database.

``` c
/*
 * Shows a vendor record. Each vendor record is an instance of
 * a vendor structure. See load_vendor_database() in
 * example_database_load for how this structure was originally
 * put into the database.
 */
int
show_vendor_record(char *vendor_name, DB *vendor_dbp)
{
    DBT key, data;
    VENDOR my_vendor;
    int ret;

    /* Zero our DBTs */
    memset(&key, 0, sizeof(DBT));
    memset(&data, 0, sizeof(DBT));

    /* Set the search key to the vendor's name */
    key.data = vendor_name;
    key.size = strlen(vendor_name) + 1;

    /*
     * Make sure we use the memory we set aside for the VENDOR
     * structure rather than the memory that DB allocates.
     * Some systems may require structures to be aligned in memory
     * in a specific way, and DB may not get it right.
     */

    data.data = &my_vendor;
    data.ulen = sizeof(VENDOR);
    data.flags = DB_DBT_USERMEM;

    /* Get the record */
    ret = vendor_dbp->get(vendor_dbp, 0, &key, &data, 0);
    if (ret != 0) {
        vendor_dbp->err(vendor_dbp, ret, 
            "Error searching for vendor: '%s'", vendor_name);
        return(ret);
    } else {
        printf("\t\t%s\n", my_vendor.name);
        printf("\t\t%s\n", my_vendor.street);
        printf("\t\t%s, %s\n", my_vendor.city, my_vendor.state);
        printf("\t\t%s\n\n", my_vendor.zipcode);
        printf("\t\t%s\n\n", my_vendor.phone_number);
        printf("\t\tContact: %s\n", my_vendor.sales_rep);
        printf("\t\t%s\n", my_vendor.sales_rep_phone);
    }
    return(0);
} 
```

  

That completes the implementation of `example_database_read()`. In the next chapter, we will extend this application to make use of a secondary database so that we can query the inventory database for a specific inventory item.
