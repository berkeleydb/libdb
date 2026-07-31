---
title: "Secondary Database Example"
api-name: "Secondary Database Example"
source: docs/gsg/C/coreindexusage.html
---
## Secondary Database Example

<span class="sect2"> [Secondary Databases with example_database_load](coreindexusage.md#edlWIndexes) </span>

<span class="sect2"> [Secondary Databases with example_database_read](coreindexusage.md#edrWIndexes) </span>

In previous chapters in this book, we built applications that load and display several DB databases. In this example, we will extend those examples to use secondary databases. Specifically:

- In <a href="DbUsage.md" class="xref" title="Database Usage Example">Database Usage Example</a> we built an application that can open and load data into several databases. In <a href="coreindexusage.md#edlWIndexes" class="xref" title="Secondary Databases with example_database_load">Secondary Databases with example_database_load</a> we will extend that application to also open a secondary database for the purpose of indexing inventory item names.

- In <a href="CoreCursorUsage.md" class="xref" title="Cursor Example">Cursor Example</a> we built an application to display our inventory database (and related vendor information). In <a href="coreindexusage.md#edrWIndexes" class="xref" title="Secondary Databases with example_database_read">Secondary Databases with example_database_read</a> we will extend that application to show inventory records based on the index we cause to be loaded using `example_database_load`.

### Secondary Databases with example_database_load

`example_database_load` uses several utility functions to open and close its databases. In order to cause `example_database_load` to maintain an index of inventory item names, all we really need to do is update the utility functions to:

1.  Create a new database to be used as a secondary database.

2.  Associate our new database to the inventory primary database.

3.  Close the secondary database when we close the rest of our databases.

We also need a function that can create our secondary keys for us.

Because DB maintains secondary databases for us; once this work is done we need not make any other changes to `example_database_load`. Therefore, we can limit all our work to the code found in `gettingstarted_common.h` and `gettingstarted_common.c`.

Remember that you can find the complete implementation of these functions in:

``` c
DB_INSTALL/examples_c/getting_started
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

To begin, we need to update the `stock_dbs` structure to accommodate the additional database. We defined this structure in `gettingstarted_common.h`. We can limit our update to this file to just that structure definition:

Remember that new code is in **`bold`**.

``` c
/* file: gettingstarted_common.h */
#include <db.h>

typedef struct stock_dbs {
    DB *inventory_dbp; /* Database containing inventory information */
    DB *vendor_dbp;    /* Database containing vendor information */
    DB *itemname_sdbp; /* Index based on the item name index */

    char *db_home_dir;       /* Directory containing the database files */
    char *itemname_db_name;  /* Itemname secondary database */
    char *inventory_db_name; /* Name of the inventory database */
    char *vendor_db_name;    /* Name of the vendor database */
} STOCK_DBS;

/* Function prototypes */
int databases_setup(STOCK_DBS *, const char *, FILE *);
int databases_close(STOCK_DBS *);
void initialize_stockdbs(STOCK_DBS *);
int open_database(DB **, const char *, const char *, FILE *, int);
void set_db_filenames(STOCK_DBS *my_stock);
```

Because we updated our stock_dbs structure, we need to update our stock_dbs utility functions (<a href="CoreDbUsage.md#stock-db-functions" class="xref" title="Example 2.2 The stock_db Utility Functions">The stock_db Utility Functions</a>) accordingly. The updates are trivial and so we won't show them here in the interest of space. You can find their complete implementation in the `gettingstarted_common.c` file accompanying this example in your DB distribution.

More importantly, however, we need to go to `gettingstarted_common.c` and create our secondary key extractor function. When we store our inventory items, we place the item name in the buffer immediately after a `float` and an `int`, so retrieving the string from the buffer is fairly easy to do:

``` c
/* file: gettingstarted_common.c */
#include "gettingstarted_common.h"

/*
 * Used to extract an inventory item's name from an
 * inventory database record. This function is used to create
 * keys for secondary database records.
 */
int
get_item_name(DB *dbp, const DBT *pkey, const DBT *pdata, DBT *skey)
{
    int offset;

    /*
     * First, obtain the buffer location where we placed the
     * item's name. In this example, the item's name is located
     * in the primary data. It is the first string in the
     * buffer after the price (a float) and the quantity (an int).
     *
     * See load_inventory_database() in example_database_load.c
     * for how we marshalled the inventory information into the
     * data DBT.
     */
    offset = sizeof(float) + sizeof(int);

    /* Check to make sure there's data */
    if (pdata->size < offset)
        return (-1); /* Returning non-zero means that the
                      * secondary record is not created/updated.
                      */

    /* Now set the secondary key's data to be the item name */
    memset(skey, 0, sizeof(DBT));
    skey->data = pdata->data + offset;
    skey->size = strlen(skey->data) + 1;

    return (0);
} 
```

Having completed that function, we need to update `set_db_filenames()` and `initialize_stockdbs()` to handle the new secondary databases that our application will now use. These functions were originally introduced in <a href="CoreDbUsage.md#stock-db-functions" class="xref" title="Example 2.2 The stock_db Utility Functions">The stock_db Utility Functions</a>.

``` c
/* Initializes the STOCK_DBS struct.*/
void
initialize_stockdbs(STOCK_DBS *my_stock)
{
    my_stock->db_home_dir = DEFAULT_HOMEDIR;
    my_stock->inventory_dbp = NULL;
    my_stock->vendor_dbp = NULL;
    my_stock->itemname_sdbp = NULL;

    my_stock->inventory_db_name = NULL;
    my_stock->vendor_db_name = NULL;
    my_stock->itemname_db_name = NULL;
}

/* Identify all the files that will hold our databases. */
void
set_db_filenames(STOCK_DBS *my_stock)
{
    size_t size;

    /* Create the Inventory DB file name */
    size = strlen(my_stock->db_home_dir) + strlen(INVENTORYDB) + 1;
    my_stock->inventory_db_name = malloc(size);
    snprintf(my_stock->inventory_db_name, size, "%s%s",
      my_stock->db_home_dir, INVENTORYDB);
    
    /* Create the Vendor DB file name */
    size = strlen(my_stock->db_home_dir) + strlen(VENDORDB) + 1;
    my_stock->vendor_db_name = malloc(size);
    snprintf(my_stock->vendor_db_name, size, "%s%s",
      my_stock->db_home_dir, VENDORDB);

    /* Create the itemname DB file name */
    size = strlen(my_stock->db_home_dir) + strlen(ITEMNAMEDB) + 1;
    my_stock->itemname_db_name = malloc(size);
    snprintf(my_stock->itemname_db_name, size, "%s%s",
      my_stock->db_home_dir, ITEMNAMEDB);
} 
```

We also need to update the `open_database()` (as described in <a href="CoreDbUsage.md#open-db" class="xref" title="Example 2.3 open_database() Function">open_database() Function</a>) to take special actions if we are opening a secondary database. Unlike our primary databases, we want to support sorted duplicates for our secondary database. This is because we are indexing based on an item's name, and item names are shared by multiple inventory records. As a result every key the secondary database (an item name) will be used by multiple records (pointers to records in our primary database). We allow this by configuring our secondary database to support duplicate records. Further, because BTrees perform best when their records are sorted, we go ahead and configure our secondary database for sorted duplicates.

To do this, we add a parameter to the function that indicates whether we are opening a secondary database, and we add in the few lines of code necessary to set the sorted duplicates flags.

``` c
/* Opens a database */
int
open_database(DB **dbpp,       /* The DB handle that we are opening */
    const char *file_name,     /* The file in which the db lives */
    const char *program_name,  /* Name of the program calling this
                                * function */
    FILE *error_file_pointer,
    int is_secondary)
{
    DB *dbp;    /* For convenience */
    u_int32_t open_flags;
    int ret;

    /* Initialize the DB handle */
    ret = db_create(&dbp, NULL, 0);
    if (ret != 0) {
        fprintf(error_file_pointer, "%s: %s\n", program_name,
                db_strerror(ret));
        return (ret);
    }

    /* Point to the memory malloc'd by db_create() */
    *dbpp = dbp;

    /* Set up error handling for this database */
    dbp->set_errfile(dbp, error_file_pointer);
    dbp->set_errpfx(dbp, program_name);

    /*
     * If this is a secondary database, then we want to allow
     * sorted duplicates.
     */
    if (is_secondary) {
        ret = dbp->set_flags(dbp, DB_DUPSORT);
        if (ret != 0) {
            dbp->err(dbp, ret, "Attempt to set DUPSORT flag failed.",
              file_name);
            return (ret);
        }
    }

    /* Set the open flags */
    open_flags = DB_CREATE;

    /* Now open the database */
    ret = dbp->open(dbp,        /* Pointer to the database */
                    NULL,       /* Txn pointer */
                    file_name,  /* File name */
                    NULL,       /* Logical db name (unneeded) */
                    DB_BTREE,   /* Database type (using btree) */
                    open_flags, /* Open flags */
                    0);         /* File mode. Using defaults */
    if (ret != 0) {
        dbp->err(dbp, ret, "Database '%s' open failed.", file_name);
        return (ret);
    }

    return (ret);
}   
```

That done, we can now update `databases_setup()` (see <a href="CoreDbUsage.md#databasesetup" class="xref" title="Example 2.4 The databases_setup() Function">The databases_setup() Function</a>) to create and open our secondary database. To do this, we have to add a flag to each call to `open_database()` that indicates whether the database is a secondary. We also have to associate our secondary database with the inventory database (the primary).

Note that we do not anywhere in this example show the definition of `PRIMARY_DB` and `SECONDARY_DB`. See `gettingstarted_common.h` in your DB examples directory for those definitions (they are just `0` and `1`, respectively).

``` c
/* opens all databases */
int
databases_setup(STOCK_DBS *my_stock, const char *program_name,
  FILE *error_file_pointer)
{
    int ret;

    /* Open the vendor database */
    ret = open_database(&(my_stock->vendor_dbp),
      my_stock->vendor_db_name,
      program_name, error_file_pointer,
      PRIMARY_DB);
    if (ret != 0)
        /*
         * Error reporting is handled in open_database() so just return
         * the return code here.
         */
        return (ret);

    /* Open the inventory database */
    ret = open_database(&(my_stock->inventory_dbp),
      my_stock->inventory_db_name,
      program_name, error_file_pointer,
      PRIMARY_DB);
    if (ret != 0)
        /*
         * Error reporting is handled in open_database() so just return
         * the return code here.
         */
        return (ret);

    /*
     * Open the itemname secondary database. This is used to
     * index the product names found in the inventory
     * database.
     */
    ret = open_database(&(my_stock->itemname_sdbp),
      my_stock->itemname_db_name,
      program_name, error_file_pointer,
      SECONDARY_DB);
    if (ret != 0)
        /*
         * Error reporting is handled in open_database() so just return
         * the return code here.
         */
        return (ret);

    /*
     * Associate the itemname db with its primary db
     * (inventory db).
     */
     my_stock->inventory_dbp->associate(
       my_stock->inventory_dbp,    /* Primary db */
       NULL,                       /* txn id */
       my_stock->itemname_sdbp,    /* Secondary db */
       get_item_name,              /* Secondary key extractor */
       0);                         /* Flags */
     

    printf("databases opened successfully\n");
    return (0);
}
```

Finally, we need to update `databases_close()` (<a href="CoreDbUsage.md#database_close" class="xref" title="Example 2.5 The databases_close() Function">The databases_close() Function</a>) to close our new secondary database. Note that we are careful to close the secondary before the primary, even though the database close routine is single threaded.

``` c
/* Closes all the databases and secondary databases. */
int
databases_close(STOCK_DBS *my_stock)
{
    int ret;
    /*
     * Note that closing a database automatically flushes its cached data
     * to disk, so no sync is required here.
     */

    if (my_stock->itemname_sdbp != NULL) {
        ret = my_stock->itemname_sdbp->close(my_stock->itemname_sdbp, 0);
        if (ret != 0)
            fprintf(stderr, "Itemname database close failed: %s\n",
              db_strerror(ret));
    }

    if (my_stock->inventory_dbp != NULL) {
        ret = my_stock->inventory_dbp->close(my_stock->inventory_dbp, 0);
        if (ret != 0)
            fprintf(stderr, "Inventory database close failed: %s\n",
              db_strerror(ret));
    }

    if (my_stock->vendor_dbp != NULL) {
        ret = my_stock->vendor_dbp->close(my_stock->vendor_dbp, 0);
        if (ret != 0)
            fprintf(stderr, "Vendor database close failed: %s\n",
              db_strerror(ret));
    }

    printf("databases closed.\n");
    return (0);
} 
```

And the implementation changes slightly to take advantage of the new boolean. Note that to save space, we just show the constructor where the code actually changes:

That completes our update to `example_database_load`. Now when this program is called, it will automatically index inventory items based on their names. We can then query for those items using the new index. We show how to do that in the next section.

### Secondary Databases with example_database_read

In <a href="CoreCursorUsage.md" class="xref" title="Cursor Example">Cursor Example</a> we wrote an application that displays every inventory item in the Inventory database. In this section, we will update that example to allow us to search for and display an inventory item given a specific name. To do this, we will make use of the secondary database that `example_database_load` now creates.

Because we manage all our database open and close activities in `databases_setup()` and `databases_close()`, the update to `example_database_read` is relatively modest. We need only add a command line parameter on which we can specify the item name, and we will need a new function in which we will perform the query and display the results.

To begin, we add a single forward declaration to the application, and update our usage function slightly:

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

Next, we update `main()` to accept the new command line switch. We also need a new variable to contain the item's name.

``` c
/*
 * Searches for a inventory item based on that item's name. The search is
 * performed using the item name secondary database. Displays all
 * inventory items that use the specified name, as well as the vendor
 * associated with that inventory item.
 *
 * If no item name is provided, then all inventory items are displayed.
 */
int
main(int argc, char *argv[])
{
    STOCK_DBS my_stock;
    int ret;
    char *itemname;

    /* Initialize the STOCK_DBS struct */
    initialize_stockdbs(&my_stock);

    itemname = NULL;
    /*
     * Parse the command line arguments here and determine
     * the location of the database files as well as the
     * inventory item we want displayed, if any. This step is
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
```

The final update to the `main()` entails a little bit of logic to determine whether we want to display all available inventory items, or just the ones that match a name provided on the `-i` command line parameter.

``` c
    /* 
     * Show either a single item or all items, depending
     * on whether itemname is set to a value.
     */
    if (itemname == NULL)
        ret = show_all_records(&my_stock);
    else
        ret = show_records(&my_stock, itemname);

    /* Close our databases */
    databases_close(&my_stock);
    return (ret);
} 
```

The only other thing that we need to add to the application is the implementation of the `show_records()` function.

### Note

In the interest of space, we refrain from showing the other functions used by this application. For their implementation, please see <a href="CoreCursorUsage.md" class="xref" title="Cursor Example">Cursor Example</a>. Alternatively, you can see the entire implementation of this application in:

``` c
DB_INSTALL/examples_c/getting_started
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

``` c
/*
 * Search for an inventory item given its name (using the inventory item
 * secondary database) and display that record and any duplicates that may
 * exist.
 */
int
show_records(STOCK_DBS *my_stock, char *itemname)
{
    DBC *itemname_cursorp;
    DBT key, data;
    char *the_vendor;
    int ret, exit_value;

    /* Initialize our DBTs. */
    memset(&key, 0, sizeof(DBT));
    memset(&data, 0, sizeof(DBT));

    /* Get a cursor to the itemname db */
    my_stock->itemname_sdbp->cursor(my_stock->itemname_sdbp, 0,
      &itemname_cursorp, 0);

    /*
     * Get the search key. This is the name on the inventory
     * record that we want to examine.
     */
    key.data = itemname;
    key.size = strlen(itemname) + 1;

    /*
     * Position our cursor to the first record in the secondary
     * database that has the appropriate key.
     */
    exit_value = 0;
    ret = itemname_cursorp->get(itemname_cursorp, &key, &data, DB_SET);
    if (!ret) {
        do {
            /*
             * Show the inventory record and the vendor responsible
             * for this inventory item.
             */
            the_vendor = show_inventory_item(data.data);
            ret = show_vendor_record(the_vendor, my_stock->vendor_dbp);
            if (ret) {
                exit_value = ret;
                break;
            }
            /*
             * Our secondary allows duplicates, so we need to loop over
             * the next duplicate records and show them all. This is done
             * because an inventory item's name is not a unique value.
             */
        } while(itemname_cursorp->get(itemname_cursorp, &key, &data,
            DB_NEXT_DUP) == 0);
    } else {
        printf("No records found for '%s'\n", itemname);
    }

    /* Close the cursor */
    itemname_cursorp->close(itemname_cursorp);

    return (exit_value);
} 
```

This completes our update to `example_inventory_read`. Using this update, you can now search for and show all inventory items that match a particular name. For example:

``` c
    example_inventory_read -i "Zulu Nut"
```
