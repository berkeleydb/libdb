---
title: "Database Example"
api-name: "Database Example"
source: docs/gsg/C/CoreDbUsage.html
---
## Database Example

Throughout this book we will build a couple of applications that load and retrieve inventory data from DB databases. While we are not yet ready to begin reading from or writing to our databases, we can at least create some important structures and functions that we will use to manage our databases.

Note that subsequent examples in this book will build on this code to perform the more interesting work of writing to and reading from the databases.

Note that you can find the complete implementation of these functions in:

``` c
DB_INSTALL/examples_c/getting_started
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

**Example 2.1 The stock_db Structure**

To begin, we create a structure that we will use to hold all our database pointers and database names:

``` c
/* File: gettingstarted_common.h */
#include <db.h>

typedef struct stock_dbs {
    DB *inventory_dbp; /* Database containing inventory information */
    DB *vendor_dbp;    /* Database containing vendor information */

    char *db_home_dir;       /* Directory containing the database files */
    char *inventory_db_name; /* Name of the inventory database */
    char *vendor_db_name;    /* Name of the vendor database */
} STOCK_DBS;

/* Function prototypes */
int databases_setup(STOCK_DBS *, const char *, FILE *);
int databases_close(STOCK_DBS *);
void initialize_stockdbs(STOCK_DBS *);
int open_database(DB **, const char *, const char *,
    FILE *);
void set_db_filenames(STOCK_DBS *my_stock); 
```

  
**Example 2.2 The stock_db Utility Functions**

Before continuing, we want some utility functions that we use to make sure the stock_db structure is in a sane state before using it. One is a simple function that initializes all the structure's pointers to a useful default.The second is more interesting in that it is used to place a common path on all our database names so that we can explicitly identify where all the database files should reside.

``` c
/* File: gettingstarted_common.c */
#include "gettingstarted_common.h"

/* Initializes the STOCK_DBS struct.*/
void
initialize_stockdbs(STOCK_DBS *my_stock)
{
    my_stock->db_home_dir = DEFAULT_HOMEDIR;
    my_stock->inventory_dbp = NULL;
    my_stock->vendor_dbp = NULL;

    my_stock->inventory_db_name = NULL;
    my_stock->vendor_db_name = NULL;
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
} 
```

  
**Example 2.3 open_database() Function**

We are opening multiple databases, and we are opening those databases using identical flags and error reporting settings. It is therefore worthwhile to create a function that performs this operation for us:

``` c
/* File: gettingstarted_common.c */
    
/* Opens a database */
int
open_database(DB **dbpp,       /* The DB handle that we are opening */
    const char *file_name,     /* The file in which the db lives */
    const char *program_name,  /* Name of the program calling this 
                                * function */
    FILE *error_file_pointer)  /* File where we want error messages 
                                  sent */
{
    DB *dbp;    /* For convenience */
    u_int32_t open_flags;
    int ret;

    /* Initialize the DB handle */
    ret = db_create(&dbp, NULL, 0);
    if (ret != 0) {
        fprintf(error_file_pointer, "%s: %s\n", program_name,
                db_strerror(ret));
        return(ret);
    }

    /* Point to the memory malloc'd by db_create() */
    *dbpp = dbp;

    /* Set up error handling for this database */
    dbp->set_errfile(dbp, error_file_pointer);
    dbp->set_errpfx(dbp, program_name);

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
        return(ret);
    }

    return (0);
}
```

  
**Example 2.4 The databases_setup() Function**

Now that we have our `open_database()` function, we can use it to open a database. We now create a simple function that will open all our databases for us.

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
      program_name, error_file_pointer);
    if (ret != 0)
        /*
         * Error reporting is handled in open_database() so just return
         * the return code here.
         */
        return (ret);

    /* Open the inventory database */
    ret = open_database(&(my_stock->inventory_dbp),
      my_stock->inventory_db_name,
      program_name, error_file_pointer);
    if (ret != 0)
        /*
         * Error reporting is handled in open_database() so just return
         * the return code here.
         */
        return (ret);

    printf("databases opened successfully\n");
    return (0);
}
```

  
**Example 2.5 The databases_close() Function**

Finally, it is useful to have a function that can close all our databases for us:

``` c
/* Closes all the databases. */
int
databases_close(STOCK_DBS *my_stock)
{
    int ret;
    /*
     * Note that closing a database automatically flushes its cached data
     * to disk, so no sync is required here.
     */

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
