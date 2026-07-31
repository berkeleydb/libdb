---
title: "Implementing Key Extractors"
api-name: "Implementing Key Extractors"
source: docs/gsg/C/keyCreator.html
---
## Implementing Key Extractors

<span class="sect2"> [Working with Multiple Keys](keyCreator.md#multikeys) </span>

You must provide every secondary database with a callback that creates keys from primary records. You identify this callback when you associate your secondary database to your primary.

You can create keys using whatever data you want. Typically you will base your key on some information found in a record's data, but you can also use information found in the primary record's key. How you build your keys is entirely dependent upon the nature of the index that you want to maintain.

You implement a key extractor by writing a function that extracts the necessary information from a primary record's key or data. This function must conform to a specific prototype, and it must be provided as a callback to the `associate()` method.

For example, suppose your primary database records contain data that uses the following structure:

``` c
typedef struct vendor {
    char name[MAXFIELD];             /* Vendor name */
    char street[MAXFIELD];           /* Street name and number */
    char city[MAXFIELD];             /* City */
    char state[3];                   /* Two-digit US state code */
    char zipcode[6];                 /* US zipcode */
    char phone_number[13];           /* Vendor phone number */
    char sales_rep[MAXFIELD];        /* Name of sales representative */
    char sales_rep_phone[MAXFIELD];  /* Sales rep's phone number */
} VENDOR; 
```

Further suppose that you want to be able to query your primary database based on the name of a sales representative. Then you would write a function that looks like this:

``` c
#include <db.h>

...

int
get_sales_rep(DB *sdbp,          /* secondary db handle */
              const DBT *pkey,   /* primary db record's key */
              const DBT *pdata,  /* primary db record's data */
              DBT *skey)         /* secondary db record's key */
{
    VENDOR *vendor;

    /* First, extract the structure contained in the primary's data */
    vendor = pdata->data;

    /* Now set the secondary key's data to be the representative's name */
    memset(skey, 0, sizeof(DBT));
    skey->data = vendor->sales_rep;
    skey->size = strlen(vendor->sales_rep) + 1;

    /* Return 0 to indicate that the record can be created/updated. */
    return (0);
} 
```

In order to use this function, you provide it on the `associate()` method after the primary and secondary databases have been created and opened:

``` c
dbp->associate(dbp,            /* Primary database */
               NULL,           /* TXN id */
               sdbp,           /* Secondary database */
               get_sales_rep,  /* Callback used for key creation. */
               0);             /* Flags */
```

### Working with Multiple Keys

Until now we have only discussed indexes as if there is a one-to-one relationship between the secondary key and the primary database record. In fact, it is possible to generate multiple keys for any given record, provided that you take appropriate steps in your key creator to do so.

For example, suppose you had a database that contained information about books. Suppose further that you sometimes want to look up books by author. Because sometimes books have multiple authors, you may want to return multiple secondary keys for every book that you index.

To do this, you write a key extractor that returns a DBT whose `data` member points to an array of DBTs. Each such member of this array contains a single secondary key. In addition, the DBT returned by your key extractor must have a size field equal to the number of elements contained in the DBT array. Also, the flag field for the DBT returned by the callback must include `DB_DBT_MULTIPLE`. For example:

### Note

It is important that the array of secondary keys created by your callback not contain repeats. That is, every element in the array must be unique. If the array does not contain a unique set, then the secondary can get out of sync with the primary.

``` c
int
my_callback(DB *dbp, const DBT *pkey, const DBT *pdata, DBT *skey)
{
    DBT *tmpdbt;
    char *tmpdata1, tmpdata2;

    /*
     * This example skips the step of extracting the data you
     * want to use for building your secondary keys from the
     * pkey or pdata DBT.
     *
     * Assume for the purpose of this example that the data 
     * is temporarily stored in two variables, 
     * tmpdata1 and tmpdata2.
     */

    /* 
     * Create an array of DBTs that is large enough for the
     * number of keys that you want to return. In this case, 
     * we go with an array of size two. 
     */

    tmpdbt = malloc(sizeof(DBT) * 2);
    memset(tmpdbt, 0, sizeof(DBT) * 2);

    /* Now assign secondary keys to each element of the array. */
    tmpdbt[0].data = tmpdata1;
    tmpdbt[0].size = (u_int32_t)strlen(tmpdbt[0].data) + 1;
    tmpdbt[1].data = tmpdata2;
    tmpdbt[1].size = (u_int32_t)strlen(tmpdbt[1].data) + 1;

    /* 
     * Now we set flags for the returned DBT. DB_DBT_MULTIPLE is
     * required in order for DB to know that the DBT references an 
     * array. In addition, we set DB_DBT_APPMALLOC because we
     * dynamically allocated memory for the DBT's data field.
     * DB_DBT_APPMALLOC causes DB to release that memory once it
     * is done with the returned DBT. 
     */
    skey->flags = DB_DBT_MULTIPLE | DB_DBT_APPMALLOC;

    /* Point the results data field to the arrays of DBTs */
    skey->data = tmpdbt;

    /* Indicate the returned array is of size 2 */
    skey->size = 2;

    return (0);
} 
```
