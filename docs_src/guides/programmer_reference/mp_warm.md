---
title: "Warming the memory pool"
api-name: "Warming the memory pool"
source: docs/programmer_reference/mp_warm.html
---
## Warming the memory pool

<span class="sect2"> [The warm_cache() function](mp_warm.md#warm_cache) </span>

Some applications find it is useful to pre-load the memory pool upon application startup. This is a strictly optional activity that provides faster initial access to your data at the expense of longer application startup times.

To warm the cache, you simply have to read the records that your application will operate on most frequently. You can do this with normal database reads, and you can also use cursors. But the most efficient way to warm the cache is to use memory pool APIs to get the pages that contain your most frequently accessed records.

You read pages into the memory pool using the `DB_MPOOLFILE->get()` method. This method acquires locks on the page, so immediately upon getting the page you need to put it so as to release the locks.

Also, you obtain a memory pool file handle using a database handle. This means that if your data is contained in more than one Berkeley DB database, you must operate on each database handle in turn.

The following example code illustrates this. It does the following:

- Opens an environment and two database handles.

- Determines how many database pages can fit into the memory pool.

- Uses `DB_MPOOLFILE->get()` and `DB_MPOOLFILE->put()` to load that number of pages into the memory pool.

First, we include the libraries that we need, forward declare some functions, and intialize some variables.

``` c
#include <stdio.h> 
#include <stdlib.h>
#include <string.h>
#include <db.h>

/* Forward declarations */
int warm_cache(DB *, int *, int);
int open_db(DB_ENV *, DB **, const char *);

int
main(void)
{
    DB *dbp1 = 0, *dbp2 = 0;
    DB_ENV *envp = 0;
    u_int32_t env_flags, pagesize, gbytes, bytes;
    int ret = 0, ret_t = 0, numcachepages, pagecount; 
```

Then we open the environment and our databases. The `open_db()` function that we use here simply opens a database. We will provide that code at the end of this example, but it should hold no surprises for you. We only use the function so as to reuse the code.

``` c
    /*
     * Open the environment and the databases
     */
    ret = db_env_create(&envp, 0);
    if (ret != 0) {
        fprintf(stderr, "Error creating environment handle: %s\n",
                db_strerror(ret));
        goto err;
    }

    env_flags =
      DB_CREATE     |  /* Create the environment if it does 
                          not exist */
      DB_RECOVER    |  /* Run normal recovery. */
      DB_INIT_LOCK  |  /* Initialize the locking subsystem */
      DB_INIT_LOG   |  /* Initialize the logging subsystem */
      DB_INIT_TXN   |  /* Initialize the transactional subsystem. This
                        * also turns on logging. */
      DB_INIT_MPOOL; /* Initialize the memory pool */

    /* Now actually open the environment */
    ret = envp->open(envp, "./env", env_flags, 0);
    if (ret != 0) {
        fprintf(stderr, "Error opening environment: %s\n",
            db_strerror(ret));
        goto err;
    }

    ret = open_db(envp, &dbp1, "mydb1.db");
    if (ret != 0)
        goto err;

    ret = open_db(envp, &dbp2, "mydb2.db");
    if (ret != 0)
        goto err; 
```

Next we determine how many database pages we can fit into the cache. We do this by finding out how large our pages are, and then finding out how large our cache can be.

``` c
    /* Find out how many pages can fit at most in the cache */
    ret = envp->get_mp_pagesize(envp, &pagesize);
    if (ret != 0) {
        fprintf(stderr, "Error retrieving the cache pagesize: %s\n",
            db_strerror(ret));
        goto err;
    }

    ret = envp->get_cache_max(envp, &gbytes, &bytes);
    if (ret != 0) {
        fprintf(stderr, "Error retrieving maximum cache size: %s\n",
            db_strerror(ret));
        goto err;
    }
    /* Avoid an overflow by first calculating pages per gigabyte. */
    numcachepages = gbytes * ((1024 * 1024 * 1024) / pagesize);
    numcachepages += bytes / pagesize; 
```

Now we call our `warm_cache()` function. We will describe this function in a little while, but note that we call `warm_cache()` twice. This is because our example uses two databases, and the memory pool methods operate on a per-handle basis.

``` c
    /*
     * Warm the cache by loading pages from each of the databases
     * in turn.
     */
    pagecount = 0;
    ret = warm_cache(dbp1, &pagecount, numcachepages);
    if (ret != 0) {
        fprintf(stderr, "Error warming the cache: %s\n",
            db_strerror(ret));
        goto err;
    }

    ret = warm_cache(dbp2, &pagecount, numcachepages);
    if (ret != 0) {
        fprintf(stderr, "Error warming the cache: %s\n",
            db_strerror(ret));
        goto err;
    } 
```

Now we close all our handles and finish our `main()` function. Again, this is straight-forward boilerplate code that we provide simply to be complete.

``` c
err:
    /* Close our database handles, if they were opened. */
    if (dbp1 != NULL) {
        ret_t = dbp1->close(dbp1, 0);
        if (ret_t != 0) {
            fprintf(stderr, "dbp1 close failed: %s\n",
                db_strerror(ret_t));
            ret = ret_t;
        }
    }

    if (dbp2 != NULL) {
        ret_t = dbp2->close(dbp2, 0);
        if (ret_t != 0) {
            fprintf(stderr, "dbp2 close failed: %s\n",
                db_strerror(ret_t));
            ret = ret_t;
        }
    }

    /* Close our environment, if it was opened. */
    if (envp != NULL) {
        ret_t = envp->close(envp, 0);
        if (ret_t != 0) {
            fprintf(stderr, "environment close failed: %s\n",
            db_strerror(ret_t));
            ret = ret_t;
        }
    }

    /* Final status message and return. */
    printf("I'm all done.\n");
    return (ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
} 
```

As noted above, this example uses an `open_db()` function, which opens a database handle inside the provided environment. To be complete, this is the implementation of that function:

``` c
/* Open a database handle */
int
open_db(DB_ENV *envp, DB **dbpp, const char *file_name)
{
    int ret = 0;
    u_int32_t db_flags = 0;
    DB *dbp;

    /* Open the database */
    ret = db_create(&dbp, envp, 0);
    if (ret != 0) {
        fprintf(stderr, "Error opening database: %s : %s\n",
            file_name, db_strerror(ret));
        return ret;
    }

    /* Point to the memory malloc'd by db_create() */
    *dbpp = dbp;

    db_flags = DB_CREATE       |   /* Create the database if it does 
                                      not exist */
               DB_AUTO_COMMIT;     /* Allow autocommit */

    ret = dbp->open(dbp,   /* Pointer to the database */
            0,             /* Txn pointer */
            file_name,     /* File name */
            0,             /* Logical db name */
            DB_BTREE,      /* Database type (using btree) */
            db_flags,      /* Open flags */
            0);            /* File mode. Using defaults */
    if (ret != 0) {
        dbp->err(dbp, ret, "Database open failed: %s : %s\n",
            file_name, db_strerror(ret));
        return ret;
    }
    return 0;
} 
```

### The warm_cache() function

In this section we provide the implementation of the `warm_cache()` function. This example function simply loads all the database pages that will fit into the memory pool. It starts from the first database page and continues until it either runs out of database pages or it runs out of room in the memory pool.

``` c
/* Warm the cache */
int
warm_cache(DB *dbp, int *pagecountp, int numcachepages)
{
    DB_MPOOLFILE *mpf = 0;
    void *page_addrp = 0;
    db_pgno_t page_number = 0;
    int ret = 0;
    int pagecount = *pagecountp;

    /*
     * Get the mpool handle
     */
    mpf = dbp->get_mpf(dbp);

    /* Load pages until there are no more pages in the database,
     * or until we've put as many pages into the cache as will fit.
     */
    while (ret != DB_PAGE_NOTFOUND && pagecount < numcachepages) {
        /* 
         * Get the page from the cache. This causes DB to retrieve
         * the page from disk if it isn't already in the cache. 
         */
        ret = mpf->get(mpf, &page_number, 0, 0, &page_addrp);
        if (ret && ret != DB_PAGE_NOTFOUND) {
            fprintf(stderr, "Error retrieving db page: %i : %s\n",
                page_number, db_strerror(ret));
            return ret;
        }

        /* 
         * If a page was retrieved, put it back into the cache. This
         * releases the page latch so that the page can be evicted
         * if DB needs more room in the cache at some later time.
         */
        if (ret != DB_PAGE_NOTFOUND) {
            ret = mpf->put(mpf, page_addrp, DB_PRIORITY_UNCHANGED, 0);
            if (ret) {
                fprintf(stderr, "Error putting db page: %i : %s\n",
                    page_number, db_strerror(ret));
                return ret;
            }
        }
        ++page_number;
        ++pagecount;
        *pagecountp = pagecount;
    }

    return 0;
} 
```
