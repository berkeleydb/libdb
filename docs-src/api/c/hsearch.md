---
title: "hsearch"
api-name: "hsearch"
source: docs/api_reference/C/hsearch.html
---
## hsearch

``` c
#define DB_DBM_HSEARCH    1
#include <db.h>

typedef enum {
        FIND, ENTER
} ACTION;

typedef struct entry {
        char *key;
        void *data;
} ENTRY;

ENTRY *
hsearch(ENTRY item, ACTION action);

int
hcreate(size_t nelem);

void
hdestroy(void);  
```

The hsearch functions are intended to provide a high-performance implementation and source code compatibility for applications written to the historic hsearch interface. It is not recommended for any other purpose.

To compile hsearch applications, replace the application's **\#include** of the hsearch include file (for example, **\#include \<search.h\>**) with the following two lines:

``` c
#define DB_DBM_HSEARCH    1 
        #include <db.h>
```

and recompile.

The hcreate function creates an in-memory database. The **nelem** parameter is an estimation of the maximum number of key/data pairs that will be stored in the database.

The **hdestroy** function discards the database.

Database elements are structures of type **ENTRY**, which contain at least two fields: **key** and **data**. The field **key** is declared to be of type **char \***, and is the key used for storage and retrieval. The field **data** is declared to be of type **void \***, and is its associated data.

The hsearch function retrieves key/data pairs from, and stores key/data pairs into the database.

The **action** parameter must be set to one of two values:

- **ENTER**

  If the key does not already appear in the database, insert the key/data pair into the database. If the key already appears in the database, return a reference to an **ENTRY** structure which refers to the existing key and its associated data element.

- **FIND**

  Retrieve the specified key/data pair from the database.

### Compatibility Notes

Historically, hsearch required applications to maintain the keys and data in the application's memory for as long as the **hsearch** database existed. Because Berkeley DB handles key and data management internally, there is no requirement that applications maintain local copies of key and data items, although the only effect of doing so should be the allocation of additional memory.

### Hsearch Diagnostics

The **hcreate** function returns 0 on failure, setting **errno**, and non-zero on success.

The **hsearch** function returns a pointer to an ENTRY structure on success, and NULL, setting **errno**, if the **action** specified was FIND and the item did not appear in the database.

### Hsearch Errors

The **hsearch** function will fail, setting **errno** to 0, if the **action** specified was FIND and the item did not appear in the database.

In addition, the hcreate, hsearch and hdestroy functions may fail and return an error for errors specified for other Berkeley DB and C library or system functions.
