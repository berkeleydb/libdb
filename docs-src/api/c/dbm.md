---
title: "dbm/ndbm"
api-name: "dbm/ndbm"
source: docs/api_reference/C/dbm.html
---
## dbm/ndbm

``` c
#define DB_DBM_HSEARCH    1
#include <db.h>

typedef struct {
    char *dptr;
    int dsize;
} datum;  
```

**Dbm Functions**

``` c
int
dbminit(char *file);

int
dbmclose();

datum
fetch(datum key);

int
store(datum key, datum content);

int
delete(datum key);

datum
firstkey(void);

datum
nextkey(datum key);
```

**Ndbm Functions**

``` c
DBM *
dbm_open(char *file, int flags, int mode);

void
dbm_close(DBM *db);

datum
dbm_fetch(DBM *db, datum key);

int
dbm_store(DBM *db, datum key, datum content, int flags);

int
dbm_delete(DBM *db, datum key);

datum
dbm_firstkey(DBM *db);

datum
dbm_nextkey(DBM *db);

int
dbm_error(DBM *db);

int
dbm_clearerr(DBM *db); 
```

The dbm functions are intended to provide high-performance implementations and source code compatibility for applications written to historic interfaces. They are not recommended for any other purpose. The historic dbm database format **is not supported**, and databases previously built using the real dbm libraries cannot be read by the Berkeley DB functions.

To compile dbm applications, replace the application's **\#include** of the dbm or ndbm include file (for example, **\#include \<dbm.h\>** or **\#include \<ndbm.h\>**) with the following two lines:

``` c
#define DB_DBM_HSEARCH    1 
#include <db.h>
```

and recompile. If the application attempts to load against a dbm library (for example, **-ldbm**), remove the library from the load line.

**Key** and **content** parameters are objects described by the **datum** typedef. A **datum** specifies a string of **dsize** bytes pointed to by **dptr**. Arbitrary binary data, as well as normal text strings, are allowed.

### Dbm Functions

Before a database can be accessed, it must be opened by dbminit. This will open and/or create the database **file.db**. If created, the database file is created read/write by owner only (as described in **chmod**(2)) and modified by the process' umask value at the time of creation (see **umask**(2)). The group ownership of created files is based on the system and directory defaults, and is not further specified by Berkeley DB.

A database may be closed, and any held resources released, by calling dbmclose.

Once open, the data stored under a key is accessed by fetch, and data is placed under a key by store. A key (and its associated contents) are deleted by delete. A linear pass through all keys in a database may be made, in an (apparently) random order, by using firstkey and nextkey. The firstkey method will return the first key in the databaseThe nextkey method will return the next key in satabase.

The following code will traverse the database:

``` c
for (key = firstkey(key);
    key.dptr != NULL; key = nextkey(key)) {
        ...
} 
```

### Ndbm Functions

Before a database can be accessed, it must be opened by dbm_open. This will open and/or create the database file **file.db**, depending on the flags parameter (see **open**(2)). If created, the database file is created with mode **mode** (as described in **chmod**(2)) and modified by the process' umask value at the time of creation (see **umask**(2)). The group ownership of created files is based on the system and directory defaults, and is not further specified by Berkeley DB.

Once open, the data stored under a key is accessed by dbm_fetch, and data is placed under a key by dbm_store. The **flags** field can be either **DBM_INSERT** or **DBM_REPLACE**. **DBM_INSERT** will only insert new entries into the database, and will not change an existing entry with the same key. **DBM_REPLACE** will replace an existing entry if it has the same key. A key (and its associated contents) are deleted by dbm_delete. A linear pass through all keys in a database may be made, in an (apparently) random order, by using dbm_firstkey and dbm_nextkey. The dbm_firstkey method will return the first key in the database. The dbm_nextkey method will return the next key in the database.

The following code will traverse the database:

``` c
for (key = dbm_firstkey(db);
    key.dptr != NULL; key = dbm_nextkey(db)) {
        ...
} 
```

### Compatibility Notes

The historic dbm library created two underlying database files, traditionally named **file.dir** and **file.pag**. The Berkeley DB library creates a single database file named **file.db**. Applications that are aware of the underlying database filenames may require additional source code modifications.

The historic dbminit function required that the underlying **.dir** and **.pag** files already exist (empty databases were created by first manually creating zero-length **.dir** and **.pag** files). Applications that expect to create databases using this method may require additional source code modifications.

The historic dbm_dirfno and dbm_pagfno macros are supported, but will return identical file descriptors because there is only a single underlying file used by the Berkeley DB hashing access method. Applications using both file descriptors for locking may require additional source code modifications.

If applications using the dbm function exits without first closing the database, it may lose updates because the Berkeley DB library buffers writes to underlying databases. Such applications will require additional source code modifications to work correctly with the Berkeley DB library.

### Dbm Diagnostics

The dbminit function returns -1 on failure, setting **errno**, and 0 on success.

The fetch function sets the **dptr** field of the returned **datum** to NULL on failure, setting **errno**, and returns a non-NULL **dptr** on success.

The store function returns -1 on failure, setting **errno**, and 0 on success.

The delete function returns -1 on failure, setting **errno**, and 0 on success.

The firstkey function sets the **dptr** field of the returned **datum** to NULL on failure, setting **errno**, and returns a non-NULL **dptr** on success.

The nextkey function sets the **dptr** field of the returned **datum** to NULL on failure, setting **errno**, and returns a non-NULL **dptr** on success.

### Dbm Errors

The dbminit, fetch, store, delete, firstkey, and nextkey functions may fail and return an error for errors specified for other Berkeley DB and C library or system functions.

### Ndbm Diagnostics

The dbm_close method returns non-zero when an error has occurred reading or writing the database.

The dbm_close method resets the error condition on the named database.

The dbm_open function returns NULL on failure, setting **errno**, and a DBM reference on success.

The dbm_fetch function sets the **dptr** field of the returned **datum** to NULL on failure, setting **errno**, and returns a non-NULL **dptr** on success.

The dbm_store function returns -1 on failure, setting **errno**, 0 on success, and 1 if DBM_INSERT was set and the specified key already existed in the database.

The dbm_delete function returns -1 on failure, setting **errno**, and 0 on success.

The dbm_firstkey function sets the **dptr** field of the returned **datum** to NULL on failure, setting **errno**, and returns a non-NULL **dptr** on success.

The dbm_nextkey function sets the **dptr** field of the returned **datum** to NULL on failure, setting **errno**, and returns a non-NULL **dptr** on success.

The dbm_close function returns -1 on failure, setting **errno**, and 0 on success.

The dbm_close function returns -1 on failure, setting **errno**, and 0 on success.

### Ndbm Errors

The dbm_open, dbm_close, dbm_fetch, dbm_store, dbm_delete, dbm_firstkey, and dbm_nextkey functions may fail and return an error for errors specified for other Berkeley DB and C library or system functions.
