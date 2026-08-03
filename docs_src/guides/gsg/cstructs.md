---
title: "Using C Structures with DB"
api-name: "Using C Structures with DB"
source: docs/gsg/C/cstructs.html
---
## Using C Structures with DB

<span class="sect2"> [C Structures with Pointers](cstructs.md#cstructdynamic) </span>

Storing data in structures is a handy way to pack varied types of information into each database record. DB databases are sometimes thought of as a two column table where column 1 is the key and column 2 is the data. By using structures, you can effectively turn this table into <span class="emphasis">*n*</span> columns where <span class="emphasis">*n-1*</span> columns are contained in the structure.

So long as a C structure contains fields that are not pointers, you can safely store and retrieve them in the same way as you would any primitive datatype. The following code fragment illustrates this:

``` c
#include <db.h>
#include <string.h>

typedef struct my_struct {
    int id;
    char familiar_name[MAXLINE]; /* Some suitably large value */
    char surname[MAXLINE];
} MY_STRUCT;

...

DBT key, data;
DB *my_database;
MY_STRUCT user;
char *fname = "David";
char *sname = "Rider";

/* Database open omitted for clarity */

user.id = 1;
strncpy(user.familiar_name, fname, strlen(fname)+1);
strncpy(user.surname, sname, strlen(sname)+1);

/* Zero out the DBTs before using them. */
memset(&key, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));

key.data = &(user.id);
key.size = sizeof(int);

data.data = &user;
data.size = sizeof(MY_STRUCT); 

my_database->put(my_database, NULL, &key, &data, DB_NOOVERWRITE);
```

To retrieve the structure, make sure you supply your own memory. The reason why is that like real numbers, some systems require structures to be aligned in a specific way. Because it is possible that the memory DB provides is not aligned properly, for safest result simply use your own memory:

``` c
#include <db.h>
#include <string.h>

...

DBT key, data;
DB *my_database;
MY_STRUCT user;

/* Database open omitted for clarity */

/* Zero out the DBTs before using them. */
memset(&key, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));

/* Initialize the structure */
memset(&user, 0, sizeof(MY_STRUCT));
user.id = 1;

key.data = &user.id;
key.size = sizeof(int);

/* Use our memory to retrieve the structure */
data.data = &user;
data.ulen = sizeof(MY_STRUCT); 
data.flags = DB_DBT_USERMEM;

my_database->get(my_database, NULL, &key, &data, 0);

printf("Familiar name: %s\n", user.familiar_name);
printf("Surname: %s\n", user.surname); 
```

Be aware that while this is the easiest way to manage structures stored in DB databases, this approach does suffer from causing your database to be larger than is strictly necessary. Each structure stored in the database is of a fixed size, and you do not see any space savings from storing a (for example) 5 character surname versus a 20 character surname.

For a simple example such as this, the padding stored with each record is probably not critical. However, if you are storing structures that contain a very large number of character arrays, or if you are simply storing millions of records, then you may want to avoid this approach. The wasted space in each record will only serve to make your databases larger than need be, which will in turn require a larger cache and more disk I/O than you would ordinarily need.

An alternative approach is described next.

### C Structures with Pointers

It is often necessary in C structures to use fields that are pointers to dynamically allocated memory. This is particularly true if you want to store character strings (or any kind of an array for that matter), and you want to avoid any overhead caused by pre-designating the size of the array.

When storing structures like these you need to make sure that all of the data pointed to and contained by the structure is lined up in a single contiguous block of memory. Remember that DB stores data located at a specific address and of a particular size. If your structure includes fields that are pointing to dynamically allocated memory, then the data that you want to store can be located in different, not necessarily contiguous, locations on the heap.

The easiest way to solve this problem is to pack your data into a single memory location and then store the data in that location. (This process is sometimes called <span class="emphasis">*marshalling the data*</span>.) For example:

``` c
#include <db.h>
#include <string.h>
#include <stdlib.h>

typedef struct my_struct {
    int id;
    char *familiar_name;
    char *surname;
} MY_STRUCT;

...

DBT key, data;
DB *my_database;
MY_STRUCT user;
int buffsize, bufflen;
char fname[ ] = "Pete";
char sname[10];
char *databuff;

strncpy(sname, "Oar", strlen("Oar")+1);

/* Database open omitted for clarity */

user.id = 1;
user.familiar_name = fname;
user.surname = sname;

/* Some of the structure's data is on the stack, and 
 * some is on the heap. To store this structure's data, we
 * need to marshall it -- pack it all into a single location 
 * in memory.
 */

/* Get the buffer */
buffsize = sizeof(int) + 
  (strlen(user.familiar_name) + strlen(user.surname) + 2);
databuff = malloc(buffsize);
memset(databuff, 0, buffsize);

/* copy everything to the buffer */
memcpy(databuff, &(user.id), sizeof(int));
bufflen = sizeof(int);

memcpy(databuff + bufflen, user.familiar_name, 
  strlen(user.familiar_name) + 1);
bufflen += strlen(user.familiar_name) + 1;

memcpy(databuff + bufflen, user.surname, 
  strlen(user.surname) + 1);
bufflen += strlen(user.surname) + 1;

/* Now store it */

/* Zero out the DBTs before using them. */
memset(&key, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));

key.data = &(user.id);
key.size = sizeof(int);

data.data = databuff;
data.size = bufflen;

my_database->put(my_database, NULL, &key, &data, DB_NOOVERWRITE);
free(sname);
free(databuff);
```

To retrieve the stored structure:

``` c
#include <db.h>
#include <string.h>
#include <stdlib.h>

typedef struct my_struct {
    char *familiar_name;
    char *surname;
    int id;
} MY_STRUCT;

...

int id;
DBT key, data;
DB *my_database;
MY_STRUCT user;
char *buffer;

/* Database open omitted for clarity */

/* Zero out the DBTs before using them. */
memset(&key, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));

id = 1;
key.data = &id;
key.size = sizeof(int);

my_database->get(my_database, NULL, &key, &data, 0);

/* 
 * Some compilers won't allow pointer arithmetic on void *'s,
 * so use a char * instead.
 */
buffer = data.data;

user.id = *((int *)data.data);
user.familiar_name = buffer + sizeof(int);
user.surname = buffer + sizeof(int) + strlen(user.familiar_name) + 1; 
```
