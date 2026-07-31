---
title: "DB->put"
api-name: "DB->put"
source: docs/upgrading/upgrade_3_1_put.html
---
## DB-\>put

For the Queue and Recno access methods, when the <a href="../../api/c/dbput.md#dbput_DB_APPEND" class="olink">DB_APPEND</a> flag is specified to the <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a> method, the allocated record number is returned to the application in the **key** <a href="../../api/c/dbt.md" class="olink">DBT</a> argument. In previous releases of Berkeley DB, this <a href="../../api/c/dbt.md" class="olink">DBT</a> structure did not follow the usual <a href="../../api/c/dbt.md" class="olink">DBT</a> conventions. For example, it was not possible to cause Berkeley DB to allocate space for the returned record number. Rather, it was always assumed that the **data** field of the **key** structure referred to memory that could be used as storage for a db_recno_t type.

As of the Berkeley DB 3.1.0 release, the **key** structure behaves as described in the <a href="../../api/c/dbt.md" class="olink">DBT</a> C++/Java class or C structure documentation.

Applications which are using the <a href="../../api/c/dbput.md#dbput_DB_APPEND" class="olink">DB_APPEND</a> flag for Queue and Recno access method databases will require a change to upgrade to the Berkeley DB 3.1 releases. The simplest change is likely to be to add the <a href="../../api/c/dbt.md#dbt_DB_DBT_USERMEM" class="olink">DB_DBT_USERMEM</a> flag to the **key** structure. For example, code that appears as follows:

``` c
DBT key;
db_recno_t recno;

memset(&key, 0, sizeof(DBT));
key.data = &recno;
key.size = sizeof(recno);
DB->put(DB, NULL, &key, &data, DB_APPEND);
printf("new record number is %lu\n", (u_long)recno);
```

would be changed to:

``` c
DBT key;
db_recno_t recno;

memset(&key, 0, sizeof(DBT));
key.data = &recno;
key.ulen = sizeof(recno);
key.flags = DB_DBT_USERMEM;
DB->put(DB, NULL, &key, &data, DB_APPEND);
printf("new record number is %lu\n", (u_long)recno);
```

Note that the **ulen** field is now set as well as the flag value. An alternative change would be:

``` c
DBT key;
db_recno_t recno;

memset(&key, 0, sizeof(DBT));
DB->put(DB, NULL, &key, &data, DB_APPEND);
recno = *(db_recno_t *)key->data;
printf("new record number is %lu\n", (u_long)recno);
```
