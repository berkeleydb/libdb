---
title: "Partitioning databases"
api-name: "Partitioning databases"
source: docs/programmer_reference/am_partition.html
---
## Partitioning databases

<span class="sect2"> [Specifying partition keys](am_partition.md#am_partition_keys) </span>

<span class="sect2"> [Partitioning callback](am_partition.md#am_partition_function) </span>

<span class="sect2"> [Placing partition files](am_partition.md#partition_file_placement) </span>

You can improve concurrency on your database reads and writes by splitting access to a single database into multiple databases. This helps to avoid contention for internal database pages, as well as allowing you to spread your databases across multiple disks, which can help to improve disk I/O.

### Note

Database partitions are not supported by the C# and Java APIs at this time.

While you can manually do this by creating and using more than one database for your data, DB is capable of partitioning your database for you. When you use DB's built-in database partitioning feature, your access to your data is performed in exactly the same way as if you were only using one database; all the work of knowing which database to use to access a particular record is handled for you under the hood.

Only the BTree and Hash access methods are supported for partitioned databases.

You indicate that you want your database to be partitioned by calling <a href="../../api/c/dbset_partition.md" class="olink">DB-&gt;set_partition()</a> before opening your database the first time. You can indicate the directory in which each partition is contained using the <a href="../../api/c/dbset_partition_dirs.md" class="olink">DB-&gt;set_partition_dirs()</a> method.

Once you have partitioned a database, you cannot change your partitioning scheme.

There are two ways to indicate what key/data pairs should go on which partition. The first is by specifying an array of <a href="../../api/c/dbt.md" class="olink">DBT</a>s that indicate the minimum key value for a given partition. The second is by providing a callback that returns the number of the partition on which a specified key is placed.

### Specifying partition keys

For simple cases, you can partition your database by providing an array of <a href="../../api/c/dbt.md" class="olink">DBT</a>s, each element of which provides the minimum key value to be placed on a partition. There must be one fewer elements in this array than you have partitions. The first element of the array indicates the minimum key value for the second partition in your database. Key values that are less than the first key value provided in this array are placed on the first partition (partition 0).

### Note

You can use partition keys only if you are using the Btree access method.

For example, suppose you had a database of fruit, and you want three partitions for your database. Then you need a <a href="../../api/c/dbt.md" class="olink">DBT</a> array of size two. The first element in this array indicates the minimum keys that should be placed on partition 1. The second element in this array indicates the minimum key value placed on partition 2. Keys that compare less than the first <a href="../../api/c/dbt.md" class="olink">DBT</a> in the array are placed on partition 0.

All comparisons are performed according to the lexicographic comparison used by your platform.

For example, suppose you want all fruits whose names begin with:

- 'a' - 'f' to go on partition 0

- 'g' - 'p' to go on partition 1

- 'q' - 'z' to go on partition 2.

Then you would accomplish this with the following code fragment:

### Note

The <a href="../../api/c/dbset_partition.md" class="olink">DB-&gt;set_partition()</a> partition callback parameter must be `NULL` if you are using an array of <a href="../../api/c/dbt.md" class="olink">DBT</a>s to partition your database.

``` c
DB *dbp = NULL;
    DB_ENV *envp = NULL;
    DBT partKeys[2];
    u_int32_t db_flags;
    const char *file_name = "mydb.db";
    int ret;

...
    
    /* Skipping environment open to shorten this example */

    /* Initialize the DB handle */
    ret = db_create(&dbp, envp, 0);
    if (ret != 0) {
        fprintf(stderr, "%s\n", db_strerror(ret));
        return (EXIT_FAILURE);
    }

    /* Setup the partition keys */
     memset(&partKeys[0], 0, sizeof(DBT));
     partKeys[0].data = "g";
     partKeys[0].size = sizeof("g") - 1;

     memset(&partKeys[1], 0, sizeof(DBT));
     partKeys[1].data = "q";
     partKeys[1].size = sizeof("q") - 1;

     dbp->set_partition(dbp, 3, partKeys, NULL);

    /* Now open the database */
    db_flags = DB_CREATE;       /* Allow database creation */

    ret = dbp->open(dbp,        /* Pointer to the database */
                    NULL,       /* Txn pointer */
                    file_name,  /* File name */
                    NULL,       /* Logical db name */
                    DB_BTREE,   /* Database type (using btree) */
                    db_flags,   /* Open flags */
                    0);         /* File mode. Using defaults */
    if (ret != 0) {
        dbp->err(dbp, ret, "Database '%s' open failed",
            file_name);
        return (EXIT_FAILURE);
    } 
```

### Partitioning callback

In some cases, a simple lexicographical comparison of key data will not sufficiently support a partitioning scheme. For those situations, you should write a partitioning function. This function accepts a pointer to the <a href="../../api/c/db.md" class="olink">DB</a> and the <a href="../../api/c/dbt.md" class="olink">DBT</a>, and it returns the number of the partition on which the key belongs.

Note that <a href="../../api/c/db.md" class="olink">DB</a> actually places the key on the partition calculated by:

``` c
returned_partition modulo number_of_partitions
```

Also, remember that if you use a partitioning function when you create your database, then you must use the same partitioning function every time you open that database in the future.

The following code fragment illustrates a partition callback:

``` c
u_int32_t db_partition_fn(DB *db, DBT *key) {
    char *key_data;
    u_int32_t ret_number;
    /* Obtain your key data, unpacking it as necessary
     * Here, we do the very simple thing just for illustrative purposes.
     */

    key_data = (char *)key->data;

    /* Here you would perform whatever comparison you require to determine
     * what partition the key belongs on. If you return either 0 or the
     * number of partitions in the database, the key is placed in the first
     * database partition. Else, it is placed on:
     *
     *       returned_number mod number_of_partitions
     */

    ret_number = 0;

    return ret_number;
} 
```

You then cause your partition callback to be used by providing it to the <a href="../../api/c/dbset_partition.md" class="olink">DB-&gt;set_partition()</a> method, as illustrated by the following code fragment.

### Note

The <a href="../../api/c/dbset_partition.md" class="olink">DB-&gt;set_partition()</a> <a href="../../api/c/dbt.md" class="olink">DBT</a> array parameter must be `NULL` if you are using a partition call back to partition your database.

``` c
DB *dbp = NULL;
    DB_ENV *envp = NULL;
    u_int32_t db_flags;
    const char *file_name = "mydb.db";
    int ret;

...
    
    /* Skipping environment open to shorten this example */

    /* Initialize the DB handle */
    ret = db_create(&dbp, envp, 0);
    if (ret != 0) {
        fprintf(stderr, "%s\n", db_strerror(ret));
        return (EXIT_FAILURE);
    }

     dbp->set_partition(dbp, 3, NULL, db_partition_fn);

    /* Now open the database */
    db_flags = DB_CREATE;       /* Allow database creation */

    ret = dbp->open(dbp,        /* Pointer to the database */
                    NULL,       /* Txn pointer */
                    file_name,  /* File name */
                    NULL,       /* Logical db name */
                    DB_BTREE,   /* Database type (using btree) */
                    db_flags,   /* Open flags */
                    0);         /* File mode. Using defaults */
    if (ret != 0) {
        dbp->err(dbp, ret, "Database '%s' open failed",
            file_name);
        return (EXIT_FAILURE);
    } 
```

### Placing partition files

When you partition a database, a database file is created on disk in the same way as if you were not partitioning the database. That is, this file uses the name you provide to the <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> `file` parameter.

However, DB then also creates a series of database files on disk, one for each partition that you want to use. These partition files share the same name as the database file name, but are also number sequentially. So if you create a database named `mydb.db`, and you create 3 partitions for it, then you will see the following database files on disk:

``` c
            mydb.db
            __dbp.mydb.db.000
            __dbp.mydb.db.001
            __dbp.mydb.db.002 
```

All of the database's contents go into the numbered database files. You can cause these files to be placed in different directories (and, hence, different disk partitions or even disks) by using the <a href="../../api/c/dbset_partition_dirs.md" class="olink">DB-&gt;set_partition_dirs()</a> method.

<a href="../../api/c/dbset_partition_dirs.md" class="olink">DB-&gt;set_partition_dirs()</a> takes a NULL-terminated array of strings, each one of which should represent an existing filesystem directory.

If you are using an environment, the directories specified using <a href="../../api/c/dbset_partition_dirs.md" class="olink">DB-&gt;set_partition_dirs()</a> must also be included in the environment list specified by <a href="../../api/c/envadd_data_dir.md" class="olink">DB_ENV-&gt;add_data_dir()</a>.

If you are not using an environment, then the the directories specified to <a href="../../api/c/dbset_partition_dirs.md" class="olink">DB-&gt;set_partition_dirs()</a> can be either complete paths to currently existing directories, or paths relative to the application's current working directory.

Ideally, you will provide <a href="../../api/c/dbset_partition_dirs.md" class="olink">DB-&gt;set_partition_dirs()</a> with an array that is the same size as the number of partitions you are creating for your database. Partition files are then placed according to the order that directories are contained in the array; partition 0 is placed in directory_array\[0\], partition 1 in directory_array\[1\], and so forth. However, if you provide an array of directories that is smaller than the number of database partitions, then the directories are used on a round-robin fashion.

You must call <a href="../../api/c/dbset_partition_dirs.md" class="olink">DB-&gt;set_partition_dirs()</a> before you create your database, and before you open your database each time thereafter. The array provided to <a href="../../api/c/dbset_partition_dirs.md" class="olink">DB-&gt;set_partition_dirs()</a> must not change after the database has been created.
