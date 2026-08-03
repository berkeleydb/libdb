---
title: "Db class for C++ and Java"
api-name: "Db class for C++ and Java"
source: docs/upgrading/upgrade_3_0_db_cxx.html
---
## Db class for C++ and Java

The static Db::open method and the DbInfo class have been removed in the Berkeley DB 3.0 release. The way to open a database file is to use the new Db constructor with two arguments, followed by set_XXX methods to configure the Db object, and finally a call to the new (nonstatic) Db::open(). In comparing the Berkeley DB 3.0 release open method with the 2.X static open method, the second argument is new. It is a database name, which can be null. The DbEnv argument has been removed, as the environment is now specified in the constructor. The open method no longer returns a Db, since it operates on one.

Here's a C++ example opening a Berkeley DB database using the 2.X interface:

``` c
// Note: by default, errors are thrown as exceptions
Db *table;
Db::open("lookup.db", DB_BTREE, DB_CREATE, 0644, dbenv, 0, &table);
```

In the Berkeley DB 3.0 release, this code would be written as:

``` c
// Note: by default, errors are thrown as exceptions
Db *table = new Db(dbenv, 0);
table->open("lookup.db", NULL, DB_BTREE, DB_CREATE, 0644);
```

Here's a Java example opening a Berkeley DB database using the 2.X interface:

``` c
// Note: errors are thrown as exceptions
Db table = Db.open("lookup.db", Db.DB_BTREE, Db.DB_CREATE, 0644, dbenv, 0);
```

In the Berkeley DB 3.0 release, this code would be written as:

``` c
// Note: errors are thrown as exceptions
Db table = new Db(dbenv, 0);
table.open("lookup.db", null, Db.DB_BTREE, Db.DB_CREATE, 0644);
```

Note that if the dbenv argument is null, the database will not exist within an environment.
