---
title: "Java"
api-name: "Java"
source: docs/upgrading/upgrade_4_3_java.html
---
## Java

The Berkeley DB Java API has changed significantly in the 4.3 release, in ways incompatible with previous releases. This has been done to provide a consistent Java-like API for Berkeley DB as well as to make the Berkeley DB Java API match the API in Berkeley DB Java Edition, to ease application-porting between the two libraries.

Here is a summary of the major changes:

- Db -\> Database
- Dbc -\> Cursor
- Dbt -\> DatabaseEntry
- DbEnv -\> Environment
- DbTxn -\> Transaction
- Db.cursor -\> Database.openCursor
- Dbc.get(..., DbConstants.DB_CURRENT) -\> Cursor.getCurrent(...)

1.  The low-level wrapper around the C API has been moved into a package called com.sleepycat.db.internal.
2.  There is a new public API in the package com.sleepycat.db.
3.  All flags and error numbers have been eliminated from the public API. All configuration is done through method calls on configuration objects.
4.  All classes and methods are named to Java standards, matching Berkeley DB Java Edition. For example:
5.  The statistics classes have "getter" methods for all fields.
6.  In transactional applications, the Java API infers whether to auto-commit operations: if an update is performed on a transactional database without supplying a transaction, it is implicitly auto-committed.
7.  The com.sleepycat.bdb.\* packages have been reorganized so that the binding classes can be used with the base API in the com.sleepycat.db package. The bind and collection classes are now essentially the same in Berkeley DB and Berkeley DB Java Edition. The former com.sleepycat.bdb.bind.\* packages are now the com.sleepycat.bind.\* packages. The former com.sleepycat.bdb, com.sleepycat.bdb.collections, and com.sleepycat.bdb.factory packages are now combined in the new com.sleepycat.collections package.
8.  A layer of the former collections API has been removed to simplify the API and to remove the redundant implementation of secondary indices. The former DataStore, DataIndex, and ForeignKeyIndex classes have been removed. Instead of wrapping a Database in a DataStore or DataIndex, the Database object is now passed directly to the constructor of a StoredMap, StoredList, etc.
