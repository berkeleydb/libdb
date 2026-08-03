---
title: "Specifying a Berkeley DB schema using SQL DDL"
api-name: "Specifying a Berkeley DB schema using SQL DDL"
source: docs/programmer_reference/am_misc_db_sql.html
---
## Specifying a Berkeley DB schema using SQL DDL

When starting a new Berkeley DB project, much of the code that you must write is dedicated to defining the BDB environment: what databases it contains, the types of the databases, and so forth. Also, since records in BDB are just byte arrays, you must write code that assembles and interprets these byte arrays.

Much of this code can be written automatically (in C) by the db_sql_codegen utility. To use it, you first specify the schema of your Berkeley DB environment in SQL Data Definition Language (DDL). Then you invoke the db_sql_codegen command, giving the DDL as input. <span class="command">**db_sql_codegen**</span> reads the DDL, and writes C code that implements a storage-layer API suggested by the DDL.

The generated API includes a general-purpose initialization function, which sets up the environment and the databases (creating them if they don't already exist). It also includes C structure declarations for each record type, and numerous specialized functions for storing and retrieving those records.

<span class="command">**db_sql_codegen**</span> can also produce a simple test program that exercises the generated API. This program is useful as an example of how to use the API. It contains calls to all of the interface functions, along with commentary explaining what the code is doing.

Once the storage layer API is produced, your application may use it as is, or you may customize it as much as you like by editing the generated source code. Be warned, however: <span class="command">**db_sql_codegen**</span> is a one-way process; there is no way to automatically incorporate customizations into newly generated code, if you decide to run <span class="command">**db_sql_codegen**</span> again.

To learn more about <span class="command">**db_sql_codegen**</span>, please consult the db_sql_codegen utility manual page in the Berkeley DB C API Reference Guide.
