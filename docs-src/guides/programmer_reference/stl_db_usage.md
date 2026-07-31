---
title: "Berkeley DB configuration"
api-name: "Berkeley DB configuration"
source: docs/programmer_reference/stl_db_usage.html
---
## Berkeley DB configuration

<span class="sect2"> [Registering database and environment handles](stl_db_usage.md#idp51381760) </span>

<span class="sect2"> [Truncate requirements](stl_db_usage.md#idp51405208) </span>

<span class="sect2"> [Auto commit support](stl_db_usage.md#idp51416888) </span>

<span class="sect2"> [Database and environment identity checks](stl_db_usage.md#idp51379224) </span>

<span class="sect2"> [Products, constructors and configurations](stl_db_usage.md#idp51415360) </span>

While dbstl behaves like the C++ STL APIs in most situations, there are some Berkeley DB configuration activities that you can and should perform using dbstl. These activities are described in the following sections.

### Registering database and environment handles

Remember the following things as you use Berkeley DB Database and Environment handles with dbstl:

- If you share environment or database handles among multiple threads, remember to specify the <a href="../../api/c/dbopen.md#open_DB_THREAD" class="olink">DB_THREAD</a> flag in the open call to the handle.

- If you create or open environment and/or database handles without using the dbstl helper functions, `dbstl::open_db()` or `dbstl::open_env()`, remember that your environment and database handles should be:

  1.  Allocated in the heap via "new" operator.

  2.  Created using the <a href="../api_reference/CXX/envcreate.html#env_DB_CXX_NO_EXCEPTIONS" class="olink">DB_CXX_NO_EXCEPTIONS</a> flag.

  3.  In each thread sharing the handles, the handles are registered using either `dbstl::register_db()` or `dbstl::register_dbenv()`.

- If you opened the database or environment handle using the `open_db()` or `open_env()` functions, the thread opening the handles should not call `register_db()` or `register_env()` again. This is because they have already been registered by the `open_db()` or `open_env()` functions. However, other threads sharing these handles still must register them locally.

### Truncate requirements

Some Berkeley DB operations require there to be no open cursors on the database handle at the time the operation occurs. Dbstl is aware of these requirements, and will attempt to close the cursors opened in the current thread when it performs these operations. However, the scope of dbstl's activities in this regard are limited to the current thread; it makes no attempt to close cursors opened in other threads. So you are required to ensure there are no open cursors on database handles shared across threads when operations are performed that require all cursors on that handle to be closed.

There are only a a few operations which require all open cursors to be closed. This include all container `clear()` and `swap()` functions, and all versions of `db_vection<>::assign()` functions. These functions require all cursors to be closed for the database because by default they remove all key/data pairs from the database by truncating it.

When a function removes all key/data pairs from a database, there are two ways it can perform this activity:

- The default method is to truncate the database, which is an operation that requires all cursors to be closed. As mentioned above, it is your responsibility to close cursors opened in other threads before performing this operation. Otherwise, the operation will fail.

- Alternatively, you can specify that the database not be truncated. Instead, you can cause dbstl to delete all key/data pairs individually, one after another. In this situation, open cursors in the database will not cause the delete operations to fail. However, due to lock contention, the delete operations might not complete until all cursors are closed, which is when all their read locks are released.

### Auto commit support

Dbstl supports auto commit for some of its container's operations. When a dbstl container is created using a `Db` or `DbEnv` object, if that object was opened using the <a href="../../api/c/envset_flags.md#envset_flags_DB_AUTO_COMMIT" class="olink">DB_AUTO_COMMIT</a> flag, then every operation subsequently performed on that object will be automatically enclosed in a unique transaction (unless the operation is already in an external transaction). This is identical to how the Berkeley DB C, C++ and Java APIs behave.

Note that only a subset of a container's operations support auto commit. This is because those operations that accept or return an iterator have to exist in an external transactional context and so cannot support auto commit.

The dbstl API documentation identifies when a method supports auto commit transactions.

### Database and environment identity checks

When a container member function involves another container (for example, `db_vector::swap(self& v2)`), the two containers involved in the operation must not use the same database. Further, if the function is in an external or internal transaction context, then both containers must belong to the same transactional database environment; Otherwise, the two containers can belong to the same database environment, or two different ones.

For example, if `db_vector::swap(self& v2)` is an auto commit method or it is in an external transaction context, then `v2` must be in the same transactional database environment as this container, because a transaction is started internally that must be used by both `v2` and this container. If this container and the `v2` container have different database environments, and either of them are using transactions, an exception is thrown. This condition is checked in every such member function.

However, if the function is not in a transactional context, then the databases used by these containers can be in different environments because in this situation dbstl makes no attempt to wrap container operations in a common transaction context.

### Products, constructors and configurations

You can use dbstl with all Berkeley DB products (DS, CDS, TDS, and HA). Because dbstl is a Berkeley DB interface, all necessary configurations for these products are performed using Berkeley DB's standard create/open/set APIs.

As a result, the dbstl container constructors differ from those of C++ STL because in dbstl no configuration is supported using the container constructors. On the other hand, dbstl container constructors accept already opened and configured environment and database handles. They also provide functions to retrieve some handle configuration, such as key comparison and hash functions, as required by the C++ STL specifications.

The constructors verify that the handles passed to them are well configured. This means they ensure that no banned settings are used, as well as ensuring that all required setting are performed. If the handles are not well configured, an `InvalidArgumentException` is thrown.

If a container constructor is not passed a database or environment handle, an internal anonymous database is created for you by dbstl. This anonymous database does not provide data persistence.
