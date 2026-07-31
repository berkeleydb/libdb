---
title: "Using advanced Berkeley DB features with dbstl"
api-name: "Using advanced Berkeley DB features with dbstl"
source: docs/programmer_reference/stl_db_advanced_usage.html
---
## Using advanced Berkeley DB features with dbstl

<span class="sect2"> [Using bulk retrieval iterators](stl_db_advanced_usage.md#idp51421384) </span>

<span class="sect2"> [Using the DB_RMW flag](stl_db_advanced_usage.md#idp51410312) </span>

<span class="sect2"> [Using secondary index database and secondary containers](stl_db_advanced_usage.md#idp51398048) </span>

This section describes advanced Berkeley DB features that are available through dbstl.

### Using bulk retrieval iterators

Bulk retrieval is an optimization option for const iterators and nonconst but read-only iterators. Bulk retrieval can minimize the number of database accesses performed by your application. It does this by reading multiple entries at a time, which reduces read overhead. Note that non-sequential reads will benefit less from, or even be hurt by, this behavior, because it might result in unneeded data being read from the database. Also, non-serializable reads may read obsolete data, because part of the data read from the bulk read buffer may have been updated since the retrieval.

When using the default transaction isolation, iterators will perform serializable reads. In this situation, the bulk-retrieved data cannot be updated until the iterator's cursor is closed.

Iterators using a different isolation levels, such as <a href="../../api/c/dbcget.md#dbcget_DB_READ_COMMITTED" class="olink">DB_READ_COMMITTED</a> or <a href="../../api/c/dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="olink">DB_READ_UNCOMMITTED</a> will not perform serializable reads. The same is true for any iterators that do not use transactions.

A bulk retrieval iterator can only move in a singled direction, from beginning to end. This means that iterators only support operator++, and reverse iterators only support operator--.

Iterator objects that use bulk retrieval might contain hundreds of kilobytes of data, which makes copying the iterator object an expensive operation. If possible, use ++iterator rather than iterator++. This can save a useless copy construction of the iterator, as well as an unnecessary dup/close of the cursor.

You can configure bulk retrieval for each container using both in the const and non-const version of the `begin()` method. The non-const version of `begin()` will return a read-only cursor. Note that read-only means something different in C++ than it does when referring to an iterator. The latter only means that it cannot be used to update the database.

To configure the bulk retrieval buffer for an iterator when calling the `begin()` method, use the `BulkRetrievelItrOpt::bulk_retrieval(u_int32_t bulk_buffer_size)` function.

If you move a `db_vector_iterator` randomly rather than sequentially, then dbstl will not perform bulk retrieval because there is little performance gain from bulk retrieval in such an access pattern.

You can call `iterator::set_bulk_buffer()` to modify the iterator's bulk buffer size. Note that once bulk read is enabled, only the bulk buffer size can be modified. This means that bulk read cannot be disabled. Also, if bulk read was not enabled when you created the iterator, you can't enable it after creation.

Example code using this feature can be found in the `StlAdvancedFeaturesExample::bulk_retrieval_read()` method.

### Using the DB_RMW flag

The <a href="../../api/c/dbcget.md#dbcget_DB_RMW" class="olink">DB_RMW</a> flag is an optimization for non-const (read-write) iterators. This flag causes the underlying cursor to acquire a write lock when reading so as to avoid deadlocks. Passing `ReadModifyWriteOption::read_modify_write()` to a container's `begin()` method creates an iterator whose cursor has this behavior.

### Using secondary index database and secondary containers

Because duplicate keys are forbidden in primary databases, only `db_map`, `db_set` and `db_vector` are allowed to use primary databases. For this reason, they are called **primary containers**. A secondary database that supports duplicate keys can be used with `db_multimap` containers. These are called **secondary containers**. Finally, a secondary database that forbids duplicate keys can back a `db_map` container.

The **data_type** of this `db_multimap` secondary container is the **data_type** for the primary container. For example, a `db_map<int, Person>` object where the `Person` class has an `age` property of type `size_t`, a `db_multimap<size_t, Person>` using a secondary database allows access to a person by age.

A container created from a secondary database can only be used to iterate, search or delete. It can not be used to update or insert. While dbstl does expose the update and insert operations, Berkeley DB does not, and an exception will be thrown if attempts are made to insert objects into or update objects of a secondary container.

Example code demonstrating this feature is available in the `StlAdvancedFeaturesExample::secondary_containers()` method.
