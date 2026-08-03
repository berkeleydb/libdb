---
title: "Berkeley DB Concepts"
api-name: "Berkeley DB Concepts"
source: docs/gsg/CXX/concepts.html
---
## Berkeley DB Concepts

Before continuing, it is useful to describe some of the larger concepts that you will encounter when building a DB application.

Conceptually, DB databases contain <span class="emphasis">*records*</span>. Logically each record represents a single entry in the database. Each such record contains two pieces of information: a key and a data. This manual will on occasion describe a <span class="emphasis">*a record's key*</span> or a <span class="emphasis">*record's data*</span> when it is necessary to speak to one or the other portion of a database record.

Because of the key/data pairing used for DB databases, they are sometimes thought of as a two-column table. However, data (and sometimes keys, depending on the access method) can hold arbitrarily complex data. Frequently, C structures and other such mechanisms are stored in the record. This effectively turns a 2-column table into a table with <span class="emphasis">*n*</span> columns, where <span class="emphasis">*n-1*</span> of those columns are provided by the structure's fields.

Note that a DB database is very much like a table in a relational database system in that most DB applications use more than one database (just as most relational databases use more than one table).

Unlike relational systems, however, a DB database contains a single collection of records organized according to a given access method (BTree, Queue, Hash, and so forth). In a relational database system, the underlying access method is generally hidden from you.

In any case, frequently DB applications are designed so that a single database stores a specific type of data (just as in a relational database system, a single table holds entries containing a specific set of fields). Because most applications are required to manage multiple kinds of data, a DB application will often use multiple databases.

For example, consider an accounting application. This kind of an application may manage data based on bank accounts, checking accounts, stocks, bonds, loans, and so forth. An accounting application will also have to manage information about people, banking institutions, customer accounts, and so on. In a traditional relational database, all of these different kinds of information would be stored and managed using a (probably very) complex series of tables. In a DB application, all of this information would instead be divided out and managed using multiple databases.

DB applications can efficiently use multiple databases using an optional mechanism called an <span class="emphasis">*environment*</span>. For more information, see <a href="environments.md" class="xref" title="Environments">Environments</a>.

You interact with most DB APIs using special structures that contain pointers to functions. These callbacks are called <span class="emphasis">*methods*</span> because they look so much like a method on a C++ class. The variable that you use to access these methods is often referred to as a <span class="emphasis">*handle*</span>. For example, to use a database you will obtain a handle to that database.

Retrieving a record from a database is sometimes called <span class="emphasis">*getting the record*</span> because the method that you use to retrieve the records is called `get()`. Similarly, storing database records is sometimes called <span class="emphasis">*putting the record*</span> because you use the `put()` method to do this.

When you store, or put, a record to a database using its handle, the record is stored according to whatever sort order is in use by the database. Sorting is mostly performed based on the key, but sometimes the data is considered too. If you put a record using a key that already exists in the database, then the existing record is replaced with the new data. However, if the database supports duplicate records (that is, records with identical keys but different data), then that new record is stored as a duplicate record and any existing records are not overwritten.

If a database supports duplicate records, then you can use a database handle to retrieve only the first record in a set of duplicate records.

In addition to using a database handle, you can also read and write data using a special mechanism called a <span class="emphasis">*cursor*</span>. Cursors are essentially iterators that you can use to walk over the records in a database. You can use cursors to iterate over a database from the first record to the last, and from the last to the first. You can also use cursors to seek to a record. In the event that a database supports duplicate records, cursors are the only way you can access all the records in a set of duplicates.

Finally, DB provides a special kind of a database called a <span class="emphasis">*secondary database*</span>. Secondary databases serve as an index into normal databases (called primary database to distinguish them from secondaries). Secondary databases are interesting because DB records can hold complex data types, but seeking to a given record is performed only based on that record's key. If you wanted to be able to seek to a record based on some piece of information that is not the key, then you enable this through the use of secondary databases.
