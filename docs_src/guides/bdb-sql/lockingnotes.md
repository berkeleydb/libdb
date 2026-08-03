---
title: "Chapter 2. Locking Notes"
api-name: "Chapter 2. Locking Notes"
source: docs/bdb-sql/lockingnotes.html
---
## Chapter 2. Locking Notes

**Table of Contents**

<span class="sect1"> [Internal Database Usage](lockingnotes.md#dbusage) </span>

<span class="sect1"> [Lock Handling](lockhandling.md) </span>

<span class="sect2"> [SQLite Lock Usage](lockhandling.md#sqllockmodel) </span>

<span class="sect2"> [Lock Usage with the BDB SQL Interface](lockhandling.md#bdblockusage) </span>

There are some important performance differences between the BDB SQL interface and SQLite, especially in a concurrent environment. This chapter gives you enough information about how the BDB SQL interface uses its database, as opposed to how SQLite uses its database, in order for you to understand the difference between the two interfaces. It then gives you some advice on how to best approach working with the BDB SQL interface in a multi-threaded environment.

If you are an existing user of SQLite, and you care about improving your application performance when using the BDB SQL interface in a concurrent situation, you should read this chapter. Existing users of Berkeley DB may also find some interesting information in this chapter, although it is mostly geared towards SQLite users.

## Internal Database Usage

The BDB SQL interface and SQLite do different things when it comes to locking data in their databases. In order to provide ACID transactions, both products must prevent concurrent access during write operations. Further, both products prevent concurrent access by obtaining software level locks that allow only the current holder of the lock to perform write access to the locked data.

The difference between the two is that when SQLite requires a lock (such as when a transaction is underway), it locks the entire database and all tables. (This is known as <span class="emphasis">*database level locking*</span>.) The BDB SQL interface, on the other hand, only locks the portion of the table being operated on within the current transactional context (this is known as <span class="emphasis">*page level locking*</span>). In most situations, this allows applications using the BDB SQL interface to operate concurrently and so have better read/write throughput than applications using SQLite. This is because there is less lock contention.

By default, one Berkeley DB logical database is created within the single database file for every SQL table that you create. Within each such logical database, each table row is represented as a Berkeley DB key/data pair.

This is important because the BDB SQL interface uses Berkeley DB's Transaction Data Store product. This means that Berkeley DB does not have to lock an entire database (all the tables within a database file) when it acquires a lock. Instead, it locks a single Berkeley DB database page (which usually contains a small sub-set of rows within a single table).

The size of database pages will differ from platform to platform (you can also manually configure this), but usually a database page can hold multiple key/data pairs; that is, multiple rows from a SQL table. Exactly how many table rows fit on a database page depends on the size of your page and the size of your table rows.

If you have an exceptionally small table, it is possible for the entire table to fit on a single database page. In this case, Berkeley DB is in essence forced to serialize access to the entire table when it requires a lock for it.

Note, however, that the case of a single table fitting on a single database page is very rare, and it in fact represents the abnormal case. Normally tables span multiple pages and so Berkeley DB will lock only portions of your tables. This locking behavior is automatic and transparent to your application.
