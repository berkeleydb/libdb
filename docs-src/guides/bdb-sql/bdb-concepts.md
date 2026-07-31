---
title: "Berkeley DB Concepts"
api-name: "Berkeley DB Concepts"
source: docs/bdb-sql/bdb-concepts.html
---
## Berkeley DB Concepts

If you are a SQLite user who is migrating to the BDB SQL interface, then there are a few Berkeley DB-specific concepts you might want to know about.

- Environments. The directory that is created alongside your database file, and which ends with the "-journal" suffix, is actually a Berkeley DB environment directory.

- The Locking Subsystem

  The Berkeley DB library implements locking in a different way to SQLite. SQLite implements locking at a database level - any operation will take a lock on the entire database. Berkeley DB implements a scheme called page level locking. The database divides data into relatively small blocks. Each block corresponds to a page in database terms. Each block can contain multiple pieces of user information. Berkeley DB takes locks on individual pages. This allows for greater concurrency in applications, but means that applications are more likely to encounter deadlocks.

  See: <a href="lockingnotes.md" class="xref" title="Chapter 2. Locking Notes">Locking Notes</a> for more information.

- The Journal Subsystem

  The BDB SQL interface implements write ahead logging (WAL), it stores journal files differently to the SQLite WAL implementation. BDB SQL interface rolls over journal files when they get to a certain size (default 10MB). It is possible for the to be multiple journal files active at one time with BDB SQL interface.
