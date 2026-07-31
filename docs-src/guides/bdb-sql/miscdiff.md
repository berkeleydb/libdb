---
title: "Miscellaneous Differences"
api-name: "Miscellaneous Differences"
source: docs/bdb-sql/miscdiff.html
---
## Miscellaneous Differences

The following miscellaneous differences also exist between the BDB SQL interface and SQLite:

- The BDB SQL interface does not support the `IMMEDIATE` keyword (`BEGIN IMMEDIATE` behaves just like `BEGIN`).

- When an exclusive transaction is active, it will block any new transactions from beginning (they will be blocked during their first operation until the exclusive transactions commits or aborts). Non-exclusive transactions that are active when the exclusive transaction begins will not be able to execute any more operations without being blocked until the exclusive transactions finishes.

- Enabling MVCC mostly disables exclusive transactions. Exclusive transactions can still be used, but they will run concurrently with regular transactions, even ones that write to the database. The only advantage of exclusive transactions in this case is that two exclusive transactions will be forced to run in serial, and that if an exclusive transaction and non-exclusive transaction experience deadlock, then the non-exclusive transaction will always be the transaction forced to release its locks.

  For more information on MVCC and snapshot isolation, see <a href="mvcc.md" class="xref" title="Using Multiversion Concurrency Control">Using Multiversion Concurrency Control</a>

- There are differences in how the two products work in a concurrent application that will cause the BDB SQL interface to deadlock where SQLite would result in a different error. This is because the products use different locking paradigms. See <a href="lockingnotes.md" class="xref" title="Chapter 2. Locking Notes">Locking Notes</a> for more information.

- The BDB SQL does not call the busy callback when a session attempts to operate the same database page that another session has locked. It blocks instead. This means that the functions `sqlite3_busy_handler` and `sqlite3_busy_timeout` are not effective in BDB SQL.

- The BDB SQL does not support two phase commit across databases. Attaching to multiple databases can lead to inconsistency after recovery and undetected deadlocks when accessing multiple databases from concurrent transactions in different order. Hence, applications must ensure that they access databases in the same order in any transaction that spans multiple databases. Else, a deadlock can occur that causes threads to block, and the deadlock will not be detected by Berkeley DB.

- In BDB SQL, when two sessions accessing the same database perform conflicting operations on the same page, one session will be blocked until the conflicting operations are resolved. For example,

  **Session 1:**

  ``` c
  dbsql> insert into a values (4);
  dbsql> begin;
  dbsql> insert into a values (5); 
  ```

  **Session 2:**

  ``` c
  dbsql> select * from a;
  ```

  What happens here is that Session 2 is blocked until Session 1 commits the transaction.

  **Session 1:**

  ``` c
  dbsql> commit;
  ```

  **Session 2:**

  ``` c
  dbsql> select * from a;
  4
  5
  ```

  Under such situations in SQLite, operations poll instead of blocking, and a callback is used to determine whether to continue polling.

- By default, you always only have a single database file when you use BDB SQL interface SQL, just as you do when you use SQLite. However, you can configure BDB SQL interface at compile time to create one BDB SQL interface database file for each SQL table that you create. How to perform this configuration is described in the *Berkeley DB Installation and Build Guide*.
