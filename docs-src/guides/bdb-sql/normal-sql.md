---
title: "Differences for Users of other SQL Engines"
api-name: "Differences for Users of other SQL Engines"
source: docs/bdb-sql/normal-sql.html
---
## Differences for Users of other SQL Engines

If you are used to a SQL implementation from other SQL engine (such as Oracle's RDBMS), the SQL used by the BDB SQL interface (which is the same as used by SQLite) may hold some surprises for you.

Some things in particular to take note of:

- Datatyping is weaker in SQLite than it is with standard SQL. For example, SQLite does not enforce the length of a `VARCHAR`. While standard SQL will truncate a `VARCHAR` that is too long, you could (for example) declare a `VARCHAR(10)` then put 500 characters in it without any truncation, ever.

  SQLite datatyping is described in detail on the Datatypes in SQLite Version 3 page.

- Do not use autocommit with SQLite. Instead, use `begin exclusive` and then `commit`.

- How NULLs are handled in SQLite may be different from what you are used to. See NULL Handling in SQLite Versus Other Database Engines for details.

- There are some features of SQL that SQLite does not support. For more information, see SQL Features That SQLite Does Not Implement.
