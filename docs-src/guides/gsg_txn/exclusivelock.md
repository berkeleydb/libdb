---
title: "Exclusive Database Handles"
api-name: "Exclusive Database Handles"
source: docs/gsg_txn/C/exclusivelock.html
---
## Exclusive Database Handles

In some cases, concurrent applications can benefit from occasionally granting exclusive access to the entire database to a single database handle. This is desirable when a thread will perform an operation that touches all or most of the pages in a database.

To configure a handle to have exclusive access to a database, you give it a single write lock to the entire database. This causes all other threads to block when they attempt to gain a read or write lock to any part of that database.

The exclusive lock allows for improved throughput because the handle will not attempt to acquire any further locks once it has the exclusive write lock. It will also never be blocked waiting for a lock, and there is no possibility of a deadlock/retry cycle.

Note that an exclusive database handle can only have one transaction active for it at a time.

To configure a database handle with an exclusive lock, you use the `DB->set_lk_exclusive()` method before you open the database handle. Setting a value of `0` to this method means that the handle open operation will block until it can obtain the exclusive lock. A non-zero value means that if the method cannot obtain the exclusive lock immediately when the handle is opened, the open operation will exit with a `DB_LOCK_NOTGRANTED` error return.

Once configured and opened, a handled configured with an exclusive database lock will hold that lock until the handle is closed.
