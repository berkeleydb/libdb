---
title: "DB_NOSYNC Flag to Flush Files"
api-name: "DB_NOSYNC Flag to Flush Files"
source: docs/installation/build_unix_db_nosync.html
---
## DB_NOSYNC Flag to Flush Files

Applications must now pass the DB_NOSYNC flag to the methods - `DB->remove`, `DB->rename`, `DB_ENV->dbremove`, and `DB_ENV->dbrename`, to avoid a multi-database file to be flushed from cache. This flag is applicable if you have created the database handle in a non-transactional environment.

By default, all non-transactional database remove/rename operations cause data to be synced to disk. This can now be overridden using the DB_NOSYNC flag so that files can be accessed outside the environment after the database handles are closed.
