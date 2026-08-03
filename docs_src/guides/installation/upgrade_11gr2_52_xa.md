---
title: "Berkeley DB X/Open Compliant XA Resource Manager"
api-name: "Berkeley DB X/Open Compliant XA Resource Manager"
source: docs/installation/upgrade_11gr2_52_xa.html
---
## Berkeley DB X/Open Compliant XA Resource Manager

<span class="sect2"> [Constraints](upgrade_11gr2_52_xa.md#idp973264) </span>

<span class="sect2"> [New Flag](upgrade_11gr2_52_xa.md#idp978200) </span>

<span class="sect2"> [Modified Function](upgrade_11gr2_52_xa.md#idp982256) </span>

The Berkeley DB X/open compliant XA resource manager has been restored. (It was removed from the product after the 4.7 release.) The new implementation includes support for multi-threaded servers. Consult the documentation of your chosen transaction manager to learn how to implement a multi-threaded server.

### Constraints

Applictions that use a BDB XA resource manager must now take into account the following constraints.

- No in-memory logging.
- No application-level child transactions.
- All database-level operations (open, close, create and the like) must be performed outside of a global transactions (i.e., they can be performed in local BDB transactions, but not while a distributed XA transaction is active).
- Environment configuration must be done using a DB_CONFIG file.
- Cursors must be closed before a service invocation returns.

### New Flag

- `DB_XA_CREATE` - This flag is passed to <a href="../../api/c/dbcreate.md" class="olink">db_create()</a> to create a `DB` handle that supports XA transactions.

### Modified Function

- <a href="../../api/c/dbstat.md" class="olink">DB-&gt;stat()</a> now returns the field `DB_TXN_STAT->DB_TXN_ACTIVE->xa_status`, which contains information on the XA transactions.
