---
title: "DB->cursor()"
api-name: "DB->cursor()"
source: docs/api_reference/C/dbcursor.html
---
## DB-\>cursor()

``` c
#include <db.h>

int
DB->cursor(DB *db, DB_TXN *txnid, DBC **cursorp, u_int32_t flags);  
```

The `DB->cursor()` method returns a created database cursor.

Cursors may span threads, but only serially, that is, the application must serialize access to the cursor handle.

The `DB->cursor()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### txnid

To transaction-protect cursor operations, cursors must be opened and closed within the context of a transaction. The **txnid** parameter specifies the transaction context in which the cursor may be used.

Cursor operations are not automatically transaction-protected, even if the <a href="envset_flags.md#envset_flags_DB_AUTO_COMMIT" class="link">DB_AUTO_COMMIT</a> flag is specified to the <a href="envset_flags.md" class="xref" title="DB_ENV-&gt;set_flags()">DB_ENV-&gt;set_flags()</a> or <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> methods. If cursor operations are to be transaction-protected, the **txnid** parameter must be a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; otherwise, NULL.

#### cursorp

The **cursorp** parameter references memory into which a pointer to the allocated cursor is copied.

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_CURSOR_BULK`

  Configure a cursor to optimize for bulk operations. Each successive operation on a cursor configured with this flag attempts to continue on the same database page as the previous operation, falling back to a search if a different page is required. This avoids searching if there is a high degree of locality between cursor operations. This flag is currently only effective with the btree access method. For other access methods, this flag is ignored.

- `DB_READ_COMMITTED`

  Configure a transactional cursor to have degree 2 isolation. This ensures the stability of the current data item read by this cursor but permits data read by this cursor to be modified or deleted prior to the commit of the transaction for this cursor.

- `DB_READ_UNCOMMITTED`

  Configure a transactional cursor to have degree 1 isolation. Read operations performed by the cursor may return modified but not yet committed data. Silently ignored if the <a href="dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="link">DB_READ_UNCOMMITTED</a> flag was not specified when the underlying database was opened.

- `DB_WRITECURSOR`

  Specify that the cursor will be used to update the database. The underlying database environment must have been opened using the <a href="envopen.md#envopen_DB_INIT_CDB" class="link">DB_INIT_CDB</a> flag.

- `DB_TXN_SNAPSHOT`

  Configure a transactional cursor to operate with read-only <a href="../../guides/programmer_reference/transapp_read.md" class="olink">snapshot isolation</a>. For databases with the <a href="dbopen.md#dbopen_DB_MULTIVERSION" class="link">DB_MULTIVERSION</a> flag set, data values will be read as they are when the cursor is opened, without taking read locks.

  This flag implicitly begins a transaction that is committed when the cursor is closed.

  This flag is silently ignored if <a href="dbopen.md#dbopen_DB_MULTIVERSION" class="link">DB_MULTIVERSION</a> is not set on the underlying database or if a transaction is supplied in the **txnid** parameter.

### Errors

The `DB->cursor()` method may fail and return one of the following non-zero errors:

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="dbc.md#dbclist" class="xref" title="Database Cursors and Related Methods">Database Cursors and Related Methods</a>
