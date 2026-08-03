---
title: "DB->join()"
api-name: "DB->join()"
source: docs/api_reference/C/dbjoin.html
---
## DB-\>join()

``` c
#include <db.h>

int
DB->join(DB *primary,
    DBC **curslist, DBC **dbcp, u_int32_t flags);  
```

The `DB->join()` method creates a specialized join cursor for use in performing equality or natural joins on secondary indices. For information on how to organize your data to use this functionality, see <a href="../../guides/programmer_reference/am_cursor.md#am_join" class="olink">Equality join</a>.

The `DB->join()` method is called using the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle of the primary database.

The join cursor supports only the <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> and <a href="dbcclose.md" class="xref" title="DBcursor-&gt;close()">DBcursor-&gt;close()</a> cursor functions:

- **<a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a>**

  Iterates over the values associated with the keys to which each item in **curslist** was initialized. Any data value that appears in all items specified by the **curslist** parameter is then used as a key into the **primary**, and the key/data pair found in the **primary** is returned. The **flags** parameter must be set to 0 or the following value:

  - **`DB_JOIN_ITEM`**

    Do not use the data value found in all the cursors as a lookup key for the **primary**, but simply return it in the key parameter instead. The data parameter is left unchanged.

  In addition, the following flag may be set by bitwise inclusively **OR**'ing it into the **flags** parameter:

  - **`DB_READ_UNCOMMITTED`**

    Configure a transactional join operation to have degree 1 isolation, reading modified but not yet committed data. Silently ignored if the <a href="dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="link">DB_READ_UNCOMMITTED</a> flag was not specified when the underlying database was opened.

  - **`DB_RMW`**

    Acquire write locks instead of read locks when doing the read, if locking is configured. Setting this flag can eliminate deadlock during a read-modify-write cycle by acquiring the write lock during the read part of the cycle so that another thread of control acquiring a read lock for the same item, in its own read-modify-write cycle, will not result in deadlock.

- **<a href="dbcclose.md" class="xref" title="DBcursor-&gt;close()">DBcursor-&gt;close()</a>**

  Close the returned cursor and release all resources. (Closing the cursors in **curslist** is the responsibility of the caller.)

The `DB->join()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### curslist

The **curslist** parameter contains a NULL terminated array of cursors. Each cursor must have been initialized to refer to the key on which the underlying database should be joined. Typically, this initialization is done by a <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> call with the <a href="dbcget.md#dbcget_DB_SET" class="link">DB_SET</a> flag specified. Once the cursors have been passed as part of a **curslist**, they should not be accessed or modified until the newly created join cursor has been closed, or else inconsistent results may be returned.

Joined values are retrieved by doing a sequential iteration over the first cursor in the **curslist** parameter, and a nested iteration over each secondary cursor in the order they are specified in the **curslist** parameter. This requires database traversals to search for the current datum in all the cursors after the first. For this reason, the best join performance normally results from sorting the cursors from the one that refers to the least number of data items to the one that refers to the most. By default, `DB->join()` does this sort on behalf of its caller.

For the returned join cursor to be used in a transaction-protected manner, the cursors listed in **curslist** must have been created within the context of the same transaction.

#### dbcp

The newly created join cursor is returned in the memory location to which **dbcp** refers.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_JOIN_NOSORT`

  Do not sort the cursors based on the number of data items to which they refer. If the data are structured so that cursors with many data items also share many common elements, higher performance will result from listing those cursors before cursors with fewer data items; that is, a sort order other than the default. The DB_JOIN_NOSORT flag permits applications to perform join optimization prior to calling the `DB->join()` method.

### Errors

The `DB->join()` method may fail and return one of the following non-zero errors:

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

#### DB_SECONDARY_BAD

A secondary index references a nonexistent primary key.

#### EINVAL

If cursor methods other than <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> or <a href="dbcclose.md" class="xref" title="DBcursor-&gt;close()">DBcursor-&gt;close()</a> were called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
