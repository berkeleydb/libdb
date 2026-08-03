---
title: "Closing Databases"
api-name: "Closing Databases"
source: docs/gsg/CXX/coredbclose.html
---
## Closing Databases

Once you are done using the database, you must close it. You use the `Db::close()` method to do this.

Closing a database causes it to become unusable until it is opened again. It is recommended that you close any open cursors before closing your database. Active cursors during a database close can cause unexpected results, especially if any of those cursors are writing to the database. You should always make sure that all your database accesses have completed before closing your database.

Cursors are described in <a href="Cursors.md" class="xref" title="Chapter 4. Using Cursors">Using Cursors</a> later in this manual.

Be aware that when you close the last open handle for a database, then by default its cache is flushed to disk. This means that any information that has been modified in the cache is guaranteed to be written to disk when the last handle is closed. You can manually perform this operation using the `Db::sync()` method, but for normal shutdown operations it is not necessary. For more information about syncing your cache, see <a href="usingDbt.md#datapersist" class="xref" title="Data Persistence">Data Persistence</a>.

The following code fragment illustrates a database close:

``` c
#include <db_cxx.h>

...

Db db(NULL, 0);

 // Database open and access operations happen here.

try {
    // Close the database
    db.close(0);
// DbException is not subclassed from std::exception, so
// need to catch both of these.
} catch(DbException &e) {
    // Error handling code goes here    
} catch(std::exception &e) {
    // Error handling code goes here
} 
```
