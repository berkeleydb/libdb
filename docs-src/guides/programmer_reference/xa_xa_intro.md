---
title: "XA Introduction"
api-name: "XA Introduction"
source: docs/programmer_reference/xa_xa_intro.html
---
## XA Introduction

Berkeley DB can be used as an XA-compliant resource manager. The XA implementation is known to work with the Tuxedo transaction manager.

The XA support is encapsulated in the resource manager switch db_xa_switch, which defines the following functions:

- <span class="emphasis">*\_\_db_xa_close.*</span> Close the resource manager.
- <span class="emphasis">*\_\_db_xa_commit.* </span> Commit the specified transaction.
- <span class="emphasis">*\_\_db_xa_complete.*</span> Wait for asynchronous operations to complete.
- <span class="emphasis">*\_\_db_xa_end.* </span> Disassociate the application from a transaction.
- <span class="emphasis">*\_\_db_xa_forget.*</span> Forget about a transaction that was heuristically completed. (Berkeley DB does not support heuristic completion.)
- <span class="emphasis">*\_\_db_xa_open.*</span> Open the resource manager.
- <span class="emphasis">*\_\_db_xa_prepare.*</span> Prepare the specified transaction.
- <span class="emphasis">*\_\_db_xa_recover.*</span> Return a list of prepared, but not yet committed transactions.
- <span class="emphasis">*\_\_db_xa_rollback.*</span> Abort the specified transaction.
- <span class="emphasis">*\_\_db_xa_start.*</span> Associate the application with a transaction.

The Berkeley DB resource manager does not support the following optional XA features:

- Asynchronous operations
- Transaction migration

The Tuxedo System is available from <a href="http://www.oracle.com/us/bea/index.html" class="ulink" target="_top">Oracle BEA Systems</a>.
