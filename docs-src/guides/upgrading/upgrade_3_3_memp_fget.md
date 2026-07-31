---
title: "memp_fget, EIO"
api-name: "memp_fget, EIO"
source: docs/upgrading/upgrade_3_3_memp_fget.html
---
## memp_fget, EIO

Previous releases of Berkeley DB returned the system error EIO when the memp_fget function was called to retrieve a page, the page did not exist, and the `DB_MPOOL_CREATE` flag was not set. In the 3.3 release, the error `DB_PAGE_NOTFOUND` is returned instead, to allow applications to distinguish between recoverable and non-recoverable errors. Applications calling the memp_fget function and checking for a return of EIO should check for `DB_PAGE_NOTFOUND` instead.

Previous releases of Berkeley DB treated filesystem I/O failure (the most common of which the filesystem running out of space), as a fatal error, returning <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_RUNRECOVERY" class="olink">DB_RUNRECOVERY</a>. When a filesystem failure happens in the 3.3 release Berkeley DB returns the underlying system error (usually EIO), but can continue to run. Applications should abort any enclosing transaction when a recoverable system error occurs in order to recover from the error.
