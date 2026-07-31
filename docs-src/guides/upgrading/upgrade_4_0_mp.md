---
title: "memp_XXX"
api-name: "memp_XXX"
source: docs/upgrading/upgrade_4_0_mp.html
---
## memp_XXX

The C API for the Berkeley DB Memory Pool subsystem was reworked in the 4.0 release as follows:

| Historic functional interface | Berkeley DB 4.X method |
|----|----|
| memp_register | <a href="../../api/c/mempregister.md" class="olink">DB_ENV-&gt;memp_register()</a> |
| memp_stat | <a href="../../api/c/mempstat.md" class="olink">DB_ENV-&gt;memp_stat()</a> |
| memp_sync | <a href="../../api/c/mempsync.md" class="olink">DB_ENV-&gt;memp_sync()</a> |
| memp_trickle | <a href="../../api/c/memptrickle.md" class="olink">DB_ENV-&gt;memp_trickle()</a> |
| memp_fopen | <a href="../../api/c/mempfcreate.md" class="olink">DB_ENV-&gt;memp_fcreate()</a> |
| DB_MPOOL_FINFO: ftype | <a href="../../api/c/mempset_ftype.md" class="olink">DB_MPOOLFILE-&gt;set_ftype()</a> |
| DB_MPOOL_FINFO: pgcookie | <a href="../../api/c/mempset_pgcookie.md" class="olink">DB_MPOOLFILE-&gt;set_pgcookie()</a> |
| DB_MPOOL_FINFO: fileid | <a href="../../api/c/mempset_fileid.md" class="olink">DB_MPOOLFILE-&gt;set_fileid()</a> |
| DB_MPOOL_FINFO: lsn_offset | <a href="../../api/c/mempset_lsn_offset.md" class="olink">DB_MPOOLFILE-&gt;set_lsn_offset()</a> |
| DB_MPOOL_FINFO: clear_len | <a href="../../api/c/mempset_clear_len.md" class="olink">DB_MPOOLFILE-&gt;set_clear_len()</a> |
| memp_fopen | <a href="../../api/c/mempfopen.md" class="olink">DB_MPOOLFILE-&gt;open()</a> |
| memp_fclose | <a href="../../api/c/mempfclose.md" class="olink">DB_MPOOLFILE-&gt;close()</a> |
| memp_fput | <a href="../../api/c/mempput.md" class="olink">DB_MPOOLFILE-&gt;put()</a> |
| memp_fset | DB_MPOOLFILE-\>set |
| memp_fsync | <a href="../../api/c/mempfsync.md" class="olink">DB_MPOOLFILE-&gt;sync()</a> |

Applications calling any of the memp_register, memp_stat, memp_sync or memp_trickle functions should update those calls to use the enclosing <a href="../../api/c/env.md" class="olink">DB_ENV class</a> handle's method (easily done as the first argument to the existing call is the correct <a href="../../api/c/env.md" class="olink">DB_ENV class</a> handle).

In addition, the <a href="../../api/c/mempstat.md" class="olink">DB_ENV-&gt;memp_stat()</a> call has been changed in the 4.0 release to take a flags argument. To leave their historic behavior unchanged, applications should add a final argument of 0 to any calls made to <a href="../../api/c/mempstat.md" class="olink">DB_ENV-&gt;memp_stat()</a>.

Applications calling the memp_fopen function should update those calls as follows: First, acquire a <a href="../../api/c/memp.md" class="olink">Cache chapter</a> handle using the <a href="../../api/c/mempfcreate.md" class="olink">DB_ENV-&gt;memp_fcreate()</a> method. Second, if the DB_MPOOL_FINFO structure reference passed to the memp_fopen function was non-NULL, call the <a href="../../api/c/memp.md" class="olink">Cache chapter</a> method corresponding to each initialized field in the DB_MPOOL_FINFO structure. Third, call the <a href="../../api/c/mempfopen.md" class="olink">DB_MPOOLFILE-&gt;open()</a> method method to open the underlying file. If the <a href="../../api/c/mempfopen.md" class="olink">DB_MPOOLFILE-&gt;open()</a> method call fails, then <a href="../../api/c/mempfclose.md" class="olink">DB_MPOOLFILE-&gt;close()</a> method must be called to destroy the allocated handle.

Applications calling the memp_fopen, memp_fclose, memp_fput, memp_fset, or memp_fsync functions should update those calls to use the enclosing <a href="../../api/c/memp.md" class="olink">Cache chapter</a> handle's method. Again, this is easily done as the first argument to the existing call is the correct <a href="../../api/c/memp.md" class="olink">Cache chapter</a> handle. With one exception, the calling conventions of the old and new interfaces are identical; the one exception is the <a href="../../api/c/mempfclose.md" class="olink">DB_MPOOLFILE-&gt;close()</a> method, which requires an additional flag parameter that should be set to 0.
