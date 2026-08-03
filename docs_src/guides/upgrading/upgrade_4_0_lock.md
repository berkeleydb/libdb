---
title: "lock_XXX"
api-name: "lock_XXX"
source: docs/upgrading/upgrade_4_0_lock.html
---
## lock_XXX

The C API for the Berkeley DB Locking subsystem was reworked in the 4.0 release as follows:

| Historic functional interface | Berkeley DB 4.X method |
|----|----|
| lock_detect | <a href="../../api/c/lockdetect.md" class="olink">DB_ENV-&gt;lock_detect()</a> |
| lock_get | <a href="../../api/c/lockget.md" class="olink">DB_ENV-&gt;lock_get()</a> |
| lock_id | <a href="../../api/c/lockid.md" class="olink">DB_ENV-&gt;lock_id()</a> |
| lock_put | <a href="../../api/c/lockput.md" class="olink">DB_ENV-&gt;lock_put()</a> |
| lock_stat | <a href="../../api/c/lockstat.md" class="olink">DB_ENV-&gt;lock_stat()</a> |
| lock_vec | <a href="../../api/c/lockvec.md" class="olink">DB_ENV-&gt;lock_vec()</a> |

Applications calling any of these functions should update their calls to use the enclosing <a href="../../api/c/env.md" class="olink">DB_ENV</a> handle's method (easily done as the first argument to the existing call is the correct handle to use).

In addition, the <a href="../../api/c/lockstat.md" class="olink">DB_ENV-&gt;lock_stat()</a> call has been changed in the 4.0 release to take a flags argument. To leave their historic behavior unchanged, applications should add a final argument of 0 to any calls made to <a href="../../api/c/lockstat.md" class="olink">DB_ENV-&gt;lock_stat()</a>.

The C++ and Java APIs for the DbLock::put (DbLock.put) method was reworked in the 4.0 release to make the lock put interface a method of the <a href="../../api/c/env.md" class="olink">DB_ENV</a> handle rather than the DbLock handle. Applications calling the DbLock::put or DbLock.put method should update their calls to use the enclosing <a href="../../api/c/env.md" class="olink">DB_ENV</a> handle's method (easily done as the first argument to the existing call is the correct handle to use).
