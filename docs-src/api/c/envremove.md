---
title: "DB_ENV->remove()"
api-name: "DB_ENV->remove()"
source: docs/api_reference/C/envremove.html
---
## DB_ENV-\>remove()

``` c
#include <db.h>

int
DB_ENV->remove(DB_ENV *dbenv, char *db_home, u_int32_t flags);  
```

The `DB_ENV->remove()` method destroys a Berkeley DB environment if it is not currently in use. The environment regions, including any backing files, are removed. Any log or database files and the environment directory are not removed.

If there are processes that have called <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> without calling <a href="envclose.md" class="xref" title="DB_ENV-&gt;close()">DB_ENV-&gt;close()</a> (that is, there are processes currently using the environment), `DB_ENV->remove()` will fail without further action unless the <a href="envremove.md#envremove_DB_FORCE" class="link">DB_FORCE</a> flag is set, in which case `DB_ENV->remove()` will attempt to remove the environment, regardless of any processes still using it.

The result of attempting to forcibly destroy the environment when it is in use is unspecified. Processes using an environment often maintain open file descriptors for shared regions within it. On UNIX systems, the environment removal will usually succeed, and processes that have already joined the region will continue to run in that region without change. However, processes attempting to join the environment will either fail or create new regions. On other systems in which the **unlink**(2) system call will fail if any process has an open file descriptor for the file (for example Windows/NT), the region removal will fail.

Calling `DB_ENV->remove()` should not be necessary for most applications because the Berkeley DB environment is cleaned up as part of normal database recovery procedures. However, applications may want to call `DB_ENV->remove()` as part of application shut down to free up system resources. For example, if the <a href="envopen.md#envopen_DB_SYSTEM_MEM" class="link">DB_SYSTEM_MEM</a> flag was specified to <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a>, it may be useful to call `DB_ENV->remove()` in order to release system shared memory segments that have been allocated. Or, on architectures in which mutexes require allocation of underlying system resources, it may be useful to call `DB_ENV->remove()` in order to release those resources. Alternatively, if recovery is not required because no database state is maintained across failures, and no system resources need to be released, it is possible to clean up an environment by simply removing all the Berkeley DB files in the database environment's directories.

In multithreaded applications, only a single thread may call the `DB_ENV->remove()` method.

A <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle that has already been used to open an environment should not be used to call the `DB_ENV->remove()` method; a new <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle should be created for that purpose.

After `DB_ENV->remove()` has been called, regardless of its return, the Berkeley DB environment handle may not be accessed again.

The `DB_ENV->remove()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### db_home

The **db_home** parameter names the database environment to be removed.

When using a Unicode build on Windows (the default), the **db_home** argument will be interpreted as a UTF-8 string, which is equivalent to ASCII for Latin characters.

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_FORCE`

  If set, the environment is removed, regardless of any processes that may still using it, and no locks are acquired during this process. (Generally, this flag is specified only when applications were unable to shut down cleanly, and there is a risk that an application may have died holding a Berkeley DB lock.)

- `DB_USE_ENVIRON`

  The Berkeley DB process' environment may be permitted to specify information to be used when naming files; see <a href="../../guides/programmer_reference/env_naming.md" class="olink">Berkeley DB File Naming</a>. Because permitting users to specify which files are used can create security problems, environment information will be used in file naming for all users only if the `DB_USE_ENVIRON` flag is set.

- `DB_USE_ENVIRON_ROOT`

  The Berkeley DB process' environment may be permitted to specify information to be used when naming files; see <a href="../../guides/programmer_reference/env_naming.md" class="olink">Berkeley DB File Naming</a>. Because permitting users to specify which files are used can create security problems, if the `DB_USE_ENVIRON_ROOT` flag is set, environment information will be used in file naming only for users with appropriate permissions (for example, users with a user-ID of 0 on `UNIX` systems).

### Errors

The `DB_ENV->remove()` method may fail and return one of the following non-zero errors:

#### EBUSY

The shared memory region was in use and the force flag was not set.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
