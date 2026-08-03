---
title: "Creating a database environment"
api-name: "Creating a database environment"
source: docs/programmer_reference/env_create.html
---
## Creating a database environment

The Berkeley DB environment is created and described by the <a href="../../api/c/envcreate.md" class="olink">db_env_create()</a> and <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> interfaces. In situations where customization is desired, such as storing log files on a separate disk drive or selection of a particular cache size, applications must describe the customization by either creating an environment configuration file in the environment home directory or by arguments passed to other <a href="../../api/c/env.md" class="olink">DB_ENV</a> handle methods.

Once an environment has been created, database files specified using relative pathnames will be named relative to the home directory. Using pathnames relative to the home directory allows the entire environment to be easily moved, simplifying restoration and recovery of a database in a different directory or on a different system.

Applications first obtain an environment handle using the <a href="../../api/c/envcreate.md" class="olink">db_env_create()</a> method, then call the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method which creates or joins the database environment. There are a number of options you can set to customize <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> for your environment. These options fall into four broad categories:

<span class="term">Subsystem Initialization:</span>  
These flags indicate which Berkeley DB subsystems will be initialized for the environment, and what operations will happen automatically when databases are accessed within the environment. The flags include <a href="../../api/c/envopen.md#envopen_DB_INIT_CDB" class="olink">DB_INIT_CDB</a>, <a href="../../api/c/envopen.md#envopen_DB_INIT_LOCK" class="olink">DB_INIT_LOCK</a>, <a href="../../api/c/envopen.md#envopen_DB_INIT_LOG" class="olink">DB_INIT_LOG</a>, <a href="../../api/c/envopen.md#envopen_DB_INIT_MPOOL" class="olink">DB_INIT_MPOOL</a>, and <a href="../../api/c/envopen.md#envopen_DB_INIT_TXN" class="olink">DB_INIT_TXN</a>. The <a href="../../api/c/envopen.md#envopen_DB_INIT_CDB" class="olink">DB_INIT_CDB</a> flag does initialization for Berkeley DB Concurrent Data Store applications. (See <a href="cam.md#cam_intro" class="xref" title="Concurrent Data Store introduction">Concurrent Data Store introduction</a> for more information.) The rest of the flags initialize a single subsystem; that is, when <a href="../../api/c/envopen.md#envopen_DB_INIT_LOCK" class="olink">DB_INIT_LOCK</a> is specified, applications reading and writing databases opened in this environment will be using locking to ensure that they do not overwrite each other's changes.

<span class="term">Recovery options:</span>  
These flags, which include <a href="../../api/c/envopen.md#envopen_DB_RECOVER" class="olink">DB_RECOVER</a> and <a href="../../api/c/envopen.md#envopen_DB_RECOVER_FATAL" class="olink">DB_RECOVER_FATAL</a>, indicate what recovery is to be performed on the environment before it is opened for normal use.

<span class="term">Naming options:</span>  
These flags, which include <a href="../../api/c/envopen.md#envopen_DB_USE_ENVIRON" class="olink">DB_USE_ENVIRON</a> and <a href="../../api/c/envopen.md#envopen_DB_USE_ENVIRON_ROOT" class="olink">DB_USE_ENVIRON_ROOT</a>, modify how file naming happens in the environment.

<span class="term">Miscellaneous:</span>  
Finally, there are a number of miscellaneous flags, for example, <a href="../../api/c/dbopen.md#open_DB_CREATE" class="olink">DB_CREATE</a> which causes underlying files to be created as necessary. See the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> manual pages for further information.

Most applications either specify only the <a href="../../api/c/envopen.md#envopen_DB_INIT_MPOOL" class="olink">DB_INIT_MPOOL</a> flag or they specify all four subsystem initialization flags (<a href="../../api/c/envopen.md#envopen_DB_INIT_MPOOL" class="olink">DB_INIT_MPOOL</a>, <a href="../../api/c/envopen.md#envopen_DB_INIT_LOCK" class="olink">DB_INIT_LOCK</a>, <a href="../../api/c/envopen.md#envopen_DB_INIT_LOG" class="olink">DB_INIT_LOG</a>, and <a href="../../api/c/envopen.md#envopen_DB_INIT_TXN" class="olink">DB_INIT_TXN</a>). The former configuration is for applications that simply want to use the basic Access Method interfaces with a shared underlying buffer pool, but don't care about recoverability after application or system failure. The latter is for applications that need recoverability. There are situations in which other combinations of the initialization flags make sense, but they are rare.

The <a href="../../api/c/envopen.md#envopen_DB_RECOVER" class="olink">DB_RECOVER</a> flag is specified by applications that want to perform any necessary database recovery when they start running. That is, if there was a system or application failure the last time they ran, they want the databases to be made consistent before they start running again. It is not an error to specify this flag when no recovery needs to be done.

The <a href="../../api/c/envopen.md#envopen_DB_RECOVER_FATAL" class="olink">DB_RECOVER_FATAL</a> flag is more special-purpose. It performs catastrophic database recovery, and normally requires that some initial arrangements be made; that is, archived log files be brought back into the filesystem. Applications should not normally specify this flag. Instead, under these rare conditions, the <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility should be used.

The following is a simple example of a function that opens a database environment for a transactional program.

``` c
DB_ENV *
db_setup(char *home, char *data_dir, FILE *errfp, char *progname)
    {
    DB_ENV *dbenv;
    int ret;

    /*
     * Create an environment and initialize it for additional error
     * reporting.
     */
    if ((ret = db_env_create(&dbenv, 0)) != 0) {
        fprintf(errfp, "%s: %s\n", progname, db_strerror(ret));
        return (NULL);
    }
    dbenv->set_errfile(dbenv, errfp);
    dbenv->set_errpfx(dbenv, progname);

    /*
     * Specify the shared memory buffer pool cachesize: 5MB.
     * Databases are in a subdirectory of the environment home.
     */
    if ((ret = dbenv->set_cachesize(dbenv, 0, 
                                     5 * 1024 * 1024, 0)) != 0) {
        dbenv->err(dbenv, ret, "set_cachesize");
        goto err;
    }
    if ((ret = dbenv->set_data_dir(dbenv, data_dir)) != 0) {
        dbenv->err(dbenv, ret, "set_data_dir: %s", data_dir);
        goto err;
    }

    /* Open the environment with full transactional support. */
    if ((ret = dbenv->open(dbenv, home, DB_CREATE | DB_INIT_LOG | 
                DB_INIT_LOCK | DB_INIT_MPOOL | 
                DB_INIT_TXN, 0)) != 0) {
        dbenv->err(dbenv, ret, "environment open: %s", home);
        goto err;
    }

    return (dbenv);

err:    (void)dbenv->close(dbenv, 0);
    return (NULL);
}
```
