---
title: "Using dbstl in multithreaded applications"
api-name: "Using dbstl in multithreaded applications"
source: docs/programmer_reference/stl_mt_usage.html
---
## Using dbstl in multithreaded applications

Multithreaded use of dbstl must obey the following guidelines:

1.  For a few non-standard platforms, you must first configure dbstl for that platform, but usually the configure script will detect the applicable thread local storage (TLS) modifier to use, and then use it. If no appropriate TLS is found, the pthread TLS API is used.

2.  Perform all initializations in a single thread. `dbstl::dbstl_startup()` should be called mutually exclusive in a single thread before using dbstl. If dbstl is used in only a single thread, this function does not need to be called.

    If necessary, callback functions for a complex type T must be registered to the singleton of DbstlElemTraits\<T\> before any container related to T (for example, `db_vector<T>`), is used, and certain isolation may be required among multiple threads. The best way to do this is to register all callback function pointers into the singleton in a single thread before making use of the containers.

    All container cursor open flags and auto commit transaction begin/commit flags must be set in a single thread before storing objects into or reading objects from the container.

3.  Environment and database handles can optionally be shared across threads. If handles are shared, they must be registered in each thread that is using the handle (either directly, or indirectly using the containers that own the handles). You do this using the `dbstl::register_db()` and `dbstl::register_db_env()` functions. Note that these functions are not necessary if the current thread called `dbstl::open_db()` or `dbstl::open_env()` for the handle that is being shared. This is because the open functions automatically register the handle for you.

    Note that the get/set functions that provide access to container data members are not mutex-protected because these data members are supposed to be set only once at container object initialization. Applications wishing to modify them after initialization must supply their own protection.

4.  While container objects can be shared between multiple threads, iterators and transactions can not be shared.

5.  Set the **directdb_get** parameter of the container `begin()` method to `true` in order to guarantee that referenced key/data pairs are always obtained from the database and not from an iterator's cached value. (This is the default behavior.) You should do this because otherwise a rare situation may occur. Given db_vector_iterator i1 and i2 used in the same iteration, setting \*i1 = new_value will not update i2, and \*i2 will return the original value.

6.  If using a CDS database, only const iterators or read-only non-const iterators should be used for read only iterations. Otherwise, when multiple threads try to open read-write iterators at the same time, performance is greatly degraded because CDS only supports one write cursor open at any moment. The use of read-only iterators is good practice in general because dbstl contains internal optimizations for read-only iterators.

    To create a read-only iterator, do one of the following:

    - Use a `const` reference to the container object, then call the container's `begin()` method using the const reference, and then store the return value from the `begin()` method in a `db_vector::const_iterator`.

    - If you are using a non-const container object, then simply pass `true` to the **readonly** parameter of the non-const `begin()` method.

7.  When using DS, CDS or TDS, enable the locking subsystem by passing the <a href="../../api/c/envopen.md#envopen_DB_INIT_LOCK" class="olink">DB_INIT_LOCK</a> flag to `DbEnv::open()`.

8.  Perform portable thread synchronization within a process by calling the following functions. These are all global functions in the "dbstl" name space:

    |                                 |
    |---------------------------------|
    | `db_mutex_t alloc_mutex();`     |
    | `int lock_mutex(db_mutex_t);`   |
    | `int unlock_mutex(db_mutex_t);` |
    | `void free_mutex(db_mutex_t);`  |

    These functions use an internal dbstl environment's mutex functionality to synchronize. As a result, the synchronization is portable across all platforms supported by Berkeley DB.

The `WorkerThread` class provides example code demonstrating the use of dbstl in multi-threaded applications. You can find this class implemented in the dbstl test suite.
