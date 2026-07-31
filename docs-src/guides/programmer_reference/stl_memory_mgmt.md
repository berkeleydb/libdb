---
title: "Dbstl memory management"
api-name: "Dbstl memory management"
source: docs/programmer_reference/stl_memory_mgmt.html
---
## Dbstl memory management

<span class="sect2"> [Freeing memory](stl_memory_mgmt.md#idp51564672) </span>

<span class="sect2"> [Type specific notes](stl_memory_mgmt.md#idp51569240) </span>

### Freeing memory

When using dbstl, make sure memory allocated in the heap is released after use. The rules for this are:

- dbstl will free/delete any memory allocated by dbstl itself.

- You are responsible for freeing/deleting any memory allocated by your code outside of dbstl.

### Type specific notes

#### DbEnv/Db

When you open a `DbEnv` or `Db` object using `dbstl::open_env()` or `dbstl::open_db()`, you do not need to delete that object. However, if you new'd that object and then opened it without using the `dbstl::open_env()` or `dbstl::open_db()` methods, you are responsible for deleting the object.

Note that you must `new` the `Db` or `DbEnv` object, which allocates it on the heap. You can not allocate it on the stack. If you do, the order of destruction is uncontrollable, which makes dbstl unable to work properly.

You can call `dbstl_exit()` before the process exits, to release any memory allocated by dbstl that has to live during the entire process lifetime. Releasing the memory explicitly will not make much difference, because the process is about to exit and so all memory allocated on the heap is going to be returned to the operating system anyway. The only real difference is that your memory leak checker will not report false memory leaks.

`dbstl_exit()` releases any memory allocated by dbstl on the heap. It also performs other required shutdown operations, such as closing any databases and environments registered to dbstl and shared across the process.

If you are calling the `dbstl_exit()` function, and your `DbEnv` or `Db` objects are new'd by your code, the `dbstl_exit()` function should be called before deleting the `DbEnv` or `Db` objects, because they need to be closed before being deleted. Alternatively, you can call the `dbstl::close_env()` or `dbstl::close_db()` functions before deleting the `DbEnv` or `Db` objects in order to explicitly close the databases or environments. If you do this, can then delete these objects, and then call `dbstl_exit()`.

In addition, before exiting a thread that uses dbstl API, you can call the `dbstl_thread_exit() `function to release any Berkeley DB handles if they are not used by other threads. If you do not call the `dbstl_thread_exit() `function or call this function only in some threads, all open Berkeley DB handles will be closed by the `dbstl_exit()`function. You must call the `dbstl_exit() `function before the process exits, to avoid memory leak and database update loss, if you do not have transactions and persistent log files.

#### DbstlDbt

Only when you are storing raw bytes (such as a bitmap) do you have to store and retrieve data by using the `DbstlDbt` helper class. Although you also can do so simply by using the Berkeley DB `Dbt` class, the `DbstlDbt` class offers more convenient memory management behavior.

When you are storing `DbstlDbt` objects (such as `db_vector<DbstlDbt>`), you <span class="emphasis">*must*</span> allocate heap memory explicitly using the `malloc()` function for the `DbstlDbt` object to reference, but you do not need to free the memory – it is automatically freed by the `DbstlDbt` object that owns it by calling the standard C library `free()` function.

However, because dbstl supports storing any type of object or primitive data, it is rare that you would have to store data using `DbstlDbt` objects while using dbstl. Examples of storing `DbstlDbt` objects can be found in the `StlAdvancedFeaturesExample::arbitrary_object_storage()` and `StlAdvancedFeaturesExample::char_star_string_storage()` methods.
