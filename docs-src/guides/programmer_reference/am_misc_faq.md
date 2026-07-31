---
title: "Access method FAQ"
api-name: "Access method FAQ"
source: docs/programmer_reference/am_misc_faq.html
---
## Access method FAQ

1.  **Is a Berkeley DB database the same as a "table"?**

    Yes; "tables" are databases, "rows" are key/data pairs, and "columns" are application-encapsulated fields within a data item (to which Berkeley DB does not directly provide access).

2.  **I'm getting an error return in my application, but I can't figure out what the library is complaining about.**

    See <a href="../../api/c/envset_errcall.md" class="olink">DB_ENV-&gt;set_errcall()</a>, <a href="../../api/c/envset_errfile.md" class="olink">DB_ENV-&gt;set_errfile()</a> and <a href="../../api/c/dbset_errfile.md" class="olink">DB-&gt;set_errfile()</a> for ways to get additional information about error returns from Berkeley DB.

3.  **Are Berkeley DB databases portable between architectures with different integer sizes and different byte orders ?**

    Yes. Specifically, databases can be moved between 32- and 64-bit machines, as well as between little- and big-endian machines. See <a href="general_am_conf.md#am_conf_byteorder" class="xref" title="Selecting a byte order">Selecting a byte order</a> for more information.

4.  **I'm seeing database corruption when creating multiple databases in a single physical file.**

    This problem is usually the result of <a href="../../api/c/db.md" class="olink">DB</a> handles not sharing an underlying database environment. See <a href="am_opensub.md" class="xref" title="Opening multiple databases in a single file">Opening multiple databases in a single file</a> for more information.

5.  **I'm using integers as keys for a Btree database, and even though the key/data pairs are entered in sorted order, the page-fill factor is low.**

    This is usually the result of using integer keys on little-endian architectures such as the x86. Berkeley DB sorts keys as byte strings, and little-endian integers don't sort well when viewed as byte strings. For example, take the numbers 254 through 257. Their byte patterns on a little-endian system are:

    ``` c
    254 fe 0 0 0
    255 ff 0 0 0
    256  0 1 0 0
    257  1 1 0 0
    ```

    If you treat them as strings, then they sort badly:

    ``` c
    256
    257
    254
    255
    ```

    On a big-endian system, their byte patterns are:

    ``` c
    254 0 0 0 fe
    255 0 0 0 ff
    256 0 0 1 0
    257 0 0 1 1
    ```

    and so, if you treat them as strings they sort nicely. Which means, if you use steadily increasing integers as keys on a big-endian system Berkeley DB behaves well and you get compact trees, but on a little-endian system Berkeley DB produces much less compact trees. To avoid this problem, you may want to convert the keys to flat text or big-endian representations, or provide your own <a href="bt_conf.md#am_conf_bt_compare" class="xref" title="Btree comparison">Btree comparison</a>

6.  **Is there any way to avoid double buffering in the Berkeley DB system?**

    While you cannot avoid double buffering entirely, there are a few things you can do to address this issue:

    First, the Berkeley DB cache size can be explicitly set. Rather than allocate additional space in the Berkeley DB cache to cover unexpectedly heavy load or large table sizes, double buffering may suggest you size the cache to function well under normal conditions, and then depend on the file buffer cache to cover abnormal conditions. Obviously, this is a trade-off, as Berkeley DB may not then perform as well as usual under abnormal conditions.

    Second, depending on the underlying operating system you're using, you may be able to alter the amount of physical memory devoted to the system's file buffer cache. Altering this type of resource configuration may require appropriate privileges, or even operating system reboots and/or rebuilds, on some systems.

    Third, changing the size of the Berkeley DB environment regions can change the amount of space the operating system makes available for the file buffer cache, and it's often worth considering exactly how the operating system is dividing up its available memory. Further, moving the Berkeley DB database environment regions from filesystem backed memory into system memory (or heap memory), can often make additional system memory available for the file buffer cache, especially on systems without a unified buffer cache and VM system.

    Finally, for operating systems that allow buffering to be turned off, specifying the <a href="../../api/c/envset_flags.md#set_flags_DB_DIRECT_DB" class="olink">DB_DIRECT_DB</a> and <a href="../../api/c/envlog_set_config.md#log_set_config_DB_LOG_DIRECT" class="olink">DB_LOG_DIRECT</a> flags will attempt to do so.

7.  **I'm seeing database corruption when I run out of disk space.**

    Berkeley DB can continue to run when when out-of-disk-space errors occur, but it requires the application to be transaction protected. Applications which do not enclose update operations in transactions cannot recover from out-of-disk-space errors, and the result of running out of disk space may be database corruption.

8.  **How can I associate application information with a <a href="../../api/c/db.md" class="olink">DB</a> or <a href="../../api/c/env.md" class="olink">DB_ENV</a> handle?**

    In the C API, the <a href="../../api/c/db.md" class="olink">DB</a> and <a href="../../api/c/env.md" class="olink">DB_ENV</a> structures each contain an "app_private" field intended to be used to reference application-specific information. See the <a href="../../api/c/dbcreate.md" class="olink">db_create()</a> and <a href="../../api/c/envcreate.md" class="olink">db_env_create()</a> documentation for more information.

    In the C++ or Java APIs, the easiest way to associate application-specific data with a handle is to subclass the <a href="../api_reference/CXX/db.html" class="olink">Db</a> or <a href="../api_reference/CXX/env.html" class="olink">DbEnv</a>, for example subclassing <a href="../api_reference/CXX/db.html" class="olink">Db</a> to get MyDb. Objects of type MyDb will still have the Berkeley DB API methods available on them, and you can put any extra data or methods you want into the MyDb class. If you are using "callback" APIs that take <a href="../api_reference/CXX/db.html" class="olink">Db</a> or <a href="../api_reference/CXX/env.html" class="olink">DbEnv</a> arguments (for example, <a href="../../api/c/dbset_bt_compare.md" class="olink">DB-&gt;set_bt_compare()</a>) these will always be called with the <a href="../api_reference/CXX/db.html" class="olink">Db</a> or <a href="../api_reference/CXX/env.html" class="olink">DbEnv</a> objects you create. So if you always use MyDb objects, you will be able to take the first argument to the callback function and cast it to a MyDb (in C++, cast it to (MyDb\*)). That will allow you to access your data members or methods.
