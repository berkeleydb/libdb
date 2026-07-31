---
title: "Environment FAQ"
api-name: "Environment FAQ"
source: docs/programmer_reference/env_faq.html
---
## Environment FAQ

1.  **I'm using multiple processes to access an Berkeley DB database environment; is there any way to ensure that two processes don't run transactional recovery at the same time, or that all processes have exited the database environment so that recovery can be run?**

    See <a href="transapp_fail.md" class="xref" title="Handling failure in Transactional Data Store applications">Handling failure in Transactional Data Store applications</a> and <a href="transapp_app.md" class="xref" title="Architecting Transactional Data Store applications">Architecting Transactional Data Store applications</a> for a full discussion of this topic.

2.  **How can I associate application information with a <a href="../../api/c/db.md" class="olink">DB</a> or <a href="../../api/c/env.md" class="olink">DB_ENV</a> handle?**

    In the C API, the <a href="../../api/c/db.md" class="olink">DB</a> and <a href="../../api/c/env.md" class="olink">DB_ENV</a> structures each contain an "app_private" field intended to be used to reference application-specific information. See the <a href="../../api/c/dbcreate.md" class="olink">db_create()</a> and <a href="../../api/c/envcreate.md" class="olink">db_env_create()</a> documentation for more information.

    In the C++ or Java APIs, the easiest way to associate application-specific data with a handle is to subclass the <a href="../api_reference/CXX/db.html" class="olink">Db</a> or <a href="../api_reference/CXX/env.html" class="olink">DbEnv</a>, for example subclassing <a href="../api_reference/CXX/db.html" class="olink">Db</a> to get MyDb. Objects of type MyDb will still have the Berkeley DB API methods available on them, and you can put any extra data or methods you want into the MyDb class. If you are using "callback" APIs that take <a href="../api_reference/CXX/db.html" class="olink">Db</a> or <a href="../api_reference/CXX/env.html" class="olink">DbEnv</a> arguments (for example, <a href="../api_reference/CXX/dbset_bt_compare.html" class="olink">Db::set_bt_compare()</a>) these will always be called with the <a href="../api_reference/CXX/db.html" class="olink">Db</a> or <a href="../api_reference/CXX/env.html" class="olink">DbEnv</a> objects you create. So if you always use MyDb objects, you will be able to take the first argument to the callback function and cast it to a MyDb (in C++, cast it to (MyDb\*)). That will allow you to access your data members or methods.
