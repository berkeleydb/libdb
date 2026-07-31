---
title: "Chapter 2.  Dbstl Container Classes"
api-name: "Chapter 2.  Dbstl Container Classes"
source: docs/api_reference/STL/dbstl_containers.html
---
## Chapter 2.  Dbstl Container Classes

A dbstl container is very much like a C++ STL container.

It stores a collection of data items, or key/data pairs. Each container is backed by a Berkeley DB database created in an explicit database environment or an internal private environment; And the database itself can be created explicitly with all kinds of configurations, or by dbstl internally. For each type of container, some specific type of database and/or configurations must be used or specified to the database and its environment. dbstl will check the database and environment conform to the requirement. When users don't have a chance to specify a container's backing database and environment, like in copy constructors, dbstl will create proper databases and/or environment for it. There are two helper functions to make it easier to create/open an environment or database, they are <a href="stldbstl_global_functionsopen_db.md" class="link" title="open_db">dbstl::open_db()</a> and <a href="stldbstl_global_functionsopen_env.md" class="link" title="open_env">dbstl::open_env()</a> ;

#### See Also

<a href="stldbstl_global_functionsopen_db.md" class="link" title="open_db">dbstl::open_db()</a> <a href="stldbstl_global_functionsopen_env.md" class="link" title="open_env">dbstl::open_env()</a> <a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a> <a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a> <a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a> <a href="db_set.md" class="link" title="Chapter 7.  Db_set">db_set</a> <a href="db_multiset.md" class="link" title="Chapter 8.  Db_multiset">db_multiset</a>

#### Public Members

| Member | Description |
|----|----|
| <a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> | db_container |
| <a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a> | db_map |
| <a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a> | db_multimap |
| <a href="db_set.md" class="link" title="Chapter 7.  Db_set">db_set</a> | db_set |
| <a href="db_multiset.md" class="link" title="Chapter 8.  Db_multiset">db_multiset</a> | db_multiset |
| <a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a> | db_vector |

#### Group

None
