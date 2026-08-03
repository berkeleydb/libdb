---
title: "Chapter 9.  Dbstl Iterator Classes"
api-name: "Chapter 9.  Dbstl Iterator Classes"
source: docs/api_reference/STL/dbstl_iterators.html
---
## Chapter 9.  Dbstl Iterator Classes

Common information for all dbstl iterators:.

1\. Each instance of a dbstl iterator uniquely owns a Berkeley DB cursor, so that the key/data pair it currently sits on is always valid before it moves elsewhere. It also caches the current key/data pair values in order for member functions like operator\* /operator-\> to work properly, but caching is not compatible with standard C++ Stl behavior --- the C++ standard requires the iterator refer to a shared piece of memory where the data is stored, thus two iterators of the same container sitting on the same element should point to the same memory location, which is false for dbstl iterators.

2\. There are some functions common to each child class of this class which have identical behaviors, so we will document them here.

This class is the base class for all dbstl iterators, there is no much to say about this class itself, and users are not supposed to directly use this class at all. So we will talk about some common functions of dbstl iterators in this section.

#### See Also

<a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a> <a href="db_vector_iterator.md" class="link" title="Chapter 13.  Db_vector_iterator">db_vector_iterator</a> <a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a> <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a> <a href="db_set_base_iterator.md" class="link" title="Chapter 18.  Db_set_base_iterator">db_set_base_iterator</a> <a href="db_set_iterator.md" class="link" title="Chapter 19.  Db_set_iterator">db_set_iterator</a>

#### Public Members

| Member | Description |
|----|----|
| <a href="db_base_iterator.md" class="link" title="Chapter 10.  Db_base_iterator">db_base_iterator</a> | db_base_iterator |
| <a href="db_reverse_iterator.md" class="link" title="Chapter 20.  Db_reverse_iterator">db_reverse_iterator</a> | db_reverse_iterator |
| <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a> | db_map_iterator |
| <a href="db_map_iterators.md" class="link" title="Chapter 14.  Iterator Classes for db_map and db_multimap">Iterator classes for db_map and db_multimap.</a> | Iterator classes for db_map and db_multimap. |
| <a href="dbset_iterators.md" class="link" title="Chapter 17.  Iterator Classes for db_set and db_multiset">Iterator classes for db_set and db_multiset.</a> | Iterator classes for db_set and db_multiset. |
| <a href="db_vector_iterators.md" class="link" title="Chapter 11.  Iterator Classes for db_vector">Iterator classes for db_vector.</a> | Iterator classes for db_vector. |

#### Group

None
