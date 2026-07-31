---
title: "Chapter 17.  Iterator Classes for db_set and db_multiset"
api-name: "Chapter 17.  Iterator Classes for db_set and db_multiset"
source: docs/api_reference/STL/dbset_iterators.html
---
## Chapter 17.  Iterator Classes for db_set and db_multiset

<a href="db_set_base_iterator.md" class="link" title="Chapter 18.  Db_set_base_iterator">db_set_base_iterator</a> and <a href="db_set_iterator.md" class="link" title="Chapter 19.  Db_set_iterator">db_set_iterator</a> are the const iterator and iterator class for <a href="db_set.md" class="link" title="Chapter 7.  Db_set">db_set</a> and <a href="db_multiset.md" class="link" title="Chapter 8.  Db_multiset">db_multiset</a> .

They have identical behaviors to std::set::const_iterator and std::set::iterator respectively.

The difference between the two classes is that the <a href="db_set_base_iterator.md" class="link" title="Chapter 18.  Db_set_base_iterator">db_set_base_iterator</a> can only be used to read its referenced value, while <a href="db_set_iterator.md" class="link" title="Chapter 19.  Db_set_iterator">db_set_iterator</a> allows both read and write access. If the access pattern is readonly, it is strongly recommended that you use the const iterator because it is faster and more efficient.

The two classes inherit several functions from <a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a> and <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a> respectively.

#### See Also

<a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a> <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a>

#### Public Members

| Member | Description |
|----|----|
| <a href="db_set_base_iterator.md" class="link" title="Chapter 18.  Db_set_base_iterator">db_set_base_iterator</a> | db_set_base_iterator |
| <a href="db_set_iterator.md" class="link" title="Chapter 19.  Db_set_iterator">db_set_iterator</a> | db_set_iterator |

#### Group

<a href="dbstl_iterators.md" class="xref" title="Chapter 9.  Dbstl Iterator Classes">Dbstl Iterator Classes</a>
