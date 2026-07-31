---
title: "Chapter 14.  Iterator Classes for db_map and db_multimap"
api-name: "Chapter 14.  Iterator Classes for db_map and db_multimap"
source: docs/api_reference/STL/db_map_iterators.html
---
## Chapter 14.  Iterator Classes for db_map and db_multimap

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a> has two iterator class templates -- <a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a> and <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a> .

They are the const iterator class and iterator class for <a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a> and <a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a> . <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a> inherits from <a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a> .

The two classes have identical behaviors to std::map::const_iterator and std::map::iterator respectively. Note that the common public member function behaviors are described in the <a href="db_base_iterator.md" class="link" title="Chapter 10.  Db_base_iterator">db_base_iterator</a> section.

The differences between the two classes are that the <a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a> can only be used to read its referenced value, while <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a> allows both read and write access. If your access pattern is readonly, it is strongly recommended that you use the const iterator because it is faster and more efficient.

#### Public Members

| Member | Description |
|----|----|
| <a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a> | db_map_base_iterator |
| <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a> | db_map_iterator |

#### Group

<a href="dbstl_iterators.md" class="xref" title="Chapter 9.  Dbstl Iterator Classes">Dbstl Iterator Classes</a>
