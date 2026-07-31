---
title: "Chapter 11.  Iterator Classes for db_vector"
api-name: "Chapter 11.  Iterator Classes for db_vector"
source: docs/api_reference/STL/db_vector_iterators.html
---
## Chapter 11.  Iterator Classes for db_vector

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a> has two iterator classes --- <a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a> and <a href="db_vector_iterator.md" class="link" title="Chapter 13.  Db_vector_iterator">db_vector_iterator</a> .

The differences between the two classes are that the <a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a> can only be used to read its referenced value, so it is intended as db_vector's const iterator; While the other class allows both read and write access. If your access pattern is readonly, it is strongly recommended that you use the const iterator because it is faster and more efficient. The two classes have identical behaviors to std::vector::const_iterator and std::vector::iterator respectively. Note that the common public member function behaviors are described in the <a href="db_base_iterator.md" class="link" title="Chapter 10.  Db_base_iterator">db_base_iterator</a> section.

#### See Also

<a href="db_base_iterator.md" class="link" title="Chapter 10.  Db_base_iterator">db_base_iterator</a>

#### Public Members

| Member | Description |
|----|----|
| <a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a> | db_vector_base_iterator |
| <a href="db_vector_iterator.md" class="link" title="Chapter 13.  Db_vector_iterator">db_vector_iterator</a> | db_vector_iterator |

#### Group

<a href="dbstl_iterators.md" class="xref" title="Chapter 9.  Dbstl Iterator Classes">Dbstl Iterator Classes</a>
