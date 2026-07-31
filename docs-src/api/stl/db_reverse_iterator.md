---
title: "Chapter 20.  Db_reverse_iterator"
api-name: "Chapter 20.  Db_reverse_iterator"
source: docs/api_reference/STL/db_reverse_iterator.html
---
## Chapter 20.  Db_reverse_iterator

This class is the reverse class adaptor for all dbstl iterator classes.

It inherits from real iterator classes like <a href="db_vector_iterator.md" class="link" title="Chapter 13.  Db_vector_iterator">db_vector_iterator</a> , <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a> or <a href="db_set_iterator.md" class="link" title="Chapter 19.  Db_set_iterator">db_set_iterator</a> . When you call container::rbegin(), you will get an instance of this class.

#### See Also

<a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a> <a href="db_vector_iterator.md" class="link" title="Chapter 13.  Db_vector_iterator">db_vector_iterator</a> <a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a> <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a> <a href="db_set_base_iterator.md" class="link" title="Chapter 18.  Db_set_base_iterator">db_set_base_iterator</a> <a href="db_set_iterator.md" class="link" title="Chapter 19.  Db_set_iterator">db_set_iterator</a>

#### Public Members

| Member | Description |
|----|----|
| <a href="db_reverse_iterator.md#stldb_reverse_iteratoroperator_incr" class="xref" title="operator++">operator++</a> | Move this iterator forward by one element. |
| <a href="stldb_reverse_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Move this iterator backward by one element. |
| <a href="stldb_reverse_iteratoroperator_add.md" class="xref" title="operator+">operator+</a> | Iterator shuffle operator. |
| <a href="stldb_reverse_iteratoroperator_sub.md" class="xref" title="operator-">operator-</a> | Iterator shuffle operator. |
| <a href="stldb_reverse_iteratoroperator_ia.md" class="xref" title="operator+=">operator+=</a> | Iterator shuffle operator. |
| <a href="stldb_reverse_iteratoroperator_sa.md" class="xref" title="operator-=">operator-=</a> | Iterator shuffle operator. |
| <a href="stldb_reverse_iteratoroperator_lt.md" class="xref" title="operator&lt;">operator&lt;</a> | Less compare operator. |
| <a href="stldb_reverse_iteratoroperator_gt.md" class="xref" title="operator&gt;">operator&gt;</a> | Greater compare operator. |
| <a href="stldb_reverse_iteratoroperator_le.md" class="xref" title="operator&lt;=">operator&lt;=</a> | Less equal compare operator. |
| <a href="stldb_reverse_iteratoroperator_ge.md" class="xref" title="operator&gt;=">operator&gt;=</a> | Greater equal compare operator. |
| <a href="stldb_reverse_iteratordb_reverse_iterator.md" class="xref" title="db_reverse_iterator">db_reverse_iterator</a> | Constructor. Construct from an iterator of wrapped type. |
| <a href="stldb_reverse_iteratoroperator_assign.md" class="xref" title="operator=">operator=</a> | Assignment operator. |
| <a href="stldb_reverse_iteratoroperator_sqbrk.md" class="xref" title="operator[]">operator[]</a> | Return the reference of the element which can be reached by moving this reverse iterator by Off times backward. |

#### Group

<a href="dbstl_iterators.md" class="xref" title="Chapter 9.  Dbstl Iterator Classes">Dbstl Iterator Classes</a>

## operator++

### Function Details

``` c
self& operator++()
 
```

Move this iterator forward by one element.

#### Return Value

The moved iterator at new position.

``` c
self operator++(int)
 
```

Move this iterator forward by one element.

#### Return Value

The original iterator at old position.

### Group: Reverse iterator movement functions

When we talk about reverse iterator movement, we think the container is a uni-directional range, represented by \[begin, end), and this is true no matter we are using iterators or reverse iterators.

When an iterator is moved closer to "begin", we say it is moved forward, otherwise we say it is moved backward.

### Class

<a href="db_reverse_iterator.md" class="link" title="Chapter 20.  Db_reverse_iterator">db_reverse_iterator</a>
