---
title: "Chapter 12.  Db_vector_base_iterator"
api-name: "Chapter 12.  Db_vector_base_iterator"
source: docs/api_reference/STL/db_vector_base_iterator.html
---
## Chapter 12.  Db_vector_base_iterator

This class is the const iterator class for <a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a> , and it is inheirted by the <a href="db_vector_iterator.md" class="link" title="Chapter 13.  Db_vector_iterator">db_vector_iterator</a> class, which is the iterator class for <a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a> .

#### Public Members

| Member | Description |
|----|----|
| <a href="db_vector_base_iterator.md#stldb_vector_base_iteratordb_vector_base_iterator" class="xref" title="db_vector_base_iterator">db_vector_base_iterator</a> |  |
| <a href="stldb_vector_base_iteratordstr_db_vector_base_iterator.md" class="xref" title="~db_vector_base_iterator">~db_vector_base_iterator</a> |  |
| <a href="stldb_vector_base_iteratoroperator_eq.md" class="xref" title="operator==">operator==</a> | Equality comparison operator. |
| <a href="stldb_vector_base_iteratoroperator_ueq.md" class="xref" title="operator!=">operator!=</a> | Unequal compare, identical to !operator(==itr). |
| <a href="stldb_vector_base_iteratoroperator_lt.md" class="xref" title="operator&lt;">operator&lt;</a> | Less than comparison operator. |
| <a href="stldb_vector_base_iteratoroperator_le.md" class="xref" title="operator&lt;=">operator&lt;=</a> | Less equal comparison operator. |
| <a href="stldb_vector_base_iteratoroperator_ge.md" class="xref" title="operator&gt;=">operator&gt;=</a> | Greater equal comparison operator. |
| <a href="stldb_vector_base_iteratoroperator_gt.md" class="xref" title="operator&gt;">operator&gt;</a> | Greater comparison operator. |
| <a href="stldb_vector_base_iteratoroperator_incr.md" class="xref" title="operator++">operator++</a> | Pre-increment. |
| <a href="stldb_vector_base_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Pre-decrement. |
| <a href="stldb_vector_base_iteratoroperator_assign.md" class="xref" title="operator=">operator=</a> | Assignment operator. |
| <a href="stldb_vector_base_iteratoroperator_add.md" class="xref" title="operator+">operator+</a> | Iterator movement operator. |
| <a href="stldb_vector_base_iteratoroperator_ia.md" class="xref" title="operator+=">operator+=</a> | Move this iterator backward by n elements. |
| <a href="stldb_vector_base_iteratoroperator_sub.md" class="xref" title="operator-">operator-</a> | Iterator movement operator. |
| <a href="stldb_vector_base_iteratoroperator_sa.md" class="xref" title="operator-=">operator-=</a> | Move this iterator forward by n elements. |
| <a href="stldb_vector_base_iteratoroperator__star.md" class="xref" title="operator *">operator *</a> | Dereference operator. |
| <a href="stldb_vector_base_iteratoroperator_arrow.md" class="xref" title="operator-&gt;">operator-&gt;</a> | Arrow operator. |
| <a href="stldb_vector_base_iteratoroperator_sqbrk.md" class="xref" title="operator[]">operator[]</a> | Iterator index operator. |
| <a href="stldb_vector_base_iteratorget_current_index.md" class="xref" title="get_current_index">get_current_index</a> | Get current index of within the vector. |
| <a href="stldb_vector_base_iteratormove_to.md" class="xref" title="move_to">move_to</a> | Iterator movement function. |
| <a href="stldb_vector_base_iteratorrefresh.md" class="xref" title="refresh">refresh</a> | Refresh iterator cached value. |
| <a href="stldb_vector_base_iteratorclose_cursor.md" class="xref" title="close_cursor">close_cursor</a> | Close underlying Berkeley DB cursor of this iterator. |
| <a href="stldb_vector_base_iteratorset_bulk_buffer.md" class="xref" title="set_bulk_buffer">set_bulk_buffer</a> | Modify bulk buffer size. |
| <a href="stldb_vector_base_iteratorget_bulk_bufsize.md" class="xref" title="get_bulk_bufsize">get_bulk_bufsize</a> | Get bulk retrieval buffer size in bytes. |

#### Group

<a href="db_vector_iterators.md" class="xref" title="Chapter 11.  Iterator Classes for db_vector">Iterator Classes for db_vector</a>

## db_vector_base_iterator

### Function Details

``` c
db_vector_base_iterator(const db_vector_base_iterator< T > &vi)
 
```

``` c
db_vector_base_iterator(db_container *powner, u_int32_t b_bulk_retrieval=0,
    bool rmw=false, bool directdbget=true,
    bool readonly=false)
 
```

``` c
db_vector_base_iterator()
 
```

### Group: Constructors and destroctor

Do not construct iterators explictily using these constructors, but call db_vector::begin() const to get an valid iterator.

db_vector::begin() const

### Class

<a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a>
