---
title: "Chapter 13.  Db_vector_iterator"
api-name: "Chapter 13.  Db_vector_iterator"
source: docs/api_reference/STL/db_vector_iterator.html
---
## Chapter 13.  Db_vector_iterator

#### Public Members

| Member | Description |
|----|----|
| <a href="db_vector_iterator.md#stldb_vector_iteratordb_vector_iterator" class="xref" title="db_vector_iterator">db_vector_iterator</a> |  |
| <a href="stldb_vector_iteratordstr_db_vector_iterator.md" class="xref" title="~db_vector_iterator">~db_vector_iterator</a> |  |
| <a href="stldb_vector_iteratoroperator_incr.md" class="xref" title="operator++">operator++</a> | Pre-increment. |
| <a href="stldb_vector_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Pre-decrement. |
| <a href="stldb_vector_iteratoroperator_assign.md" class="xref" title="operator=">operator=</a> | Assignment operator. |
| <a href="stldb_vector_iteratoroperator_add.md" class="xref" title="operator+">operator+</a> | Iterator movement operator. |
| <a href="stldb_vector_iteratoroperator_ia.md" class="xref" title="operator+=">operator+=</a> | Move this iterator backward by n elements. |
| <a href="stldb_vector_iteratoroperator_sub.md" class="xref" title="operator-">operator-</a> | Iterator movement operator. |
| <a href="stldb_vector_iteratoroperator_sa.md" class="xref" title="operator-=">operator-=</a> | Move this iterator forward by n elements. |
| <a href="stldb_vector_iteratoroperator__star.md" class="xref" title="operator *">operator *</a> | Dereference operator. |
| <a href="stldb_vector_iteratoroperator_arrow.md" class="xref" title="operator-&gt;">operator-&gt;</a> | Arrow operator. |
| <a href="stldb_vector_iteratoroperator_sqbrk.md" class="xref" title="operator[]">operator[]</a> | Iterator index operator. |
| <a href="stldb_vector_iteratorrefresh.md" class="xref" title="refresh">refresh</a> | Refresh iterator cached value. |

#### Group

<a href="db_vector_iterators.md" class="xref" title="Chapter 11.  Iterator Classes for db_vector">Iterator Classes for db_vector</a>

## db_vector_iterator

### Function Details

``` c
db_vector_iterator(const db_vector_iterator< T,
    value_type_sub > &vi)
 
```

``` c
db_vector_iterator(db_container *powner, u_int32_t b_bulk_retrieval=0,
    bool brmw=false, bool directdbget=true,
    bool b_read_only=false)
 
```

``` c
db_vector_iterator()
 
```

``` c
db_vector_iterator(const db_vector_base_iterator< T > &obj)
 
```

### Group: Constructors and destructor

Do not construct iterators explictily using these constructors, but call <a href="db_vector.md#stldb_vectorbegin" class="link" title="begin">db_vector::begin</a> to get an valid iterator.

<a href="db_vector.md#stldb_vectorbegin" class="link" title="begin">db_vector::begin</a>

### Class

<a href="db_vector_iterator.md" class="link" title="Chapter 13.  Db_vector_iterator">db_vector_iterator</a>
