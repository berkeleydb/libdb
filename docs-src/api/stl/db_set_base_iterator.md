---
title: "Chapter 18.  Db_set_base_iterator"
api-name: "Chapter 18.  Db_set_base_iterator"
source: docs/api_reference/STL/db_set_base_iterator.html
---
## Chapter 18.  Db_set_base_iterator

#### Public Members

| Member | Description |
|----|----|
| <a href="db_set_base_iterator.md#stldb_set_base_iteratordstr_db_set_base_iterator" class="xref" title="~db_set_base_iterator">~db_set_base_iterator</a> | Destructor. |
| <a href="stldb_set_base_iteratordb_set_base_iterator.md" class="xref" title="db_set_base_iterator">db_set_base_iterator</a> | Constructor. |
| <a href="stldb_set_base_iteratoroperator_incr.md" class="xref" title="operator++">operator++</a> | Post-increment. |
| <a href="stldb_set_base_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Post-decrement. |
| <a href="stldb_set_base_iteratoroperator__star.md" class="xref" title="operator *">operator *</a> | Dereference operator. |
| <a href="stldb_set_base_iteratoroperator_arrow.md" class="xref" title="operator-&gt;">operator-&gt;</a> | Arrow operator. |
| <a href="stldb_set_base_iteratorrefresh.md" class="xref" title="refresh">refresh</a> | Refresh iterator cached value. |

#### Group

<a href="dbset_iterators.md" class="xref" title="Chapter 17.  Iterator Classes for db_set and db_multiset">Iterator Classes for db_set and db_multiset</a>

## ~db_set_base_iterator

### Function Details

``` c
virtual ~db_set_base_iterator()
 
```

Destructor.

### Group: Constructors and destructor

Do not use these constructors to create iterators, but call db_set::begin() const or db_multiset::begin() const to create valid iterators.

### Class

<a href="db_set_base_iterator.md" class="link" title="Chapter 18.  Db_set_base_iterator">db_set_base_iterator</a>
