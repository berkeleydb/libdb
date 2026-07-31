---
title: "Chapter 19.  Db_set_iterator"
api-name: "Chapter 19.  Db_set_iterator"
source: docs/api_reference/STL/db_set_iterator.html
---
## Chapter 19.  Db_set_iterator

#### Public Members

| Member | Description |
|----|----|
| <a href="db_set_iterator.md#stldb_set_iteratordstr_db_set_iterator" class="xref" title="~db_set_iterator">~db_set_iterator</a> | Destructor. |
| <a href="stldb_set_iteratordb_set_iterator.md" class="xref" title="db_set_iterator">db_set_iterator</a> | Constructor. |
| <a href="stldb_set_iteratoroperator_incr.md" class="xref" title="operator++">operator++</a> | Pre-increment. |
| <a href="stldb_set_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Pre-decrement. |
| <a href="stldb_set_iteratoroperator__star.md" class="xref" title="operator *">operator *</a> | Dereference operator. |
| <a href="stldb_set_iteratoroperator_arrow.md" class="xref" title="operator-&gt;">operator-&gt;</a> | Arrow operator. |
| <a href="stldb_set_iteratorrefresh.md" class="xref" title="refresh">refresh</a> | Refresh iterator cached value. |

#### Group

<a href="dbset_iterators.md" class="xref" title="Chapter 17.  Iterator Classes for db_set and db_multiset">Iterator Classes for db_set and db_multiset</a>

## ~db_set_iterator

### Function Details

``` c
virtual ~db_set_iterator()
 
```

Destructor.

### Group: Constructors and destructor

Do not use these constructors to create iterators, but call <a href="stldb_mapbegin.md" class="link" title="begin">db_set::begin()</a> or <a href="stldb_mapbegin.md" class="link" title="begin">db_multiset::begin()</a> to create valid ones.

### Class

<a href="db_set_iterator.md" class="link" title="Chapter 19.  Db_set_iterator">db_set_iterator</a>
