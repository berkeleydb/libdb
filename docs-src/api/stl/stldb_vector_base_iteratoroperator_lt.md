---
title: "operator<"
api-name: "operator<"
source: docs/api_reference/STL/stldb_vector_base_iteratoroperator_lt.html
---
## operator\<

### Function Details

``` c
bool operator<(const self &itr) const
 
```

Less than comparison operator.

#### Parameters

##### itr

The iterator to compare against.

#### Return Value

True if this iterator is less than itr.

### Group: Iterator comparison operators

The way to compare two iterators is to compare the index values of the two elements they point to.

The iterator sitting on an element with less index is regarded to be smaller. And the invalid iterator sitting after last element is greater than any other iterators, because it is assumed to have an index equal to last element's index plus one; The invalid iterator sitting before first element is less than any other iterators because it is assumed to have an index -1.

### Class

<a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a>
