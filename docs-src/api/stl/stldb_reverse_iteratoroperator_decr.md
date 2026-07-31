---
title: "operator--"
api-name: "operator--"
source: docs/api_reference/STL/stldb_reverse_iteratoroperator_decr.html
---
## operator--

### Function Details

``` c
self& operator--()
 
```

Move this iterator backward by one element.

#### Return Value

The moved iterator at new position.

``` c
self operator--(int)
 
```

Move this iterator backward by one element.

#### Return Value

The original iterator at old position.

### Group: Reverse iterator movement functions

When we talk about reverse iterator movement, we think the container is a uni-directional range, represented by \[begin, end), and this is true no matter we are using iterators or reverse iterators.

When an iterator is moved closer to "begin", we say it is moved forward, otherwise we say it is moved backward.

### Class

<a href="db_reverse_iterator.md" class="link" title="Chapter 20.  Db_reverse_iterator">db_reverse_iterator</a>
