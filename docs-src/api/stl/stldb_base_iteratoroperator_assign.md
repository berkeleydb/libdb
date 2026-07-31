---
title: "operator="
api-name: "operator="
source: docs/api_reference/STL/stldb_base_iteratoroperator_assign.html
---
## operator=

### Function Details

``` c
const self& operator=(const self &bi)
 
```

Iterator assignment operator.

Iterator assignment will cause the underlying cursor of the right iterator to be duplicated to the left iterator after its previous cursor is closed, to make sure each iterator owns one unique cursor. The key/data cached in the right iterator is copied to the left iterator. Consequently, the left iterator points to the same key/data pair in the database as the the right value after the assignment, and have identical cached key/data pair.

#### Parameters

##### bi

The other iterator to assign with.

#### Return Value

The iterator bi's reference.

### Class

<a href="db_base_iterator.md" class="link" title="Chapter 10.  Db_base_iterator">db_base_iterator</a>
