---
title: "move_to"
api-name: "move_to"
source: docs/api_reference/STL/stldb_map_base_iteratormove_to.html
---
## move_to

### Function Details

``` c
int move_to(const kdt &k,
    int flag=DB_SET) const
 
```

Iterator movement function.

Move this iterator to the specified key k, by default moves exactly to k, and update cached data element, you can also specify DB_SET_RANGE, to move to the biggest key smaller than k. The btree/hash key comparison routine determines which key is bigger. When the iterator is on a multiple container, move_to will move itself to the first key/data pair of the identical keys.

#### Parameters

##### k

The target key value to move to.

##### flag

Flags available: DB_SET(default) or DB_SET_RANGE. DB_SET will move this iterator exactly at k; DB_SET_RANGE moves this iterator to k or the smallest key greater than k. If fail to find such a key, this iterator will become invalid.

#### Return Value

0 if succeed; non-0 otherwise, and this iterator becomes invalid. Call db_strerror with the return value to get the error message.

### Class

<a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a>
