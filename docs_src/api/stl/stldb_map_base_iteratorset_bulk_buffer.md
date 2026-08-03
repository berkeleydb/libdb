---
title: "set_bulk_buffer"
api-name: "set_bulk_buffer"
source: docs/api_reference/STL/stldb_map_base_iteratorset_bulk_buffer.html
---
## set_bulk_buffer

### Function Details

``` c
bool set_bulk_buffer(u_int32_t sz)
 
```

Modify bulk buffer size.

Bulk read is enabled when creating an iterator, so users later can only modify the bulk buffer size to another value, but can't enable/disable bulk read while an iterator is already alive.

#### Parameters

##### sz

The new size of the bulk read buffer of this iterator.

#### Return Value

Returns true if succeeded, false otherwise.

#### See Also

<a href="stldb_base_iteratorset_bulk_buffer.md" class="link" title="set_bulk_buffer">db_base_iterator::set_bulk_buffer(u_int32_t )</a>

### Class

<a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a>
