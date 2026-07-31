---
title: "set_bulk_buffer"
api-name: "set_bulk_buffer"
source: docs/api_reference/STL/stldb_base_iteratorset_bulk_buffer.html
---
## set_bulk_buffer

### Function Details

``` c
bool set_bulk_buffer(u_int32_t sz)
 
```

Call this function to modify bulk buffer size.

Bulk retrieval is enabled when creating an iterator, so users later can only modify the bulk buffer size to another value, but can't enable/disable bulk read while an iterator is already alive.

#### Parameters

##### sz

The new buffer size in bytes.

#### Return Value

true if succeeded, false otherwise.

### Class

<a href="db_base_iterator.md" class="link" title="Chapter 10.  Db_base_iterator">db_base_iterator</a>
