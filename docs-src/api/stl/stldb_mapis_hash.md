---
title: "is_hash"
api-name: "is_hash"
source: docs/api_reference/STL/stldb_mapis_hash.html
---
## is_hash

### Function Details

``` c
bool is_hash() const
 
```

Get container category.

Determines whether this container object is a std::map\<\> equivalent(when returns false) or that of hash_map\<\> class(when returns true). This method is not in stl, but it may be called by users because some operations are not supported by both type(map/hash_map) of containers, you need to call this function to distinguish the two types. dbstl will not stop you from calling the wrong methods of this class.

#### Return Value

Returns true if this container is a hash container based on a Berkeley DB hash database; returns false if it is based on a Berkeley DB btree database.

### Group: Metadata Functions

These functions return metadata about the container.

### Class

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>
