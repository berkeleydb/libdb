---
title: "erase"
api-name: "erase"
source: docs/api_reference/STL/stldb_maperase.html
---
## erase

### Function Details

``` c
void erase(iterator pos)
 
```

Erase a key/data pair at specified position.

#### Parameters

##### pos

An valid iterator of this container to erase.

``` c
size_type erase(const key_type &x)
 
```

Erase elements by key.

All key/data pairs with specified key x will be removed from underlying database. This function supports auto-commit.

#### Parameters

##### x

The key to remove from the container.

#### Return Value

The number of key/data pairs removed.

``` c
void erase(iterator first,
    iterator last)
 
```

Range erase.

Erase all key/data pairs within the valid range \[first, last).

#### Parameters

##### last

The open boundary of the range.

##### first

The closed boundary of the range.

### Group: Erase Functions

<a href="http://www.cplusplus.com/reference/stl/map/erase/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/map/erase/</a>

### Class

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>
