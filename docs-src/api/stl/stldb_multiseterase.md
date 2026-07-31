---
title: "erase"
api-name: "erase"
source: docs/api_reference/STL/stldb_multiseterase.html
---
## erase

### Function Details

``` c
size_type erase(const key_type &x)
 
```

Erase elements by key.

All key/data pairs with specified key x will be removed from the underlying database. This function supports auto-commit.

#### Parameters

##### x

The key to remove from the container.

#### Return Value

The number of key/data pairs removed.

``` c
void erase(iterator pos)
 
```

Erase a key/data pair at specified position.

#### Parameters

##### pos

A valid iterator of this container to erase.

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

<a href="http://www.cplusplus.com/reference/stl/multiset/erase/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/multiset/erase/</a>

### Class

<a href="db_multiset.md" class="link" title="Chapter 8.  Db_multiset">db_multiset</a>
