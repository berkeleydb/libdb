---
title: "splice"
api-name: "splice"
source: docs/api_reference/STL/stldb_vectorsplice.html
---
## splice

### Function Details

``` c
void splice(iterator position,
    self &x)
 
```

Moves elements from list x into this list.

Moves all elements in list x into this list container at the specified position, effectively inserting the specified elements into the container and removing them from x. This function supports auto-commit.

#### Parameters

##### position

Position within the container where the elements of x are inserted.

##### x

The other list container to splice from.

#### See Also

<a href="http://www.cplusplus.com/reference/stl/list/splice/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/list/splice/</a>

``` c
void splice(iterator position, self &x,
    iterator i)
 
```

Moves elements from list x into this list.

Moves elements at position i of list x into this list container at the specified position, effectively inserting the specified elements into the container and removing them from x. This function supports auto-commit.

#### Parameters

##### i

The position of element in x to move into this list.

##### position

Position within the container where the elements of x are inserted.

##### x

The other list container to splice from.

#### See Also

<a href="http://www.cplusplus.com/reference/stl/list/splice/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/list/splice/</a>

``` c
void splice(iterator position, self &x, iterator first,
    iterator last)
 
```

Moves elements from list x into this list.

Moves elements in range \[first, last) of list x into this list container at the specified position, effectively inserting the specified elements into the container and removing them from x. This function supports auto-commit.

#### Parameters

##### position

Position within the container where the elements of x are inserted.

##### first

The range's closed boundary.

##### last

The range's open boundary.

##### x

The other list container to splice from.

#### See Also

<a href="http://www.cplusplus.com/reference/stl/list/splice/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/list/splice/</a>

### Group: std::list specific functions

<a href="http://www.cplusplus.com/reference/stl/list/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/list/</a>

### Class

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a>
