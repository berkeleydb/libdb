---
title: "assign"
api-name: "assign"
source: docs/api_reference/STL/stldb_vectorassign.html
---
## assign

### Function Details

``` c
void assign(InputIterator first, InputIterator last,
    bool b_truncate=true)
 
```

Assign a range \[first, last) to this container.

#### Parameters

##### b_truncate

See its member group doc for details.

##### last

The range open boundary.

##### first

The range closed boundary.

``` c
void assign(const_iterator first, const_iterator last,
    bool b_truncate=true)
 
```

Assign a range \[first, last) to this container.

#### Parameters

##### b_truncate

See its member group doc for details.

##### last

The range open boundary.

##### first

The range closed boundary.

``` c
void assign(size_type n, const T &u,
    bool b_truncate=true)
 
```

Assign n number of elements of value u into this container.

#### Parameters

##### b_truncate

See its member group doc for details. This function supports auto-commit.

##### u

The value of elements to insert.

##### n

The number of elements in this container after the call.

### Group: Assign functions

See the function documentation for the correct usage of b_truncate parameter.

The following four member functions have default parameter b_truncate, because they require all key/data pairs in the database be deleted before the real operation, and by default we use Db::truncate to truncate the database rather than delete the key/data pairs one by one, but Db::truncate requirs no open cursors on the database handle, and the four member functions will close any open cursors of backing database handle in current thread, but can do nothing to cursors of other threads opened from the same database handle. So you must make sure there are no open cursors of the database handle in any other threads. On the other hand, users can specify "false" to the b_truncate parameter and thus the key/data pairs will be deleted one by one. Other than that, they have identical behaviors as their counterparts in std::vector.

<a href="http://www.cplusplus.com/reference/stl/vector/assign/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/vector/assign/</a>.

### Class

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a>
