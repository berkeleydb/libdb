---
title: "operator[]"
api-name: "operator[]"
source: docs/api_reference/STL/stldb_mapoperator_sqbrk.html
---
## operator\[\]

### Function Details

``` c
data_type_wrap operator[](const key_type &x)
 
```

Retrieve data element by key.

This function returns an reference to the underlying data element of the specified key x. The returned object can be used to read or write the data element of the key/data pair. Do use a data_type_wrap of <a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a> or value_type::second_type(they are the same) type of variable to hold the return value of this function.

#### Parameters

##### x

The target key to get value from.

#### Return Value

Data element reference.

``` c
const ddt operator[](const key_type &x) const
 
```

Retrieve data element by key.

This function returns the value of the underlying data element of specified key x. You can only read the element, but unable to update the element via the return value of this function. And you need to use the container's const reference to call this method.

#### Parameters

##### x

The target key to get value from.

#### Return Value

Data element, read only, can't be used to modify it.

### Class

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>
