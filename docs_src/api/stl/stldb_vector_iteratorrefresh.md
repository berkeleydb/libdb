---
title: "refresh"
api-name: "refresh"
source: docs/api_reference/STL/stldb_vector_iteratorrefresh.html
---
## refresh

### Function Details

``` c
virtual int refresh(bool from_db=true)
 
```

Refresh iterator cached value.

#### Parameters

##### from_db

If not doing direct database get and this parameter is true, we will retrieve data directly from db.

#### See Also

<a href="db_base_iterator.md#stldb_base_iteratorrefresh" class="link" title="refresh">db_base_iterator::refresh(bool)</a>

### Class

<a href="db_vector_iterator.md" class="link" title="Chapter 13.  Db_vector_iterator">db_vector_iterator</a>
