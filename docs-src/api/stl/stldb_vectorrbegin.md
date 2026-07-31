---
title: "rbegin"
api-name: "rbegin"
source: docs/api_reference/STL/stldb_vectorrbegin.html
---
## rbegin

### Function Details

``` c
reverse_iterator rbegin(ReadModifyWriteOption rmw=
    ReadModifyWriteOption::no_read_modify_write(), bool readonly=false,
    BulkRetrievalOption bulk_read=BulkRetrievalOption::no_bulk_retrieval(),
    bool directdb_get=true)
 
```

Create a reverse iterator.

This function creates a reverse iterator initialized to sit on the last element in the underlying database, and can be used to read/write. The meaning and usage of its parameters are identical to the above begin function.

#### Parameters

##### directdb_get

Same as that of begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

##### bulk_read

Same as that of begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

##### rmw

Same as that of begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

##### readonly

Same as that of begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

#### Return Value

The created iterator.

#### See Also

begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

``` c
const_reverse_iterator rbegin(BulkRetrievalOption bulkretrieval=
    BulkRetrievalOption(BulkRetrievalOption::no_bulk_retrieval()),
    bool directdb_get=true) const
 
```

Create a const reverse iterator.

This function creates a const reverse iterator initialized to sit on the last element in the backing database, and can only read the element, it is only available to const <a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a> containers. The meaning and usage of its parameters are identical as above.

#### Parameters

##### directdb_get

Same as that of begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

##### bulkretrieval

Same as that of begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

#### Return Value

The created iterator.

#### See Also

begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

### Class

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a>
