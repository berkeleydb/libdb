---
title: "begin"
api-name: "begin"
source: docs/api_reference/STL/stldb_mapbegin.html
---
## begin

### Function Details

``` c
iterator begin(ReadModifyWriteOption rmw=
    ReadModifyWriteOption::no_read_modify_write(), bool readonly=false,
    BulkRetrievalOption bulkretrieval=
        BulkRetrievalOption::no_bulk_retrieval(),
    bool directdb_get=true)
 
```

Begin a read-write or readonly iterator which sits on the first key/data pair of the database.

#### Parameters

##### directdb_get

Same as that of db_vector::begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

##### readonly

Same as that of db_vector::begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

##### rmw

Same as that of db_vector::begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

##### bulkretrieval

Same as that of db_vector::begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

#### Return Value

The created iterator.

#### See Also

<a href="db_vector.md#stldb_vectorbegin" class="link" title="begin">db_vector::begin</a> (<a href="ReadModifyWriteOption.md" class="link" title="Chapter 28.  ReadModifyWriteOption">ReadModifyWriteOption</a> , bool, <a href="BulkRetrievalOption.md" class="link" title="Chapter 27.  BulkRetrievalOption">BulkRetrievalOption</a> , bool)

``` c
const_iterator begin(BulkRetrievalOption bulkretrieval=
    BulkRetrievalOption::no_bulk_retrieval(),
    bool directdb_get=true) const
 
```

Begin a read-only iterator.

#### Parameters

##### directdb_get

Same as that of begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

##### bulkretrieval

Same as that of begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

#### Return Value

The created const iterator.

#### See Also

<a href="db_vector.md#stldb_vectorbegin" class="link" title="begin">db_vector::begin</a> (ReadModifyWrite, bool, <a href="BulkRetrievalOption.md" class="link" title="Chapter 27.  BulkRetrievalOption">BulkRetrievalOption</a> , bool);

### Group: Iterator Functions

The parameters in begin functions of this group have identical meaning to thoes in <a href="db_vector.md#stldb_vectorbegin" class="link" title="begin">db_vector::begin</a> , refer to those functions for details.

<a href="db_vector.md#stldb_vectorbegin" class="link" title="begin">db_vector::begin()</a>

### Class

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>
