---
title: "Chapter 10.  Db_base_iterator"
api-name: "Chapter 10.  Db_base_iterator"
source: docs/api_reference/STL/db_base_iterator.html
---
## Chapter 10.  Db_base_iterator

#### Public Members

| Member | Description |
|----|----|
| <a href="db_base_iterator.md#stldb_base_iteratorrefresh" class="xref" title="refresh">refresh</a> | Read data from underlying database via its cursor, and update its cached value. |
| <a href="stldb_base_iteratorclose_cursor.md" class="xref" title="close_cursor">close_cursor</a> | Close its cursor. |
| <a href="stldb_base_iteratorset_bulk_buffer.md" class="xref" title="set_bulk_buffer">set_bulk_buffer</a> | Call this function to modify bulk buffer size. |
| <a href="stldb_base_iteratorget_bulk_bufsize.md" class="xref" title="get_bulk_bufsize">get_bulk_bufsize</a> | Return current bulk buffer size. |
| <a href="stldb_base_iteratordb_base_iterator.md" class="xref" title="db_base_iterator">db_base_iterator</a> | Default constructor. |
| <a href="stldb_base_iteratoroperator_assign.md" class="xref" title="operator=">operator=</a> | Iterator assignment operator. |
| <a href="stldb_base_iteratordstr_db_base_iterator.md" class="xref" title="~db_base_iterator">~db_base_iterator</a> | Destructor. |
| <a href="stldb_base_iteratorget_bulk_retrieval.md" class="xref" title="get_bulk_retrieval">get_bulk_retrieval</a> | Get bulk buffer size. |
| <a href="stldb_base_iteratoris_rmw.md" class="xref" title="is_rmw">is_rmw</a> | Get DB_RMW setting. |
| <a href="stldb_base_iteratoris_directdb_get.md" class="xref" title="is_directdb_get">is_directdb_get</a> | Get direct database get setting. |

#### Group

<a href="dbstl_iterators.md" class="xref" title="Chapter 9.  Dbstl Iterator Classes">Dbstl Iterator Classes</a>

## refresh

### Function Details

``` c
int refresh(bool from_db=true)
 
```

Read data from underlying database via its cursor, and update its cached value.

#### Parameters

##### from_db

Whether retrieve data from database rather than using the cached data in this iterator.

#### Return Value

0 if succeeded. Otherwise an <a href="DbstlException.md" class="link" title="Chapter 30.  DbstlException">DbstlException</a> exception will be thrown.

### Class

<a href="db_base_iterator.md" class="link" title="Chapter 10.  Db_base_iterator">db_base_iterator</a>
