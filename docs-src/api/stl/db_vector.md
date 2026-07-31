---
title: "Chapter 4.  Db_vector"
api-name: "Chapter 4.  Db_vector"
source: docs/api_reference/STL/db_vector.html
---
## Chapter 4.  Db_vector

The <a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a> class has the union set of public member functions as std::vector, std::deque and std::list, and each method has identical default semantics to that in the std equivalent containers.

The difference is that the data is maintained using a Berkeley DB database as well as some Berkeley DB related extensions.

#### See Also

<a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a> <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(const db_container&amp;)</a>

### Class Template Parameters

#### T

The type of data to store.

#### value_type_sub

If T is a class/struct type, do not specify anything for this parameter; Otherwise, specify ElementHolder\<T\> to it. Database(dbp) and environment(penv) handle requirement(applies for all constructors of this class template): dbp must meet the following requirement: 1. dbp must be a DB_RECNO type of database handle. 2. DB_THREAD must be set to dbp's open flags. 3. An optional flag DB_RENUMBER is required if the container object is supposed to be a std::vector or std::deque equivalent; Not required if it is a std::list equivalent. But dbstl will not check whether DB_RENUMBER is set to this database handle. Setting DB_RENUMBER will cause the index values of all elements in the underlying databse to be maintained consecutive and in order, which involves potentially a lot of work because many indices may be updated. See the <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a> for more information about the two parameters.

#### Public Members

| Member | Description |
|----|----|
| <a href="db_vector.md#stldb_vectorbegin" class="xref" title="begin">begin</a> | Create a read-write or read-only iterator. |
| <a href="stldb_vectorend.md" class="xref" title="end">end</a> | Create an open boundary iterator. |
| <a href="stldb_vectorrbegin.md" class="xref" title="rbegin">rbegin</a> | Create a reverse iterator. |
| <a href="stldb_vectorrend.md" class="xref" title="rend">rend</a> | Create an open boundary iterator. |
| <a href="stldb_vectormax_size.md" class="xref" title="max_size">max_size</a> | Get max size. |
| <a href="stldb_vectorcapacity.md" class="xref" title="capacity">capacity</a> | Get capacity. |
| <a href="stldb_vectoroperator_sqbrk.md" class="xref" title="operator[]">operator[]</a> | Index operator, can act as both a left value and a right value. |
| <a href="stldb_vectorat.md" class="xref" title="at">at</a> | Index function. |
| <a href="stldb_vectorfront.md" class="xref" title="front">front</a> | Return a reference to the first element. |
| <a href="stldb_vectorback.md" class="xref" title="back">back</a> | Return a reference to the last element. |
| <a href="stldb_vectoroperator_eq.md" class="xref" title="operator==">operator==</a> | Container equality comparison operator. |
| <a href="stldb_vectoroperator_ueq.md" class="xref" title="operator!=">operator!=</a> | Container in-equality comparison operator. |
| <a href="stldb_vectoroperator_lt.md" class="xref" title="operator&lt;">operator&lt;</a> | Container less than comparison operator. |
| <a href="stldb_vectorassign.md" class="xref" title="assign">assign</a> | Assign a range \[first, last) to this container. |
| <a href="stldb_vectorpush_front.md" class="xref" title="push_front">push_front</a> | Push an element x into the vector from front. |
| <a href="stldb_vectorpop_front.md" class="xref" title="pop_front">pop_front</a> | Pop out the front element from the vector. |
| <a href="stldb_vectorinsert.md" class="xref" title="insert">insert</a> | Insert x before position pos. |
| <a href="stldb_vectorerase.md" class="xref" title="erase">erase</a> | Erase element at position pos. |
| <a href="stldb_vectorremove.md" class="xref" title="remove">remove</a> | Remove all elements whose values are "value" from the list. |
| <a href="stldb_vectorremove_if.md" class="xref" title="remove_if">remove_if</a> | Remove all elements making "pred" return true. |
| <a href="stldb_vectormerge.md" class="xref" title="merge">merge</a> | Merge content with another container. |
| <a href="stldb_vectorunique.md" class="xref" title="unique">unique</a> | Remove consecutive duplicate values from this list. |
| <a href="stldb_vectorsort.md" class="xref" title="sort">sort</a> | Sort this list. |
| <a href="stldb_vectorreverse.md" class="xref" title="reverse">reverse</a> | Reverse this list. |
| <a href="stldb_vectorsplice.md" class="xref" title="splice">splice</a> | Moves elements from list x into this list. |
| <a href="stldb_vectorsize.md" class="xref" title="size">size</a> | Return the number of elements in this container. |
| <a href="stldb_vectorempty.md" class="xref" title="empty">empty</a> | Returns whether this container is empty. |
| <a href="stldb_vectordb_vector.md" class="xref" title="db_vector">db_vector</a> | Constructor. |
| <a href="stldb_vectordstr_db_vector.md" class="xref" title="~db_vector">~db_vector</a> |  |
| <a href="stldb_vectoroperator_assign.md" class="xref" title="operator=">operator=</a> | Container assignment operator. |
| <a href="stldb_vectorresize.md" class="xref" title="resize">resize</a> | Resize this container to specified size n, insert values t if need to enlarge the container. |
| <a href="stldb_vectorreserve.md" class="xref" title="reserve">reserve</a> | Reserve space. |
| <a href="stldb_vectorpush_back.md" class="xref" title="push_back">push_back</a> | Push back an element into the vector. |
| <a href="stldb_vectorpop_back.md" class="xref" title="pop_back">pop_back</a> | Pop out last element from the vector. |
| <a href="stldb_vectorswap.md" class="xref" title="swap">swap</a> | Swap content with another vector vec. |
| <a href="stldb_vectorclear.md" class="xref" title="clear">clear</a> | Remove all elements of the vector, make it an empty vector. |

#### Group

<a href="dbstl_containers.md" class="xref" title="Chapter 2.  Dbstl Container Classes">Dbstl Container Classes</a>

## begin

### Function Details

``` c
iterator begin(ReadModifyWriteOption rmw=
    ReadModifyWriteOption::no_read_modify_write(), bool readonly=false,
    BulkRetrievalOption bulk_read=BulkRetrievalOption::no_bulk_retrieval(),
    bool directdb_get=true)
 
```

Create a read-write or read-only iterator.

We allow users to create a readonly iterator here so that they don't have to use a const container to create a const_iterator. But using const_iterator is faster. The flags set via db_container::set_cursor_oflags() is used as the cursor open flags.

#### Parameters

##### directdb_get

Whether always read key/data pair from backing db rather than using the value cached in the iterator. The current key/data pair is cached in the iterator and always kept updated on iterator movement, but in some extreme conditions, errors can happen if you use cached key/data pairs without always refreshing them from database. By default we are always reading from database when we are accessing the data the iterator sits on, except when we are doing bulk retrievals. But your application can gain extra performance promotion if you can set this flag to false.

##### readonly

Whether the iterator is created as a readonly iterator. Read only iterators can not update its underlying key/data pair.

##### bulk_read

Whether read database key/data pairs in bulk, by specifying DB_MULTIPLE_KEY flag to underlying cursor's Dbc::get function. Only readonly iterators can do bulk retrieval, if iterator is not read only, this parameter is ignored. Bulk retrieval can accelerate reading speed because each database read operation will read many key/data pairs, thus saved many database read operations. The default bulk buffer size is 32KB, you can set your desired bulk buffer size by specifying BulkRetrievalOpt::bulk_retrieval(your_bulk_buffer_size); If you don't want bulk retrieval, set BulkRetrievalItrOpt::no_bulk_retrieval() as the real parameter.

##### rmw

Whether this iterator will open a Berkeley DB cursor with DB_RMW flag set. If the iterator is used to read a key/data pair, then update it and store back to db, it is good to set the DB_RMW flag, by specifying RMWItrOpt::read_modify_write() If you don't want to set the DB_RMW flag, specify RMWItrOpt::no_read_modify_write(), which is the default behavior.

#### Return Value

The created iterator.

#### See Also

db_container::set_cursor_oflags();

``` c
const_iterator begin(BulkRetrievalOption bulkretrieval=
    (BulkRetrievalOption::no_bulk_retrieval()),
    bool directdb_get=true) const
 
```

Create a const iterator.

The created iterator can only be used to read its referenced data element. Can only be called when using a const reference to the contaienr object. The parameters have identical meanings and usage to those of the other non-const begin function.

#### Parameters

##### directdb_get

Same as that of begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

##### bulkretrieval

Same as that of begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

#### Return Value

The created const iterator.

#### See Also

begin(ReadModifyWrite, bool, BulkRetrievalOption, bool);

### Class

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a>
