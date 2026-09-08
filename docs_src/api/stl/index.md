---
title: "Berkeley DB C++ Standard Template Library API Reference"
api-name: "Berkeley DB C++ Standard Template Library API Reference"
source: docs/api_reference/STL/index.html
---
# Berkeley DB C++ Standard Template Library API Reference

| DB C++ STL API | Description |
|----|----|
| <a href="dbstl_global_functions.md" class="xref" title="Dbstl Global Public Functions">Dbstl Global Public Functions</a> | 23 members |
| <a href="dbstl_containers.md" class="xref" title="Dbstl Container Classes">Dbstl Container Classes</a> | 6 members |
| <a href="db_container.md" class="xref" title="Db_container">Db_container</a> | 14 members |
| <a href="db_vector.md" class="xref" title="Db_vector">Db_vector</a> | 36 members |
| <a href="db_map.md" class="xref" title="Db_map">Db_map</a> | 28 members |
| <a href="db_multimap.md" class="xref" title="Db_multimap">Db_multimap</a> | 12 members |
| <a href="db_set.md" class="xref" title="Db_set">Db_set</a> | 8 members |
| <a href="db_multiset.md" class="xref" title="Db_multiset">Db_multiset</a> | 8 members |
| <a href="dbstl_iterators.md" class="xref" title="Dbstl Iterator Classes">Dbstl Iterator Classes</a> | 6 members |
| <a href="db_base_iterator.md" class="xref" title="Db_base_iterator">Db_base_iterator</a> | 10 members |
| <a href="db_vector_iterators.md" class="xref" title="Iterator Classes for db_vector">Iterator Classes for db_vector</a> | 2 members |
| <a href="db_vector_base_iterator.md" class="xref" title="Db_vector_base_iterator">Db_vector_base_iterator</a> | 24 members |
| <a href="db_vector_iterator.md" class="xref" title="Db_vector_iterator">Db_vector_iterator</a> | 13 members |
| <a href="db_map_iterators.md" class="xref" title="Iterator Classes for db_map and db_multimap">Iterator Classes for db_map and db_multimap</a> | 2 members |
| <a href="db_map_base_iterator.md" class="xref" title="Db_map_base_iterator">Db_map_base_iterator</a> | 14 members |
| <a href="db_map_iterator.md" class="xref" title="Db_map_iterator">Db_map_iterator</a> | 8 members |
| <a href="dbset_iterators.md" class="xref" title="Iterator Classes for db_set and db_multiset">Iterator Classes for db_set and db_multiset</a> | 2 members |
| <a href="db_set_base_iterator.md" class="xref" title="Db_set_base_iterator">Db_set_base_iterator</a> | 7 members |
| <a href="db_set_iterator.md" class="xref" title="Db_set_iterator">Db_set_iterator</a> | 7 members |
| <a href="db_reverse_iterator.md" class="xref" title="Db_reverse_iterator">Db_reverse_iterator</a> | 13 members |
| <a href="dbstl_helper_classes.md" class="xref" title="Dbstl Helper Classes">Dbstl Helper Classes</a> | 5 members |
| <a href="Element_wrappers.md" class="xref" title="ElementRef and ElementHolder Wappers">ElementRef and ElementHolder Wappers</a> | 2 members |
| <a href="ElementHolder.md" class="xref" title="ElementHolder">ElementHolder</a> | 18 members |
| <a href="ElementRef.md" class="xref" title="ElementRef">ElementRef</a> | 5 members |
| <a href="DbstlDbt.md" class="xref" title="DbstlDbt">DbstlDbt</a> | 3 members |
| <a href="DbstlElemTraits.md" class="xref" title="DbstlElemTraits">DbstlElemTraits</a> | 34 members |
| <a href="BulkRetrievalOption.md" class="xref" title="BulkRetrievalOption">BulkRetrievalOption</a> | 6 members |
| <a href="ReadModifyWriteOption.md" class="xref" title="ReadModifyWriteOption">ReadModifyWriteOption</a> | 4 members |
| <a href="Exception_classes_group.md" class="xref" title="Dbstl Exception Classes">Dbstl Exception Classes</a> | 10 members |
| <a href="DbstlException.md" class="xref" title="DbstlException">DbstlException</a> | 3 members |
| <a href="InvalidDbtException.md" class="xref" title="InvalidDbtException">InvalidDbtException</a> | 1 members |
| <a href="FailedAssertionException.md" class="xref" title="FailedAssertionException">FailedAssertionException</a> | 3 members |
| <a href="InvalidCursorException.md" class="xref" title="InvalidCursorException">InvalidCursorException</a> | 1 members |
| <a href="NoSuchKeyException.md" class="xref" title="NoSuchKeyException">NoSuchKeyException</a> | 1 members |
| <a href="NotEnoughMemoryException.md" class="xref" title="NotEnoughMemoryException">NotEnoughMemoryException</a> | 1 members |
| <a href="NotSupportedException.md" class="xref" title="NotSupportedException">NotSupportedException</a> | 1 members |
| <a href="InvalidIteratorException.md" class="xref" title="InvalidIteratorException">InvalidIteratorException</a> | 1 members |
| <a href="InvalidFunctionCall.md" class="xref" title="InvalidFunctionCall">InvalidFunctionCall</a> | 1 members |
| <a href="InvalidArgumentException.md" class="xref" title="InvalidArgumentException">InvalidArgumentException</a> | 1 members |

## <a href="dbstl_global_functions.md" class="xref" title="Dbstl Global Public Functions">Dbstl Global Public Functions</a>

| Member | Description |
|----|----|
| <a href="dbstl_global_functions.md#stldbstl_global_functionsclose_db" class="xref" title="close_db">close_db</a> | Close pdb regardless of reference count. |
| <a href="stldbstl_global_functionsclose_all_dbs.md" class="xref" title="close_all_dbs">close_all_dbs</a> | Close all open database handles regardless of reference count. |
| <a href="stldbstl_global_functionsclose_db_env.md" class="xref" title="close_db_env">close_db_env</a> | Close specified database environment handle regardless of reference count. |
| <a href="stldbstl_global_functionsclose_all_db_envs.md" class="xref" title="close_all_db_envs">close_all_db_envs</a> | Close all open database environment handles regardless of reference count. |
| <a href="stldbstl_global_functionsbegin_txn.md" class="xref" title="begin_txn">begin_txn</a> | Begin a new transaction from the specified environment "env". |
| <a href="stldbstl_global_functionscommit_txn.md" class="xref" title="commit_txn">commit_txn</a> | Commit current transaction opened in the environment "env". |
| <a href="stldbstl_global_functionsabort_txn.md" class="xref" title="abort_txn">abort_txn</a> | Abort current transaction of environment "env". |
| <a href="stldbstl_global_functionscurrent_txn.md" class="xref" title="current_txn">current_txn</a> | Get current transaction of environment "env". |
| <a href="stldbstl_global_functionsset_current_txn_handle.md" class="xref" title="set_current_txn_handle">set_current_txn_handle</a> | Set environment env's current transaction handle to be newtxn. |
| <a href="stldbstl_global_functionsregister_db.md" class="xref" title="register_db">register_db</a> | Register a Db handle "pdb1". |
| <a href="stldbstl_global_functionsregister_db_env.md" class="xref" title="register_db_env">register_db_env</a> | Register a DbEnv handle env1, this handle and handles opened in it will be closed by ResourceManager . |
| <a href="stldbstl_global_functionsopen_db.md" class="xref" title="open_db">open_db</a> | Helper function to open a database and register it into dbstl for the calling thread. |
| <a href="stldbstl_global_functionsopen_env.md" class="xref" title="open_env">open_env</a> | Helper function to open an environment and register it into dbstl for the calling thread. |
| <a href="stldbstl_global_functionsalloc_mutex.md" class="xref" title="alloc_mutex">alloc_mutex</a> | Allocate a Berkeley DB mutex. |
| <a href="stldbstl_global_functionslock_mutex.md" class="xref" title="lock_mutex">lock_mutex</a> | Lock a mutex, wait if it is held by another thread. |
| <a href="stldbstl_global_functionsunlock_mutex.md" class="xref" title="unlock_mutex">unlock_mutex</a> | Unlock a mutex, and return immediately. |
| <a href="stldbstl_global_functionsfree_mutex.md" class="xref" title="free_mutex">free_mutex</a> | Free a mutex, and return immediately. |
| <a href="stldbstl_global_functionsdbstl_startup.md" class="xref" title="dbstl_startup">dbstl_startup</a> | If there are multiple threads within a process that make use of dbstl, then this function should be called in a single thread mutual exclusively before any use of dbstl in a process; Otherwise, you don't need to call it, but are allowed to call it anyway. |
| <a href="stldbstl_global_functionsdbstl_exit.md" class="xref" title="dbstl_exit">dbstl_exit</a> | This function releases any memory allocated in the heap by code of dbstl. |
| <a href="stldbstl_global_functionsdbstl_thread_exit.md" class="xref" title="dbstl_thread_exit">dbstl_thread_exit</a> | This function closes all Berkeley DB handles in the right order, if other threads do not use them. |
| <a href="stldbstl_global_functionsoperator_eq.md" class="xref" title="operator==">operator==</a> | Operators to compare two Dbt objects. |
| <a href="stldbstl_global_functionsset_global_dbfile_suffix_number.md" class="xref" title="set_global_dbfile_suffix_number">set_global_dbfile_suffix_number</a> | If exisiting random temporary database name generation mechanism is still causing name clashes, users can set this global suffix number which will be append to each temporary database file name and incremented after each append, and by default it is 0. |
| <a href="stldbstl_global_functionsclose_db_cursors.md" class="xref" title="close_db_cursors">close_db_cursors</a> | Close cursors opened in dbp1. |

## <a href="dbstl_containers.md" class="xref" title="Dbstl Container Classes">Dbstl Container Classes</a>

| Member | Description |
|----|----|
| <a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> | db_container |
| <a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a> | db_map |
| <a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a> | db_multimap |
| <a href="db_set.md" class="link" title="Chapter 7.  Db_set">db_set</a> | db_set |
| <a href="db_multiset.md" class="link" title="Chapter 8.  Db_multiset">db_multiset</a> | db_multiset |
| <a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a> | db_vector |

## <a href="db_container.md" class="xref" title="Db_container">Db_container</a>

| Member | Description |
|----|----|
| <a href="db_container.md#stldb_containerget_db_open_flags" class="xref" title="get_db_open_flags">get_db_open_flags</a> | Get the backing database's open flags. |
| <a href="stldb_containerget_db_set_flags.md" class="xref" title="get_db_set_flags">get_db_set_flags</a> | Get the backing database's flags that are set via Db::set_flags() function. |
| <a href="stldb_containerget_db_handle.md" class="xref" title="get_db_handle">get_db_handle</a> | Get the backing database's handle. |
| <a href="stldb_containerget_db_env_handle.md" class="xref" title="get_db_env_handle">get_db_env_handle</a> | Get the backing database environment's handle. |
| <a href="stldb_containerset_db_handle.md" class="xref" title="set_db_handle">set_db_handle</a> | Set the underlying database's handle, and optionally environment handle if the environment has also changed. |
| <a href="stldb_containerset_all_flags.md" class="xref" title="set_all_flags">set_all_flags</a> | Set the flags required by the Berkeley DB functions DbEnv::txn_begin(), DbTxn::commit() and DbEnv::cursor(). |
| <a href="stldb_containerset_txn_begin_flags.md" class="xref" title="set_txn_begin_flags">set_txn_begin_flags</a> | Set flag of DbEnv::txn_begin() call. |
| <a href="stldb_containerget_txn_begin_flags.md" class="xref" title="get_txn_begin_flags">get_txn_begin_flags</a> | Get flag of DbEnv::txn_begin() call. |
| <a href="stldb_containerset_commit_flags.md" class="xref" title="set_commit_flags">set_commit_flags</a> | Set flag of DbTxn::commit() call. |
| <a href="stldb_containerget_commit_flags.md" class="xref" title="get_commit_flags">get_commit_flags</a> | Get flag of DbTxn::commit() call. |
| <a href="stldb_containerget_cursor_open_flags.md" class="xref" title="get_cursor_open_flags">get_cursor_open_flags</a> | Get flag of Db::cursor() call. |
| <a href="stldb_containerset_cursor_open_flags.md" class="xref" title="set_cursor_open_flags">set_cursor_open_flags</a> | Set flag of Db::cursor() call. |
| <a href="stldb_containerdb_container.md" class="xref" title="db_container">db_container</a> | Default constructor. |
| <a href="stldb_containerdstr_db_container.md" class="xref" title="~db_container">~db_container</a> | The backing database is not closed in this function. |

## <a href="db_vector.md" class="xref" title="Db_vector">Db_vector</a>

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

## <a href="db_map.md" class="xref" title="Db_map">Db_map</a>

| Member | Description |
|----|----|
| <a href="db_map.md#stldb_mapdb_map" class="xref" title="db_map">db_map</a> | Create a std::map/hash_map equivalent associative container. |
| <a href="stldb_mapdstr_db_map.md" class="xref" title="~db_map">~db_map</a> |  |
| <a href="stldb_mapinsert.md" class="xref" title="insert">insert</a> | Insert a single key/data pair if the key is not in the container. |
| <a href="stldb_mapbegin.md" class="xref" title="begin">begin</a> | Begin a read-write or readonly iterator which sits on the first key/data pair of the database. |
| <a href="stldb_mapend.md" class="xref" title="end">end</a> | Create an open boundary iterator. |
| <a href="stldb_maprbegin.md" class="xref" title="rbegin">rbegin</a> | Begin a read-write or readonly reverse iterator which sits on the first key/data pair of the database. |
| <a href="stldb_maprend.md" class="xref" title="rend">rend</a> | Create an open boundary iterator. |
| <a href="stldb_mapis_hash.md" class="xref" title="is_hash">is_hash</a> | Get container category. |
| <a href="stldb_mapbucket_count.md" class="xref" title="bucket_count">bucket_count</a> | Only for std::hash_map, return number of hash bucket in use. |
| <a href="stldb_mapsize.md" class="xref" title="size">size</a> | This function supports auto-commit. |
| <a href="stldb_mapmax_size.md" class="xref" title="max_size">max_size</a> | Get max size. |
| <a href="stldb_mapempty.md" class="xref" title="empty">empty</a> | Returns whether this container is empty. |
| <a href="stldb_maperase.md" class="xref" title="erase">erase</a> | Erase a key/data pair at specified position. |
| <a href="stldb_mapfind.md" class="xref" title="find">find</a> | Find the key/data pair with specified key x. |
| <a href="stldb_maplower_bound.md" class="xref" title="lower_bound">lower_bound</a> | Find the greatest key less than or equal to x. |
| <a href="stldb_mapequal_range.md" class="xref" title="equal_range">equal_range</a> | Find the range within which all keys equal to specified key x. |
| <a href="stldb_mapcount.md" class="xref" title="count">count</a> | Count the number of key/data pairs having specified key x. |
| <a href="stldb_mapupper_bound.md" class="xref" title="upper_bound">upper_bound</a> | Find the least key greater than x. |
| <a href="stldb_mapkey_eq.md" class="xref" title="key_eq">key_eq</a> | Function to get key compare functor. |
| <a href="stldb_maphash_funct.md" class="xref" title="hash_funct">hash_funct</a> | Function to get hash key generating functor. |
| <a href="stldb_mapvalue_comp.md" class="xref" title="value_comp">value_comp</a> | Function to get value compare functor. |
| <a href="stldb_mapkey_comp.md" class="xref" title="key_comp">key_comp</a> | Function to get key compare functor. |
| <a href="stldb_mapoperator_assign.md" class="xref" title="operator=">operator=</a> | Container content assignment operator. |
| <a href="stldb_mapoperator_sqbrk.md" class="xref" title="operator[]">operator[]</a> | Retrieve data element by key. |
| <a href="stldb_mapswap.md" class="xref" title="swap">swap</a> | Swap content with container mp. |
| <a href="stldb_mapclear.md" class="xref" title="clear">clear</a> | Clear contents in this container. |
| <a href="stldb_mapoperator_eq.md" class="xref" title="operator==">operator==</a> | Map content equality comparison operator. |
| <a href="stldb_mapoperator_ueq.md" class="xref" title="operator!=">operator!=</a> | Container unequality comparison operator. |

## <a href="db_multimap.md" class="xref" title="Db_multimap">Db_multimap</a>

| Member | Description |
|----|----|
| <a href="db_multimap.md#stldb_multimapinsert" class="xref" title="insert">insert</a> | Range insertion. |
| <a href="stldb_multimaperase.md" class="xref" title="erase">erase</a> | Erase elements by key. |
| <a href="stldb_multimapequal_range.md" class="xref" title="equal_range">equal_range</a> | Find the range within which all keys equal to specified key x. |
| <a href="stldb_multimapequal_range_N.md" class="xref" title="equal_range_N">equal_range_N</a> | Find equal range and number of key/data pairs in the range. |
| <a href="stldb_multimapcount.md" class="xref" title="count">count</a> | Count the number of key/data pairs having specified key x. |
| <a href="stldb_multimapupper_bound.md" class="xref" title="upper_bound">upper_bound</a> | Find the least key greater than x. |
| <a href="stldb_multimapdb_multimap.md" class="xref" title="db_multimap">db_multimap</a> | Constructor. |
| <a href="stldb_multimapdstr_db_multimap.md" class="xref" title="~db_multimap">~db_multimap</a> |  |
| <a href="stldb_multimapoperator_assign.md" class="xref" title="operator=">operator=</a> | Container content assignment operator. |
| <a href="stldb_multimapswap.md" class="xref" title="swap">swap</a> | Swap content with another multimap container. |
| <a href="stldb_multimapoperator_eq.md" class="xref" title="operator==">operator==</a> | Returns whether the two containers have identical content. |
| <a href="stldb_multimapoperator_ueq.md" class="xref" title="operator!=">operator!=</a> | Container unequality comparison operator. |

## <a href="db_set.md" class="xref" title="Db_set">Db_set</a>

| Member | Description |
|----|----|
| <a href="db_set.md#stldb_setdb_set" class="xref" title="db_set">db_set</a> | Create a std::set/hash_set equivalent associative container. |
| <a href="stldb_setdstr_db_set.md" class="xref" title="~db_set">~db_set</a> |  |
| <a href="stldb_setinsert.md" class="xref" title="insert">insert</a> | Insert a single key/data pair if the key is not in the container. |
| <a href="stldb_setoperator_assign.md" class="xref" title="operator=">operator=</a> | Container content assignment operator. |
| <a href="stldb_setvalue_comp.md" class="xref" title="value_comp">value_comp</a> | Get value comparison functor. |
| <a href="stldb_setswap.md" class="xref" title="swap">swap</a> | Swap content with another container. |
| <a href="stldb_setoperator_eq.md" class="xref" title="operator==">operator==</a> | Set content equality comparison operator. |
| <a href="stldb_setoperator_ueq.md" class="xref" title="operator!=">operator!=</a> | Inequality comparison operator. |

## <a href="db_multiset.md" class="xref" title="Db_multiset">Db_multiset</a>

| Member | Description |
|----|----|
| <a href="db_multiset.md#stldb_multisetdb_multiset" class="xref" title="db_multiset">db_multiset</a> | Create a std::multiset/hash_multiset equivalent associative container. |
| <a href="stldb_multisetdstr_db_multiset.md" class="xref" title="~db_multiset">~db_multiset</a> |  |
| <a href="stldb_multisetinsert.md" class="xref" title="insert">insert</a> | Insert a single key if the key is not in the container. |
| <a href="stldb_multiseterase.md" class="xref" title="erase">erase</a> | Erase elements by key. |
| <a href="stldb_multisetoperator_assign.md" class="xref" title="operator=">operator=</a> | Container content assignment operator. |
| <a href="stldb_multisetswap.md" class="xref" title="swap">swap</a> | Swap content with another container. |
| <a href="stldb_multisetoperator_eq.md" class="xref" title="operator==">operator==</a> | Container content equality compare operator. |
| <a href="stldb_multisetoperator_ueq.md" class="xref" title="operator!=">operator!=</a> | Inequality comparison operator. |

## <a href="dbstl_iterators.md" class="xref" title="Dbstl Iterator Classes">Dbstl Iterator Classes</a>

| Member | Description |
|----|----|
| <a href="db_base_iterator.md" class="link" title="Chapter 10.  Db_base_iterator">db_base_iterator</a> | db_base_iterator |
| <a href="db_reverse_iterator.md" class="link" title="Chapter 20.  Db_reverse_iterator">db_reverse_iterator</a> | db_reverse_iterator |
| <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a> | db_map_iterator |
| <a href="db_map_iterators.md" class="link" title="Chapter 14.  Iterator Classes for db_map and db_multimap">Iterator classes for db_map and db_multimap.</a> | Iterator classes for db_map and db_multimap. |
| <a href="dbset_iterators.md" class="link" title="Chapter 17.  Iterator Classes for db_set and db_multiset">Iterator classes for db_set and db_multiset.</a> | Iterator classes for db_set and db_multiset. |
| <a href="db_vector_iterators.md" class="link" title="Chapter 11.  Iterator Classes for db_vector">Iterator classes for db_vector.</a> | Iterator classes for db_vector. |

## <a href="db_base_iterator.md" class="xref" title="Db_base_iterator">Db_base_iterator</a>

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

## <a href="db_vector_iterators.md" class="xref" title="Iterator Classes for db_vector">Iterator Classes for db_vector</a>

| Member | Description |
|----|----|
| <a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a> | db_vector_base_iterator |
| <a href="db_vector_iterator.md" class="link" title="Chapter 13.  Db_vector_iterator">db_vector_iterator</a> | db_vector_iterator |

## <a href="db_vector_base_iterator.md" class="xref" title="Db_vector_base_iterator">Db_vector_base_iterator</a>

| Member | Description |
|----|----|
| <a href="db_vector_base_iterator.md#stldb_vector_base_iteratordb_vector_base_iterator" class="xref" title="db_vector_base_iterator">db_vector_base_iterator</a> |  |
| <a href="stldb_vector_base_iteratordstr_db_vector_base_iterator.md" class="xref" title="~db_vector_base_iterator">~db_vector_base_iterator</a> |  |
| <a href="stldb_vector_base_iteratoroperator_eq.md" class="xref" title="operator==">operator==</a> | Equality comparison operator. |
| <a href="stldb_vector_base_iteratoroperator_ueq.md" class="xref" title="operator!=">operator!=</a> | Unequal compare, identical to !operator(==itr). |
| <a href="stldb_vector_base_iteratoroperator_lt.md" class="xref" title="operator&lt;">operator&lt;</a> | Less than comparison operator. |
| <a href="stldb_vector_base_iteratoroperator_le.md" class="xref" title="operator&lt;=">operator&lt;=</a> | Less equal comparison operator. |
| <a href="stldb_vector_base_iteratoroperator_ge.md" class="xref" title="operator&gt;=">operator&gt;=</a> | Greater equal comparison operator. |
| <a href="stldb_vector_base_iteratoroperator_gt.md" class="xref" title="operator&gt;">operator&gt;</a> | Greater comparison operator. |
| <a href="stldb_vector_base_iteratoroperator_incr.md" class="xref" title="operator++">operator++</a> | Pre-increment. |
| <a href="stldb_vector_base_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Pre-decrement. |
| <a href="stldb_vector_base_iteratoroperator_assign.md" class="xref" title="operator=">operator=</a> | Assignment operator. |
| <a href="stldb_vector_base_iteratoroperator_add.md" class="xref" title="operator+">operator+</a> | Iterator movement operator. |
| <a href="stldb_vector_base_iteratoroperator_ia.md" class="xref" title="operator+=">operator+=</a> | Move this iterator backward by n elements. |
| <a href="stldb_vector_base_iteratoroperator_sub.md" class="xref" title="operator-">operator-</a> | Iterator movement operator. |
| <a href="stldb_vector_base_iteratoroperator_sa.md" class="xref" title="operator-=">operator-=</a> | Move this iterator forward by n elements. |
| <a href="stldb_vector_base_iteratoroperator__star.md" class="xref" title="operator *">operator *</a> | Dereference operator. |
| <a href="stldb_vector_base_iteratoroperator_arrow.md" class="xref" title="operator-&gt;">operator-&gt;</a> | Arrow operator. |
| <a href="stldb_vector_base_iteratoroperator_sqbrk.md" class="xref" title="operator[]">operator[]</a> | Iterator index operator. |
| <a href="stldb_vector_base_iteratorget_current_index.md" class="xref" title="get_current_index">get_current_index</a> | Get current index of within the vector. |
| <a href="stldb_vector_base_iteratormove_to.md" class="xref" title="move_to">move_to</a> | Iterator movement function. |
| <a href="stldb_vector_base_iteratorrefresh.md" class="xref" title="refresh">refresh</a> | Refresh iterator cached value. |
| <a href="stldb_vector_base_iteratorclose_cursor.md" class="xref" title="close_cursor">close_cursor</a> | Close underlying Berkeley DB cursor of this iterator. |
| <a href="stldb_vector_base_iteratorset_bulk_buffer.md" class="xref" title="set_bulk_buffer">set_bulk_buffer</a> | Modify bulk buffer size. |
| <a href="stldb_vector_base_iteratorget_bulk_bufsize.md" class="xref" title="get_bulk_bufsize">get_bulk_bufsize</a> | Get bulk retrieval buffer size in bytes. |

## <a href="db_vector_iterator.md" class="xref" title="Db_vector_iterator">Db_vector_iterator</a>

| Member | Description |
|----|----|
| <a href="db_vector_iterator.md#stldb_vector_iteratordb_vector_iterator" class="xref" title="db_vector_iterator">db_vector_iterator</a> |  |
| <a href="stldb_vector_iteratordstr_db_vector_iterator.md" class="xref" title="~db_vector_iterator">~db_vector_iterator</a> |  |
| <a href="stldb_vector_iteratoroperator_incr.md" class="xref" title="operator++">operator++</a> | Pre-increment. |
| <a href="stldb_vector_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Pre-decrement. |
| <a href="stldb_vector_iteratoroperator_assign.md" class="xref" title="operator=">operator=</a> | Assignment operator. |
| <a href="stldb_vector_iteratoroperator_add.md" class="xref" title="operator+">operator+</a> | Iterator movement operator. |
| <a href="stldb_vector_iteratoroperator_ia.md" class="xref" title="operator+=">operator+=</a> | Move this iterator backward by n elements. |
| <a href="stldb_vector_iteratoroperator_sub.md" class="xref" title="operator-">operator-</a> | Iterator movement operator. |
| <a href="stldb_vector_iteratoroperator_sa.md" class="xref" title="operator-=">operator-=</a> | Move this iterator forward by n elements. |
| <a href="stldb_vector_iteratoroperator__star.md" class="xref" title="operator *">operator *</a> | Dereference operator. |
| <a href="stldb_vector_iteratoroperator_arrow.md" class="xref" title="operator-&gt;">operator-&gt;</a> | Arrow operator. |
| <a href="stldb_vector_iteratoroperator_sqbrk.md" class="xref" title="operator[]">operator[]</a> | Iterator index operator. |
| <a href="stldb_vector_iteratorrefresh.md" class="xref" title="refresh">refresh</a> | Refresh iterator cached value. |

## <a href="db_map_iterators.md" class="xref" title="Iterator Classes for db_map and db_multimap">Iterator Classes for db_map and db_multimap</a>

| Member | Description |
|----|----|
| <a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a> | db_map_base_iterator |
| <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a> | db_map_iterator |

## <a href="db_map_base_iterator.md" class="xref" title="Db_map_base_iterator">Db_map_base_iterator</a>

| Member | Description |
|----|----|
| <a href="db_map_base_iterator.md#stldb_map_base_iteratordb_map_base_iterator" class="xref" title="db_map_base_iterator">db_map_base_iterator</a> | Copy constructor. |
| <a href="stldb_map_base_iteratordstr_db_map_base_iterator.md" class="xref" title="~db_map_base_iterator">~db_map_base_iterator</a> | Destructor. |
| <a href="stldb_map_base_iteratoroperator_incr.md" class="xref" title="operator++">operator++</a> | Pre-increment. |
| <a href="stldb_map_base_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Pre-decrement. |
| <a href="stldb_map_base_iteratoroperator_eq.md" class="xref" title="operator==">operator==</a> | Equal comparison operator. |
| <a href="stldb_map_base_iteratoroperator_ueq.md" class="xref" title="operator!=">operator!=</a> | Unequal comparison operator. |
| <a href="stldb_map_base_iteratoroperator__star.md" class="xref" title="operator *">operator *</a> | Dereference operator. |
| <a href="stldb_map_base_iteratoroperator_arrow.md" class="xref" title="operator-&gt;">operator-&gt;</a> | Arrow operator. |
| <a href="stldb_map_base_iteratorrefresh.md" class="xref" title="refresh">refresh</a> | Refresh iterator cached value. |
| <a href="stldb_map_base_iteratorclose_cursor.md" class="xref" title="close_cursor">close_cursor</a> | Close underlying Berkeley DB cursor of this iterator. |
| <a href="stldb_map_base_iteratormove_to.md" class="xref" title="move_to">move_to</a> | Iterator movement function. |
| <a href="stldb_map_base_iteratorset_bulk_buffer.md" class="xref" title="set_bulk_buffer">set_bulk_buffer</a> | Modify bulk buffer size. |
| <a href="stldb_map_base_iteratorget_bulk_bufsize.md" class="xref" title="get_bulk_bufsize">get_bulk_bufsize</a> | Get bulk retrieval buffer size in bytes. |
| <a href="stldb_map_base_iteratoroperator_assign.md" class="xref" title="operator=">operator=</a> | Assignment operator. |

## <a href="db_map_iterator.md" class="xref" title="Db_map_iterator">Db_map_iterator</a>

| Member | Description |
|----|----|
| <a href="db_map_iterator.md#stldb_map_iteratordb_map_iterator" class="xref" title="db_map_iterator">db_map_iterator</a> | Copy constructor. |
| <a href="stldb_map_iteratordstr_db_map_iterator.md" class="xref" title="~db_map_iterator">~db_map_iterator</a> | Destructor. |
| <a href="stldb_map_iteratoroperator_incr.md" class="xref" title="operator++">operator++</a> | Pre-increment. |
| <a href="stldb_map_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Pre-decrement. |
| <a href="stldb_map_iteratoroperator__star.md" class="xref" title="operator *">operator *</a> | Dereference operator. |
| <a href="stldb_map_iteratoroperator_arrow.md" class="xref" title="operator-&gt;">operator-&gt;</a> | Arrow operator. |
| <a href="stldb_map_iteratorrefresh.md" class="xref" title="refresh">refresh</a> | Refresh iterator cached value. |
| <a href="stldb_map_iteratoroperator_assign.md" class="xref" title="operator=">operator=</a> | Assignment operator. |

## <a href="dbset_iterators.md" class="xref" title="Iterator Classes for db_set and db_multiset">Iterator Classes for db_set and db_multiset</a>

| Member | Description |
|----|----|
| <a href="db_set_base_iterator.md" class="link" title="Chapter 18.  Db_set_base_iterator">db_set_base_iterator</a> | db_set_base_iterator |
| <a href="db_set_iterator.md" class="link" title="Chapter 19.  Db_set_iterator">db_set_iterator</a> | db_set_iterator |

## <a href="db_set_base_iterator.md" class="xref" title="Db_set_base_iterator">Db_set_base_iterator</a>

| Member | Description |
|----|----|
| <a href="db_set_base_iterator.md#stldb_set_base_iteratordstr_db_set_base_iterator" class="xref" title="~db_set_base_iterator">~db_set_base_iterator</a> | Destructor. |
| <a href="stldb_set_base_iteratordb_set_base_iterator.md" class="xref" title="db_set_base_iterator">db_set_base_iterator</a> | Constructor. |
| <a href="stldb_set_base_iteratoroperator_incr.md" class="xref" title="operator++">operator++</a> | Post-increment. |
| <a href="stldb_set_base_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Post-decrement. |
| <a href="stldb_set_base_iteratoroperator__star.md" class="xref" title="operator *">operator *</a> | Dereference operator. |
| <a href="stldb_set_base_iteratoroperator_arrow.md" class="xref" title="operator-&gt;">operator-&gt;</a> | Arrow operator. |
| <a href="stldb_set_base_iteratorrefresh.md" class="xref" title="refresh">refresh</a> | Refresh iterator cached value. |

## <a href="db_set_iterator.md" class="xref" title="Db_set_iterator">Db_set_iterator</a>

| Member | Description |
|----|----|
| <a href="db_set_iterator.md#stldb_set_iteratordstr_db_set_iterator" class="xref" title="~db_set_iterator">~db_set_iterator</a> | Destructor. |
| <a href="stldb_set_iteratordb_set_iterator.md" class="xref" title="db_set_iterator">db_set_iterator</a> | Constructor. |
| <a href="stldb_set_iteratoroperator_incr.md" class="xref" title="operator++">operator++</a> | Pre-increment. |
| <a href="stldb_set_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Pre-decrement. |
| <a href="stldb_set_iteratoroperator__star.md" class="xref" title="operator *">operator *</a> | Dereference operator. |
| <a href="stldb_set_iteratoroperator_arrow.md" class="xref" title="operator-&gt;">operator-&gt;</a> | Arrow operator. |
| <a href="stldb_set_iteratorrefresh.md" class="xref" title="refresh">refresh</a> | Refresh iterator cached value. |

## <a href="db_reverse_iterator.md" class="xref" title="Db_reverse_iterator">Db_reverse_iterator</a>

| Member | Description |
|----|----|
| <a href="db_reverse_iterator.md#stldb_reverse_iteratoroperator_incr" class="xref" title="operator++">operator++</a> | Move this iterator forward by one element. |
| <a href="stldb_reverse_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Move this iterator backward by one element. |
| <a href="stldb_reverse_iteratoroperator_add.md" class="xref" title="operator+">operator+</a> | Iterator shuffle operator. |
| <a href="stldb_reverse_iteratoroperator_sub.md" class="xref" title="operator-">operator-</a> | Iterator shuffle operator. |
| <a href="stldb_reverse_iteratoroperator_ia.md" class="xref" title="operator+=">operator+=</a> | Iterator shuffle operator. |
| <a href="stldb_reverse_iteratoroperator_sa.md" class="xref" title="operator-=">operator-=</a> | Iterator shuffle operator. |
| <a href="stldb_reverse_iteratoroperator_lt.md" class="xref" title="operator&lt;">operator&lt;</a> | Less compare operator. |
| <a href="stldb_reverse_iteratoroperator_gt.md" class="xref" title="operator&gt;">operator&gt;</a> | Greater compare operator. |
| <a href="stldb_reverse_iteratoroperator_le.md" class="xref" title="operator&lt;=">operator&lt;=</a> | Less equal compare operator. |
| <a href="stldb_reverse_iteratoroperator_ge.md" class="xref" title="operator&gt;=">operator&gt;=</a> | Greater equal compare operator. |
| <a href="stldb_reverse_iteratordb_reverse_iterator.md" class="xref" title="db_reverse_iterator">db_reverse_iterator</a> | Constructor. Construct from an iterator of wrapped type. |
| <a href="stldb_reverse_iteratoroperator_assign.md" class="xref" title="operator=">operator=</a> | Assignment operator. |
| <a href="stldb_reverse_iteratoroperator_sqbrk.md" class="xref" title="operator[]">operator[]</a> | Return the reference of the element which can be reached by moving this reverse iterator by Off times backward. |

## <a href="dbstl_helper_classes.md" class="xref" title="Dbstl Helper Classes">Dbstl Helper Classes</a>

| Member | Description |
|----|----|
| <a href="BulkRetrievalOption.md" class="link" title="Chapter 27.  BulkRetrievalOption">BulkRetrievalOption</a> | BulkRetrievalOption |
| <a href="ReadModifyWriteOption.md" class="link" title="Chapter 28.  ReadModifyWriteOption">ReadModifyWriteOption</a> | ReadModifyWriteOption |
| <a href="DbstlElemTraits.md" class="link" title="Chapter 26.  DbstlElemTraits">DbstlElemTraits</a> | DbstlElemTraits |
| <a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> | DbstlDbt |
| <a href="Element_wrappers.md" class="link" title="Chapter 22.  ElementRef and ElementHolder Wappers">ElementRef and ElementHolder wrappers.</a> | ElementRef and ElementHolder wrappers. |

## <a href="Element_wrappers.md" class="xref" title="ElementRef and ElementHolder Wappers">ElementRef and ElementHolder Wappers</a>

| Member | Description |
|----|----|
| <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> | ElementRef |
| <a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a> | ElementHolder |

## <a href="ElementHolder.md" class="xref" title="ElementHolder">ElementHolder</a>

| Member | Description |
|----|----|
| <a href="ElementHolder.md#stlElementHolderElementHolder" class="xref" title="ElementHolder">ElementHolder</a> | Constructor. |
| <a href="stlElementHolderdstr_ElementHolder.md" class="xref" title="~ElementHolder">~ElementHolder</a> | Destructor. |
| <a href="stlElementHolderoperator_ia.md" class="xref" title="operator+=">operator+=</a> |  |
| <a href="stlElementHolderoperator_sa.md" class="xref" title="operator-=">operator-=</a> |  |
| <a href="stlElementHolderoperator__ma.md" class="xref" title="operator *=">operator *=</a> |  |
| <a href="stlElementHolderoperator_da.md" class="xref" title="operator/=">operator/=</a> |  |
| <a href="stlElementHolderoperator_modasg.md" class="xref" title="operator%=">operator%=</a> |  |
| <a href="stlElementHolderoperator__aa.md" class="xref" title="operator &amp;=">operator &amp;=</a> |  |
| <a href="stlElementHolderoperator_oa.md" class="xref" title="operator|=">operator|=</a> |  |
| <a href="stlElementHolderoperator_xa.md" class="xref" title="operator^=">operator^=</a> |  |
| <a href="stlElementHolderoperator_gt_ge.md" class="xref" title="operator&gt;&gt;=">operator&gt;&gt;=</a> |  |
| <a href="stlElementHolderoperator_lt_le.md" class="xref" title="operator&lt;&lt;=">operator&lt;&lt;=</a> |  |
| <a href="stlElementHolderoperator_incr.md" class="xref" title="operator++">operator++</a> |  |
| <a href="stlElementHolderoperator_decr.md" class="xref" title="operator--">operator--</a> |  |
| <a href="stlElementHolderoperator_assign.md" class="xref" title="operator=">operator=</a> |  |
| <a href="stlElementHolderoperator_ptype.md" class="xref" title="operator ptype">operator ptype</a> | This operator is a type converter. |
| <a href="stlElementHolder_DB_STL_value.md" class="xref" title="_DB_STL_value">_DB_STL_value</a> | Returns the data element this wrapper object wraps;. |
| <a href="stlElementHolder_DB_STL_StoreElement.md" class="xref" title="_DB_STL_StoreElement">_DB_STL_StoreElement</a> | Function to store the data element. |

## <a href="ElementRef.md" class="xref" title="ElementRef">ElementRef</a>

| Member | Description |
|----|----|
| <a href="ElementRef.md#stlElementRefdstr_ElementRef" class="xref" title="~ElementRef">~ElementRef</a> | Destructor. |
| <a href="stlElementRefElementRef.md" class="xref" title="ElementRef">ElementRef</a> | Constructor. |
| <a href="stlElementRefoperator_assign.md" class="xref" title="operator=">operator=</a> | Assignment Operator. |
| <a href="stlElementRef_DB_STL_StoreElement.md" class="xref" title="_DB_STL_StoreElement">_DB_STL_StoreElement</a> | Function to store the data element. |
| <a href="stlElementRef_DB_STL_value.md" class="xref" title="_DB_STL_value">_DB_STL_value</a> | Returns the data element this wrapper object wraps. |

## <a href="DbstlDbt.md" class="xref" title="DbstlDbt">DbstlDbt</a>

| Member | Description |
|----|----|
| <a href="DbstlDbt.md#stlDbstlDbtDbstlDbt" class="xref" title="DbstlDbt">DbstlDbt</a> | Construct an object with an existing chunk of memory of size1 bytes, refered by data1,. |
| <a href="stlDbstlDbtdstr_DbstlDbt.md" class="xref" title="~DbstlDbt">~DbstlDbt</a> | The memory will be free'ed by the destructor. |
| <a href="stlDbstlDbtoperator_assign.md" class="xref" title="operator=">operator=</a> | The memory will be reallocated if neccessary. |

## <a href="DbstlElemTraits.md" class="xref" title="DbstlElemTraits">DbstlElemTraits</a>

| Member | Description |
|----|----|
| <a href="DbstlElemTraits.md#stlDbstlElemTraitsassign" class="xref" title="assign">assign</a> | Assignone object to another. |
| <a href="stlDbstlElemTraitseq.md" class="xref" title="eq">eq</a> | Check for equality of two objects. |
| <a href="stlDbstlElemTraitslt.md" class="xref" title="lt">lt</a> | Less than comparison. |
| <a href="stlDbstlElemTraitscompare.md" class="xref" title="compare">compare</a> | Sequence comparison. |
| <a href="stlDbstlElemTraitslength.md" class="xref" title="length">length</a> | Returns the number of elements in sequence seq1. |
| <a href="stlDbstlElemTraitscopy.md" class="xref" title="copy">copy</a> | Copy first cnt number of elements from seq2 to seq1. |
| <a href="stlDbstlElemTraitsfind.md" class="xref" title="find">find</a> | Find within the first cnt elements of sequence seq the position of element equal to elem. |
| <a href="stlDbstlElemTraitsmove.md" class="xref" title="move">move</a> | Sequence movement. |
| <a href="stlDbstlElemTraitsto_char_type.md" class="xref" title="to_char_type">to_char_type</a> |  |
| <a href="stlDbstlElemTraitsto_int_type.md" class="xref" title="to_int_type">to_int_type</a> |  |
| <a href="stlDbstlElemTraitseq_int_type.md" class="xref" title="eq_int_type">eq_int_type</a> |  |
| <a href="stlDbstlElemTraitseof.md" class="xref" title="eof">eof</a> |  |
| <a href="stlDbstlElemTraitsnot_eof.md" class="xref" title="not_eof">not_eof</a> |  |
| <a href="stlDbstlElemTraitsset_restore_function.md" class="xref" title="set_restore_function">set_restore_function</a> |  |
| <a href="stlDbstlElemTraitsget_restore_function.md" class="xref" title="get_restore_function">get_restore_function</a> |  |
| <a href="stlDbstlElemTraitsset_assign_function.md" class="xref" title="set_assign_function">set_assign_function</a> |  |
| <a href="stlDbstlElemTraitsget_assign_function.md" class="xref" title="get_assign_function">get_assign_function</a> |  |
| <a href="stlDbstlElemTraitsget_size_function.md" class="xref" title="get_size_function">get_size_function</a> |  |
| <a href="stlDbstlElemTraitsset_size_function.md" class="xref" title="set_size_function">set_size_function</a> |  |
| <a href="stlDbstlElemTraitsget_copy_function.md" class="xref" title="get_copy_function">get_copy_function</a> |  |
| <a href="stlDbstlElemTraitsset_copy_function.md" class="xref" title="set_copy_function">set_copy_function</a> |  |
| <a href="stlDbstlElemTraitsset_sequence_len_function.md" class="xref" title="set_sequence_len_function">set_sequence_len_function</a> |  |
| <a href="stlDbstlElemTraitsget_sequence_len_function.md" class="xref" title="get_sequence_len_function">get_sequence_len_function</a> |  |
| <a href="stlDbstlElemTraitsget_sequence_copy_function.md" class="xref" title="get_sequence_copy_function">get_sequence_copy_function</a> |  |
| <a href="stlDbstlElemTraitsset_sequence_copy_function.md" class="xref" title="set_sequence_copy_function">set_sequence_copy_function</a> |  |
| <a href="stlDbstlElemTraitsset_compare_function.md" class="xref" title="set_compare_function">set_compare_function</a> |  |
| <a href="stlDbstlElemTraitsget_compare_function.md" class="xref" title="get_compare_function">get_compare_function</a> |  |
| <a href="stlDbstlElemTraitsset_sequence_compare_function.md" class="xref" title="set_sequence_compare_function">set_sequence_compare_function</a> |  |
| <a href="stlDbstlElemTraitsget_sequence_compare_function.md" class="xref" title="get_sequence_compare_function">get_sequence_compare_function</a> |  |
| <a href="stlDbstlElemTraitsset_sequence_n_compare_function.md" class="xref" title="set_sequence_n_compare_function">set_sequence_n_compare_function</a> |  |
| <a href="stlDbstlElemTraitsget_sequence_n_compare_function.md" class="xref" title="get_sequence_n_compare_function">get_sequence_n_compare_function</a> |  |
| <a href="stlDbstlElemTraitsinstance.md" class="xref" title="instance">instance</a> | Factory method to create a singeleton instance of this class. |
| <a href="stlDbstlElemTraitsdstr_DbstlElemTraits.md" class="xref" title="~DbstlElemTraits">~DbstlElemTraits</a> |  |
| <a href="stlDbstlElemTraitsDbstlElemTraits.md" class="xref" title="DbstlElemTraits">DbstlElemTraits</a> |  |

## <a href="BulkRetrievalOption.md" class="xref" title="BulkRetrievalOption">BulkRetrievalOption</a>

| Member | Description |
|----|----|
| <a href="BulkRetrievalOption.md#stlBulkRetrievalOptionBulkRetrievalOption" class="xref" title="BulkRetrievalOption">BulkRetrievalOption</a> |  |
| <a href="stlBulkRetrievalOptionoperator_eq.md" class="xref" title="operator==">operator==</a> | Equality comparison. |
| <a href="stlBulkRetrievalOptionoperator_assign.md" class="xref" title="operator=">operator=</a> | Assignment operator. |
| <a href="stlBulkRetrievalOptionbulk_buf_size.md" class="xref" title="bulk_buf_size">bulk_buf_size</a> | Return the buffer size set to this object. |
| <a href="stlBulkRetrievalOptionbulk_retrieval.md" class="xref" title="bulk_retrieval">bulk_retrieval</a> | This function indicates that you need a bulk retrieval iterator, and it can be also used to optionally set the bulk read buffer size. |
| <a href="stlBulkRetrievalOptionno_bulk_retrieval.md" class="xref" title="no_bulk_retrieval">no_bulk_retrieval</a> | This function indicates that you do not need a bulk retrieval iterator. |

## <a href="ReadModifyWriteOption.md" class="xref" title="ReadModifyWriteOption">ReadModifyWriteOption</a>

| Member | Description |
|----|----|
| <a href="ReadModifyWriteOption.md#stlReadModifyWriteOptionoperator_assign" class="xref" title="operator=">operator=</a> | Assignment operator. |
| <a href="stlReadModifyWriteOptionoperator_eq.md" class="xref" title="operator==">operator==</a> | Equality comparison. |
| <a href="stlReadModifyWriteOptionread_modify_write.md" class="xref" title="read_modify_write">read_modify_write</a> | Call this function to tell the container's begin() function that you need a read-modify-write iterator. |
| <a href="stlReadModifyWriteOptionno_read_modify_write.md" class="xref" title="no_read_modify_write">no_read_modify_write</a> | Call this function to tell the container's begin() function that you do not need a read-modify-write iterator. |

## <a href="Exception_classes_group.md" class="xref" title="Dbstl Exception Classes">Dbstl Exception Classes</a>

| Member | Description |
|----|----|
| <a href="DbstlException.md" class="link" title="Chapter 30.  DbstlException">DbstlException</a> | DbstlException |
| <a href="NotEnoughMemoryException.md" class="link" title="Chapter 35.  NotEnoughMemoryException">NotEnoughMemoryException</a> | NotEnoughMemoryException |
| <a href="InvalidIteratorException.md" class="link" title="Chapter 37.  InvalidIteratorException">InvalidIteratorException</a> | InvalidIteratorException |
| <a href="InvalidCursorException.md" class="link" title="Chapter 33.  InvalidCursorException">InvalidCursorException</a> | InvalidCursorException |
| <a href="InvalidDbtException.md" class="link" title="Chapter 31.  InvalidDbtException">InvalidDbtException</a> | InvalidDbtException |
| <a href="FailedAssertionException.md" class="link" title="Chapter 32.  FailedAssertionException">FailedAssertionException</a> | FailedAssertionException |
| <a href="NoSuchKeyException.md" class="link" title="Chapter 34.  NoSuchKeyException">NoSuchKeyException</a> | NoSuchKeyException |
| <a href="InvalidArgumentException.md" class="link" title="Chapter 39.  InvalidArgumentException">InvalidArgumentException</a> | InvalidArgumentException |
| <a href="NotSupportedException.md" class="link" title="Chapter 36.  NotSupportedException">NotSupportedException</a> | NotSupportedException |
| <a href="InvalidFunctionCall.md" class="link" title="Chapter 38.  InvalidFunctionCall">InvalidFunctionCall</a> | InvalidFunctionCall |

## <a href="DbstlException.md" class="xref" title="DbstlException">DbstlException</a>

| Member | Description |
|----|----|
| <a href="DbstlException.md#stlDbstlExceptionDbstlException" class="xref" title="DbstlException">DbstlException</a> |  |
| <a href="stlDbstlExceptionoperator_assign.md" class="xref" title="operator=">operator=</a> |  |
| <a href="stlDbstlExceptiondstr_DbstlException.md" class="xref" title="~DbstlException">~DbstlException</a> |  |

## <a href="InvalidDbtException.md" class="xref" title="InvalidDbtException">InvalidDbtException</a>

| Member | Description |
|----|----|
| <a href="InvalidDbtException.md#stlInvalidDbtExceptionInvalidDbtException" class="xref" title="InvalidDbtException">InvalidDbtException</a> |  |

## <a href="FailedAssertionException.md" class="xref" title="FailedAssertionException">FailedAssertionException</a>

| Member | Description |
|----|----|
| <a href="FailedAssertionException.md#stlFailedAssertionExceptionwhat" class="xref" title="what">what</a> |  |
| <a href="stlFailedAssertionExceptionFailedAssertionException.md" class="xref" title="FailedAssertionException">FailedAssertionException</a> |  |
| <a href="stlFailedAssertionExceptiondstr_FailedAssertionException.md" class="xref" title="~FailedAssertionException">~FailedAssertionException</a> |  |

## <a href="InvalidCursorException.md" class="xref" title="InvalidCursorException">InvalidCursorException</a>

| Member | Description |
|----|----|
| <a href="InvalidCursorException.md#stlInvalidCursorExceptionInvalidCursorException" class="xref" title="InvalidCursorException">InvalidCursorException</a> |  |

## <a href="NoSuchKeyException.md" class="xref" title="NoSuchKeyException">NoSuchKeyException</a>

| Member | Description |
|----|----|
| <a href="NoSuchKeyException.md#stlNoSuchKeyExceptionNoSuchKeyException" class="xref" title="NoSuchKeyException">NoSuchKeyException</a> |  |

## <a href="NotEnoughMemoryException.md" class="xref" title="NotEnoughMemoryException">NotEnoughMemoryException</a>

| Member | Description |
|----|----|
| <a href="NotEnoughMemoryException.md#stlNotEnoughMemoryExceptionNotEnoughMemoryException" class="xref" title="NotEnoughMemoryException">NotEnoughMemoryException</a> |  |

## <a href="NotSupportedException.md" class="xref" title="NotSupportedException">NotSupportedException</a>

| Member | Description |
|----|----|
| <a href="NotSupportedException.md#stlNotSupportedExceptionNotSupportedException" class="xref" title="NotSupportedException">NotSupportedException</a> |  |

## <a href="InvalidIteratorException.md" class="xref" title="InvalidIteratorException">InvalidIteratorException</a>

| Member | Description |
|----|----|
| <a href="InvalidIteratorException.md#stlInvalidIteratorExceptionInvalidIteratorException" class="xref" title="InvalidIteratorException">InvalidIteratorException</a> |  |

## <a href="InvalidFunctionCall.md" class="xref" title="InvalidFunctionCall">InvalidFunctionCall</a>

| Member | Description |
|----|----|
| <a href="InvalidFunctionCall.md#stlInvalidFunctionCallInvalidFunctionCall" class="xref" title="InvalidFunctionCall">InvalidFunctionCall</a> |  |

## <a href="InvalidArgumentException.md" class="xref" title="InvalidArgumentException">InvalidArgumentException</a>

| Member | Description |
|----|----|
| <a href="InvalidArgumentException.md#stlInvalidArgumentExceptionInvalidArgumentException" class="xref" title="InvalidArgumentException">InvalidArgumentException</a> |  |

**Legal Notice**

This documentation is distributed under an open source license. You may review the terms of this license at: <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html" class="ulink" target="_top">http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html</a>

Oracle, Berkeley DB, and Sleepycat are trademarks or registered trademarks of Oracle. All rights to these marks are reserved. No third-party use is permitted without the express prior written consent of Oracle.

Other names may be trademarks of their respective owners.

To obtain a copy of this document's original source code, please submit a request to the Oracle Technology Network forum at: <a href="http://forums.oracle.com/forums/forum.jspa?forumID=271" class="ulink" target="_top">http://forums.oracle.com/forums/forum.jspa?forumID=271</a>

9/9/2013

------------------------------------------------------------------------
