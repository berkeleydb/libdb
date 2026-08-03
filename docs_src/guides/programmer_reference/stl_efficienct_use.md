---
title: "Using dbstl efficiently"
api-name: "Using dbstl efficiently"
source: docs/programmer_reference/stl_efficienct_use.html
---
## Using dbstl efficiently

<span class="sect2"> [Using iterators efficiently](stl_efficienct_use.md#idp51530568) </span>

<span class="sect2"> [Using containers efficiently](stl_efficienct_use.md#idp51530352) </span>

### Using iterators efficiently

To make the most efficient possible use of iterators:

- Close an iterator's cursor as soon as possible.

  Each iterator has an open cursor associated with it, so when you are finished using the iterator it is a good habit to explicitly close its cursor. This can potentially improve performance by avoiding locking issues, which will enhanced concurrency. Dbstl will close the cursor when the iterator is destroyed, but you can close the cursor before that time. If the cursor is closed, the associated iterator cannot any longer be used.

  In some functions of container classes, an iterator is used to access the database, and its cursor is internally created by dbstl. So if you want to specify a non-zero flag for the `Db::cursor()` call, you need to call the container's `set_cursor_open_flag()` function to do so.

- Use const iterators where applicable.

  If your data access is read only, you are strongly recommended to use a const iterator. In order to create a const iterator, you must use a const reference to the container object. For example, supposed we have:

  ``` c
  db_vector<int> intv(10);
  ```

  then we must use a:

  ``` c
  const db_vector<int>& intv_ref = intv;
  ```

  reference to invoke the const begin/end functions. `intv_ref.begin()` will give you a const iterator. You can use a const iterator only to read its referenced data elements, not update them. However, you should have better performance with this iterator using, for example, either `iterator::operator*` or `iterator::operator->member`. Also, using array indices like `intv_ref[i]` will also perform better.

  All functions in dbstl's containers which return an iterator or data element reference have two versions — one returns a const iterator/reference, the other returns an iterator/reference. If your access is read only, choose the version returning const iterators/references.

  Remember that you can only use a const reference to a container object to call the const versions of `operator*` and `operator[]`.

  You can also use the non-const container object or its non-const reference to create a read only iterator by passing `true` to the **readonly** parameter in the container's `begin()` method.

- Use pre-increment/pre-decrement rather than post-increment/post-decrement where possible

  Pre-increment operations are more efficient because the `++iterator` avoids two iterator copy constructions. This is true when you are using C++ standard STL iterators as well.

- Use bulk retrieval in iterators

  If your access pattern is to go through the entire database read only, or if you are reading a continuous range of the database, bulk retrieval can be very useful because it returns multiple key/data pairs in one database call. But be aware that you can only read the returned data, you can not update it. Also, if you do a bulk retrieval and read the data, and simultaneously some other thread of control updates that same data, then unless you are using a serializable transaction, you will now be working with old data.

### Using containers efficiently

To make the most efficient possible use of containers:

- Avoid using container methods that return references. These because they are a little more expensive.

  To implement reference semantics, dbstl has to wrap the data element with the current key/data pair, and must invoke two iterator copy constructions and two Berkeley DB cursor duplications for each such a call. This is true of non-const versions of these functions:

  |                              |
  |------------------------------|
  | `db_vector<T>::operator[]()` |
  | `db_vector<T>::front()`      |
  | `db_vector<T>::back()`       |
  | `db_vector<T>::at()`         |
  | `db_map<>::operator[]()`     |

  There are alternatives to these functions, mainly through explicit use of iterators.

- Use const containers where possible.

  The const versions of the functions listed above have less overhead than their non-const counterparts. Using const containers and iterators can bring more performance when you call the const version of the overloaded container/iterator methods. To do so, you define a const container reference to an existing container, and then use this reference to call the methods. For example, if you have:

  ``` c
  db_vector<int> container int_vec
  ```

  then you can define a const reference to `int_vec`:

  ``` c
  const db_vector<int>& int_vec_ref; 
  ```

  Then you use `int_vec_ref.begin()` to create a const iterator, `citr`. You can now can use `int_vec_ref` to call the const versions of the container's member functions, and then use `citr` to access the data read only. By using `int_vec_ref` and `citr`, we can gain better performance.

  It is acceptable to call the non-const versions of container functions that return non-const iterators, and then assign these return values to const iterator objects. But if you are using Berkeley DB concurrent data store (CDS), be sure to set the **readonly** parameter for each container method that returns an iterator to `true`. This is because each iterator corresponds to a Berkeley DB cursor, and so for best performance you should specify that the returned iterator be read-only so that the underlying cursor is also read-only. Otherwise, the cursor will be a writable cursor, and performance might be somewhat degraded. If you are not using CDS, but instead TDS or DS or HA, there is no distinction between read-only cursors and read-write cursors. Consequently, you do not need to specify the **readonly** parameter at all.
