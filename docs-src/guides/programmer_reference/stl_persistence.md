---
title: "Dbstl persistence"
api-name: "Dbstl persistence"
source: docs/programmer_reference/stl_persistence.html
---
## Dbstl persistence

<span class="sect2"> [Direct database get](stl_persistence.md#directdbget) </span>

<span class="sect2"> [Change persistence](stl_persistence.md#chg_persistence) </span>

<span class="sect2"> [Object life time and persistence](stl_persistence.md#obj_life_persistence) </span>

The following sections provide information on how to achieve persistence using dbstl.

### Direct database get

Each container has a **begin()** method which produces an iterator. These **begin** methods take a boolean parameter, **directdb_get**, which controls the caching behavior of the iterator. The default value of this parameter is `true`.

If **directdb_get** is `true`, then the persistent object is fetched anew from the database each time the iterator is dereferenced as a pointer by use of the star-operator (**\*iterator**) or by use of the arrow-operator (**iterator-\>member**). If **directdb_get** is `false`, then the first dereferencing of the iterator fetches the object from the database, but later dereferences can return cached data.

With **directdb_get** set to `true`, if you call:

``` c
(*iterator).datamember1=new-value1; 
(*iterator).datamember2=new-value2;  
```

then the assignment to `datamember1` will be lost, because the second dereferencing of the iterator would cause the cached copy of the object to be overwritten by the object's persistent data from the database.

You also can use the arrow operator like this:

``` c
iterator->datamember1=new-value1; 
iterator->datamember2=new-value2;  
```

This works exactly the same way as **iterator::operator\***. For this reason, the same caching rules apply to arrow operators as they do for star operators.

One way to avoid this problem is to create a reference to the object, and use it to access the object:

``` c
container::value_type &ref = *iterator;
ref.datamember1=new-value1;
ref.datamember2=new-value2;
...// more member function calls and datamember assignments
ref._DB_STL_StoreElement();  
```

The above code will not lose the newly assigned value of `ref.datamember1` in the way that the previous example did.

In order to avoid these complications, you can assign to the object referenced by an iterator with another object of the same type like this:

``` c
container::value_type obj2;
obj2.datamember1 = new-value1;
obj2.datamember2 = new-value2;
*itr = obj2; 
```

This code snippet causes the new values in `obj2` to be stored into the underlying database.

If you have two iterators going through the same container like this:

``` c
for (iterator1 = v.begin(), iterator2 = v.begin();
     iterator1 != v.end();
     ++iterator1, ++iterator2) {
        *iterator1 = new_value;
        print(*iterator2);
}  
```

then the printed value will depend on the value of **directdb_get** with which the iterator had been created. If **directdb_get** is `false`, then the original, persistent value is printed; otherwise the newly assigned value is returned from the cache when `iterator2` is dereferenced. This happens because each iterator has its own cached copy of the persistent object, and the dereferencing of `iterator2` refreshes `iterator2`'s copy from the database, retrieving the value stored by the assignment to `*iterator1`.

Alternatively, you can set **directdb_get** to `false` and call `iterator2->refresh()` immediately before the dereferencing of `iterator2`, so that `iterator2`'s cached value is refreshed.

If **directdb_get** is `false`, a few of the tests in dbstl's test kit will fail. This is because the above contrived case appears in several of C++ STL tests. Consequently, the default value of the **directdb_get** parameter in the `container::begin()` methods is `true`. If your use cases avoid such bizarre usage of iterators, you can set it to `false`, which makes the iterator read operation faster.

### Change persistence

If you modify the object to which an iterator refers by using one of the following:

``` c
(*iterator).member_function_call()
```

or

``` c
(*iterator).data_member = new_value
```

then you should call `iterator->_DB_STL_StoreElement()` to store the change. Otherwise the change is lost after the iterator moves on to other elements.

If you are storing a sequence, and you modified some part of it, you should also call `iterator->_DB_STL_StoreElement()` before moving the iterator.

And in both cases, if **directdb_get** is `true` (this is the default value), you should call `_DB_STL_StoreElement()` after the change and before the next iterator movement OR the next dereferencing of the iterator by the star or arrow operators (`iterator::operator*` or `iterator::operator->`). Otherwise, you will lose the change.

If you update the element by assigning to a dereferenced iterator like this:

``` c
*iterator = new_element;
```

then you never have to call `_DB_STL_StoreElement()` because the change is stored in the database automatically.

### Object life time and persistence

Dbstl is an interface to Berkeley DB, so it is used to store data persistently. This is really a different purpose from that of regular C++ STL. This difference in their goals has implications on expected object lifetime: In standard STL, when you store an object A of type ID into C++ stl vector V using V.push_back(A), if a proper copy constructor is provided in A's class type, then the copy of A (call it B) and everything in B, such as another object C pointed to by B's data member B.c_ptr, will be stored in V and will live as long as B is still in V and V is alive. B will be destroyed when V is destroyed or B is erased from V.

This is not true for dbstl, which will copy A's data and store it in the underlying database. The copy is by default a shallow copy, but users can register their object marshalling and unmarshalling functions using the `DbstlElemTraits` class template. So if A is passed to a `db_vector` container, `dv`, by using `dv.push_back(A)`, then dbstl copies A's data using the registered functions, and stores data into the underlying database. Consequently, A will be valid, even if the container is destroyed, because it is stored into the database.

If the copy is simply a shallow copy, and A is later destroyed, then the pointer stored in the database will become invalid. The next time we use the retrieved object, we will be using an invalid pointer, which probably will result in errors. To avoid this, store the referred object C rather than the pointer member A.c_ptr itself, by registering the right marshalling/unmarshalling function with `DbstlElemTraits`.

For example, consider the following example class declaration:

``` c
class ID
{
public:
    string Name;
    int Score;
};  
```

Here, the class ID has a data member **Name**, which refers to a memory address of the actual characters in the string. If we simply shallow copy an object, `id`, of class ID to store it, then the stored data, `idd`, is invalid when `id` is destroyed. This is because `idd` and `id` refer to a common memory address which is the base address of the memory space storing all characters in the string, and this memory space is released when `id` is destroyed. So `idd` will be referring to an invalid address. The next time we retrieve `idd` and use it, there will probably be memory corruption.

The way to store `id` is to write a marshal/unmarshal function pair like this:

``` c
void copy_id(void *dest, const ID&elem)
{
    memcpy(dest, &elem.Score, sizeof(elem.Score));
    char *p = ((char *)dest) + sizeof(elem.Score);
    strcpy(p, elem.Name.c_str());
}

void restore_id(ID& dest, const void *srcdata)
{
    memcpy(&dest.Score, srcdata, sizeof(dest.Score));
    const char *p = ((char *)srcdata) + sizeof(dest.Score);
    dest.Name = p;
}

size_t size_id(const ID& elem)
{
    return sizeof(elem.Score) + elem.Name.size() + 
        1;// store the '\0' char.
}  
```

Then register the above functions before storing any instance of `ID`:

``` c
DbstlElemTraits<ID>::instance()->set_copy_function(copy_id);
DbstlElemTraits<ID>::instance()->set_size_function(size_id);
DbstlElemTraits<ID>::instance()->set_restore_function(restore_id);  
```

This way, the actual data of instances of ID are stored, and so the data will persist even if the container itself is destroyed.
