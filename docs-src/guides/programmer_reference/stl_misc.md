---
title: "Dbstl miscellaneous notes"
api-name: "Dbstl miscellaneous notes"
source: docs/programmer_reference/stl_misc.html
---
## Dbstl miscellaneous notes

<span class="sect2"> [Special notes about trivial methods](stl_misc.md#idp51587208) </span>

<span class="sect2"> [Using correct container and iterator public types](stl_misc.md#idp51603304) </span>

### Special notes about trivial methods

There are some standard STL methods which are meaningless in dbstl, but they are kept in dbstl as no-ops so as to stay consistent with the standard. These are:

|                          |
|--------------------------|
| `db_vecter::reserve();`  |
| `db_vector::max_size();` |
| `db_vector::capacity();` |
| `db_map::reserve();`     |
| `db_map::max_size();`    |

`db_vector<>::max_size()` and `db_map<>::max_size()` both return 2^30. This does not mean that Berkeley DB can only hold that much data. This value is returned to conform to some compilers' overflow rules — if we set bigger numbers like 2^32 or 2^31, some compilers complain that the number has overflowed.

See the Berkeley DB documentation for information about limitations on how much data a database can store.

There are also some read-only functions. You set the configuration for these using the Berkeley DB API. You access them using the container's methods. Again, this is to keep consistent with C++ standard STL containers, such as:

|                         |
|-------------------------|
| `db_map::key_comp();`   |
| `db_map::value_comp();` |
| `db_map::hash_funct();` |
| `db_map::key_eq();`     |

### Using correct container and iterator public types

All public types defined by the C++ STL specification are present in dbstl. One thing to note is the **value_type**. dbstl defines the **value_type** for each iterator and container class to be the raw type without the `ElementRef`/`ElementHolder` wrapper, so this type of variable can not be used to store data in a database. There is a **value_type_wrap** type for each container and iterator type, with the raw type wrapped by the `ElementRef`/`ElementHolder`.

For example, when type `int_vector_t` is defined as

``` c
db_vector<int, ElementHolder<int> >
```

its **value_type** is `int`, its **value_type_wrap** is `ElementHolder<int>`, and its reference and pointer types are `ElementHolder<int>&` and `ElementHolder<int>*` respectively. If you need to store data, use **value_type_wrap** to make use of the wrapper to store data into database.

The reason we leave **value_type** as the raw type is that we want the existing algorithms in the STL library to work with dbstl because we have seen that without doing so, a few tests will fail.

You need to use the same type as the return type of the data element retrieval functions to hold a value in order to properly manipulate the data element. For example, when calling

``` c
db_vector<T>::operator[]
```

check that the return type for this function is

``` c
db_vector<T>::datatype_wrap
```

Then, hold the return value using an object of the same type:

``` c
db_vector<T>::datatype_wrap refelem = vctr[3];
```
