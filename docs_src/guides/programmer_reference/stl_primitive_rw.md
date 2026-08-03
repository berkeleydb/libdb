---
title: "Working with primitive types"
api-name: "Working with primitive types"
source: docs/programmer_reference/stl_primitive_rw.html
---
## Working with primitive types

<span class="sect2"> [Storing strings](stl_primitive_rw.md#idp51467888) </span>

To store simple primitive types such as `int`, `long`, `double`, and so forth, an additional type parameter for the container class templates is needed. For example, to store an `int` in a `db_vector`, use this container class:

``` c
db_vector<int, ElementHolder<int> >;
```

To map integers to doubles, use this:

``` c
db_map<int, double, ElementHolder<double> >;
```

To store a `char*` string with `long` keys, use this:

``` c
db_map<long, char*, ElementHolder<char*> >;
```

Use this for `const char*` strings:

``` c
db_map<long, const char*, ElementHolder<const char*> >;
```

To map one const string to another, use this type:

``` c
db_map<const char*, const char*, ElementHolder<const char*> >;
```

The `StlAdvancedFeaturesExample::primitive()` method demonstrates more of these examples.

### Storing strings

For `char*` and `wchar_t*` strings, `_DB_STL_StoreElement()` must be called following partial or total modifications before iterator movement, `container::operator[]` or `iterator::operator*/->` calls. Without the `_DB_STL_StoreElement()` call, the modified change will be lost. If storing an new value like this:

``` c
*iterator = new_char_star_string;
```

the call to `_DB_STL_StoreElement()` is not needed.

Note that passing a NULL pointer to a container of `char*` type or passing a `std::string` with no contents at all will insert an empty string of zero length into the database.

The string returned from a container will not live beyond the next iterator movement call, `container::operator[]` or `iterator::operator*/->` call.

A **db_map::value_type::second_type** or **db_map::datatype_wrap** should be used to hold a reference to a `container::operator[]` return value. Then the reference should be used for repeated references to that value. The \*iterator is of type `ElementHolder<char *>`, which can be automatically converted to a `char *` pointer using its type conversion operator. Wherever an auto conversion is done by the compiler, the conversion operator of `ElementHolder<T>` is called. This avoids almost all explicit conversions, except for two use cases:

1.  The \*iterator is used as a "..." parameter like this:

    ``` c
    printf("this is the special case %s", *iterator);
    ```

    This compiles but causes errors. Instead, an explicit cast should be used:

    ``` c
    printf("this is the special case %s", (char *)*iterator);
    ```

2.  For some old compilers, such as gcc3.4.6, the \*iterator cannot be used with the ternary `?` operator, like this:

    ``` c
    expr ? *iterator : var
    ```

    Even when **var** is the same type as the iterator's `value_type`, the compiler fails to perform an auto conversion.

When using `std::string` or `std::wstring` as the data type for dbstl containers — that is, `db_vector<string>`, and `db_map<string, wstring>` — the string's content rather than the string object itself is stored in order to maintain persistence.

You can find example code demonstrating string storage in the `StlAdvancedFeaturesExample::char_star_string_storage()` and `StlAdvancedFeaturesExample::storing_std_strings()` methods.
