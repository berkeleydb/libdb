---
title: "Chapter 25.  DbstlDbt"
api-name: "Chapter 25.  DbstlDbt"
source: docs/api_reference/STL/DbstlDbt.html
---
## Chapter 25.  DbstlDbt

You can persist all bytes in a chunk of contiguous memory by constructing an <a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> object A(use malloc to allocate the required number of bytes for A.data and copy the bytes to be stored into A.data, set other fields as necessary) and store A into a container, e.g.

db_vector\<DbstlDbt\>, this stores the bytes rather than the object A into the underlying database. The <a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> class can help you avoid memory leaks, so it is strongly recommended that you use <a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> rather than Dbt class.

<a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> derives from Dbt class, and it does an deep copy on copy construction and assignment --by calling malloc to allocate its own memory and then copying the bytes to it; Conversely the destructor will free the memory on destruction if the data pointer is non-NULL. The destructor assumes the memory is allocated via malloc, hence why you are required to call malloc to allocate memory in order to use <a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> .

<a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> simply inherits all methods from Dbt with no extra new methods except the constructors/destructor and assignment operator, so it is easy to use.

In practice you rarely need to use <a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> or Dbt because dbstl enables you to store any complex objects or primitive data. Only when you need to store raw bytes, e.g. a bitmap, do you need to use <a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> .

Hence, <a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> is the right class to use to store any object into Berkeley DB via dbstl without memory leaks.

Don't free the memory referenced by <a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> objects, it will be freed when the <a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> object is destructed.

Please refer to the two examples using <a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> in TestAssoc::test_arbitrary_object_storage and TestAssoc::test_char_star_string_storage member functions, which illustrate how to correctly use <a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a> in order to store raw bytes.

This class handles the task of allocating and de-allocating memory internally. Although it can be used to store data which cannot be handled by the <a href="DbstlElemTraits.md" class="link" title="Chapter 26.  DbstlElemTraits">DbstlElemTraits</a> class, in practice, it is usually more convenient to register callbacks in the <a href="DbstlElemTraits.md" class="link" title="Chapter 26.  DbstlElemTraits">DbstlElemTraits</a> class for the type you are storing/retrieving using dbstl.

#### Public Members

| Member | Description |
|----|----|
| <a href="DbstlDbt.md#stlDbstlDbtDbstlDbt" class="xref" title="DbstlDbt">DbstlDbt</a> | Construct an object with an existing chunk of memory of size1 bytes, refered by data1,. |
| <a href="stlDbstlDbtdstr_DbstlDbt.md" class="xref" title="~DbstlDbt">~DbstlDbt</a> | The memory will be free'ed by the destructor. |
| <a href="stlDbstlDbtoperator_assign.md" class="xref" title="operator=">operator=</a> | The memory will be reallocated if neccessary. |

#### Group

<a href="dbstl_helper_classes.md" class="xref" title="Chapter 21.  Dbstl Helper Classes">Dbstl Helper Classes</a>

## DbstlDbt

### Function Details

``` c
DbstlDbt(void *data1,
    u_int32_t size1)
 
```

Construct an object with an existing chunk of memory of size1 bytes, refered by data1,.

``` c
DbstlDbt()
 
```

``` c
DbstlDbt(const DbstlDbt &d)
 
```

This copy constructor does a deep copy.

### Class

<a href="DbstlDbt.md" class="link" title="Chapter 25.  DbstlDbt">DbstlDbt</a>
