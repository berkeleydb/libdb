---
title: "Store and Retrieve data or objects of complex types"
api-name: "Store and Retrieve data or objects of complex types"
source: docs/programmer_reference/stl_complex_rw.html
---
## Store and Retrieve data or objects of complex types

<span class="sect2"> [Storing varying length objects](stl_complex_rw.md#idp51458752) </span>

<span class="sect2"> [Storing arbitrary sequences](stl_complex_rw.md#idp51477944) </span>

<span class="sect2"> [Notes](stl_complex_rw.md#idp51524696) </span>

### Storing varying length objects

A structure like this:

``` c
class SMSMsg
{
public:
    size_t mysize;
    time_t when;
    size_t szmsg;
    int to;
    char msg[1];
};  
```

with a varying length string in `msg` cannot simply be stored in a `db_vector<SMSMsg>` without some configuration on your part. This is because, by default, dbstl uses the **sizeof()** operator to get the size of an object and then `memcpy()` to copy the object. This process is not suitable for this use-case as it will fail to capture the variable length string contained in `msg`.

There are currently two ways to store these kind of objects:

1.  Register callback functions with dbstl that are used to measure an object's size, and then marshal/unmarshal the object.

2.  Use a `DbstlDbt` wrapper object.

#### Storing by marshaling objects

One way to store an object that contains variable-sized fields is to marshall all of the object's data into a single contiguous area in memory, and then store the contents of that buffer. This means that upon retrieval, the contents of the buffer must be unmarshalled. To do these things, you must register three callback functions:

- `typedef void (*ElemRstoreFunct)(T& dest, const void *srcdata);`

  This callback is used to unmarshal an object, updating **dest** using data found in **srcdata**. The data in **srcdata** contains the chunk of memory into which the object was originally marshalled. The default unmarshalling function simply performs a cast (for example, `dest = *((T*)srcdata)`), which assumes the **srcdata** simply points to the memory layout of the object.

- `typedef size_t (*ElemSizeFunct)(const T& elem);`

  This callback returns the size in bytes needed to store the **elem** object. By default this function simply uses **sizeof(elem)** to determine the size of **elem**.

- `typedef void (*ElemCopyFunct)(void *dest, const T&elem);`

  This callback is used to arrange all data contained by **elem** into the chunk of memory to which **dest** refers. The size of **dest** is set by the `ElemSizeFunct` function, discussed above. The default marshalling function simply uses `memcpy()` to copy **elem** to **dest**.

The `DbstlElemTraits<SMSMsg>::instance()->set_size_function()`, `set_copy_function()` and `set_restore_function()` methods are used to register these callback functions. If a callback is not registered, its default function is used.

By providing non-default implementations of the callbacks described here, you can store objects of varying length and/or objects which do not reside in a continuous memory chunk — for example, objects containing a pointer which refers another object, or a string, and so forth. As a result, containers/iterators can manage variable length objects in the same as they would manage objects that reside in continuous chunks of memory and are of identical size.

#### Using a `DbstlDbt` wrapper object

To use a `DbstlDbt` wrapper object to store objects of variable length, a `db_vector<DbstlDbt>` container is used to store complex objects in a `db_vector`. `DbstlDbt` derives from DB C++ API's `Dbt`class, but can manage its referenced memory properly and release it upon destruction. The memory referenced by `DbstlDbt` objects is required to be allocated using the `malloc()`/`realloc()` functions from the standard C library.

Note that the use of `DbstlDbt` wrapper class is not ideal. It exists only to allow raw bytes of no specific type to be stored in a container.

To store an `SMSMsg` object into a `db_vector<DbstlDbt>` container using a `DbstlDbt` object:

1.  Wrap the `SMSMSg` object into a `DbstlDbt` object, then marshal the SMSMsg object properly into the memory chunk referenced by `DbstlDbt::data`.
2.  Store the `DbstlDbt` object into a `db_vector<DbstlDbt>` container. The bytes in the memory chunk referenced by the `DbstlDbt` object's **data** member are stored in the `db_vector<DbstlDbt>` container.
3.  Reading from the container returns a `DbstlDbt` object whose **data** field points to the `SMSMsg` object located in a continuous chunk of memory. The application needs to perform its own unmarshalling.
4.  The memory referenced by `DbstlDbt::data` is freed automatically, and so the application should not attempt to free the memory.

`ElementHolder` should not be used to store objects of a class because it doesn't support access to object members using **(\*iter).member** or **iter-\>member** expressions. In this case, the default `ElementRef<ddt>` is used automatically.

`ElementRef` inherits from `ddt`, which allows **\*iter** to return the object stored in the container. (Technically it is an `ElementRef<ddt> object`, whose "base class" part is the object you stored). There are a few data members and member functions in `ElementRef`, which all start with `_DB_STL_`. To avoid potential name clashes, applications should not use names prefixing `_DB_STL_` in classes whose instances may be stored into dbstl containers.

Example code demonstrating this feature can be found in the `StlAdvancedFeaturesExample::arbitrary_object_storage` method.

### Storing arbitrary sequences

A sequence is a group of related objects, such as an array, a string, and so forth. You can store sequences of any structure using dbstl, so long as you implement and register the proper callback functions. By using these callbacks, each object in the sequence can be a complex object with data members that are all not stored in a continuous memory chunk.

Note that when using these callbacks, when you retrieve a stored sequence from the database, the entire sequence will reside in a single continuous block of memory with the same layout as that constructed by your sequence copy function.

For example, given a type RGB:

``` c
struct RGB{char r, g, b, bright;};  
```

and an array of RGB objects, the following steps describe how to store an array into one key/data pair of a `db_map` container.

1.  Use a `db_map<int, RGB *, ElementHolder<RGB *> >` container.

2.  Define two functions. The first returns the number of objects in a sequence, the second that copies objects from a sequence to a defined destination in memory:

    ``` c
    typedef size_t (*SequenceLenFunct)(const RGB*);  
    ```

    and

    ``` c
     typedef void (*SequenceCopyFunct)(RGB*dest, const RGB*src);  
    ```

3.  Call DbstlElemTraits\<RGB\>::set_sequence_len_function()/set_sequence_copy_function() to register them as callbacks.

#### The `SequenceLenFunct` function

``` c
typedef size_t (*SequenceLenFunct)(const RGB*);  
```

A `SequenceLenFunct` function returns the number of objects in a sequence. It is called when inserting into or reading from the database, so there must be enough information in the sequence itself to enable the `SequenceLenFunct` function to tell how many objects the sequence contains. The `char*` and `wchar_t*` strings use a `'\0'` special character to do this. For example, RGB(0, 0, 0, 0) could be used to denote the end of the sequence. Note that for your implementation of this callback, you are not required to use a trailing object with a special value like `'\0'` or `RGB(0, 0, 0, 0)` to denote the end of the sequence. You are free to use what mechanism you want in your `SequenceLenFunct` function implementation to figure out the length of the sequence.

#### The `SequenceCopyFunct` function

``` c
 typedef void (*SequenceCopyFunct)(RGB*dest, const RGB*src); 
```

`SequenceCopyFunct` copies objects from the sequence **src** into memory chunk **dest**. If the objects in the sequence do not reside in a continuous memory chunk, this function must marshal each object in the sequence into the **dest** memory chunk.

The sequence objects will reside in the continuous memory chunk referred to by **dest**, which has been sized by `SequenceLenFunct` and `ElemSizeFunct` if available (which is when objects in the sequence are of varying lengths). `ElemSizeFunct` function is not needed in this example because **RGB** is a simple fixed length type, the `sizeof()` operator is sufficient to return the size of the sequence.

### Notes

- The get and set functions of this class are not protected by any mutexes. When using multiple threads to access the function pointers, the callback functions must be registered to the singleton of this class before any retrieval of the callback function pointers. Isolation may also be required among multiple threads. The best way is to register all callback function pointers in a single thread before making use of the any containers.

- If objects in a sequence are not of identical sizes, or are not located in a consecutive chunk of memory, you also need to implement and register the `DbstlElemTraits<>::ElemSizeFunct` callback function to measure the size of each object. When this function is registered, it is also used when allocating memory space.

  There is example code demonstrating the use this feature in the `StlAdvancedFeaturesExample::arbitray_sequence_storage()` method.

- A consequence of this dbstl feature is that you can not store a pointer value directly because dbstl will think it is a sequence head pointer. Instead, you need to convert the pointer into a `long` and then store it into a `long` container. And please note that pointer values are probably meaningless if the stored value is to be used across different application run times.
