---
title: "Chapter 26.  DbstlElemTraits"
api-name: "Chapter 26.  DbstlElemTraits"
source: docs/api_reference/STL/DbstlElemTraits.html
---
## Chapter 26.  DbstlElemTraits

This class is used to register callbacks to manipulate an object of a complex type.

These callbacks are used by dbstl at runtime to manipulate the object.

A complex type is a type whose members are not located in a contiguous chunk of memory. For example, the following class A is a complex type because for any instance a of class A, a.b\_ points to another object of type B, and dbstl treats the object that a.b\_ points to as part of the data of the instance a. Hence, if the user needs to store a.b\_ into a dbstl container, the user needs to register an appropriate callback to de-reference and store the object referenced by a.b. Similarly, the user also needs to register callbacks to marshall an array as well as to count the number of elements in such an array.

class A { int m; B \*p\_; }; class B { int n; };

The user also needs to register callbacks for i). returning an object¡¯s size in bytes; ii). Marshalling and unmarshalling an object; iii). Copying a complex object and and assigning an object to another object of the same type; iv). Element comparison. v). Compare two sequences of any type of objects; Measuring the length of an object sequence and copy an object sequence.

Several elements located in a contiguous chunk of memory form a sequence. An element of a sequence may be a simple object located at a contigous memory chunk, or a complex object, i.e. some of its members may contain references (pointers) to another region of memory. It is not necessary to store a special object to denote the end of the sequence. The callback to traverse the constituent elements of the sequence needs to able to determine the end of the sequence.

Marshalling means packing the object's data members into a contiguous chunk of memory; unmarshalling is the opposite of marshalling. In other words, when you unmarshall an object, its data members are populated with values from a previously marshalled version of the object.

The callbacks need not be set to every type explicitly. . dbstl will check if a needed callback function of this type is provided. If one is available, dbstl will use the registered callback. If the appropriate callback is not provided, dbstl will use reasonable defaults to do the job.

For returning the size of an object, the default behavior is to use the sizeof() operator; For marshalling and unmarshalling, dbstl uses memcpy, so the default behavior is sufficient for simple types whose data reside in a contiguous chunk of memory; Dbstl uses uses \>, == and \< for comparison operations; For char\* and wchar_t \* strings, dbstl already provides the appropriate callbacks, so you do not need to register them. In general, if the default behavior is adequate, you don't need to register the corresponding callback.

If you have registered proper callbacks, the DbstlElemTraits\<T\> can also be used as the char_traits\<T\> class for std::basic_string\<T, char_traits\<T\> \>, and you can enable your class T to form a basic_string\<T, DbstlElemTraits\<T\>\>, and use basic_string's functionality and the algorithms to manipulate it.

#### Public Members

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

#### Group

<a href="dbstl_helper_classes.md" class="xref" title="Chapter 21.  Dbstl Helper Classes">Dbstl Helper Classes</a>

## assign

### Function Details

``` c
static void assign(T &left,
    const T &right)
 
```

Assignone object to another.

``` c
static T* assign(T *seq, size_t cnt,
    T elem)
 
```

Assign first cnt number of elements of sequence seq with the value of elem.

### Group: Interface compatible with std::string's char_traits.

Following are char_traits funcitons, which make this class char_traits compatiable, so that it can be used in std::basic_string template, and be manipulated by the c++ stl algorithms.

### Class

<a href="DbstlElemTraits.md" class="link" title="Chapter 26.  DbstlElemTraits">DbstlElemTraits</a>
