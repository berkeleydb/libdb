---
title: "Chapter 28.  ReadModifyWriteOption"
api-name: "Chapter 28.  ReadModifyWriteOption"
source: docs/api_reference/STL/ReadModifyWriteOption.html
---
## Chapter 28.  ReadModifyWriteOption

Read-modify-write cursor configuration helper class.

Used by each begin() function of all containers.

#### Public Members

| Member | Description |
|----|----|
| <a href="ReadModifyWriteOption.md#stlReadModifyWriteOptionoperator_assign" class="xref" title="operator=">operator=</a> | Assignment operator. |
| <a href="stlReadModifyWriteOptionoperator_eq.md" class="xref" title="operator==">operator==</a> | Equality comparison. |
| <a href="stlReadModifyWriteOptionread_modify_write.md" class="xref" title="read_modify_write">read_modify_write</a> | Call this function to tell the container's begin() function that you need a read-modify-write iterator. |
| <a href="stlReadModifyWriteOptionno_read_modify_write.md" class="xref" title="no_read_modify_write">no_read_modify_write</a> | Call this function to tell the container's begin() function that you do not need a read-modify-write iterator. |

#### Group

<a href="dbstl_helper_classes.md" class="xref" title="Chapter 21.  Dbstl Helper Classes">Dbstl Helper Classes</a>

## operator=

### Function Details

``` c
void operator=(ReadModifyWriteOption::Option rmw1)
 
```

Assignment operator.

### Class

<a href="ReadModifyWriteOption.md" class="link" title="Chapter 28.  ReadModifyWriteOption">ReadModifyWriteOption</a>
