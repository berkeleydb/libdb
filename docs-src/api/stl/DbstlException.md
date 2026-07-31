---
title: "Chapter 30.  DbstlException"
api-name: "Chapter 30.  DbstlException"
source: docs/api_reference/STL/DbstlException.html
---
## Chapter 30.  DbstlException

Base class of all dbstl exception classes.

It is derived from Berkeley DB C++ API DbException class to maintain consistency with all Berkeley DB exceptions.

#### Public Members

| Member | Description |
|----|----|
| <a href="DbstlException.md#stlDbstlExceptionDbstlException" class="xref" title="DbstlException">DbstlException</a> |  |
| <a href="stlDbstlExceptionoperator_assign.md" class="xref" title="operator=">operator=</a> |  |
| <a href="stlDbstlExceptiondstr_DbstlException.md" class="xref" title="~DbstlException">~DbstlException</a> |  |

#### Group

<a href="Exception_classes_group.md" class="xref" title="Chapter 29.  Dbstl Exception Classes">Dbstl Exception Classes</a>

## DbstlException

### Function Details

``` c
DbstlException(const char *msg)
 
```

``` c
DbstlException(const char *msg,
    int err)
 
```

``` c
DbstlException(const DbstlException &ex)
 
```

``` c
DbstlException(int err)
 
```

``` c
DbstlException(const char *prefix, const char *msg,
    int err)
 
```

### Class

<a href="DbstlException.md" class="link" title="Chapter 30.  DbstlException">DbstlException</a>
