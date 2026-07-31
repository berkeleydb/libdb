---
title: "ElementRef"
api-name: "ElementRef"
source: docs/api_reference/STL/stlElementRefElementRef.html
---
## ElementRef

### Function Details

``` c
ElementRef(iterator_type *pitr=NULL)
 
```

Constructor.

If the pitr parameter is NULL or the default value is used, the object created is a simple wrapper and not connected to a container. If a valid iterator parameter is passed in, the wrapped element will be associated with the matching key/data pair in the underlying container.

#### Parameters

##### pitr

The iterator owning this object.

``` c
ElementRef(const ddt &dt)
 
```

Constructor.

Initializes an <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> wrapper without an iterator. It can only be used to wrap a data element in memory, it can't access an unerlying database.

#### Parameters

##### dt

The base class object to initialize this object.

``` c
ElementRef(const self &other)
 
```

Copy constructor.

The constructor takes a "deep" copy. The created object will be identical to, but independent from the original object.

#### Parameters

##### other

The object to clone from.

### Class

<a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a>
