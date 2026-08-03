---
title: "Chapter 23.  ElementHolder"
api-name: "Chapter 23.  ElementHolder"
source: docs/api_reference/STL/ElementHolder.html
---
## Chapter 23.  ElementHolder

A wrapper class for primitive types.

It has identical usage and public interface to the <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> class.

#### See Also

<a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> .

#### Public Members

| Member | Description |
|----|----|
| <a href="ElementHolder.md#stlElementHolderElementHolder" class="xref" title="ElementHolder">ElementHolder</a> | Constructor. |
| <a href="stlElementHolderdstr_ElementHolder.md" class="xref" title="~ElementHolder">~ElementHolder</a> | Destructor. |
| <a href="stlElementHolderoperator_ia.md" class="xref" title="operator+=">operator+=</a> |  |
| <a href="stlElementHolderoperator_sa.md" class="xref" title="operator-=">operator-=</a> |  |
| <a href="stlElementHolderoperator__ma.md" class="xref" title="operator *=">operator *=</a> |  |
| <a href="stlElementHolderoperator_da.md" class="xref" title="operator/=">operator/=</a> |  |
| <a href="stlElementHolderoperator_modasg.md" class="xref" title="operator%=">operator%=</a> |  |
| <a href="stlElementHolderoperator__aa.md" class="xref" title="operator &amp;=">operator &amp;=</a> |  |
| <a href="stlElementHolderoperator_oa.md" class="xref" title="operator|=">operator|=</a> |  |
| <a href="stlElementHolderoperator_xa.md" class="xref" title="operator^=">operator^=</a> |  |
| <a href="stlElementHolderoperator_gt_ge.md" class="xref" title="operator&gt;&gt;=">operator&gt;&gt;=</a> |  |
| <a href="stlElementHolderoperator_lt_le.md" class="xref" title="operator&lt;&lt;=">operator&lt;&lt;=</a> |  |
| <a href="stlElementHolderoperator_incr.md" class="xref" title="operator++">operator++</a> |  |
| <a href="stlElementHolderoperator_decr.md" class="xref" title="operator--">operator--</a> |  |
| <a href="stlElementHolderoperator_assign.md" class="xref" title="operator=">operator=</a> |  |
| <a href="stlElementHolderoperator_ptype.md" class="xref" title="operator ptype">operator ptype</a> | This operator is a type converter. |
| <a href="stlElementHolder_DB_STL_value.md" class="xref" title="_DB_STL_value">_DB_STL_value</a> | Returns the data element this wrapper object wraps;. |
| <a href="stlElementHolder_DB_STL_StoreElement.md" class="xref" title="_DB_STL_StoreElement">_DB_STL_StoreElement</a> | Function to store the data element. |

#### Group

<a href="Element_wrappers.md" class="xref" title="Chapter 22.  ElementRef and ElementHolder Wappers">ElementRef and ElementHolder Wappers</a>

## ElementHolder

### Function Details

``` c
ElementHolder(iterator_type *pitr=NULL)
 
```

Constructor.

If the pitr parameter is NULL or the default value is used, the object created is a simple wrapper and not connected to a container. If a valid iterator parameter is passed in, the wrapped element will be associated with the matching key/data pair in the underlying container.

#### Parameters

##### pitr

The iterator owning this object.

``` c
ElementHolder(const ptype &dt)
 
```

Constructor.

Initializes an <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> wrapper without an iterator. It can only be used to wrap a data element in memory, it can't access an unerlying database.

#### Parameters

##### dt

The base class object to initialize this object.

``` c
ElementHolder(const self &other)
 
```

Copy constructor.

The constructor takes a "deep" copy. The created object will be identical to, but independent from the original object.

#### Parameters

##### other

The object to clone from.

### Class

<a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a>
