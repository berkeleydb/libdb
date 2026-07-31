---
title: "Chapter 22.  ElementRef and ElementHolder Wappers"
api-name: "Chapter 22.  ElementRef and ElementHolder Wappers"
source: docs/api_reference/STL/Element_wrappers.html
---
## Chapter 22.  ElementRef and ElementHolder Wappers

An <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> and <a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a> object represents the reference to the data element referenced by an iterator.

Each iterator object has an <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> or <a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a> object that stores the data element that the iterator points to.

The <a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a> class is used to store primitive types into STL containers.

The <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> class is used to store other types into STL containers.

The <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> and <a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a> classes have identical interfaces, and are treated the same by other STL classes. Since the <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> class inherits from the template data class, all methods have a \_DB_STL\_ prefix to avoid name clashes.

An <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> or <a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a> class corresponds to a single iterator instance. An Element object is generally owned by an iterator object. The ownership relationship is swapped in some specific situations, specifically for the dereference and array index operator.

#### Public Members

| Member | Description |
|----|----|
| <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> | ElementRef |
| <a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a> | ElementHolder |

#### Group

<a href="dbstl_helper_classes.md" class="xref" title="Chapter 21.  Dbstl Helper Classes">Dbstl Helper Classes</a>
