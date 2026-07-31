---
title: "operator="
api-name: "operator="
source: docs/api_reference/STL/stldb_vectoroperator_assign.html
---
## operator=

### Function Details

``` c
const self& operator=(const self &x)
 
```

Container assignment operator.

This function supports auto-commit. This <a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a> is assumed to be valid for use, only copy content of x into this container.

#### Parameters

##### x

The right value container.

#### Return Value

The container x's reference.

### Class

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a>
