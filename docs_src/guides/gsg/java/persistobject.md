---
title: "Persistent Objects"
api-name: "Persistent Objects"
source: docs/gsg/JAVA/persistobject.html
---
## Persistent Objects

When using the DPL, you store data in the underlying DB databases by making objects <span class="emphasis">*persistent*</span>. You do this using Java annotations that both identify the type of persistent object you are declaring, as well as the primary and secondary indices.

The following are the annotations you will use with your DPL persistent classes:

| Annotation | Description |
|----|----|
| @Entity | Declares an entity class; that is, a class with a primary index and optionally one or more indices. |
| @Persistent | Declares a persistent class; that is, a class used by an entity class. They do not have indices but instead are are stored or retrieved when an entity class makes direct use of them. |
| @PrimaryKey | Declares a specific data member in an entity class to be the primary key for that object. This annotation must be used one and only one time for every entity class. |
| @SecondaryKey | Declares a specific data member in an entity class to be a secondary key for that object. This annotation is optional, and can be used multiple times for an entity class. |

For example, the following is declared to be an entity class:

``` c
package persist.gettingStarted;

import com.sleepycat.persist.model.Entity;
import com.sleepycat.persist.model.PrimaryKey;

@Entity
public class ExampleEntity {

    // The primary key must be unique in the database.
    @PrimaryKey
    private String aPrimaryKey;

    @SecondaryKey(relate=MANY_TO_ONE)
    private String aSecondaryKey;

    ...

    // The remainder of the class' implementation is purposefully
    // omitted in the interest of brevity.

    ...
} 
```

We discuss primary and secondary keys in more detail in <a href="persist_index.md" class="xref" title="Chapter 4. Working with Indices">Working with Indices</a>.
