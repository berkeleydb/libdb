---
title: "Chapter 5. Saving and Retrieving Objects"
api-name: "Chapter 5. Saving and Retrieving Objects"
source: docs/gsg/JAVA/persist_access.html
---
## Chapter 5. Saving and Retrieving Objects

**Table of Contents**

<span class="sect1"> [A Simple Entity Class](persist_access.md#simpleentity) </span>

<span class="sect1"> [SimpleDA.class](simpleda.md) </span>

<span class="sect1"> [Placing Objects in an Entity Store](simpleput.md) </span>

<span class="sect1"> [Retrieving Objects from an Entity Store](simpleget.md) </span>

<span class="sect1"> [Retrieving Multiple Objects](getmultiple.md) </span>

<span class="sect2"> [Cursor Initialization](getmultiple.md#dpl_cursor_initialize) </span>

<span class="sect2"> [Working with Duplicate Keys](getmultiple.md#dpl_dups) </span>

<span class="sect2"> [Key Ranges](getmultiple.md#dpl_cursor_range) </span>

<span class="sect1"> [Join Cursors](dpl_entityjoin.md) </span>

<span class="sect1"> [Deleting Entity Objects](dpl_delete.md) </span>

<span class="sect1"> [Replacing Entity Objects](dpl_replace.md) </span>

To store an object in an `EntityStore` you must annotate the class appropriately and then store it using `PrimaryIndex.put()`.

To retrieve and object from an `EntityStore` you use the `get()` method from either the `PrimaryIndex` or `SecondaryIndex`, whichever is most appropriate for your application.

In both cases, it simplifies things greatly if you create a data accessor class to organize your indexes.

In the next few sections we:

1.  Create an entity class that is ready to be stored in an entity store. This class will have both a primary index (required) declared for it, as well as a secondary index (which is optional).

    See the next section for this implementation.

2.  Create a data accessor class which is used to organize our data.

    See <a href="simpleda.md" class="xref" title="SimpleDA.class">SimpleDA.class</a> for this implementation.

3.  Create a simple class that is used to put objects to our entity store.

    See <a href="simpleput.md" class="xref" title="Placing Objects in an Entity Store">Placing Objects in an Entity Store</a> for this implementation.

4.  Create another class that retrieves objects from our entity store.

    See <a href="simpleget.md" class="xref" title="Retrieving Objects from an Entity Store">Retrieving Objects from an Entity Store</a> for this implementation.

## A Simple Entity Class

For clarity's sake, this entity class is a simple a class as we can write. It contains only two data members, both of which are set and retrieved by simple setter and getter methods. Beyond that, by design this class does not do anything or particular interest.

Its implementation is as follows:

``` c
package persist.gettingStarted;

import com.sleepycat.persist.model.Entity;
import com.sleepycat.persist.model.PrimaryKey;
import static com.sleepycat.persist.model.Relationship.*;
import com.sleepycat.persist.model.SecondaryKey;

@Entity
public class SimpleEntityClass {

    // Primary key is pKey
    @PrimaryKey
    private String pKey;

    // Secondary key is the sKey
    @SecondaryKey(relate=MANY_TO_ONE)
    private String sKey;

    public void setPKey(String data) {
        pKey = data;
    }

    public void setSKey(String data) {
        sKey = data;
    }

    public String getPKey() {
        return pKey;
    }

    public String getSKey() {
        return sKey;
    }
} 
```
