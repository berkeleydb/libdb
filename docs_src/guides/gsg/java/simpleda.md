---
title: "SimpleDA.class"
api-name: "SimpleDA.class"
source: docs/gsg/JAVA/simpleda.html
---
## SimpleDA.class

As mentioned above, we organize our primary and secondary indexes using a specialize data accessor class. The main reason for this class to exist is to provide convenient access to all the indexes in use for our entity class (see the previous section, <a href="persist_access.md#simpleentity" class="xref" title="A Simple Entity Class">A Simple Entity Class</a>, for that implementation).

For a description on retrieving primary and secondary indexes under the DPL, see <a href="persist_index.md" class="xref" title="Chapter 4. Working with Indices">Working with Indices</a>

``` c
package persist.gettingStarted;

import java.io.File;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.persist.EntityStore;
import com.sleepycat.persist.PrimaryIndex;
import com.sleepycat.persist.SecondaryIndex;

public class SimpleDA {
    // Open the indices
    public SimpleDA(EntityStore store)
        throws DatabaseException {

        // Primary key for SimpleEntityClass classes
        pIdx = store.getPrimaryIndex(
            String.class, SimpleEntityClass.class);

        // Secondary key for SimpleEntityClass classes
        // Last field in the getSecondaryIndex() method must be
        // the name of a class member; in this case, an 
        // SimpleEntityClass.class data member.
        sIdx = store.getSecondaryIndex(
            pIdx, String.class, "sKey");
    }

    // Index Accessors
    PrimaryIndex<String,SimpleEntityClass> pIdx;
    SecondaryIndex<String,String,SimpleEntityClass> sIdx;
} 
```
