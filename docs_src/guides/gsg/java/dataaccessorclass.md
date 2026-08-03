---
title: "DataAccessor.java"
api-name: "DataAccessor.java"
source: docs/gsg/JAVA/dataaccessorclass.html
---
## DataAccessor.java

Now that we have implemented our data classes, we can write a class that will provide convenient access to our primary and secondary indexes. Note that like our data classes, this class is shared by both our example programs.

If you compare this class against our `Vendor` and `Inventory` class implementations, you will see that the primary and secondary indices declared there are referenced by this class.

See <a href="dpl_example.md#vendorclass" class="xref" title="Vendor.java">Vendor.java</a> and <a href="inventoryclass.md" class="xref" title="Inventory.java">Inventory.java</a> for those implementations.

``` c
package persist.gettingStarted;

import java.io.File;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.persist.EntityStore;
import com.sleepycat.persist.PrimaryIndex; 
import com.sleepycat.persist.SecondaryIndex;
                            
public class DataAccessor {
    // Open the indices
    public DataAccessor(EntityStore store)
        throws DatabaseException {

        // Primary key for Inventory classes
        inventoryBySku = store.getPrimaryIndex(
            String.class, Inventory.class);

        // Secondary key for Inventory classes
        // Last field in the getSecondaryIndex() method must be
        // the name of a class member; in this case, an Inventory.class
        // data member.
        inventoryByName = store.getSecondaryIndex(
            inventoryBySku, String.class, "itemName");

        // Primary key for Vendor class
        vendorByName = store.getPrimaryIndex(
            String.class, Vendor.class);
    }

    // Inventory Accessors
    PrimaryIndex<String,Inventory> inventoryBySku;
    SecondaryIndex<String,String,Inventory> inventoryByName;

    // Vendor Accessors
    PrimaryIndex<String,Vendor> vendorByName;
} 
```
