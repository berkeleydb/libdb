---
title: "Deleting Entity Objects"
api-name: "Deleting Entity Objects"
source: docs/gsg/JAVA/dpl_delete.html
---
## Deleting Entity Objects

The simplest way to remove an object from your entity store is to delete it by its primary index. For example, using the `SimpleDA` class that we created earlier in this document (see <a href="simpleda.md" class="xref" title="SimpleDA.class">SimpleDA.class</a>), you can delete the `SimpleEntityClass` object with a primary key of `keyone` as follows:

``` c
sda.pIdx.delete("keyone");
```

You can also delete objects by their secondary keys. When you do this, all objects related to the secondary key are deleted, unless the key is a foreign object.

For example, the following deletes all `SimpleEntityClass` with a secondary key of `skeyone`:

``` c
sda.sIdx.delete("skeyone");
```

You can delete any single object by positioning a cursor to that object and then calling the cursor's `delete()` method.

``` c
PrimaryIndex<String,SimpleEntityClass> pi =
    store.getPrimaryIndex(String.class, SimpleEntityClass.class);

SecondaryIndex<String,String,SimpleEntityClass> si = 
    store.getSecondaryIndex(pi, String.class, "sKey");

EntityCursor<SimpleEntityClass> sec_cursor = 
    si.subIndex("skeyone").entities(); 

try {
    SimpleEntityClass sec;
    Iterator<SimpleEntityClass> i = sec_cursor.iterator();
    while (sec = i.nextDup() != null) {
        if (sec.getSKey() == "some value") {
            i.delete();
        }
    }
// Always make sure the cursor is closed when we are done with it.
} finally {
    sec_cursor.close(); } 
```

Finally, if you are indexing by foreign key, then the results of deleting the key is determined by the foreign key constraint that you have set for the index. See <a href="dplindexcreate.md#foreignkey" class="xref" title="Foreign Key Constraints">Foreign Key Constraints</a> for more information.
