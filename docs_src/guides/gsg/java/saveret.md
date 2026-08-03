---
title: "Saving and Retrieving Data"
api-name: "Saving and Retrieving Data"
source: docs/gsg/JAVA/saveret.html
---
## Saving and Retrieving Data

All data stored using the DPL has one primary index and zero or more secondary indices associated with it. (Sometimes these are referred to as the primary and secondary <span class="emphasis">*keys*</span>.) So to store data under the DPL, you must:

1.  Declare a class to be an entity class.

2.  Identify the features on the class which represent indexed material.

3.  Retrieve the store's primary index for a given class using the `EntityStore.getPrimaryIndex()` method.

4.  Put class objects to the store using the `PrimaryIndex.put()` method.

In order to retrieve an object from the store, you use the index that is most convenient for your purpose. This may be the primary index, or it may be some other secondary index that you declared on your entity class.

You obtain a primary index in the same was as when you put the object to the store: using `EntityStore.getPrimaryIndex()`. You can get a secondary index for the store using the `EntityStore.getSecondaryIndex()` method. Note that `getSecondaryIndex()` requires you to provide a `PrimaryIndex` class instance when you call it, so a class's primary index is always required when retrieving objects from an entity store.

Usually all of the activity surrounding saving and retrieving data is organized within a class or classes specialized to that purpose. We describe the construction of these data accessor classes in <a href="simpleda.md" class="xref" title="SimpleDA.class">SimpleDA.class</a>. But before you perform any entity store activity, you need to understand indexes. We therefore describe them in the next chapter.
