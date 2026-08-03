---
title: "Replacing Entity Objects"
api-name: "Replacing Entity Objects"
source: docs/gsg/JAVA/dpl_replace.html
---
## Replacing Entity Objects

To modify a stored entity object, retrieve it, update it, then put it back to the entity store:

``` c
SimpleEntityClass sec = sda.pIdx.get("keyone");
sec.setSKey("skeyoneupdated");
sda.pIdx.put(sec);
```

Note that because we updated a field on the object that is a secondary key, this object will now be accessible by the secondary key of `skeyoneupdated` instead of the previous value, which was `skeyone`

Be aware that if you modify the object's primary key, the behavior is somewhat different. In this case, you cause a new instance of the object to be created in the store, instead of replacing an existing instance:

``` c
// Results in two objects in the store.  One with a
// primary index of "keyfive" and the other with primary index of 
//'keyfivenew'.
SimpleEntityClass sec = sda.pIdx.get("keyfive");
sec.setPKey("keyfivenew");
sda.pIdx.put(sec); 
```

Finally, if you are iterating over a collection of objects using an `EntityCursor`, you can update each object in turn using `EntityCursor.update()`. Note, however, that you must be iterating using a `PrimaryIndex`; this operation is not allowed if you are using a `SecondaryIndex`.

For example, the following iterates over every `SimpleEntityClass` object in the entity store, and it changes them all so that they have a secondary index of `updatedskey`:

``` c
EntityCursor<SimpleEntityClass> sec_pcursor = sda.pIdx.entities();
for (SimpleEntityClass sec : sec_pcursor) {
    sec.setSKey("updatedskey");
    sec_pcursor.update(item);
}
sec_pcursor.close(); 
```
