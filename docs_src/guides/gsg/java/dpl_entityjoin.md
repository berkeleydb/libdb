---
title: "Join Cursors"
api-name: "Join Cursors"
source: docs/gsg/JAVA/dpl_entityjoin.html
---
## Join Cursors

If you have two or more secondary indexes set for an entity object, then you can retrieve sets of objects based on the intersection of multiple secondary index values. You do this using an `EntityJoin` class.

For example, suppose you had an entity class that represented automobiles. In that case, you might be storing information about automobiles such as color, number of doors, fuel mileage, automobile type, number of passengers, make, model, and year, to name just a few.

If you created a secondary index based this information, then you could use an `EntityJoin` to return all those objects representing cars with, say, two doors, that were built in 2002, and which are green in color.

To create a join cursor, you:

1.  Open the primary index for the entity class on which you want to perform the join.

2.  Open the secondary indexes that you want to use for the join.

3.  Instantiate an `EntityJoin` object (you use the primary index to do this).

4.  Use two or more calls to `EntityJoin.addCondition()` to identify the secondary indexes and their values that you want to use for the equality match.

5.  Call `EntityJoin.entities()` to obtain a cursor that you can use to iterate over the join results.

For example, suppose we had an entity class that included the following features:

``` c
package persist.gettingStarted;

import com.sleepycat.persist.model.Entity;
import com.sleepycat.persist.model.PrimaryKey;
import static com.sleepycat.persist.model.Relationship.*;
import com.sleepycat.persist.model.SecondaryKey;

@Entity
public class Automobiles {

    // Primary key is the vehicle identification number
    @PrimaryKey
    private String vin;

    // Secondary key is the vehicle's make
    @SecondaryKey(relate=MANY_TO_ONE)
    private String make;

    // Secondary key is the vehicle's color
    @SecondaryKey(relate=MANY_TO_ONE)
    private String color;

    ...

    public String getVIN() {
        return vin;
    }

    public String getMake() {
        return make;
    }

    public String getColor() {
        return color;
    }
    
    ... 
```

Then we could perform an entity join that searches for all the red automobiles made by Toyota as follows:

``` c
PrimaryIndex<String,Automobiles> vin_pidx;
SecondaryIndex<String,String,Automobiles> make_sidx;
SecondaryIndex<String,String,Automobiles> color_sidx;

EntityJoin<String,Automobiles> join = new EntityJoin(vin_pidx);
join.addCondition(make_sidx,"Toyota");
join.addCondition(color_sidx,"Red");

// Now iterate over the results of the join operation
ForwardCursor<Automobiles> join_cursor = join.entities();
try {
    for (Automobiles autoi : join_cursor) {
        // do something with each object "autoi"
    }
// Always make sure the cursor is closed when we are done with it.
} finally {
    join_cursor.close();
} 
```
