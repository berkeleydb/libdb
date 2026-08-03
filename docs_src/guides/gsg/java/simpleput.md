---
title: "Placing Objects in an Entity Store"
api-name: "Placing Objects in an Entity Store"
source: docs/gsg/JAVA/simpleput.html
---
## Placing Objects in an Entity Store

In order to place an object in a DPL entity store, you must:

1.  Open the environment and store.

2.  Instantiate the object.

3.  Put the object to the store using the `put()` method for the object's primary index.

The following example uses the `SimpleDA` class that we show in <a href="simpleda.md" class="xref" title="SimpleDA.class">SimpleDA.class</a> to put a `SimpleEntityClass` object (see <a href="persist_access.md#simpleentity" class="xref" title="A Simple Entity Class">A Simple Entity Class</a>) to the entity store.

To begin, we import the Java classes that our example needs. We also instantiate the private data members that we require.

``` c
package persist.gettingStarted;

import java.io.File;
import java.io.FileNotFoundException;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import com.sleepycat.persist.EntityStore;
import com.sleepycat.persist.StoreConfig; 

public class SimpleStorePut {

    private static File envHome = new File("./JEDB");

    private Environment envmnt;
    private EntityStore store;
    private SimpleDA sda; 
```

Next we create a method that simply opens our database environment and entity store for us.

``` c
   // The setup() method opens the environment and store
    // for us.
    public void setup()
        throws DatabaseException {

        EnvironmentConfig envConfig = new EnvironmentConfig();
        StoreConfig storeConfig = new StoreConfig();

        envConfig.setAllowCreate(true);
        storeConfig.setAllowCreate(true);

        try {
            // Open the environment and entity store
            envmnt = new Environment(envHome, envConfig);
            store = new EntityStore(envmnt, "EntityStore", storeConfig);
        } catch (FileNotFoundException fnfe) {
            System.err.println("setup(): " + fnfe.toString());
            System.exit(-1);
        }
    } 
```

We also need a method to close our environment and store.

``` c
    // Close our environment and store.
    public void shutdown()
        throws DatabaseException {

        store.close();
        envmnt.close();
    } 
```

Now we need to create a method to actually write objects to our store. This method creates a `SimpleDA` object (see <a href="simpleda.md" class="xref" title="SimpleDA.class">SimpleDA.class</a> that we will use to access our indexes. Then we instantiate a series of `SimpleEntityClass` (see <a href="persist_access.md#simpleentity" class="xref" title="A Simple Entity Class">A Simple Entity Class</a>) objects that we will place in our store. Finally, we use our primary index (obtained from the `SimpleDA` class instance) to actually place these objects in our store.

In <a href="simpleget.md" class="xref" title="Retrieving Objects from an Entity Store">Retrieving Objects from an Entity Store</a> we show a class that is used to retrieve these objects.

``` c
    // Populate the entity store
    private void run()
        throws DatabaseException {

        setup();

        // Open the data accessor. This is used to store
        // persistent objects.
        sda = new SimpleDA(store);

        // Instantiate and store some entity classes
        SimpleEntityClass sec1 = new SimpleEntityClass();
        SimpleEntityClass sec2 = new SimpleEntityClass();
        SimpleEntityClass sec3 = new SimpleEntityClass();
        SimpleEntityClass sec4 = new SimpleEntityClass();
        SimpleEntityClass sec5 = new SimpleEntityClass();

        sec1.setPKey("keyone");
        sec1.setSKey("skeyone");

        sec2.setPKey("keytwo");
        sec2.setSKey("skeyone");

        sec3.setPKey("keythree");
        sec3.setSKey("skeytwo");

        sec4.setPKey("keyfour");
        sec4.setSKey("skeythree");

        sec5.setPKey("keyfive");
        sec5.setSKey("skeyfour");

        sda.pIdx.put(sec1);
        sda.pIdx.put(sec2);
        sda.pIdx.put(sec3);
        sda.pIdx.put(sec4);
        sda.pIdx.put(sec5);

        shutdown();
    } 
```

Finally, to complete our class, we need a `main()` method, which simply calls our `run()` method.

``` c
    // main
    public static void main(String args[]) {
        SimpleStorePut ssp = new SimpleStorePut();
        try {
            ssp.run();
        } catch (DatabaseException dbe) {
            System.err.println("SimpleStorePut: " + dbe.toString());
            dbe.printStackTrace();
        } catch (Exception e) {
            System.out.println("Exception: " + e.toString());
            e.printStackTrace();
        }
        System.out.println("All done.");
    }

} 
```
