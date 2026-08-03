---
title: "Retrieving Objects from an Entity Store"
api-name: "Retrieving Objects from an Entity Store"
source: docs/gsg/JAVA/simpleget.html
---
## Retrieving Objects from an Entity Store

You retrieve objects placed in an entity store by using either the object's primary index, or the appropriate secondary index if it exists. The following application illustrates this by retrieving some of the objects that we placed in an entity store in the previous section.

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

public class SimpleStoreGet {

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

Now we retrieve a few objects. To do this, we instantiate a `SimpleDA` (see <a href="simpleda.md" class="xref" title="SimpleDA.class">SimpleDA.class</a>) class that we use to access our primary and secondary indexes. Then we retrieve objects based on a primary or secondary index value. And finally, we display the retrieved objects.

``` c
    // Retrieve some SimpleEntityClass objects from the store.
    private void run()
        throws DatabaseException {

        setup();

        // Open the data accessor. This is used to store
        // persistent objects.
        sda = new SimpleDA(store);

        // Instantiate and store some entity classes
        SimpleEntityClass sec1 = sda.pIdx.get("keyone");
        SimpleEntityClass sec2 = sda.pIdx.get("keytwo");

        SimpleEntityClass sec4 = sda.sIdx.get("skeythree");

        System.out.println("sec1: " + sec1.getPKey());
        System.out.println("sec2: " + sec2.getPKey());
        System.out.println("sec4: " + sec4.getPKey());

        shutdown();
    } 
```

Finally, to complete our class, we need a `main()` method, which simply calls our `run()` method.

``` c
    // main
    public static void main(String args[]) {
        SimpleStoreGet ssg = new SimpleStoreGet();
        try {
            ssg.run();
        } catch (DatabaseException dbe) {
            System.err.println("SimpleStoreGet: " + dbe.toString());
            dbe.printStackTrace();
        } catch (Exception e) {
            System.out.println("Exception: " + e.toString());
            e.printStackTrace();
        }
        System.out.println("All done.");
    }

} 
```
