---
title: "MyDbEnv"
api-name: "MyDbEnv"
source: docs/gsg/JAVA/mydbenv-persist.html
---
## MyDbEnv

The applications that we are building for our example both must open and close environments and entity stores. One of our applications is writing to the entity store, so this application needs to open the store as read-write. It also wants to be able to create the store if it does not exist.

Our second application only reads from the store. In this case, the store should be opened as read-only.

We perform these activities by creating a single class that is responsible for opening and closing our store and environment. This class is shared by both our applications. To use it, callers need to only provide the path to the environment home directory, and to indicate whether the object is meant to be read-only. The class implementation is as follows:

``` c
package persist.gettingStarted;

import java.io.File;
import java.io.FileNotFoundException;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import com.sleepycat.persist.EntityStore;
import com.sleepycat.persist.StoreConfig;

public class MyDbEnv {

    private Environment myEnv;
    private EntityStore store;

    // Our constructor does nothing
    public MyDbEnv() {}

    // The setup() method opens the environment and store
    // for us.
    public void setup(File envHome, boolean readOnly)
        throws DatabaseException {

        EnvironmentConfig myEnvConfig = new EnvironmentConfig();
        StoreConfig storeConfig = new StoreConfig();

        myEnvConfig.setReadOnly(readOnly);
        storeConfig.setReadOnly(readOnly);

        // If the environment is opened for write, then we want to be 
        // able to create the environment and entity store if 
        // they do not exist.
        myEnvConfig.setAllowCreate(!readOnly);
        storeConfig.setAllowCreate(!readOnly);

        try {
            // Open the environment and entity store
            myEnv = new Environment(envHome, myEnvConfig);
            store = new EntityStore(myEnv, "EntityStore", storeConfig);
        } catch (FileNotFoundException fnfe) {
            System.err.println("setup(): " + fnfe.toString());
            System.exit(-1);
        }

    }

    // Return a handle to the entity store
    public EntityStore getEntityStore() {
        return store;
    }

    // Return a handle to the environment
    public Environment getEnv() {
        return myEnv;
    }

    // Close the store and environment.
    public void close() {
        if (store != null) {
            try {
                store.close();
            } catch(DatabaseException dbe) {
                System.err.println("Error closing store: " +
                                    dbe.toString());
               System.exit(-1);
            }
        }

        if (myEnv != null) {
            try {
                // Finally, close the environment.
                myEnv.close();
            } catch(DatabaseException dbe) {
                System.err.println("Error closing MyDbEnv: " +
                                    dbe.toString());
               System.exit(-1);
            }
        }
    }
} 
```
