---
title: "Chapter 3. Direct Persistence Layer First Steps"
api-name: "Chapter 3. Direct Persistence Layer First Steps"
source: docs/gsg/JAVA/persist_first.html
---
## Chapter 3. Direct Persistence Layer First Steps

**Table of Contents**

<span class="sect1"> [Entity Stores](persist_first.md#entitystore) </span>

<span class="sect2"> [Opening and Closing Environments and Stores](persist_first.md#persist-open) </span>

<span class="sect1"> [Persistent Objects](persistobject.md) </span>

<span class="sect1"> [Saving and Retrieving Data](saveret.md) </span>

This chapter guides you through the first few steps required to use the DPL with your application. These steps include:

1.  Opening your environment as was described in <a href="Env.md#EnvOpen" class="xref" title="Opening Database Environments">Opening Database Environments</a>.

2.  Opening your entity store.

3.  Identifying the classes that you want to store in DB as either a `persistent` class or an `entity`.

Once you have done these things, you can write your classes to the DB databases, read them back from the databases, delete them from the databases, and so forth. These activities are described in the chapters that follow in this part of this manual.

## Entity Stores

<span class="sect2"> [Opening and Closing Environments and Stores](persist_first.md#persist-open) </span>

Entity stores are the basic unit of storage that you use with the DPL. That is, it is a unit of encapsulation for the classes that you want to store in DB. Under the hood it actually interacts with DB databases, but the DPL provides a layer of abstraction from the underlying DB APIs. The store, therefore, provides a simplified mechanism by which you read and write your stored classes. By using a store, you have access to your classes that is more simplified than if you were interacting with databases directly, but this simplified access comes at the cost of reduced flexibility.

Entity stores have configurations in the same way that environments have configurations. You can use a `StoreConfig` object to identify store properties. Among these are methods that allow you to declare whether:

- the store can be created if it does not exist at the time it is opened. Use the `StoreConfig.setAllowCreate()` method to set this.

- the store is read-only. Use the `StoreConfig.setReadOnly()` method to set this.

- the store supports transactions. Use the `StoreConfig.setTransactional()` method to set this.

  Writing DB transactional applications is described in the *Berkeley DB, Java Edition Getting Started with Transaction Processing* guide.

`EntityStore` objects also provide methods for retrieving information about the store, such as:

- the store's name. Use the `EntityStore.getStoreName()` method to retrieve this.

- a handle to the environment in which the store is opened. Use the `EntityStore.getEnvironment` method to retrieve this handle.

You can also use the `EntityStore` to retrieve all the primary and secondary indexes related to a given type of entity object contained in the store. See <a href="persist_index.md" class="xref" title="Chapter 4. Working with Indices">Working with Indices</a> for more information.

### Opening and Closing Environments and Stores

As described in <a href="Env.md" class="xref" title="Chapter 2. Database Environments">Database Environments</a>, an <span class="emphasis">*environment*</span> is a unit of encapsulation for DB databases. It also provides a handle by which activities common across the databases can be managed.

To use an entity store, you must first open an environment and then provide that environment handle to the `EntityStore` constructor.

For example, the following code fragment configures both the environment and the entity store such that they can be created if they do not exist. Both the environment and the entity store are then opened.

``` c
package persist.gettingStarted;

import java.io.File;
import java.io.FileNotFoundException;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import com.sleepycat.persist.EntityStore;
import com.sleepycat.persist.StoreConfig;

...

private Environment myEnv;
private EntityStore store;

try {
    EnvironmentConfig myEnvConfig = new EnvironmentConfig();
    StoreConfig storeConfig = new StoreConfig();

    myEnvConfig.setAllowCreate(!readOnly);
    storeConfig.setAllowCreate(!readOnly);

    try {
        // Open the environment and entity store
        myEnv = new Environment(envHome, myEnvConfig);
        store = new EntityStore(myEnv, "EntityStore", storeConfig);
    } catch (FileNotFoundException fnfe) {
        System.err.println(fnfe.toString());
        System.exit(-1);
    }
} catch(DatabaseException dbe) {
    System.err.println("Error opening environment and store: " +
                        dbe.toString());
    System.exit(-1);
} 
```

As always, before you exit your program you should close both your store and your environment. It is recommended that you close your store before you close your environment.

``` c
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
        // Finally, close environment.
        myEnv.close();
    } catch(DatabaseException dbe) {
        System.err.println("Error closing MyDbEnv: " +
                            dbe.toString());
        System.exit(-1);
    }
} 
```
