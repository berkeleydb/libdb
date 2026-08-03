---
title: "Chapter 2. Database Environments"
api-name: "Chapter 2. Database Environments"
source: docs/gsg/JAVA/Env.html
---
## Chapter 2. Database Environments

**Table of Contents**

<span class="sect1"> [Opening Database Environments](Env.md#EnvOpen) </span>

<span class="sect1"> [Closing Database Environments](EnvClose.md) </span>

<span class="sect1"> [Environment Properties](EnvProps.md) </span>

<span class="sect2"> [The EnvironmentConfig Class](EnvProps.md#envconfig) </span>

<span class="sect2"> [EnvironmentMutableConfig](EnvProps.md#envhandleconfig) </span>

Environments are optional, but very commonly used, for Berkeley DB applications built using the base API. If you are using the DPL, then environments are required.

Database environments encapsulate one or more databases. This encapsulation provides your threads with efficient access to your databases by allowing a single in-memory cache to be used for each of the databases contained in the environment. This encapsulation also allows you to group operations performed against multiple databases inside a single transactions (see the *Berkeley DB, Java Edition Getting Started with Transaction Processing* guide for more information).

Most commonly you use database environments to create and open databases (you close individual databases using the individual database handles). You can also use environments to delete and rename databases. For transactional applications, you use the environment to start transactions. For non-transactional applications, you use the environment to sync your in-memory cache to disk.

## Opening Database Environments

You open a database environment by instantiating an `Environment` object. You must provide to the constructor the name of the on-disk directory where the environment is to reside. This directory location must exist or the open will fail.

By default, the environment is not created for you if it does not exist. Set the <a href="EnvProps.md" class="link" title="Environment Properties">creation property</a> to `true` if you want the environment to be created. For example:

``` c
    
import com.sleepycat.je.DatabaseException;
import com.sleepycat.je.Environment;
import com.sleepycat.je.EnvironmentConfig;

import java.io.File;

...

// Open the environment. Allow it to be created if it does not already 
// exist.
Environment myDbEnvironment = null;

try {
    EnvironmentConfig envConfig = new EnvironmentConfig();
    envConfig.setAllowCreate(true);
    myDbEnvironment = new Environment(new File("/export/dbEnv"), 
                                      envConfig);
} catch (DatabaseException dbe) {
    // Exception handling goes here
} 
```

``` c
package db.gettingStarted;
    
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import java.io.File;
import java.io.FileNotFoundException;

...

// Open the environment. Allow it to be created if it does not already 
// exist.
Environment myDbEnvironment = null;

try {
    EnvironmentConfig envConfig = new EnvironmentConfig();
    envConfig.setAllowCreate(true);
    myDbEnvironment = new Environment(new File("/export/dbEnv"), 
                                      envConfig);
} catch (DatabaseException dbe) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
// Exception handling goes here
} 
```

Your application can open and use as many environments as you have disk and memory to manage, although most applications will use just one environment. Also, you can instantiate multiple `Environment` objects for the same physical environment.
