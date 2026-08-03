---
title: "Opening and Closing the Database Environment"
api-name: "Opening and Closing the Database Environment"
source: docs/collections/tutorial/opendbenvironment.html
---
## Opening and Closing the Database Environment

This section of the tutorial describes how to open and close the database environment. The database environment manages resources (for example, memory, locks and transactions) for any number of databases. A single environment instance is normally used for all databases.

The `SampleDatabase` class is used to open and close the environment. It will also be used in following sections to open and close the class catalog and other databases. Its constructor is used to open the environment and its `close()` method is used to close the environment. The skeleton for the `SampleDatabase` class follows.

``` c
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import java.io.File;
import java.io.FileNotFoundException;

public class SampleDatabase
{
    private Environment env;

    public SampleDatabase(String homeDirectory)
        throws DatabaseException, FileNotFoundException
    {
    }

    public void close()
        throws DatabaseException
    {
    }
} 
```

The first thing to notice is that the Environment class is in the com.sleepycat.db package, not the com.sleepycat.collections package. The com.sleepycat.db package contains all core Berkeley DB functionality. The com.sleepycat.collections package contains extended functionality that is based on the Java Collections API. The collections package is layered on top of the com.sleepycat.db package. Both packages are needed to create a complete application based on the DB Java Collections API.

The following statements create an <a href="../../java/com/sleepycat/db/Environment.html" class="ulink" target="_top">Environment</a> object.

``` c
public SampleDatabase(String homeDirectory)
        throws DatabaseException, FileNotFoundException
    {
        System.out.println("Opening environment in: " + homeDirectory);

        EnvironmentConfig envConfig = new EnvironmentConfig();
        envConfig.setTransactional(true);
        envConfig.setAllowCreate(true);
        envConfig.setInitializeCache(true);
        envConfig.setInitializeLocking(true);

        env = new Environment(new File(homeDirectory), envConfig);
    } 
```

The <a href="../../java/com/sleepycat/db/EnvironmentConfig.html" class="ulink" target="_top">EnvironmentConfig</a> class is used to specify environment configuration parameters. The first configuration option specified — `setTransactional()` — is set to true to create an environment where transactional (and non-transactional) databases may be opened. While non-transactional environments can also be created, the examples in this tutorial use a transactional environment.

`setAllowCreate()` is set to true to specify that the environment's files will be created if they don't already exist. If this parameter is not specified, an exception will be thrown if the environment does not already exist. A similar parameter will be used later to cause databases to be created if they don't exist.

When an `Environment` object is constructed, a home directory and the environment configuration object are specified. The home directory is the location of the environment's log files that store all database information.

The following statement closes the environment. The environment must be closed when database work is completed to free allocated resources and to avoid having to run recovery later. It is recommended that databases are closed before closing the environment.

``` c
    public void close()
        throws DatabaseException
    {
        env.close();
    } 
```

The following getter method returns the environment for use by other classes in the example program. The environment is used for opening databases and running transactions.

``` c
public class SampleDatabase
{
    ...
    public final Environment getEnvironment()
    {
        return env;
    }
    ...
} 
```
