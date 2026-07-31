---
title: "Implementing the Main Program"
api-name: "Implementing the Main Program"
source: docs/collections/tutorial/implementingmain.html
---
## Implementing the Main Program

The main program opens the environment and databases, stores and retrieves objects within a transaction, and finally closes the environment databases. This section describes the main program shell, and the next section describes how to run transactions for storing and retrieving objects.

The `Sample` class contains the main program. The skeleton for the `Sample` class follows.

``` c
import com.sleepycat.db.DatabaseException;
import java.io.FileNotFoundException;

public class Sample
{
    private SampleDatabase db;
    private SampleViews views;

    public static void main(String args)
    {
    }

    private Sample(String homeDir)
        throws DatabaseException, FileNotFoundException
    {
    }

    private void close()
        throws DatabaseException
    {
    }

    private void run()
        throws Exception
    {
    }
} 
```

The main program uses the `SampleDatabase` and `SampleViews` classes that were described in the preceding sections. The `main` method will create an instance of the `Sample` class, and call its `run()` and `close()` methods.

The following statements parse the program's command line arguments.

``` c
    public static void main(String[] args)
    {
        System.out.println("\nRunning sample: " + Sample.class);
        String homeDir = "./tmp";
        for (int i = 0; i < args.length; i += 1)
        {
            String arg = args[i];
            if (args[i].equals("-h") && i < args.length - 1)
            {
                i += 1;
                homeDir = args[i];
            }
            else
            {
                System.err.println("Usage:\n java " + 
                                   Sample.class.getName() +
                                  "\n  [-h <home-directory>]");
                System.exit(2);
            }
        }
        ...
    } 
```

The usage command is:

``` c
java com.sleepycat.examples.bdb.shipment.basic.Sample
     [-h <home-directory> ] 
```

The `-h` command is used to set the `homeDir` variable, which will later be passed to the `SampleDatabase()` constructor. Normally all Berkeley DB programs should provide a way to configure their database environment home directory.

The default for the home directory is `./tmp` — the tmp subdirectory of the current directory where the sample is run. The home directory must exist before running the sample. To re-create the sample database from scratch, delete all files in the home directory before running the sample.

The home directory was described previously in <a href="opendbenvironment.md" class="xref" title="Opening and Closing the Database Environment">Opening and Closing the Database Environment</a> .

Of course, the command line arguments shown are only examples and a real-life application may use different techniques for configuring these options.

The following statements create an instance of the `Sample` class and call its `run()` and `close()` methods.

``` c
    public static void main(String args)
    {
        ...
        Sample sample = null;
        try
        {
            sample = new Sample(homeDir);
            sample.run();
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        finally
        {
            if (sample != null)
            {
                try
                {
                    sample.close();
                }
                catch (Exception e)
                {
                    System.err.println("Exception during database close:");
                    e.printStackTrace();
                }
            }
        }
    } 
```

The `Sample()` constructor will open the environment and databases, and the `run()` method will run transactions for storing and retrieving objects. If either of these throws an exception, then the program was unable to run and should normally terminate. (Transaction retries are handled at a lower level and will be described later.) The first `catch` statement handles such exceptions.

The `finally` statement is used to call the `close()` method since an attempt should always be made to close the environment and databases cleanly. If an exception is thrown during close and a prior exception occurred above, then the exception during close is likely a side effect of the prior exception.

The `Sample()` constructor creates the `SampleDatabase` and `SampleViews` objects.

``` c
    private Sample(String homeDir)
        throws DatabaseException, FileNotFoundException
    {
        db = new SampleDatabase(homeDir);
        views = new SampleViews(db);
    } 
```

Recall that creating the `SampleDatabase` object will open the environment and all databases.

To close the database the `Sample.close()` method simply calls `SampleDatabase.close()`.

``` c
     private void close()
        throws DatabaseException
    {
        db.close();
    } 
```

The `run()` method is described in the next section.
