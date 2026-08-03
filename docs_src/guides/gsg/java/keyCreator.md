---
title: "Implementing Key Creators"
api-name: "Implementing Key Creators"
source: docs/gsg/JAVA/keyCreator.html
---
## Implementing Key Creators

<span class="sect2"> [Working with Multiple Keys](keyCreator.md#multikeys) </span>

You must provide every secondary database with a class that creates keys from primary records. You identify this class using the `SecondaryConfig.setKeyCreator()` method.

You can create keys using whatever data you want. Typically you will base your key on some information found in a record's data, but you can also use information found in the primary record's key. How you build your keys is entirely dependent upon the nature of the index that you want to maintain.

You implement a key creator by writing a class that implements the `SecondaryKeyCreator` interface. This interface requires you to implement the `SecondaryKeyCreator.createSecondaryKey()` method.

One thing to remember when implementing this method is that you will need a way to extract the necessary information from the data's `DatabaseEntry` and/or the key's `DatabaseEntry` that are provided on calls to this method. If you are using complex objects, then you are probably using the Bind APIs to perform this conversion. The easiest thing to do is to instantiate the `EntryBinding` or `TupleBinding` that you need to perform the conversion, and then provide this to your key creator's constructor. The Bind APIs are introduced in <a href="bindAPI.md" class="xref" title="Using the BIND APIs">Using the BIND APIs</a>.

`SecondaryKeyCreator.createSecondaryKey()` returns a boolean. A return value of `false` indicates that no secondary key exists, and therefore no record should be added to the secondary database for that primary record. If a record already exists in the secondary database, it is deleted.

For example, suppose your primary database uses the following class for its record data:

``` c
package db.GettingStarted;

public class PersonData {
    private String userID;
    private String surname;
    private String familiarName;

    public PersonData(String userID, String surname, 
                      String familiarName) {
        this.userID = userID;
        this.surname = surname;
        this.familiarName = familiarName;
    }

    public String getUserID() {
        return userID;
    }

    public String getSurname() {
        return surname;
    }

    public String getFamiliarName() {
        return familiarName;
    }
} 
```

Also, suppose that you have created a custom tuple binding, `PersonDataBinding`, that you use to convert `PersonData` objects to and from `DatabaseEntry` objects. (Custom tuple bindings are described in <a href="bindAPI.md#customTuple" class="xref" title="Custom Tuple Bindings">Custom Tuple Bindings</a>.)

Finally, suppose you want a secondary database that is keyed based on the person's full name.

Then in this case you might create a key creator as follows:

``` c
package db.GettingStarted;

import com.sleepycat.bind.tuple.TupleBinding;
import com.sleepycat.db.SecondaryKeyCreator;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.SecondaryDatabase;

import java.io.IOException;

public class FullNameKeyCreator implements SecondaryKeyCreator {
 
    private TupleBinding theBinding;

    public FullNameKeyCreator(TupleBinding theBinding1) {
            theBinding = theBinding1;
    }

    public boolean createSecondaryKey(SecondaryDatabase secDb,
                                      DatabaseEntry keyEntry, 
                                      DatabaseEntry dataEntry,
                                      DatabaseEntry resultEntry) {

        try {
            PersonData pd = 
                (PersonData) theBinding.entryToObject(dataEntry);
                String fullName = pd.getFamiliarName() + " " + 
                    pd.getSurname();
                resultEntry.setData(fullName.getBytes("UTF-8"));
        } catch (IOException willNeverOccur) {}
        return true;
    }
} 
```

Finally, you use this key creator as follows:

``` c
package db.GettingStarted;
import com.sleepycat.examples.db.GettingStarted.MyTupleBinding;
import com.sleepycat.bind.tuple.TupleBinding;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.SecondaryDatabase;
import com.sleepycat.db.SecondaryConfig;

import java.io.FileNotFoundException;

...

Database myDb = null;
SecondaryDatabase mySecDb = null;
try {
    // Primary database open omitted for brevity
...

    TupleBinding myDataBinding = new MyTupleBinding();
    FullNameKeyCreator fnkc = new FullNameKeyCreator(myDataBinding);

    SecondaryConfig mySecConfig = new SecondaryConfig();
    mySecConfig.setKeyCreator(fnkc);
    mySecConfig.setType(DatabaseType.BTREE);

    //Perform the actual open
    String secDbName = "mySecondaryDatabase";
    mySecDb = new SecondaryDatabase(secDbName, null, myDb, mySecConfig);
} catch (DatabaseException de) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
    // Exception handling goes here
} finally {
    try {
        if (mySecDb != null) {
            mySecDb.close();
        }

        if (myDb != null) {
            myDb.close(); 
        }
    } catch (DatabaseException dbe) {
        // Exception handling goes here
    }
}
```

### Working with Multiple Keys

Until now we have only discussed indexes as if there is a one-to-one relationship between the secondary key and the primary database record. In fact, it is possible to generate multiple keys for any given record, provided that you take appropriate steps in your key creator to do so.

For example, suppose you had a database that contained information about books. Suppose further that you sometimes want to look up books by author. Because sometimes books have multiple authors, you may want to return multiple secondary keys for every book that you index.

To do this, you write a key creator that implements `SecondaryMultiKeyCreator` instead of `SecondaryKeyCreator`. The key difference between the two is that `SecondaryKeyCreator` uses a single `DatabaseEntry` object as the result, while `SecondaryMultiKeyCreator` returns a set of `DatabaseEntry` objects (using `java.util.Set`). Also, you assign the `SecondaryMultiKeyCreator` implementation using `SecondaryConfig.setMultiKeyCreator()` instead of `SecondaryConfig.setKeyCreator()`.

For example:

``` c
package db.GettingStarted;

import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.SecondaryDatabase;
import com.sleepycat.db.SecondaryMultiKeyCreator;

import java.util.HashSet;
import java.util.Set;

public class MyMultiKeyCreator implements SecondaryMultiKeyCreator {

    // Constructor not implemented. How this is implemented depends on
    // how you want to extract the data for your keys.
    MyMultiKeyCreator() {
        ...
    }

    // Abstract method that we must implement
    public void createSecondaryKeys(SecondaryDatabase secDb,
        DatabaseEntry keyEntry,    // From the primary
        DatabaseEntry dataEntry,   // From the primary
        Set results)               // Results set
        throws DatabaseException {

        try {
            // Create your keys, adding each to the set

            // Creation of key 'a' not shown
            results.add(a)

            // Creation of key 'b' not shown
            results.add(b)

        } catch (IOException willNeverOccur) {}
    }
} 
```
