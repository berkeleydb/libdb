---
title: "ExampleDatabasePut.java"
api-name: "ExampleDatabasePut.java"
source: docs/gsg/JAVA/dpl_exampledatabaseput.html
---
## ExampleDatabasePut.java

Our example reads inventory and vendor information from flat text files, encapsulates this data in objects of the appropriate type, and then writes each object to an `EntityStore`.

To begin, we import the Java classes that our example needs. Most of the imports are related to reading the raw data from flat text files and breaking them apart for usage with our data classes. We also import classes from the DB package, but we do not actually import any classes from the DPL. The reason why is because we have placed almost all of our DPL work off into other classes, so there is no need for direct usage of those APIs here.

``` c
package persist.gettingStarted;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import com.sleepycat.db.DatabaseException; 
```

Now we can begin the class itself. Here we set default paths for the on-disk resources that we require (the environment home, and the location of the text files containing our sample data). We also declare `DataAccessor` and `MyDbEnv` members. We describe these classes and show their implementation in <a href="dataaccessorclass.md" class="xref" title="DataAccessor.java">DataAccessor.java</a> and <a href="mydbenv-persist.md" class="xref" title="MyDbEnv">MyDbEnv</a>.

``` c
public class ExampleDatabasePut {

    private static File myDbEnvPath = new File("/tmp/JEDB");
    private static File inventoryFile = new File("./inventory.txt");
    private static File vendorsFile = new File("./vendors.txt");

    private DataAccessor da;

    // Encapsulates the environment and data store.
    private static MyDbEnv myDbEnv = new MyDbEnv();
```

Next, we provide our `usage()` method. The command line options provided there are necessary only if the default values to the on-disk resources are not sufficient.

``` c
    private static void usage() {
        System.out.println("ExampleDatabasePut [-h <env directory>]");
        System.out.println("      [-i <inventory file>]");
        System.out.println("      [-v <vendors file>]");
        System.exit(-1);
    } 
```

Our `main()` method is also reasonably self-explanatory. We simply instantiate an `ExampleDatabasePut` object there and then call its `run()` method. We also provide a top-level `try` block there for any exceptions that might be thrown during runtime.

Notice that the `finally` statement in the top-level `try` block calls `MyDbEnv.close()`. This method closes our `EntityStore` and `Environment` objects. By placing it here in the `finally` statement, we can make sure that our store and environment are always cleanly closed.

``` c
    public static void main(String args[]) {
        ExampleDatabasePut edp = new ExampleDatabasePut();
        try {
            edp.run(args);
        } catch (DatabaseException dbe) {
            System.err.println("ExampleDatabasePut: " + dbe.toString());
            dbe.printStackTrace();
        } catch (Exception e) {
            System.out.println("Exception: " + e.toString());
            e.printStackTrace();
        } finally {
            myDbEnv.close();
        }
        System.out.println("All done.");
    } 
```

Our `run()` method does four things. It calls `MyDbEnv.setup()`, which opens our `Environment` and `EntityStore`. It then instantiates a `DataAccessor` object, which we will use to write data to the store. It calls `loadVendorsDb()` which loads all of the vendor information. And then it calls `loadInventoryDb()` which loads all of the inventory information.

Notice that the `MyDbEnv` object is being setup as read-write. This results in the `EntityStore` being opened for transactional support. (See <a href="mydbenv-persist.md" class="xref" title="MyDbEnv">MyDbEnv</a> for implementation details.)

``` c
    private void run(String args[])
        throws DatabaseException {
        // Parse the arguments list
        parseArgs(args);

        myDbEnv.setup(myDbEnvPath,  // Path to the environment home 
                      false);       // Environment read-only?

        // Open the data accessor. This is used to store
        // persistent objects.
        da = new DataAccessor(myDbEnv.getEntityStore());

        System.out.println("loading vendors db....");
        loadVendorsDb();

        System.out.println("loading inventory db....");
        loadInventoryDb();
    } 
```

We can now implement the `loadVendorsDb()` method. This method is responsible for reading the vendor contact information from the appropriate flat-text file, populating `Vendor` class objects with the data and then writing it to the `EntityStore`. As explained above, each individual object is written with transactional support. However, because a transaction handle is not explicitly used, the write is performed using auto-commit. This happens because the `EntityStore` was opened to support transactions.

To actually write each class to the `EntityStore`, we simply call the `PrimaryIndex.put()` method for the `Vendor` entity instance. We obtain this method from our `DataAccessor` class.

``` c
    private void loadVendorsDb()
            throws DatabaseException {

        // loadFile opens a flat-text file that contains our data
        // and loads it into a list for us to work with. The integer
        // parameter represents the number of fields expected in the
        // file.
        List vendors = loadFile(vendorsFile, 8);

        // Now load the data into the store.
        for (int i = 0; i < vendors.size(); i++) {
            String[] sArray = (String[])vendors.get(i);
            Vendor theVendor = new Vendor();
            theVendor.setVendorName(sArray[0]);
            theVendor.setAddress(sArray[1]);
            theVendor.setCity(sArray[2]);
            theVendor.setState(sArray[3]);
            theVendor.setZipcode(sArray[4]);
            theVendor.setBusinessPhoneNumber(sArray[5]);
            theVendor.setRepName(sArray[6]);
            theVendor.setRepPhoneNumber(sArray[7]);

            // Put it in the store.
            da.vendorByName.put(theVendor);
        }
    } 
```

Now we can implement our `loadInventoryDb()` method. This does exactly the same thing as the `loadVendorsDb()` method.

``` c
    private void loadInventoryDb()
        throws DatabaseException {

        // loadFile opens a flat-text file that contains our data
        // and loads it into a list for us to work with. The integer
        // parameter represents the number of fields expected in the
        // file.
        List inventoryArray = loadFile(inventoryFile, 6);

        // Now load the data into the store. The item's sku is the
        // key, and the data is an Inventory class object.

        for (int i = 0; i < inventoryArray.size(); i++) {
            String[] sArray = (String[])inventoryArray.get(i);
            String sku = sArray[1];

            Inventory theInventory = new Inventory();
            theInventory.setItemName(sArray[0]);
            theInventory.setSku(sArray[1]);
            theInventory.setVendorPrice(
                (new Float(sArray[2])).floatValue());
            theInventory.setVendorInventory(
                (new Integer(sArray[3])).intValue());
            theInventory.setCategory(sArray[4]);
            theInventory.setVendor(sArray[5]);

            // Put it in the store. Note that this causes our secondary key
            // to be automatically updated for us.
            da.inventoryBySku.put(theInventory);
        }
    } 
```

The remainder of this example simple parses the command line and loads data from a flat-text file. There is nothing here that is of specific interest to the DPL, but we show this part of the example anyway in the interest of completeness.

``` c
    private static void parseArgs(String args[]) {
        for(int i = 0; i < args.length; ++i) {
            if (args[i].startsWith("-")) {
                switch(args[i].charAt(1)) {
                  case 'h':
                    myDbEnvPath = new File(args[++i]);
                    break;
                  case 'i':
                    inventoryFile = new File(args[++i]);
                    break;
                  case 'v':
                    vendorsFile = new File(args[++i]);
                    break;
                  default:
                    usage();
                }
            }
        }
    }

    private List loadFile(File theFile, int numFields) {
        List<String[]> records = new ArrayList<String[]>();
        try {
            String theLine = null;
            FileInputStream fis = new FileInputStream(theFile);
            BufferedReader br = 
                new BufferedReader(new InputStreamReader(fis));
            while((theLine=br.readLine()) != null) {
                String[] theLineArray = theLine.split("#");
                if (theLineArray.length != numFields) {
                    System.out.println("Malformed line found in " + 
                        theFile.getPath());
                    System.out.println("Line was: '" + theLine);
                    System.out.println("length found was: " + 
                        theLineArray.length);
                    System.exit(-1);
                }
                records.add(theLineArray);
            }
            // Close the input stream handle
            fis.close();
        } catch (FileNotFoundException e) {
            System.err.println(theFile.getPath() + " does not exist.");
            e.printStackTrace();
            usage();
        } catch (IOException e)  {
            System.err.println("IO Exception: " + e.toString());
            e.printStackTrace();
            System.exit(-1);
        }
        return records;
    }

    protected ExampleDatabasePut() {}
} 
```
