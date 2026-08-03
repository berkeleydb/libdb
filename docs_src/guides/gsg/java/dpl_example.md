---
title: "Chapter 6. A DPL Example"
api-name: "Chapter 6. A DPL Example"
source: docs/gsg/JAVA/dpl_example.html
---
## Chapter 6. A DPL Example

**Table of Contents**

<span class="sect1"> [Vendor.java](dpl_example.md#vendorclass) </span>

<span class="sect1"> [Inventory.java](inventoryclass.md) </span>

<span class="sect1"> [MyDbEnv](mydbenv-persist.md) </span>

<span class="sect1"> [DataAccessor.java](dataaccessorclass.md) </span>

<span class="sect1"> [ExampleDatabasePut.java](dpl_exampledatabaseput.md) </span>

<span class="sect1"> [ExampleInventoryRead.java](dpl_exampleinventoryread.md) </span>

In order to illustrate DPL usage, we provide a complete working example in this chapter. This example reads and writes inventory and vendor information for a mythical business. The application consists of the following classes:

- Several classes used to encapsulate our application's data. See <a href="dpl_example.md#vendorclass" class="xref" title="Vendor.java">Vendor.java</a> and <a href="inventoryclass.md" class="xref" title="Inventory.java">Inventory.java</a>.

- A convenience class used to open and close our environment and entity store. See <a href="mydbenv-persist.md" class="xref" title="MyDbEnv">MyDbEnv</a>.

- A class that loads data into the store. See <a href="dpl_exampledatabaseput.md" class="xref" title="ExampleDatabasePut.java">ExampleDatabasePut.java</a>.

- Finally, a class that reads data from the store. See <a href="dpl_exampleinventoryread.md" class="xref" title="ExampleInventoryRead.java">ExampleInventoryRead.java</a>.

## Vendor.java

The simplest class that our example wants to store contains vendor contact information. This class contains no secondary indices so all we have to do is identify it as an entity class and identify the field in the class used for the primary key.

In the following example, we identify the `vendor` data member as containing the primary key. This data member is meant to contain a vendor's name. Because of the way we will use our `EntityStore`, the value provided for this data member must be unique within the store or runtime errors will result.

When used with the DPL, our `Vendor` class appears as follows. Notice that the `@Entity` annotation appears immediately before the class declaration, and the `@PrimaryKey` annotation appears immediately before the `vendor` data member declaration.

``` c
package persist.gettingStarted;

import com.sleepycat.persist.model.Entity;
import com.sleepycat.persist.model.PrimaryKey;

@Entity
public class Vendor {

    private String address;
    private String bizPhoneNumber;
    private String city;
    private String repName;
    private String repPhoneNumber;
    private String state;

    // Primary key is the vendor's name
    // This assumes that the vendor's name is
    // unique in the database.
    @PrimaryKey
    private String vendor;

    private String zipcode;

    public void setRepName(String data) {
        repName = data;
    }

    public void setAddress(String data) {
        address = data;
    }

    public void setCity(String data) {
        city = data;
    }

    public void setState(String data) {
        state = data;
    }

    public void setZipcode(String data) {
        zipcode = data;
    }

    public void setBusinessPhoneNumber(String data) {
        bizPhoneNumber = data;
    }

    public void setRepPhoneNumber(String data) {
        repPhoneNumber = data;
    }

    public void setVendorName(String data) {
        vendor = data;
    }

    public String getRepName() {
        return repName;
    }

    public String getAddress() {
        return address;
    }

    public String getCity() {
        return city;
    }

    public String getState() {
        return state;
    }

    public String getZipcode() {
        return zipcode;
    }

    public String getBusinessPhoneNumber() {
        return bizPhoneNumber;
    }

    public String getRepPhoneNumber() {
        return repPhoneNumber;
    }
} 
```

For this class, the `vendor` value is set for an individual `Vendor` class object by the `setVendorName()` method. If our example code fails to set this value before storing the object, the data member used to store the primary key is set to a null value. This would result in a runtime error.
