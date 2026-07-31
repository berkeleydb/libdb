---
title: "Chapter 2.  The Basic Program"
api-name: "Chapter 2.  The Basic Program"
source: docs/collections/tutorial/BasicProgram.html
---
## Chapter 2.  The Basic Program

**Table of Contents**

<span class="sect1"> [Defining Serialized Key and Value Classes](BasicProgram.md#keyandvalueclasses) </span>

<span class="sect1"> [Opening and Closing the Database Environment](opendbenvironment.md) </span>

<span class="sect1"> [Opening and Closing the Class Catalog](openclasscatalog.md) </span>

<span class="sect1"> [Opening and Closing Databases](opendatabases.md) </span>

<span class="sect1"> [Creating Bindings and Collections](createbindingscollections.md) </span>

<span class="sect1"> [Implementing the Main Program](implementingmain.md) </span>

<span class="sect1"> [Using Transactions](usingtransactions.md) </span>

<span class="sect1"> [Adding Database Items](addingdatabaseitems.md) </span>

<span class="sect1"> [Retrieving Database Items](retrievingdatabaseitems.md) </span>

<span class="sect1"> [Handling Exceptions](handlingexceptions.md) </span>

The Basic example is a minimal implementation of the shipment program. It writes and reads the part, supplier and shipment databases.

The complete source of the final version of the example program is included in the Berkeley DB distribution.

## Defining Serialized Key and Value Classes

The key and value classes for each type of shipment record — Parts, Suppliers and Shipments — are defined as ordinary Java classes. In this example the serialized form of the key and value objects is stored directly in the database. Therefore these classes must implement the standard Java java.io.Serializable interface. A compact form of Java serialization is used that does not duplicate the class description in each record. Instead the class descriptions are stored in the class catalog store, which is described in the next section. But in all other respects, standard Java serialization is used.

An important point is that instances of these classes are passed and returned by value, not by reference, when they are stored and retrieved from the database. This means that changing a key or value object does not automatically change the database. The object must be explicitly stored in the database after changing it. To emphasize this point the key and value classes defined here have no field setter methods. Setter methods can be defined, but it is important to remember that calling a setter method will not cause the change to be stored in the database. How to store and retrieve objects in the database will be described later.

Each key and value class contains a toString method that is used to output the contents of the object in the example program. This is meant for illustration only and is not required for database objects in general.

Notice that the key and value classes defined below do not contain any references to `com.sleepycat` packages. An important characteristic of these classes is that they are independent of the database. Therefore, they may be easily used in other contexts and may be defined in a way that is compatible with other tools and libraries.

The `PartKey` class contains only the Part's Number field.

Note that `PartKey` (as well as `SupplierKey` below) contain only a single String field. Instead of defining a specific class for each type of key, the String class by itself could have been used. Specific key classes were used to illustrate strong typing and for consistency in the example. The use of a plain String as an index key is illustrated in the next example program. It is up to the developer to use either primitive Java classes such as String and Integer, or strongly typed classes. When there is the possibility that fields will be added later to a key or value, a specific class should be used.

``` c
import java.io.Serializable;

public class PartKey implements Serializable
{
    private String number;

    public PartKey(String number) {
        this.number = number;
    }

    public final String getNumber() {
        return number;
    }

    public String toString() {
        return "[PartKey: number=" + number + ']';
    }
} 
```

The `PartData` class contains the Part's Name, Color, Weight and City fields.

``` c
import java.io.Serializable;

public class PartData implements Serializable
{
    private String name;
    private String color;
    private Weight weight;
    private String city;

    public PartData(String name, String color, Weight weight, String city)
    {
        this.name = name;
        this.color = color;
        this.weight = weight;
        this.city = city;
    }

    public final String getName()
    {
        return name;
    }

    public final String getColor()
    {
        return color;
    }

    public final Weight getWeight()
    {
        return weight;
    }

    public final String getCity()
    {
        return city;
    }

    public String toString()
    {
        return "[PartData: name=" + name +
               " color=" + color +
               " weight=" + weight +
               " city=" + city + ']';
    }
} 
```

The `Weight` class is also defined here, and is used as the type of the Part's Weight field. Just as in standard Java serialization, nothing special is needed to store nested objects as long as they are all Serializable.

``` c
import java.io.Serializable;

public class Weight implements Serializable
{
    public final static String GRAMS = "grams";
    public final static String OUNCES = "ounces";

    private double amount;
    private String units;

    public Weight(double amount, String units)
    {
        this.amount = amount;
        this.units = units;
    }

    public final double getAmount()
    {
        return amount;
    }

    public final String getUnits()
    {
        return units;
    }

    public String toString()
    {
        return "[" + amount + ' ' + units + ']';
    }
} 
```

The `SupplierKey` class contains the Supplier's Number field.

``` c
import java.io.Serializable;

public class SupplierKey implements Serializable
{
    private String number;

    public SupplierKey(String number)
    {
        this.number = number;
    }

    public final String getNumber()
    {
        return number;
    }

    public String toString()
    {
        return "[SupplierKey: number=" + number + ']';
    }
} 
```

The `SupplierData` class contains the Supplier's Name, Status and City fields.

``` c
import java.io.Serializable;

public class SupplierData implements Serializable
{
    private String name;
    private int status;
    private String city;

    public SupplierData(String name, int status, String city)
    {
        this.name = name;
        this.status = status;
        this.city = city;
    }

    public final String getName()
    {
        return name;
    }

    public final int getStatus()
    {
        return status;
    }

    public final String getCity()
    {
        return city;
    }

    public String toString()
    {
        return "[SupplierData: name=" + name +
               " status=" + status +
               " city=" + city + ']';
    }
}
    
```

The `ShipmentKey` class contains the keys of both the Part and Supplier.

``` c
import java.io.Serializable;

public class ShipmentKey implements Serializable
{
    private String partNumber;
    private String supplierNumber;

    public ShipmentKey(String partNumber, String supplierNumber)
    {
        this.partNumber = partNumber;
        this.supplierNumber = supplierNumber;
    }

    public final String getPartNumber()
    {
        return partNumber;
    }

    public final String getSupplierNumber()
    {
        return supplierNumber;
    }

    public String toString()
    {
        return "[ShipmentKey: supplier=" + supplierNumber +
                " part=" + partNumber + ']';
    }
} 
```

The `ShipmentData` class contains only the Shipment's Quantity field. Like `PartKey` and `SupplierKey`, `ShipmentData` contains only a single primitive field. Therefore the Integer class could have been used instead of defining a specific value class.

``` c
import java.io.Serializable;

public class ShipmentData implements Serializable
{
    private int quantity;

    public ShipmentData(int quantity)
    {
        this.quantity = quantity;
    }

    public final int getQuantity()
    {
        return quantity;
    }

    public String toString()
    {
        return "[ShipmentData: quantity=" + quantity + ']';
    }
} 
```
