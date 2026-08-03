---
title: "Chapter 6.  Using Serializable Entities"
api-name: "Chapter 6.  Using Serializable Entities"
source: docs/collections/tutorial/SerializableEntity.html
---
## Chapter 6.  Using Serializable Entities

**Table of Contents**

<span class="sect1"> [Using Transient Fields in an Entity Class](SerializableEntity.md#transientfieldsinclass) </span>

<span class="sect1"> [Using Transient Fields in an Entity Binding](transientfieldsinbinding.md) </span>

<span class="sect1"> [Removing the Redundant Value Classes](removingredundantvalueclasses.md) </span>

In the prior examples that used entities (the Entity and Tuple examples) you may have noticed the redundancy between the serializable value classes and the entity classes. An entity class by definition contains all properties of the value class as well as all properties of the key class.

When using serializable values it is possible to remove this redundancy by changing the entity class in two ways:

- Make the entity class serializable, so it can be used in place of the value class.

- Make the key fields transient, so they are not redundantly stored in the record.

The modified entity class can then serve double-duty: It can be serialized and stored as the record value, and it can be used as the entity class as usual along with the Java collections API. The `PartData`, `SupplierData` and `ShipmentData` classes can then be removed.

Transient fields are defined in Java as fields that are not stored in the serialized form of an object. Therefore, when an object is deserialized the transient fields must be explicitly initialized. Since the entity binding is responsible for creating entity objects, it is the natural place to initialize the transient key fields.

Note that it is not strictly necessary to make the key fields of a serializable entity class transient. If this is not done, the key will simply be stored redundantly in the record's value. This extra storage may or may not be acceptable to an application. But since we are using tuple keys and an entity binding class must be implemented anyway to extract the key from the entity, it is sensible to use transient key fields to reduce the record size. Of course there may be a reason that transient fields are not desired; for example, if an application wants to serialize the entity objects for other purposes, then using transient fields should be avoided.

The complete source of the final version of the example program is included in the Berkeley DB distribution.

## Using Transient Fields in an Entity Class

The entity classes in this example are redefined such that they can be used both as serializable value classes and as entity classes. Compared to the prior example there are three changes to the `Part`, `Supplier` and `Shipment` entity classes:

- Each class now implements the `Serializable` interface.

- The key fields in each class are declared as `transient`.

- A package-private `setKey()` method is added to each class for initializing the transient key fields. This method will be called from the entity bindings.

``` c
import java.io.Serializable;
...
public class Part implements Serializable
{
    private transient String number;
    private String name;
    private String color;
    private Weight weight;
    private String city;

    public Part(String number, String name, String color, Weight weight,
                String city)
    {
        this.number = number;
        this.name = name;
        this.color = color;
        this.weight = weight;
        this.city = city;
    }

    final void setKey(String number)
    {
        this.number = number;
    }

    public final String getNumber()
    {
        return number;
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
        return "Part: number=" + number +
               " name=" + name +
               " color=" + color +
               " weight=" + weight +
               " city=" + city + '.';
    }
}
...
public class Supplier implements Serializable
{
    private transient String number;
    private String name;
    private int status;
    private String city;

    public Supplier(String number, String name, int status, String city)
    {
        this.number = number;
        this.name = name;
        this.status = status;
        this.city = city;
    }

    void setKey(String number)
    {
        this.number = number;
    }

    public final String getNumber()
    {
        return number;
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
        return "Supplier: number=" + number +
               " name=" + name +
               " status=" + status +
               " city=" + city + '.';
    }
}
...
public class Shipment implements Serializable
{
    private transient String partNumber;
    private transient String supplierNumber;
    private int quantity;

    public Shipment(String partNumber, String supplierNumber, int quantity)
    {
        this.partNumber = partNumber;
        this.supplierNumber = supplierNumber;
        this.quantity = quantity;
    }

    void setKey(String partNumber, String supplierNumber)
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

    public final int getQuantity()
    {
        return quantity;
    }

    public String toString()
    {
        return "Shipment: part=" + partNumber +
                " supplier=" + supplierNumber +
                " quantity=" + quantity + '.';
    }
}
    
```
