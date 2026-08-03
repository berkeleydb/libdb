---
title: "Chapter 4.  Using Entity Classes"
api-name: "Chapter 4.  Using Entity Classes"
source: docs/collections/tutorial/Entity.html
---
## Chapter 4.  Using Entity Classes

**Table of Contents**

<span class="sect1"> [Defining Entity Classes](Entity.md#definingentityclasses) </span>

<span class="sect1"> [Creating Entity Bindings](creatingentitybindings.md) </span>

<span class="sect1"> [Creating Collections with Entity Bindings](collectionswithentities.md) </span>

<span class="sect1"> [Using Entities with Collections](entitieswithcollections.md) </span>

In the prior examples, the keys and values of each store were represented using separate classes. For example, a `PartKey` and a `PartData` class were used. Many times it is desirable to have a single class representing both the key and the value, for example, a `Part` class.

Such a combined key and value class is called an <span class="emphasis">*entity class*</span> and is used along with an <span class="emphasis">*entity binding*</span>. Entity bindings combine a key and a value into an entity when reading a record from a collection, and split an entity into a key and a value when writing a record to a collection. Entity bindings are used in place of value bindings, and entity objects are used with collections in place of value objects.

Some reasons for using entities are:

- When the key is a property of an entity object representing the record as a whole, the object's identity and concept are often clearer than with key and value objects that are disjoint.

- A single entity object per record is often more convenient to use than two objects.

Of course, instead of using an entity binding, you could simply create the entity yourself after reading the key and value from a collection, and split the entity into a key and value yourself before writing it to a collection. But this would detract from the convenience of the using the Java collections API. It is convenient to obtain a `Part` object directly from <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#get(java.lang.Object)" class="ulink" target="_top">Map.get</a> and to add a `Part` object using <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Set.html#add(E)" class="ulink" target="_top">Set.add</a>. Collections having entity bindings can be used naturally without combining and splitting objects each time a collection method is called; however, an entity binding class must be defined by the application.

In addition to showing how to use entity bindings, this example illustrates a key feature of all bindings: Bindings are independent of database storage parameters and formats. Compare this example to the prior Index example and you'll see that the `Sample` and `SampleViews` classes have been changed to use entity bindings, but the `SampleDatabase` class was not changed at all. In fact, the Entity program and the Index program can be used interchangeably to access the same physical database files. This demonstrates that bindings are only a "view" onto the physical stored data.

`Warning:` When using multiple bindings for the same database, it is the application's responsibility to ensure that the same format is used for all bindings. For example, a serial binding and a tuple binding cannot be used to access the same records.

The complete source of the final version of the example program is included in the Berkeley DB distribution.

## Defining Entity Classes

As described in the prior section, <span class="emphasis">*entity classes*</span> are combined key/value classes that are managed by entity bindings. In this example the `Part`, `Supplier` and `Shipment` classes are entity classes. These classes contain fields that are a union of the fields of the key and value classes that were defined earlier for each store.

In general, entity classes may be defined in any way desired by the application. The entity binding, which is also defined by the application, is responsible for mapping between key/value objects and entity objects.

The `Part`, `Supplier` and `Shipment` entity classes are defined below.

An important difference between the entity classes defined here and the key and value classes defined earlier is that the entity classes are not serializable (do not implement the <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/io/Serializable.html" class="ulink" target="_top">Serializable</a> interface). This is because the entity classes are not directly stored. The entity binding decomposes an entity object into key and value objects, and only the key and value objects are serialized for storage.

One advantage of using entities can already be seen in the `toString()` method of the classes below. These return debugging output for the combined key and value, and will be used later to create a listing of the database that is more readable than in the prior examples.

``` c
public class Part
{
    private String number;
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
```

``` c
public class Supplier
{
    private String number;
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
```

``` c
public class Shipment
{
    private String partNumber;
    private String supplierNumber;
    private int quantity;

    public Shipment(String partNumber, String supplierNumber, int quantity)
    {
        this.partNumber = partNumber;
        this.supplierNumber = supplierNumber;
        this.quantity = quantity;
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
