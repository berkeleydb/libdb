---
title: "Tutorial Introduction"
api-name: "Tutorial Introduction"
source: docs/collections/tutorial/tutorialintroduction.html
---
## Tutorial Introduction

Most of the remainder of this document illustrates the use of the DB Java Collections API by presenting a tutorial that describes usage of the API. This tutorial builds a shipment database, a familiar example from classic database texts.

The examples illustrate the following concepts of the DB Java Collections API:

- Object-to-data <span class="emphasis">*bindings*</span>

- The database <span class="emphasis">*environment*</span>

- <span class="emphasis">*Databases*</span> that contain key/value records

- <span class="emphasis">*Secondary index*</span> databases that contain index keys

- Java <span class="emphasis">*collections*</span> for accessing databases and indices

- <span class="emphasis">*Transactions*</span> used to commit or undo database changes

The examples build on each other, but at the same time the source code for each example stands alone.

- <a href="BasicProgram.md" class="xref" title="Chapter 2.  The Basic Program">The Basic Program</a>

- <a href="UsingSecondaries.md" class="xref" title="Chapter 3.  Using Secondary Indices">Using Secondary Indices</a>

- <a href="Entity.md" class="xref" title="Chapter 4.  Using Entity Classes">Using Entity Classes</a>

- <a href="Tuple.md" class="xref" title="Chapter 5.  Using Tuples">Using Tuples</a>

- <a href="SerializableEntity.md" class="xref" title="Chapter 6.  Using Serializable Entities">Using Serializable Entities</a>

The shipment database consists of three database stores: the part store, the supplier store, and the shipment store. Each store contains a number of records, and each record consists of a key and a value.

| Store    | Key                          | Value                     |
|----------|------------------------------|---------------------------|
| Part     | Part Number                  | Name, Color, Weight, City |
| Supplier | Supplier Number              | Name, Status, City        |
| Shipment | Part Number, Supplier Number | Quantity                  |

In the example programs, Java classes containing the fields above are defined for the key and value of each store: `PartKey`, `PartData`, `SupplierKey`, `SupplierData`, `ShipmentKey` and `ShipmentData`. In addition, because the Part's Weight field is itself composed of two fields — the weight value and the unit of measure — it is represented by a separate `Weight` class. These classes will be defined in the first example program.

In general the DB Java Collections API uses bindings to describe how Java objects are stored. A binding defines the stored data syntax and the mapping between a Java object and the stored data. The example programs show how to create different types of bindings, and explains the characteristics of each type.

The following tables show the record values that are used in all the example programs in the tutorial.

| Number | Name  | Color | Weight     | City   |
|--------|-------|-------|------------|--------|
| P1     | Nut   | Red   | 12.0 grams | London |
| P2     | Bolt  | Green | 17.0 grams | Paris  |
| P3     | Screw | Blue  | 17.0 grams | Rome   |
| P4     | Screw | Red   | 14.0 grams | London |
| P5     | Cam   | Blue  | 12.0 grams | Paris  |
| P6     | Cog   | Red   | 19.0 grams | London |

| Number | Name  | Status | City   |
|--------|-------|--------|--------|
| S1     | Smith | 20     | London |
| S2     | Jones | 10     | Paris  |
| S3     | Blake | 30     | Paris  |
| S4     | Clark | 20     | London |
| S5     | Adams | 30     | Athens |

| Part Number | Supplier Number | Quantity |
|-------------|-----------------|----------|
| P1          | S1              | 300      |
| P1          | S2              | 300      |
| P2          | S1              | 200      |
| P2          | S2              | 400      |
| P2          | S3              | 200      |
| P2          | S4              | 200      |
| P3          | S1              | 400      |
| P4          | S1              | 200      |
| P4          | S4              | 300      |
| P5          | S1              | 100      |
| P5          | S4              | 400      |
| P6          | S1              | 100      |
