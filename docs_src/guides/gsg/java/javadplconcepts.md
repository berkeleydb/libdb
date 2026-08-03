---
title: "Berkeley DB Concepts"
api-name: "Berkeley DB Concepts"
source: docs/gsg/JAVA/javadplconcepts.html
---
## Berkeley DB Concepts

<span class="sect2"> [Environments](javadplconcepts.md#dplenvconcepts) </span>

<span class="sect2"> [Key-Data Pairs](javadplconcepts.md#key-data) </span>

<span class="sect2"> [Storing Data](javadplconcepts.md#storing-intro) </span>

<span class="sect2"> [Duplicate Data](javadplconcepts.md#duplicatesintro) </span>

<span class="sect2"> [Replacing and Deleting Entries](javadplconcepts.md#replacedeleteIntro) </span>

<span class="sect2"> [Secondary Keys](javadplconcepts.md#secondary) </span>

<span class="sect2"> [Which API Should You Use?](javadplconcepts.md#whichapi) </span>

Before continuing, it is useful to describe some of the concepts you will encounter when building a DB application.

The concepts that you will encounter depend upon the actual API that you are using. Some of these concepts are common to both APIs, and so we present those first. Others are only interesting if you use the DPL, while others apply only to the base API. We present each of these in turn.

### Environments

Environments are required for applications built using the DPL. They are optional, but very commonly used, for applications built using the base API. Therefore, it is worthwhile to begin with them.

An <span class="emphasis">*environment*</span> is essentially an encapsulation of one or more databases. You open an environment and then you open databases in that environment. When you do so, the databases are created/located in a location relative to the environment's home directory.

Environments offer a great many features that a stand-alone DB database cannot offer:

- Multi-database files.

  It is possible in DB to contain multiple databases in a single physical file on disk. This is desirable for those application that open more than a few handful of databases. However, in order to have more than one database contained in a single physical file, your application <span class="emphasis">*must*</span> use an environment.

- Multi-thread and multi-process support

  When you use an environment, resources such as the in-memory cache and locks can be shared by all of the databases opened in the environment. The environment allows you to enable subsystems that are designed to allow multiple threads and/or processes to access DB databases. For example, you use an environment to enable the concurrent data store (CDS), the locking subsystem, and/or the shared memory buffer pool.

- Transactional processing

  DB offers a transactional subsystem that allows for full ACID-protection of your database writes. You use environments to enable the transactional subsystem, and then subsequently to obtain transaction IDs.

- High availability (replication) support

  DB offers a replication subsystem that enables single-master database replication with multiple read-only copies of the replicated data. You use environments to enable and then manage this subsystem.

- Logging subsystem

  DB offers write-ahead logging for applications that want to obtain a high-degree of recoverability in the face of an application or system crash. Once enabled, the logging subsystem allows the application to perform two kinds of recovery ("normal" and "catastrophic") through the use of the information contained in the log files.

For more information on these topics, see the *Berkeley DB Getting Started with Transaction Processing* guide and the *Berkeley DB Getting Started with Replicated Applications* guide.

### Key-Data Pairs

DB stores and retrieves data using <span class="emphasis">*key-data pairs*</span>. The <span class="emphasis">*data*</span> portion of this is the data that you have decided to store in DB for future retrieval. The <span class="emphasis">*key*</span> is the information that you want to use to look up your stored data once it has been placed inside a DB database.

For example, if you were building a database that contained employee information, then the <span class="emphasis">*data*</span> portion is all of the information that you want to store about the employees: name, address, phone numbers, physical location, their manager, and so forth.

The <span class="emphasis">*key*</span>, however, is the way that you look up any given employee. You can have more than one key if you wish, but every record in your database must have a primary key. If you are using the DPL, then this key must be unique; that is, it must not be used multiple times in the database. However, if you are using the base API, then this requirement is relaxed. See <a href="javadplconcepts.md#duplicatesintro" class="xref" title="Duplicate Data">Duplicate Data</a> for more information.

For example, in the case of an employee database, you would probably use something like the employee identification number as the primary key as this uniquely identifies a given employee.

You can optionally also have secondary keys that represent indexes into your database. These keys do not have to be unique to a given record; in fact, they often are not. For example, you might set up the employee's manager's name as a secondary key so that it is easy to locate all the employee's that work for a given manager.

### Storing Data

How you manage your stored information differs significantly, depending on which API you are using. Both APIs ultimately are doing the same thing, but the DPL hides a lot of the details from you.

#### Storing Data in the DPL

The DPL is used to store Java objects in an underlying series of databases. These databases are accessed using an `EntityStore` class object.

To use the DPL, you must decorate the classes you want to store with Java annotations that identify them as either an <span class="emphasis">*entity class*</span> or a <span class="emphasis">*persistent class*</span>.

Entity classes are classes that have a primary key, and optionally one or more secondary keys. That is, these are the classes that you will save and retrieve directly using the DPL. You identify an entity class using the `@Entity` java annotation.

Persistent classes are classes used by entity classes. They do not have primary or secondary indices used for object retrieval. Rather, they are stored or retrieved when an entity class makes direct use of them. You identify an persistent class using the `@Persistent` java annotation.

The primary key for an object is obtained from one of the class' data members. You identify which data member to use as the primary key using the `@PrimaryKey` java annotation.

Note that all non-transient instance fields of a persistent class, as well as its superclasses and subclasses, are persistent. Static and transient fields are not persistent. The persistent fields of a class may be private, package-private (default access), protected or public.

Also, simple Java types, such as `java.lang.String` and `java.util.Date`, are automatically handled as a persistent class when you use them in an entity class; you do not have to do anything special to cause these simple Java objects to be stored in the `EntityStore`.

#### Storing Data using the Base API

When you are not using the DPL, both record keys and record data must be byte arrays and are passed to and returned from DB using `DatabaseEntry` instances. `DatabaseEntry` only supports storage of Java byte arrays. Complex objects must be marshaled using either Java serialization, or more efficiently with the bind APIs provided with DB

Database records and `byte` array conversion are described in <a href="DBEntry.md" class="xref" title="Chapter 8. Database Records">Database Records</a>.

You store records in a `Database` by calling one of the put methods on a `Database` handle. DB automatically determines the record's proper placement in the database's internal B-Tree using whatever key and data comparison functions that are available to it.

You can also retrieve, or get, records using the `Database` handle. Gets are performed by providing the key (and sometimes also the data) of the record that you want to retrieve.

You can also use cursors for database puts and gets. Cursors are essentially a mechanism by which you can iterate over the records in the database. Like databases and database environments, cursors must be opened and closed. Cursors are managed using the `Cursor` class.

Databases are described in <a href="databases.md" class="xref" title="Chapter 7. Databases">Databases</a>. Cursors are described in <a href="Cursors.md" class="xref" title="Chapter 9. Using Cursors">Using Cursors</a>.

### Duplicate Data

If you are using the base API, then at creation time databases can be configured to allow duplicate data. Remember that DB database records consist of a key/data pair. <span class="emphasis">*Duplicate data*</span>, then, occurs when two or more records have identical keys, but different data. By default, a `Database` does not allow duplicate data.

If your `Database ` contains duplicate data, then a simple database get based only on a key returns just the first record that uses that key. To access all duplicate records for that key, you must use a cursor.

If you are using the DPL, then you can duplicate date using secondary keys, but not by using the primary key. For more information, see <a href="getmultiple.md" class="xref" title="Retrieving Multiple Objects">Retrieving Multiple Objects</a>.

### Replacing and Deleting Entries

If you are using the DPL, then replacing a stored entity object simply consists of retrieving it, updating it, then storing it again. To delete the object, use the `delete()` method that is available on either its primary or secondary keys. If you use the `delete()` method available on the secondary key, then all objects referenced by that key are also deleted. See <a href="dpl_delete.md" class="xref" title="Deleting Entity Objects">Deleting Entity Objects</a> for more information.

If you are using the base API, then how you replace database records depends on whether duplicate data is allowed in the database.

If duplicate data is not allowed in the database, then simply calling `Database.put()` with the appropriate key will cause any existing record to be updated with the new data. Similarly, you can delete a record by providing the appropriate key to the `Database.delete()` method.

If duplicate data is allowed in the database, then you must position a cursor to the record that you want to update, and then perform the put operation using the cursor.

To delete records using the base API, you can use either `Database.delete()` or `Cursor.delete()`. If duplicate data is not allowed in your database, then these two method behave identically. However, if duplicates are allowed in the database, then `Database.delete()` deletes every record that uses the provided key, while `Cursor.delete()` deletes just the record at which the cursor is currently positioned.

### Secondary Keys

Secondary keys provide an alternative way to locate information stored in DB, beyond that which is provided by the primary key. Frequently secondary keys refer to more than one record in the database. In this way, you can find all the cars that are green (if you are maintaining an automotive database) or all the people with brown eyes (if you are maintaining a database about people). In other words, secondary keys represent a index into your data.

How you create and maintain secondary keys differs significantly, depending on whether you are using the DPL or the base API.

#### Using Secondaries with the DPL

Under the DPL, you declare a particular field to be a secondary key by using the `@SecondaryKey` annotation. When you do this, you must declare what kind of an index you are creating. For example, you can declare a secondary key to be part of a `ONE_TO_ONE` index, in which case the key is unique to the object. Or you could declare the key to be `MANY_TO_ONE`, in which case the key can be used for multiple objects in the data store.

Once you have identified secondary keys for a class, you can access those keys by using the `EntityStore.getSecondaryIndex()` method.

For more information, see <a href="dplindexcreate.md#dplsecondaryidxdecl" class="xref" title="Declaring Secondary Indexes">Declaring Secondary Indexes</a>.

#### Using Secondaries with the Base API.

When you are using the base API, you create and maintain secondary keys using a special type of a database, called a <span class="emphasis">*secondary database*</span>. When you are using secondary databases, the database that holds the data you are indexing is called the <span class="emphasis">*primary database*</span>.

You create a secondary database by opening it and associating it with an existing primary database. You must also provide a class that generates the secondary's keys (that is, the index) from primary records. Whenever a record in the primary database is added or changed, DB uses this class to determine what the secondary key should be.

When a primary record is created, modified, or deleted, DB automatically updates the secondary database(s) for you as is appropriate for the operation performed on the primary.

You manage secondary databases using the `SecondaryDatabase` class. You identify how to create keys for your secondary databases by supplying an instance of a class that implements the `SecondaryKeyCreator` interface.

Secondary databases are described in <a href="indexes.md" class="xref" title="Chapter 10. Secondary Databases">Secondary Databases</a>.

### Which API Should You Use?

Of the two APIs that DB makes available to you, we recommend that you use the DPL if all you want to do is make classes with a relatively static schema to be persistent. However, the DPL requires Java 1.5, so if you want to use Java 1.4 then you cannot use the DPL.

Further, if you are porting an application between the C or C++ versions of DB and the Java version of this API, then you should not use the DPL as the base API is a much closer match to the other languages available for use with DB.

Additionally, if your application uses a highly dynamic schema, then the DPL is probably a poor choice for your application, although the use of Java annotations can make the DPL work a little better for you in this situation.
