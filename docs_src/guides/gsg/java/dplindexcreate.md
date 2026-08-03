---
title: "Creating Indexes"
api-name: "Creating Indexes"
source: docs/gsg/JAVA/dplindexcreate.html
---
## Creating Indexes

<span class="sect2"> [Declaring a Primary Indexes](dplindexcreate.md#dplprimaryidxdecl) </span>

<span class="sect2"> [Declaring Secondary Indexes](dplindexcreate.md#dplsecondaryidxdecl) </span>

<span class="sect2"> [Foreign Key Constraints](dplindexcreate.md#foreignkey) </span>

To create an index using the DPL, you use Java annotations to declare which feature on the class is used for the primary index, and which features (if any) are to be used as secondary indexes.

All entity classes stored in the DPL must have a primary index declared for it.

Entity classes can have zero or more secondary indexes declared for them. There is no limit on the number of secondary indexes that you can declare.

### Declaring a Primary Indexes

You declare a primary key for an entity class by using the `@PrimaryKey` annotation. This annotation must appear immediately before the data member which represents the class's primary key. For example:

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

    ... 
```

For this class, the `vendor` value is set for an individual `Vendor` class object by the `setVendorName()` method. If our example code fails to set this value before storing the object, the data member used to store the primary key is set to a null value. This would result in a runtime error.

You can avoid the need to explicitly set a value for a class's primary index by specifying a sequence to be used for the primary key. This results in an unique integer value being used as the primary key for each stored object.

You declare a sequence is to be used by specifying the `sequence` keyword to the `@PrimaryKey` annotation. You must also provide a name for the sequence. For example: For example:

``` c
@PrimaryKey(sequence="Sequence_Namespace")
long myPrimaryKey; 
```

### Declaring Secondary Indexes

To declare a secondary index, we use the `@SecondaryKey` annotation. Note that when we do this, we must declare what sort of an index it is; that is, what is its relationship to other data in the data store.

The <span class="emphasis">*kind*</span> of indices that we can declare are:

- `ONE_TO_ONE`

  This relationship indicates that the secondary key is unique to the object. If an object is stored with a secondary key that already exists in the data store, a run time error is raised.

  For example, a person object might be stored with a primary key of a social security number (in the US), with a secondary key of the person's employee number. Both values are expected to be unique in the data store.

- `MANY_TO_ONE`

  Indicates that the secondary key may be used for multiple objects in the data store. That is, the key appears more than once, but for each stored object it can be used only once.

  Consider a data store that relates managers to employees. A given manager will have multiple employees, but each employee is assumed to have just one manager. In this case, the manager's employee number might be a secondary key, so that you can quickly locate all the objects related to that manager's employees.

- `ONE_TO_MANY`

  Indicates that the secondary key might be used more than once for a given object. Index keys themselves are assumed to be unique, but multiple instances of the index can be used per object.

  For example, employees might have multiple unique email addresses. In this case, any given object can be access by one or more email addresses. Each such address is unique in the data store, but each such address will relate to a single employee object.

- `MANY_TO_MANY`

  There can be multiple keys for any given object, and for any given key there can be many related objects.

  For example, suppose your organization has a shared resource, such as printers. You might want to track which printers a given employee can use (there might be more than one). You might also want to track which employees can use a specific printer. This represents a many-to-many relationship.

Note that for `ONE_TO_ONE` and `MANY_TO_ONE` relationships, you need a simple data member (not an array or collection) to hold the key. For `ONE_TO_MANY` and `MANY_TO_MANY` relationships, you need an array or collection to hold the keys:

``` c
@SecondaryKey(relate=ONE_TO_ONE)
private String primaryEmailAddress = new String();

@SecondaryKey(relate=ONE_TO_MANY)
private Set<String> emailAddresses = new HashSet<String>(); 
```

### Foreign Key Constraints

Sometimes a secondary index is related in some way to another entity class that is also contained in the data store. That is, the secondary key might be the primary key for another entity class. If this is the case, you can declare the foreign key constraint to make data integrity easier to accomplish.

For example, you might have one class that is used to represent employees. You might have another that is used to represent corporate divisions. When you add or modify an employee record, you might want to ensure that the division to which the employee belongs is known to the data store. You do this by specifying a foreign key constraint.

When a foreign key constraint is declared:

- When a new secondary key for the object is stored, it is checked to make sure it exists as a primary key for the related entity object. If it does not, a runtime error occurs.

- When a related entity is deleted (that is, a corporate division is removed from the data store), some action is automatically taken for the entities that refer to this object (that is, the employee objects). Exactly what that action is, is definable by you. See below.

When a related entity is deleted from the data store, one of the following actions are taken:

- `ABORT`

  The delete operation is not allowed. A runtime error is raised as a result of the operation. This is the default behavior.

- `CASCADE`

  All entities related to this one are deleted as well. For example, if you deleted a `Division` object, then all `Employee` objects that belonged to the division are also deleted.

- `NULLIFY`

  All entities related to the deleted entity are updated so that the pertinent data member is nullified. That is, if you deleted a division, then all employee objects related to that division would have their division key automatically set to null.

You declare a foreign key constraint by using the `relatedEntity` keyword. You declare the foreign key constraint deletion policy using the `onRelatedEntityDelete` keyword. For example, the following declares a foreign key constraint to `Division` class objects, and it causes related objects to be deleted if the `Division` class is deleted:

``` c
@SecondaryKey(relate=ONE_TO_ONE, relatedEntity=Division.class, 
    onRelatedEntityDelete=CASCADE)
private String division = new String(); 
```
