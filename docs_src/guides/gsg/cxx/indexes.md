---
title: "Chapter 5. Secondary Databases"
api-name: "Chapter 5. Secondary Databases"
source: docs/gsg/CXX/indexes.html
---
## Chapter 5. Secondary Databases

**Table of Contents**

<span class="sect1"> [Opening and Closing Secondary Databases](indexes.md#CoreDbAssociate) </span>

<span class="sect1"> [Implementing Key Extractors](keyCreator.md) </span>

<span class="sect2"> [Working with Multiple Keys](keyCreator.md#multikeys) </span>

<span class="sect1"> [Reading Secondary Databases](readSecondary.md) </span>

<span class="sect1"> [Deleting Secondary Database Records](secondaryDelete.md) </span>

<span class="sect1"> [Using Cursors with Secondary Databases](secondaryCursor.md) </span>

<span class="sect1"> [Database Joins](joins.md) </span>

<span class="sect2"> [Using Join Cursors](joins.md#joinUsage) </span>

<span class="sect1"> [Secondary Database Example](coreindexusage.md) </span>

<span class="sect2"> [Secondary Databases with example_database_load](coreindexusage.md#edlWIndexes) </span>

<span class="sect2"> [Secondary Databases with example_database_read](coreindexusage.md#edrWIndexes) </span>

Usually you find database records by means of the record's key. However, the key that you use for your record will not always contain the information required to provide you with rapid access to the data that you want to retrieve. For example, suppose your database contains records related to users. The key might be a string that is some unique identifier for the person, such as a user ID. Each record's data, however, would likely contain a complex object containing details about people such as names, addresses, phone numbers, and so forth. While your application may frequently want to query a person by user ID (that is, by the information stored in the key), it may also on occasion want to locate people by, say, their name.

Rather than iterate through all of the records in your database, examining each in turn for a given person's name, you create indexes based on names and then just search that index for the name that you want. You can do this using secondary databases. In DB, the database that contains your data is called a <span class="emphasis">*primary database*</span>. A database that provides an alternative set of keys to access that data is called a <span class="emphasis">*secondary database*</span>. In a secondary database, the keys are your alternative (or secondary) index, and the data corresponds to a primary record's key.

You create a secondary database by creating the database, opening it, and then <span class="emphasis">*associating*</span> the database with the <span class="emphasis">*primary*</span> database (that is, the database for which you are creating the index). As a part of associating the secondary database to the primary, you must provide a callback that is used to create the secondary database keys. Typically this callback creates a key based on data found in the primary database record's key or data.

Once opened, DB manages secondary databases for you. Adding or deleting records in your primary database causes DB to update the secondary as necessary. Further, changing a record's data in the primary database may cause DB to modify a record in the secondary, depending on whether the change forces a modification of a key in the secondary database.

Note that you can not write directly to a secondary database. Any attempt to write to a secondary database results in a non-zero status return. To change the data referenced by a secondary record, modify the primary database instead. The exception to this rule is that delete operations are allowed on the secondary database. See <a href="secondaryDelete.md" class="xref" title="Deleting Secondary Database Records">Deleting Secondary Database Records</a> for more information.

### Note

Secondary database records are updated/created by DB only if the key creator callback function returns `0`. If a value other than `0` is returned, then DB will not add the key to the secondary database, and in the event of a record update it will remove any existing key. Note that the callback can use either `DB_DONOTINDEX` or some error code outside of DB's name space to indicate that the entry should not be indexed.

See <a href="keyCreator.md" class="xref" title="Implementing Key Extractors">Implementing Key <span>Extractors</span></a> for more information.

When you read a record from a secondary database, DB automatically returns the data and optionally the key from the corresponding record in the primary database.

## Opening and Closing Secondary Databases

You manage secondary database opens and closes in the same way as you would any normal database. The only difference is that:

- You must associate the secondary to a primary database using `Db::associate()`.

- When closing your databases, it is a good idea to make sure you close your secondaries before closing your primaries. This is particularly true if your database closes are not single threaded.

When you associate a secondary to a primary database, you must provide a callback that is used to generate the secondary's keys. These callbacks are described in the next section.

For example, to open a secondary database and associate it to a primary database:

``` c
#include <db_cxx.h>

...

Db my_database(NULL, 0); // Primary
Db my_index(NULL, 0);    // Secondary

// Open the primary
my_database.open(NULL,       // Transaction pointer
                 "my_db.db", // On-disk file that holds the database.
                NULL,        // Optional logical database name
                DB_BTREE,    // Database access method
                DB_CREATE,   // Open flags
                0);          // File mode (using defaults)

// Setup the secondary to use sorted duplicates.
// This is often desirable for secondary databases.
my_index.set_flags(DB_DUPSORT);

// Open the secondary
my_index.open(NULL,              // Transaction pointer
              "my_secondary.db", // On-disk file that holds the database.
              NULL,              // Optional logical database name
              DB_BTREE,          // Database access method
              DB_CREATE,         // Open flags.
              0);                // File mode (using defaults)

// Now associate the primary and the secondary
my_database.associate(NULL,          // Txn id
                      &my_index,     // Associated secondary database
                      get_sales_rep, // Callback used for key extraction.
                                     // This is described in the next
                                     // section.
                      0);            // Flags 
```

Closing the primary and secondary databases is accomplished exactly as you would for any database:

``` c
// Close the secondary before the primary
my_index.close(0);
my_database.close(0);
```
