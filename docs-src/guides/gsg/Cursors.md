---
title: "Chapter 4. Using Cursors"
api-name: "Chapter 4. Using Cursors"
source: docs/gsg/C/Cursors.html
---
## Chapter 4. Using Cursors

**Table of Contents**

<span class="sect1"> [Opening and Closing Cursors](Cursors.md#openCursor) </span>

<span class="sect1"> [Getting Records Using the Cursor](Positioning.md) </span>

<span class="sect2"> [Searching for Records](Positioning.md#cursorsearch) </span>

<span class="sect2"> [Working with Duplicate Records](Positioning.md#getdups) </span>

<span class="sect1"> [Putting Records Using Cursors](PutEntryWCursor.md) </span>

<span class="sect1"> [Deleting Records Using Cursors](DeleteEntryWCursor.md) </span>

<span class="sect1"> [Replacing Records Using Cursors](ReplacingEntryWCursor.md) </span>

<span class="sect1"> [Cursor Example](CoreCursorUsage.md) </span>

Cursors provide a mechanism by which you can iterate over the records in a database. Using cursors, you can get, put, and delete database records. If a database allows duplicate records, then cursors are the easiest way that you can access anything other than the first record for a given key.

This chapter introduces cursors. It explains how to open and close them, how to use them to modify databases, and how to use them with duplicate records.

## Opening and Closing Cursors

Cursors are managed using the `DBC` structure. To use a cursor, you must open it using the `DB->cursor()` method.

For example:

``` c
#include <db.h>

...

DB *my_database;
DBC *cursorp;

/* Database open omitted for clarity */

/* Get a cursor */
my_database->cursor(my_database, NULL, &cursorp, 0); 
```

When you are done with the cursor, you should close it. To close a cursor, call the `DBC->close()` method. Note that closing your database while cursors are still opened within the scope of the DB handle, especially if those cursors are writing to the database, can have unpredictable results. It is recommended that you close all cursor handles after their use to ensure concurrency and to release resources such as page locks.

``` c
#include <db.h>

...

DB *my_database;
DBC *cursorp;

/* Database and cursor open omitted for clarity */

if (cursorp != NULL) 
    cursorp->close(cursorp); 

if (my_database != NULL) 
    my_database->close(my_database, 0); 
```
