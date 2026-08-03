---
title: "BTree Configuration"
api-name: "BTree Configuration"
source: docs/gsg/CXX/btree.html
---
## BTree Configuration

<span class="sect2"> [Allowing Duplicate Records](btree.md#duplicateRecords) </span>

<span class="sect2"> [Setting Comparison Functions](btree.md#comparators) </span>

In going through the previous chapters in this book, you may notice that we touch on some topics that are specific to BTree, but we do not cover those topics in any real detail. In this section, we will discuss configuration issues that are unique to BTree.

Specifically, in this section we describe:

- Allowing duplicate records.

- Setting comparator callbacks.

### Allowing Duplicate Records

BTree databases can contain duplicate records. One record is considered to be a duplicate of another when both records use keys that compare as equal to one another.

By default, keys are compared using a lexicographical comparison, with shorter keys collating higher than longer keys. You can override this default using the `Db::set_bt_compare()` method. See the next section for details.

By default, DB databases do not allow duplicate records. As a result, any attempt to write a record that uses a key equal to a previously existing record results in the previously existing record being overwritten by the new record.

Allowing duplicate records is useful if you have a database that contains records keyed by a commonly occurring piece of information. It is frequently necessary to allow duplicate records for secondary databases.

For example, suppose your primary database contained records related to automobiles. You might in this case want to be able to find all the automobiles in the database that are of a particular color, so you would index on the color of the automobile. However, for any given color there will probably be multiple automobiles. Since the index is the secondary key, this means that multiple secondary database records will share the same key, and so the secondary database must support duplicate records.

#### Sorted Duplicates

Duplicate records can be stored in sorted or unsorted order. You can cause DB to automatically sort your duplicate records by specifying the `DB_DUPSORT` flag at database creation time.

If sorted duplicates are supported, then the sorting function specified on `Db::set_dup_compare()` is used to determine the location of the duplicate record in its duplicate set. If no such function is provided, then the default lexicographical comparison is used.

#### Unsorted Duplicates

For performance reasons, BTrees should always contain sorted records. (BTrees containing unsorted entries must potentially spend a great deal more time locating an entry than does a BTree that contains sorted entries). That said, DB provides support for suppressing automatic sorting of duplicate records because it may be that your application is inserting records that are already in a sorted order.

That is, if the database is configured to support unsorted duplicates, then the assumption is that your application will manually perform the sorting. In this event, expect to pay a significant performance penalty. Any time you place records into the database in a sort order not know to DB, you will pay a performance penalty

That said, this is how DB behaves when inserting records into a database that supports non-sorted duplicates:

- If your application simply adds a duplicate record using `Db::put()`, then the record is inserted at the end of its sorted duplicate set.

- If a cursor is used to put the duplicate record to the database, then the new record is placed in the duplicate set according to the flags that are provided on the `Dbc::put()` method. The relevant flags are:

  - `DB_AFTER`

    The data provided on the call to `Dbc::put()` is placed into the database as a duplicate record. The key used for this operation is the key used for the record to which the cursor currently refers. Any key provided on the call to `Dbc::put()` is therefore ignored.

    The duplicate record is inserted into the database immediately after the cursor's current position in the database.

    This flag is ignored if sorted duplicates are supported for the database.

  - `DB_BEFORE`

    Behaves the same as `DB_AFTER` except that the new record is inserted immediately before the cursor's current location in the database.

  - `DB_KEYFIRST`

    If the key provided on the call to `Dbc::put()` already exists in the database, and the database is configured to use duplicates without sorting, then the new record is inserted as the first entry in the appropriate duplicates list.

  - `DB_KEYLAST`

    Behaves identically to `DB_KEYFIRST` except that the new duplicate record is inserted as the last record in the duplicates list.

#### Configuring a Database to Support Duplicates

Duplicates support can only be configured at database creation time. You do this by specifying the appropriate flags to `Db::set_flags()` before the database is opened for the first time.

The flags that you can use are:

- `DB_DUP`

  The database supports non-sorted duplicate records.

- `DB_DUPSORT`

  The database supports sorted duplicate records. Note that this flag also sets the `DB_DUP` flag for you.

The following code fragment illustrates how to configure a database to support sorted duplicate records:

``` c
#include <db_cxx.h>
...

Db db(NULL, 0);
const char *file_name = "myd.db";

try {
    // Configure the database for sorted duplicates
    db.set_flags(DB_DUPSORT);

    // Now open the database
    db.open(NULL,       // Txn pointer
            file_name,  // File name
            NULL,       // Logical db name (unneeded)
            DB_BTREE,   // Database type (using btree)
            DB_CREATE,  // Open flags
            0);         // File mode. Using defaults
} catch(DbException &e) {
    db.err(e.get_errno(), "Database '%s' open failed.", file_name);
} catch(std::exception &e) {
    db.errx("Error opening database: %s : %s\n", file_name, e.what());
} 

...

try {
    db.close(0);
} catch(DbException &e) {
    db.err(e.get_errno(), "Database '%s' close failed.", file_name);
} catch(std::exception &e) {
    db.errx("Error closing database: %s : %s\n", file_name, e.what());
} 
```

### Setting Comparison Functions

By default, DB uses a lexicographical comparison function where shorter records collate before longer records. For the majority of cases, this comparison works well and you do not need to manage it in any way.

However, in some situations your application's performance can benefit from setting a custom comparison routine. You can do this either for database keys, or for the data if your database supports sorted duplicate records.

Some of the reasons why you may want to provide a custom sorting function are:

- Your database is keyed using strings and you want to provide some sort of language-sensitive ordering to that data. Doing so can help increase the locality of reference that allows your database to perform at its best.

- You are using a little-endian system (such as x86) and you are using integers as your database's keys. Berkeley DB stores keys as byte strings and little-endian integers do not sort well when viewed as byte strings. There are several solutions to this problem, one being to provide a custom comparison function. See <a href="http://download.oracle.com/docs/cd/E17076_02/html/programmer_reference/am_misc_faq.html" class="ulink" target="_top">http://download.oracle.com/docs/cd/E17076_02/html/programmer_reference/am_misc_faq.html</a> for more information.

- You you do not want the entire key to participate in the comparison, for whatever reason. In this case, you may want to provide a custom comparison function so that only the relevant bytes are examined.

#### Creating Comparison Functions

You set a BTree's key comparison function using `Db::set_bt_compare()`. You can also set a BTree's duplicate data comparison function using `Db::set_dup_compare()`.

You cannot use these methods after the database has been opened. Also, if the database already exists when it is opened, the function provided to these methods must be the same as that historically used to create the database or corruption can occur.

The value that you provide to the `set_bt_compare()` method is a pointer to a function that has the following signature:

``` c
int (*function)(Db *db, const Dbt *key1, const Dbt *key2)
```

This function must return an integer value less than, equal to, or greater than 0. If key1 is considered to be greater than key2, then the function must return a value that is greater than 0. If the two are equal, then the function must return 0, and if the first key is less than the second then the function must return a negative value.

The function that you provide to `set_dup_compare()` works in exactly the same way, except that the `Dbt` parameters hold record data items instead of keys.

For example, an example routine that is used to sort integer keys in the database is:

``` c
int
compare_int(Db *dbp, const Dbt *a, const Dbt *b)
{
    int ai, bi;

    // Returns: 
    // < 0 if a < b 
    // = 0 if a = b 
    // > 0 if a > b 
    memcpy(&ai, a->get_data(), sizeof(int)); 
    memcpy(&bi, b->get_data(), sizeof(int)); 
    return (ai - bi); 
} 
```

Note that the data must first be copied into memory that is appropriately aligned, as Berkeley DB does not guarantee any kind of alignment of the underlying data, including for comparison routines. When writing comparison routines, remember that databases created on machines of different architectures may have different integer byte orders, for which your code may need to compensate.

To cause DB to use this comparison function:

``` c
#include <db_cxx.h>
#include <string.h>

...

Db db(NULL, 0);

// Set up the btree comparison function for this database
db.set_bt_compare(compare_int);

// Database open call follows sometime after this.
```
