---
title: "Foreign key indexes"
api-name: "Foreign key indexes"
source: docs/programmer_reference/am_foreign.html
---
## Foreign key indexes

Foreign keys are used to ensure a level of consistency between two different databases in terms of the keys that the databases use. In a foreign key relationship, one database is the <span class="emphasis">*constrained*</span> database. This database is actually a secondary database which is associated with a primary database. The other database in this relationship is the <span class="emphasis">*foreign key*</span> database. Once this relationship has been established between a constrained database and a foreign key database, then:

1.  Key/data items cannot be added to the constrained database unless that same key already exists in the foreign key database.

2.  A key/data pair cannot be deleted from the foreign key database unless some action is also taken to keep the constrained database consistent with the foreign key database.

Because the constrained database is a secondary database, by ensuring it's consistency with a foreign key database you are actually ensuring that a primary database (the one to which the secondary database is associated) is consistent with the foreign key database.

Deletions of keys in the foreign key database affect the constrained database in one of three ways, as specified by the application:

- `Abort`

  The deletion of a record from the foreign database will not proceed if that key exists in the constrained primary database. Transactions must be used to prevent the aborted delete from corrupting either of the databases.

- `Cascade`

  The deletion of a record from the foreign database will also cause any records in the constrained primary database that use that key to also be automatically deleted.

- `Nullify`

  The deletion of a record from the foreign database will cause a user specified callback function to be called, in order to alter or nullify any records using that key in the constrained primary database.

Note that it is possible to delete a key from the constrained database, but not from the foreign key database. For this reason, if you want the keys used in both databases to be 100% accurate, then you will have to write code to ensure that when a key is removed from the constrained database, it is also removed from the foreign key database.

As an example of how foreign key indexes might be used, consider a database of customer information and a database of order information. A typical customer database would use a customer ID as the key and those keys would also appear in the order database. To ensure an order is not booked for a non-existent customer, the customer database can be associated with the order database as a foreign index.

In order to do this, you create a secondary index of the order database, which uses customer IDs as the key for its key/data pairs. This secondary index is, then, the constrained database. But because the secondary index is constrained, so too is the order database because the contents of the secondary index are programmatically tied to the contents of the order database.

The customer database, then, is the foreign key database. It is associated to the order database's secondary index using the <a href="../../api/c/dbassociate_foreign.md" class="olink">DB-&gt;associate_foreign()</a> method. In this way, an order cannot be added to the order database unless the customer ID already exists in the customer database.

Note that this relationship can also be configured to delete any outstanding orders for a customer when that customer is deleted from the customer database.

In SQL, this would be done by executing something like the following:

``` c
CREATE TABLE customers(cust_id CHAR(4) NOT NULL,
       lastname CHAR(15), firstname CHAR(15), PRIMARY KEY(cust_id));
CREATE TABLE orders(order_id CHAR(4) NOT NULL, order_num int NOT NULL,
       cust_id CHAR(4), PRIMARY KEY (order_id),
       FOREIGN KEY (cust_id) REFERENCES customers(cust_id) 
       ON DELETE CASCADE);
```

In Berkeley DB, this would work as follows:

``` c
struct customer {
       char cust_id[4];
       char last_name[15];
       char first_name[15];
};
struct order {
       char order_id[4];
       int order_number;
       char cust_id[4];
};

....

void
foreign()
{
       DB *dbp, *sdbp, *fdbp;
       int ret;

       /* Open/create order database */
       if ((ret = db_create(&dbp, dbenv, 0)) != 0)
               handle_error(ret);
       if ((ret = dbp->open(dbp, NULL,
           "orders.db", NULL, DB_BTREE, DB_CREATE, 0600)) != 0)
               handle_error(ret);

       /*
        * Open/create secondary index on customer id.  Note that it
        * supports duplicates because a customer may have multiple
        * orders.
        */
       if ((ret = db_create(&sdbp, dbenv, 0)) != 0)
               handle_error(ret);
       if ((ret = sdbp->set_flags(sdbp, DB_DUP | DB_DUPSORT)) != 0)
               handle_error(ret);
       if ((ret = sdbp->open(sdbp, NULL, "orders_cust_ids.db",
           NULL, DB_BTREE, DB_CREATE, 0600)) != 0)
               handle_error(ret);

       /* Associate the secondary with the primary. */
       if ((ret = dbp->associate(dbp, NULL, sdbp, getcustid, 0)) != 0)
               handle_error(ret);

       /* Open/create customer database */
       if ((ret = db_create(&fdbp, dbenv, 0)) != 0)
               handle_error(ret);
       if ((ret = fdbp->open(fdbp, NULL,
           "customers.db", NULL, DB_BTREE, DB_CREATE, 0600)) != 0)
               handle_error(ret);

       /* Associate the foreign with the secondary. */
       if ((ret = fdbp->associate_foreign(
           fdbp, sdbp, NULL, DB_FOREIGN_CASCADE)) != 0)
               handle_error(ret);

}

/*
* getcustid -- extracts a secondary key (the customer id) from a primary
*     key/data pair
*/
int
getcustid(secondary, pkey, pdata, skey)
       DB *secondary;
       const DBT *pkey, *pdata;
       DBT *skey;
{
       /*
        * Since the secondary key is a simple structure member of the
        * record, we don't have to do anything fancy to return it.  If
        * we have composite keys that need to be constructed from the
        * record, rather than simply pointing into it, then the user's
        * function might need to allocate space and copy data.  In
        * this case, the DB_DBT_APPMALLOC flag should be set in the
        * secondary key DBT.
        */
       memset(skey, 0, sizeof(DBT));
       skey->data = ((struct order *)pdata->data)->cust_id;
       skey->size = 4;
       return (0);
}
```
