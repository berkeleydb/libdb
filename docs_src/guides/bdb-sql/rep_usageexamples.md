---
title: "Replication Usage Examples"
api-name: "Replication Usage Examples"
source: docs/bdb-sql/rep_usageexamples.html
---
## Replication Usage Examples

<span class="sect2"> [Example 1: Distributed Read at 3 Sites](rep_usageexamples.md#rep_ex1) </span>

<span class="sect2"> [Example 2: 2-Site Failover](rep_usageexamples.md#rep_ex2) </span>

In this section we provide two examples of using replication with BDB SQL. The first example shows a typical startup process. The second demonstrates master site failover.

### Example 1: Distributed Read at 3 Sites

This example shows how a typical replication group startup sequence is performed. It initially populates the master, then starts two replicas so that read operations can be distributed. Then it shows what happens when an attempt is made to write to a replica.

Site 1:

``` c
# Start initial master.
# univ.db does not yet exist.
dbsql univ.db
     pragma replication_local_site="site1:7000";
     pragma replication_initial_master=ON;
     pragma replication=ON;

    # Create and populate university and country tables.
    .read university.sql 
```

Site 2:

``` c
# Start first replica.
# univ.db does not yet exist.
dbsql univ.db  
     pragma replication_local_site="site2:7001";
     pragma replication_remote_site="site1:7000";
     pragma replication=ON; 
```

Site 3:

``` c
# Start second replica.
# univ.db does not yet exist.
dbsql univ.db
     pragma replication_local_site="site3:7002";
     pragma replication_remote_site="site1:7000";
     pragma replication=ON; 
```

Site 1:

``` c
    # Perform some writes and reads on master.
    insert into country values ("Greenland","gl", 0, 0, 0, 2);
    insert into university values (26, "University College London",
        "ucl.edu", "uk", "Europe", 18, 39, 47, 30);
    select * from country where abbr = "gl";
    update country set top_1000 = 1 where abbr = "gl"; 
```

Site 2:

``` c
 # Perform some reads on first replica.
    select * from university where region = "Europe";
    select count(*) from country where top_100 > 0;

    # Attempt to write on first replica. 
    insert into country values ("Antarctica","an", 0, 0, 0, 0);
    .../univ.db: DBcursor->put: attempt to modify a read-only database
    Error: attempt to write to a readonly database
```

### Example 2: 2-Site Failover

This example demonstrates failover of the master from one site to another. It shows how a failed site can rejoin the replication group, and it shows that there is a window of time during which write operations cannot be performed for the replication group. Finally, it shows how to check the master's location.

Site 1:

``` c
# Start initial master.
# quote.db does not yet exist.
dbsql quote.db
     pragma replication_local_site="site1:7000";
     pragma replication_initial_master=ON;
     pragma replication=ON;

    # Create stock quote application table.
    create table stock_quote (company_name text(40), price real);
```

Site 2:

``` c
# Start replica.
# quote.db does not yet exist.
dbsql quote.db 
     pragma replication_local_site="site2:7001";
     pragma replication_remote_site="site1:7000";
     pragma replication=ON; 
```

Site 1:

``` c
    # Perform some writes on master.
    insert into stock_quote values ("General Electric", 20.25);
    insert into stock_quote values ("Nabisco", 24.75);
    insert into stock_quote values ("United Healthcare", 31.00);
    update stock_quote set price=25.25 where company_name = "Nabisco";
```

Site 2:

``` c
    # Perform some reads on replica.
    select * from stock_quote where price < 30.00;
    select price from stock_quote where 
           company_name = "General Electric";
```

Site 1:

``` c
    # Stop the initial master.
    .exit 
```

Site 2:

``` c
    ##########
    ### Now the remaining site does not accept write operations until
    ### the other site rejoins the replication group.
    ##########
    insert into stock_quote values ("Prudential", 17.25);
    .../quote.db: DBcursor->put: attempt to modify a read-only database
    Error: attempt to write to a readonly database
```

Site 1:

``` c
# Restart site, will rejoin replication group.
dbsql quote.db
    # The earlier replication=ON causes replication to be 
    # automatically started.  This site may or may not become master
    # after rejoining replication group.  Check status of site's 
    # startup and determine whether it is a master or a replica.
    .stat :rep:
    Replication summary statistics
    Environment configured as a replication master
    1/49056 Maximum permanent LSN
    2       Number of environments in the replication group
    0       Number of failed message sends
    0       Number of messages ignored due to pending recovery
    0       Number of log records currently queued

    # Assuming this site became master, perform some writes.
    # If this site is not the master, these writes will not
    # succeed and must be performed at the other site.
    insert into stock_quote values ("Raytheon", 9.25);
    insert into stock_quote values ("Cadbury", 7.75); 
```

Site 2:

``` c
    # Read operations can be performed on master or replica 
    # site.
    select * from stock_quote where price < 21.00; 
```
