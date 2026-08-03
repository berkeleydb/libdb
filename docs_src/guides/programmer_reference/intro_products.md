---
title: "The Berkeley DB products"
api-name: "The Berkeley DB products"
source: docs/programmer_reference/intro_products.html
---
## The Berkeley DB products

<span class="sect2"> [Berkeley DB Data Store](intro_products.md#idp50715960) </span>

<span class="sect2"> [Berkeley DB Concurrent Data Store](intro_products.md#idp50715552) </span>

<span class="sect2"> [Berkeley DB Transactional Data Store](intro_products.md#idp50708368) </span>

<span class="sect2"> [Berkeley DB High Availability](intro_products.md#idp50712672) </span>

Oracle provides four Berkeley DB products, each differing by the level of database support that they offer.

- Berkeley DB Data Store
- Berkeley DB Concurrent Data Store
- Berkeley DB Transactional Data Store
- Berkeley DB High Availability

Each product provides additional functionality to the product that precedes it in the list. As a result, you can download Berkeley DB and build an application that provides read-only database access for a single-user, and later add support for more complex database access patterns for multiple users.

The single Open Source distribution of Berkeley DB from Oracle includes the four products and building the distribution automatically builds all four products. However, you must use the same Berkeley DB product throughout an application or group of applications.

To redistribute Berkeley DB software, you must have a license for the Berkeley DB product you use. For further details, refer to the licensing informaion at: <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/index.html" class="ulink" target="_top">http://www.oracle.com/technetwork/database/berkeleydb/downloads/index.html</a>

A comparison of the four Berkeley DB product features is provided in the following table.

|   | Berkeley DB Data Store | Berkeley DB Concurrent Data Store | Berkeley DB Transactional Data Store | Berkeley DB High Availability |
|:---|:---|:---|:---|:---|
| What is this product? | Provides indexed, single-reader/single-writer embedded data storage | Adds simple locking with multiple-reader/single-writer capabilities | Adds complete ACID transaction support, as well as recovery | Adds single-master data replication across multiple physical machines |
| Ensures recovery operation | No | No | Yes | Yes |
| Provides Locking feature | No | Yes | Yes | Yes |
| Provides concurrent read-write access | No | Yes | Yes | Yes |
| Provides transactional support | No | No | Yes | Yes |
| Supports SQL access | No | No | Yes | No |
| Provides replication support | No | No | No | Yes |

### Berkeley DB Data Store

The Berkeley DB Data Store product is an embeddable, high-performance data store. This product supports multiple concurrent threads of control, including multiple processes and multiple threads of control within a process. However, Berkeley DB Data Store does not support locking, and hence does not guarantee correct behavior if more than one thread of control is updating the database at a time. The Berkeley DB Data Store is intended for use in read-only applications or applications which can guarantee no more than one thread of control updates the database at a time.

### Berkeley DB Concurrent Data Store

The Berkeley DB Concurrent Data Store product adds multiple-reader, single writer capabilities to the Berkeley DB Data Store product. This product provides built-in concurrency and locking feature. Berkeley DB Concurrent Data Store is intended for applications that need support for concurrent updates to a database that is largely used for reading.

### Berkeley DB Transactional Data Store

The Berkeley DB Transactional Data Store product adds support for transactions and database recovery. Berkeley DB Transactional Data Store is intended for applications that require industrial-strength database services, including excellent performance under high-concurrency workloads of read and write operations, the ability to commit or roll back multiple changes to the database at a single instant, and the guarantee that in the event of a catastrophic system or hardware failure, all committed database changes are preserved.

### Berkeley DB High Availability

The Berkeley DB High Availability product adds support for data replication. A single master system handles all updates, and distributes these updates to multiple replicas. The number of replicas depends on the application requirements. All replicas can handle read requests during normal processing. If the master system fails for any reason, one of the replicas takes over as the new master system, and distributes updates to the remaining replicas.
