---
title: "Replication Benefits"
api-name: "Replication Benefits"
source: docs/gsg_db_rep/CXX/repadvantage.html
---
## Replication Benefits

Replication offers your application a number of benefits that can be a tremendous help. Primarily replication's benefits revolve around performance, but there is also a benefit in terms of data durability guarantees.

Briefly, the reasons why you might choose to implement replication in your DB application are:

- Improve application reliability.

  By spreading your data across multiple machines, you can ensure that your application's data continues to be available even in the event of a hardware failure on any given machine in the replication group.

- Improve read performance.

  By using replication you can spread data reads across multiple machines on your network. Doing so allows you to vastly improve your application's read performance. This strategy might be particularly interesting for applications that have readers on remote network nodes; you can push your data to the network's edges thereby improving application data read responsiveness.

  Additionally, depending on the portion of your data that you read on a given replica, that replica may need to cache part of your data, decreasing cache misses and reducing I/O on the replica.

- Improve transactional commit performance

  In order to commit a transaction and achieve a transactional durability guarantee, the commit must be made <span class="emphasis">*durable*</span>. That is, the commit must be written to disk (usually, but not always, synchronously) before the application's thread of control can continue operations.

  Replication allows you to avoid this disk I/O and still maintain a degree of durability by <span class="emphasis">*committing to the network*</span>. In other words, you relax your transactional durability guarantees on the master, but by virtue of replicating the data across the network you gain some additional durability guarantees above what is provided locally.

  Usually this strategy is implemented using some form of an asynchronous transactional commit on the master. In this way your data writes will eventually be written to disk, but your application will not have to wait for the disk I/O to complete before continuing with its next operation.

  Note that it is possible to cause DB's replication implementation to wait to hear from one or more replicas as to whether they have successfully saved the write before continuing. However, in this case you might be trading performance for a even higher durability guarantee (see below).

- Improve data durability guarantee.

  In a traditional transactional application, you commit your transactions such that data modifications are saved to disk. Beyond this, the durability of your data is dependent upon the backup strategy that you choose to implement for your site.

  Replication allows you to increase this durability guarantee by ensuring that data modifications are written to multiple machines. This means that multiple disks, disk controllers, power supplies, and CPUs are used to ensure that your data modification makes it to stable storage. In other words, replication allows you to minimize the problem of a single point of failure by using more hardware to guarantee your data writes.

  If you are using replication for this reason, then you probably will want to configure your application such that it waits to hear about a successful commit from one or more replicas before continuing with the next operation. This will obviously impact your application's write performance to some degree — with the performance penalty being largely dependent upon the speed and stability of the network connecting your replication group.

  For more information, see <a href="fwrkpermmessage.md" class="xref" title="Permanent Message Handling">Permanent Message Handling</a>.
