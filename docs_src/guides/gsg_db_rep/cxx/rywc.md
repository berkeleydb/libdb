---
title: "Read-Your-Writes Consistency"
api-name: "Read-Your-Writes Consistency"
source: docs/gsg_db_rep/CXX/rywc.html
---
## Read-Your-Writes Consistency

In a distributed system, the changes made at the master are not always instantaneously available at every replica, although they eventually will be. In general, replicas not directly involved in contributing to the acknowledgement of a transaction commit will lag behind other replicas because they do not synchronize their commits with the master.

For this reason, you might want to make use of the read-your-writes consistency feature. This feature allows you to ensure that a replica is at least current enough to have the changes made by a specific transaction. Because transactions are applied serially, by ensuring a replica has a specific commit applied to it, you know that all transaction commits occurring prior to the specified transaction have also been applied to the replica.

You determine whether a transaction has been applied to a replica by generating a <span class="emphasis">*commit token*</span> at the master. You then transfer this commit token to the replica, where it is used to determine whether the replica is consistent enough relative to the master.

For example, suppose the you have a web application where a replication group is implemented within a load balanced web server group. Each request to the web server consists of an update operation followed by read operations (say, from the same client), The read operations naturally expect to see the data from the updates executed by the same request. However, the read operations might have been routed to a replica that did not execute the update.

In such a case, the update request would generate a commit token, which would be resubmitted by the browser, along with subsequent read requests. The read request could be directed at any one of the available web servers by a load balancer. The replica which services the read request would use that commit token to determine whether it can service the read operation. If the replica is current enough, it can immediately execute the transaction and satisfy the request.

What action the replica takes if it is not consistent enough to service the read request is up to you as the application developer. You can do anything from blocking while you wait for the transaction to be applied locally, to rejecting the read request outright.

For more information, see the `Read your writes consistency` section in the `Berkeley DB Replication` chapter of the *Berkeley DB Programmer's Reference Guide*.
