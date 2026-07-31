---
title: "Replication FAQ"
api-name: "Replication FAQ"
source: docs/programmer_reference/rep_faq.html
---
## Replication FAQ

1.  **Does Berkeley DB provide support for forwarding write queries from clients to masters?**

    No, it does not. In general this protocol is left entirely to the application. Note, there is no reason not to use the communications channels a Base API application establishes for replication support to forward database update messages to the master, since Berkeley DB does not require those channels to be used exclusively for replication messages. Replication Manager does not currently offer this service to the application.

2.  **Can I use replication to partition my environment across multiple sites?**

    No, this is not possible. All replicated databases must be equally shared by all environments in the replication group.

3.  **I'm running with replication but I don't see my databases on the client.**

    This problem may be the result of the application using absolute path names for its databases, and the pathnames are not valid on the client system.

4.  **How can I distinguish Berkeley DB messages from application messages?**

    There is no way to distinguish Berkeley DB messages from application-specific messages, nor does Berkeley DB offer any way to wrap application messages inside of Berkeley DB messages. Distributed applications exchanging their own messages should either enclose Berkeley DB messages in their own wrappers, or use separate network connections to send and receive Berkeley DB messages. The one exception to this rule is connection information for new sites; Berkeley DB offers a simple method for sites joining replication groups to send connection information to the other database environments in the group (see <a href="rep_newsite.md" class="xref" title="Connecting to a new site">Connecting to a new site</a> for more information).

5.  **How should I build my ****send** function?****

    This depends on the specifics of the application. One common way is to write the **rec** and **control** arguments' sizes and data to a socket connected to each remote site. On a fast, local area net, the simplest method is likely to be to construct broadcast messages. Each Berkeley DB message would be encapsulated inside an application specific message, with header information specifying the intended recipient(s) for the message. This will likely require a global numbering scheme, however, as the Berkeley DB library has to be able to send specific log records to clients apart from the general broadcast of new log records intended for all members of a replication group.

6.  **Does every one of my threads of control on the master have to set up its own connection to every client? And, does every one of my threads of control on the client have to set up its own connection to every master?**

    This is not always necessary. In the Berkeley DB replication model, any thread of control which modifies a database in the master environment must be prepared to send a message to the client environments, and any thread of control which delivers a message to a client environment must be prepared to send a message to the master. There are many ways in which these requirements can be satisfied.

    The simplest case is probably a single, multithreaded process running on the master and clients. The process running on the master would require a single write connection to each client and a single read connection from each client. A process running on each client would require a single read connection from the master and a single write connection to the master. Threads running in these processes on the master and clients would use the same network connections to pass messages back and forth.

    A common complication is when there are multiple processes running on the master and clients. A straight-forward solution is to increase the numbers of connections on the master — each process running on the master has its own write connection to each client. However, this requires only one additional connection for each possible client in the master process. The master environment still requires only a single read connection from each client (this can be done by allocating a separate thread of control which does nothing other than receive client messages and forward them into the database). Similarly, each client still only requires a single thread of control that receives master messages and forwards them into the database, and which also takes database messages and forwards them back to the master. This model requires the networking infrastructure support many-to-one writers-to-readers, of course.

    If the number of network connections is a problem in the multiprocess model, and inter-process communication on the system is inexpensive enough, an alternative is have a single process which communicates between the master and each client, and whenever a process' **send** function is called, the process passes the message to the communications process which is responsible for forwarding the message to the appropriate client. Alternatively, a broadcast mechanism will simplify the entire networking infrastructure, as processes will likely no longer have to maintain their own specific network connections.
