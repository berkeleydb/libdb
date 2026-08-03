---
title: "Ex_rep_base: putting it all together"
api-name: "Ex_rep_base: putting it all together"
source: docs/programmer_reference/rep_ex_rq.html
---
## Ex_rep_base: putting it all together

Beyond simply initializing a replicated environment, a Base API application must set up its communication infrastructure, and then make sure that incoming messages are received and processed.

To initialize replication, ex_rep_base creates a Berkeley DB environment and calls <a href="../../api/c/reptransport.md" class="olink">DB_ENV-&gt;rep_set_transport()</a> to establish a send function. (See the main function in `ex_rep/base/rep_base.c`, including its calls to the create_env and env_init functions in `ex_rep/common/rep_common.c`.)

ex_rep_base opens a listening socket for incoming connections and opens an outgoing connection to every machine that it knows about (that is, all the sites listed in the **-r** and **-R** command line arguments). Applications can structure the details of this in different ways, but ex_rep_base creates a user-level thread to listen on its socket, plus a thread to loop and handle messages on each socket, in addition to the threads needed to manage the user interface, update the database on the master, and read from the database on the client (in other words, in addition to the normal functionality of any database application).

Once the initial threads have all been started and the communications infrastructure is initialized, the application signals that it is ready for replication and joins a replication group by calling <a href="../../api/c/repstart.md" class="olink">DB_ENV-&gt;rep_start()</a>. (Again, see the main function in `ex_rep/base/rep_base.c`.)

Note the use of the optional second argument to <a href="../../api/c/repstart.md" class="olink">DB_ENV-&gt;rep_start()</a> in the client initialization code. The argument "local" is a piece of data, opaque to Berkeley DB, that will be broadcast to each member of a replication group; it allows new clients to join a replication group, without knowing the location of all its members; the new client will be contacted by the members it does not know about, who will receive the new client's contact information that was specified in "myaddr." See <a href="rep_newsite.md" class="xref" title="Connecting to a new site">Connecting to a new site</a> for more information.

The final piece of a replicated application is the code that loops, receives, and processes messages from a given remote environment. ex_rep_base runs one of these loops in a parallel thread for each socket connection (see the hm_loop function in `ex_rep/base/rep_msg.c`). Other applications may want to queue messages somehow and process them asynchronously, or select() on a number of sockets and either look up the correct environment ID for each or encapsulate the ID in the communications protocol.
