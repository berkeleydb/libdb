---
title: "The Replication APIs"
api-name: "The Replication APIs"
source: docs/gsg_db_rep/C/apioverview.html
---
## The Replication APIs

<span class="sect2"> [Replication Manager Overview](apioverview.md#repframeworkoverview) </span>

<span class="sect2"> [Replication Base API Overview](apioverview.md#repapioverview) </span>

There are two ways that you can choose to implement replication in your transactional application. The first, and preferred, mechanism is to use the pre-packaged Replication Manager that comes with the DB distribution. This framework should be sufficient for most customers.

If for some reason the Replication Manager does not meet your application's technical requirements, you will have to use the Replication Base APIs available through the Berkeley DB library to write your own custom replication framework.

Both of these approaches are described in slightly greater detail in this section. The bulk of the chapters later in this book are dedicated to these two replication implementation mechanisms.

### Replication Manager Overview

DB's pre-packaged Replication Manager exists as a layer on top of the DB library. The Replication Manager is a multi-threaded implementation that allows you to easily add replication to your existing transactional application. You access and manage the Replication Manager using methods that are available off the `DB_ENV` class.

The Replication Manager:

- Provides a multi-threaded communications layer using pthreads (on Unix-style systems and similar derivatives such as Mac OS X), or Windows threads on Microsoft Windows systems.

- Uses TCP/IP sockets. Network traffic is handled via threads that handle inbound and outbound messages. However, each process uses a single socket that is shared using `select()`.

  Note that for this reason, the Replication Manager is limited to a maximum of 60 replicas (on Windows) and approximately 1000 replicas (on Unix and related systems), depending on how your system is configured.

- Requires that only one instance of the environment handle be used.

- Upon application startup, a master can be selected either manually or via elections. After startup time, however, during the course of normal operations it is possible for the replication group to need to locate a new master (due to network or other hardware related problems, for example) and in this scenario elections are always used to select the new master.

If your application has technical requirements that do not conform to the implementation provided by the Replication Manager, you must write implement replication using the DB Replication Base APIs. See the next section for introductory details.

### Replication Base API Overview

The Replication Base API is a series of Berkeley DB library classes and methods that you can use to build your own replication infrastructure. You should use the Base API only if the Replication Manager does not meet your application's technical requirements.

To make use of the Base API, you must write your own networking code. This frees you from the technical constraints imposed by the Replication Manager. For example, by writing your own framework, you can:

- Use a threading package other than pthreads (Unix) or Windows threads (Microsoft Windows). This might be interesting to you if you are using a platform whose preferred threading package is something other than (for example) pthreads, such as is the case for Sun Microsystem's Solaris operating systems.

- Implement your own sockets. The Replication Manager uses TCP/IP sockets. While this should be acceptable for the majority of applications, sometimes UDP or even raw sockets might be desired.

For information on writing a replicated application using the Berkeley DB Replication Base APIs, see the *Berkeley DB Programmer's Reference Guide*.
