---
title: "Chapter 1.  Introduction"
api-name: "Chapter 1.  Introduction"
source: docs/collections/tutorial/intro.html
---
## Chapter 1.  Introduction

**Table of Contents**

<span class="sect1"> [Features](intro.md#features) </span>

<span class="sect1"> [Developing a DB Collections Application](developing.md) </span>

<span class="sect1"> [Tutorial Introduction](tutorialintroduction.md) </span>

The DB Java Collections API is a Java framework that extends the well known <a href="http://download.oracle.com/javase/1.5.0/docs/guide/collections/" class="ulink" target="_top">Java Collections</a> design pattern such that collections can now be stored, updated and queried in a transactional manner. The DB Java Collections API is a layer on top of DB.

Together the DB Java Collections API and Berkeley DB provide an embedded data management solution with all the benefits of a full transactional storage and the simplicity of a well known Java API. Java programmers who need fast, scalable, transactional data management for their projects can quickly adopt and deploy the DB Java Collections API with confidence.

This framework was first known as <a href="http://greybird-db.sourceforge.net/" class="ulink" target="_top">Greybird DB</a> written by Mark Hayes. Mark collaborated with us to permanently incorporate his excellent work into our distribution and to support it as an ongoing part of Berkeley DB and Berkeley DB Java Edition. The repository of source code that remains at SourceForge at version 0.9.0 is considered the last version before incorporation and will remain intact but will not be updated to reflect changes made as part of Berkeley DB or Berkeley DB Java Edition.

## Features

Berkeley DB has always provided a Java API which can be roughly described as a map and cursor interface, where the keys and values are represented as byte arrays. This API is a Java (JNI) interface to the C API and it closely modeled the Berkeley DB C API's interface. The DB Java Collections API is a layer on top of that thin JNI mapping of the C API to Berkeley DB. It adds significant new functionality in several ways.

- An implementation of the Java Collections interfaces (Map, SortedMap, Set, SortedSet, List and Iterator) is provided.

- Transactions are supported using the conventional Java transaction-per-thread model, where the current transaction is implicitly associated with the current thread.

- Transaction runner utilities are provided that automatically perform transaction retry and exception handling.

- Keys and values are represented as Java objects rather than byte arrays. Bindings are used to map between Java objects and the stored byte arrays.

- The tuple data format is provided as the simplest data representation, and is useful for keys as well as simple compact values.

- The serial data format is provided for storing arbitrary Java objects without writing custom binding code. Java serialization is extended to store the class descriptions separately, making the data records much more compact than with standard Java serialization.

- Custom data formats and bindings can be easily added. XML data format and XML bindings could easily be created using this feature, for example.

- The DB Java Collections API insulates the application from minor differences in the use of the Berkeley DB Data Store, Concurrent Data Store, and Transactional Data Store products. This allows for development with one and deployment with another without significant changes to code.

Note that the DB Java Collections API does not support caching of programming language objects nor does it keep track of their stored status. This is in contrast to "persistent object" approaches such as those defined by <a href="http://www.odmg.org/odmg.html" class="ulink" target="_top">ODMG</a> and JDO (<a href="http://www.jcp.org/en/jsr/detail?id=12" class="ulink" target="_top">JSR 12</a>). Such approaches have benefits but also require sophisticated object caching. For simplicity the DB Java Collections API treats data objects by value, not by reference, and does not perform object caching of any kind. Since the DB Java Collections API is a thin layer, its reliability and performance characteristics are roughly equivalent to those of Berkeley DB, and database tuning is accomplished in the same way as for any Berkeley DB database.
