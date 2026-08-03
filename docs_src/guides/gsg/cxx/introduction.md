---
title: "Chapter 1. Introduction to Berkeley DB"
api-name: "Chapter 1. Introduction to Berkeley DB"
source: docs/gsg/CXX/introduction.html
---
## Chapter 1. Introduction to Berkeley DB

**Table of Contents**

<span class="sect1"> [About This Manual](introduction.md#aboutthismanual) </span>

<span class="sect1"> [Berkeley DB Concepts](concepts.md) </span>

<span class="sect1"> [Access Methods](accessmethods.md) </span>

<span class="sect2"> [Selecting Access Methods](accessmethods.md#selectAM) </span>

<span class="sect2"> [Choosing between BTree and Hash](accessmethods.md#BTreeVSHash) </span>

<span class="sect2"> [Choosing between Queue and Recno](accessmethods.md#QueueVSRecno) </span>

<span class="sect1"> [Database Limits and Portability](databaseLimits.md) </span>

<span class="sect1"> [Environments](environments.md) </span>

<span class="sect1"> [Exception Handling](coreExceptions.md) </span>

<span class="sect1"> [Error Returns](returns.md) </span>

<span class="sect1"> [Getting and Using DB](gettingit.md) </span>

Welcome to Berkeley DB (DB). DB is a general-purpose embedded database engine that is capable of providing a wealth of data management services. It is designed from the ground up for high-throughput applications requiring in-process, bullet-proof management of mission-critical data. DB can gracefully scale from managing a few bytes to terabytes of data. For the most part, DB is limited only by your system's available physical resources.

You use DB through a series of programming APIs which give you the ability to read and write your data, manage your database(s), and perform other more advanced activities such as managing transactions.

Because DB is an embedded database engine, it is extremely fast. You compile and link it into your application in the same way as you would any third-party library. This means that DB runs in the same process space as does your application, allowing you to avoid the high cost of interprocess communications incurred by stand-alone database servers.

To further improve performance, DB offers an in-memory cache designed to provide rapid access to your most frequently used data. Once configured, cache usage is transparent. It requires very little attention on the part of the application developer.

Beyond raw speed, DB is also extremely configurable. It provides several different ways of organizing your data in its databases. Known as <span class="emphasis">*access methods*</span>, each such data organization mechanism provides different characteristics that are appropriate for different data management profiles. (Note that this manual focuses almost entirely on the BTree access method as this is the access method used by the vast majority of DB applications).

To further improve its configurability, DB offers many different subsystems, each of which can be used to extend DB's capabilities. For example, many applications require write-protection of their data so as to ensure that data is never left in an inconsistent state for any reason (such as software bugs or hardware failures). For those applications, a transaction subsystem can be enabled and used to transactional-protect database writes.

The list of operating systems on which DB is available is too long to detail here. Suffice to say that it is available on all major commercial operating systems, as well as on many embedded platforms.

Finally, DB is available in a wealth of programming languages. DB is officially supported in C, C++, and Java, but the library is also available in many other languages, especially scripting languages such as Perl and Python.

### Note

Before going any further, it is important to mention that DB is not a relational database (although you could use it to build a relational database). Out of the box, DB does not provide higher-level features such as triggers, or a high-level query language such as SQL. Instead, DB provides just those minimal APIs required to store and retrieve your data as efficiently as possible.

## About This Manual

This manual introduces DB. As such, this book does not examine intermediate or advanced features such as threaded library usage or transactional usage. Instead, this manual provides a step-by-step introduction to DB's basic concepts and library usage.

Specifically, this manual introduces DB environments, databases, database records, and storage and retrieval of database records. This book also introduces cursors and their usage, and it describes secondary databases.

For the most part, this manual focuses on the BTree access method. A chapter is given at the end of this manual that describes some of the concepts involving BTree usage, such as duplicate record management and comparison routines.

Examples are given throughout this book that are designed to illustrate API usage. At the end of each chapter, a complete example is given that is designed to reinforce the concepts covered in that chapter. In addition to being presented in this book, these final programs are also available in the DB software distribution. You can find them in

``` c
DB_INSTALL/examples_cxx/getting_started
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

This book uses the C++ programming languages for its examples. Note that versions of this book exist for the C and Java languages as well.
