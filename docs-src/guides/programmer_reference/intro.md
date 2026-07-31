---
title: "Chapter 1.  Introduction"
api-name: "Chapter 1.  Introduction"
source: docs/programmer_reference/intro.html
---
## Chapter 1.  Introduction

**Table of Contents**

<span class="sect1"> [An introduction to data management](intro.md#intro_data) </span>

<span class="sect1"> [Mapping the terrain: theory and practice](intro_terrain.md) </span>

<span class="sect2"> [Data access and data management](intro_terrain.md#idp50584200) </span>

<span class="sect2"> [Relational databases](intro_terrain.md#idp50577368) </span>

<span class="sect2"> [Object-oriented databases](intro_terrain.md#idp50621160) </span>

<span class="sect2"> [Network databases](intro_terrain.md#idp50574144) </span>

<span class="sect2"> [Clients and servers](intro_terrain.md#idp50647776) </span>

<span class="sect1"> [What is Berkeley DB?](intro_dbis.md) </span>

<span class="sect2"> [Data Access Services](intro_dbis.md#idp50588152) </span>

<span class="sect2"> [Data management services](intro_dbis.md#idm1374112) </span>

<span class="sect2"> [Design](intro_dbis.md#idp50659944) </span>

<span class="sect1"> [What Berkeley DB is not](intro_dbisnot.md) </span>

<span class="sect2"> [Berkeley DB is not a relational database](intro_dbisnot.md#idp50596256) </span>

<span class="sect2"> [Berkeley DB is not an object-oriented database](intro_dbisnot.md#idp50675392) </span>

<span class="sect2"> [Berkeley DB is not a network database](intro_dbisnot.md#idp50621304) </span>

<span class="sect2"> [Berkeley DB is not a database server](intro_dbisnot.md#idp50657008) </span>

<span class="sect1"> [Do you need Berkeley DB?](intro_need.md) </span>

<span class="sect1"> [What other services does Berkeley DB provide?](intro_what.md) </span>

<span class="sect1"> [What does the Berkeley DB distribution include?](intro_distrib.md) </span>

<span class="sect1"> [Where does Berkeley DB run?](intro_where.md) </span>

<span class="sect1"> [The Berkeley DB products](intro_products.md) </span>

<span class="sect2"> [Berkeley DB Data Store](intro_products.md#idp50715960) </span>

<span class="sect2"> [Berkeley DB Concurrent Data Store](intro_products.md#idp50715552) </span>

<span class="sect2"> [Berkeley DB Transactional Data Store](intro_products.md#idp50708368) </span>

<span class="sect2"> [Berkeley DB High Availability](intro_products.md#idp50712672) </span>

## An introduction to data management

Cheap, powerful computing and networking have created countless new applications that could not have existed a decade ago. The advent of the World-Wide Web, and its influence in driving the Internet into homes and businesses, is one obvious example. Equally important, though, is the shift from large, general-purpose desktop and server computers toward smaller, special-purpose devices with built-in processing and communications services.

As computer hardware has spread into virtually every corner of our lives, of course, software has followed. Software developers today are building applications not just for conventional desktop and server environments, but also for handheld computers, home appliances, networking hardware, cars and trucks, factory floor automation systems, cellphones, and more.

While these operating environments are diverse, the problems that software engineers must solve in them are often strikingly similar. Most systems must deal with the outside world, whether that means communicating with users or controlling machinery. As a result, most need some sort of I/O system. Even a simple, single-function system generally needs to handle multiple tasks, and so needs some kind of operating system to schedule and manage control threads. Also, many computer systems must store and retrieve data to track history, record configuration settings, or manage access.

Data management can be very simple. In some cases, just recording configuration in a flat text file is enough. More often, though, programs need to store and search a large amount of data, or structurally complex data. Database management systems are tools that programmers can use to do this work quickly and efficiently using off-the-shelf software.

Of course, database management systems have been around for a long time. Data storage is a problem dating back to the earliest days of computing. Software developers can choose from hundreds of good, commercially-available database systems. The problem is selecting the one that best solves the problems that their applications face.
