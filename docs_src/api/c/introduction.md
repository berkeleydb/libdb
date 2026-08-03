---
title: "Chapter 1. Introduction to Berkeley DB APIs"
api-name: "Chapter 1. Introduction to Berkeley DB APIs"
source: docs/api_reference/C/introduction.html
---
## Chapter 1. Introduction to Berkeley DB APIs

Welcome to the Berkeley DB API Reference Manual for C.

DB is a general-purpose embedded database engine that is capable of providing a wealth of data management services. It is designed from the ground up for high-throughput applications requiring in-process, bullet-proof management of mission-critical data. DB can gracefully scale from managing a few bytes to terabytes of data. For the most part, DB is limited only by your system's available physical resources.

This manual describes the various APIs and command line utilities available for use in the DB library.

For a general description of using DB beyond the reference material available in this manual, see the Getting Started Guides which are identified in this manual's preface.

This manual is broken into chapters, each one of which describes a series of APIs designed to work with one particular aspect of the DB library. In many cases, each such chapter is organized around a "handle", or class, which provides an interface to DB structures such as databases, environments or locks. However, in some cases, methods for multiple handles are combined together when they are used to control or interface with some isolated DB functionality. See, for example, the <a href="lsn.md" class="xref" title="Chapter 7.  The DB_LSN Handle">The DB_LSN Handle</a> chapter.

Within each chapter, methods, functions and command line utilities are organized alphabetically.
