---
title: "Chapter 1. Introduction"
api-name: "Chapter 1. Introduction"
source: docs/gsg_txn/JAVA/introduction.html
---
## Chapter 1. Introduction

**Table of Contents**

<span class="sect1"> [Transaction Benefits](introduction.md#txnintro) </span>

<span class="sect1"> [A Note on System Failure](sysfailure.md) </span>

<span class="sect1"> [Application Requirements](apireq.md) </span>

<span class="sect1"> [Multi-threaded and Multi-process Applications](multithread-intro.md) </span>

<span class="sect1"> [Recoverability](recovery-intro.md) </span>

<span class="sect1"> [Performance Tuning](perftune-intro.md) </span>

This book provides a thorough introduction and discussion on transactions as used with Berkeley DB (DB). Both the base API as well as the Direct Persistence Layer API is used in this manual. It begins by offering a general overview to transactions, the guarantees they provide, and the general application infrastructure required to obtain full transactional protection for your data.

This book also provides detailed examples on how to write a transactional application. Both single threaded and multi-threaded (as well as multi-process applications) are discussed. A detailed description of various backup and recovery strategies is included in this manual, as is a discussion on performance considerations for your transactional application.

You should understand the concepts from the *Getting Started with Berkeley DB* guide before reading this book.

## Transaction Benefits

Transactions offer your application's data protection from application or system failures. That is, DB transactions offer your application full ACID support:

- **A**tomicity

  Multiple database operations are treated as a single unit of work. Once committed, all write operations performed under the protection of the transaction are saved to your databases. Further, in the event that you abort a transaction, all write operations performed during the transaction are discarded. In this event, your database is left in the state it was in before the transaction began, regardless of the number or type of write operations you may have performed during the course of the transaction.

  Note that DB transactions can span one or more database handles.

- **C**onsistency

  Your databases will never see a partially completed transaction. This is true even if your application fails while there are in-progress transactions. If the application or system fails, then either all of the database changes appear when the application next runs, or none of them appear.

  In other words, whatever consistency requirements your application has will never be violated by DB. If, for example, your application requires every record to include an employee ID, and your code faithfully adds that ID to its database records, then DB will never violate that consistency requirement. The ID will remain in the database records until such a time as your application chooses to delete it.

- **I**solation

  While a transaction is in progress, your databases will appear to the transaction as if there are no other operations occurring outside of the transaction. That is, operations wrapped inside a transaction will always have a clean and consistent view of your databases. They never have to see updates currently in progress under the protection of another transaction. Note, however, that isolation guarantees can be relaxed from the default setting. See <a href="isolation.md" class="xref" title="Isolation">Isolation</a> for more information.

- **D**urability

  Once committed to your databases, your modifications will persist even in the event of an application or system failure. Note that like isolation, your durability guarantee can be relaxed. See <a href="nodurabletxn.md" class="xref" title="Non-Durable Transactions">Non-Durable Transactions</a> for more information.
