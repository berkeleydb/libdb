---
title: "Chapter 2. Transactional Application"
api-name: "Chapter 2. Transactional Application"
source: docs/gsg_db_rep/JAVA/txnapp.html
---
## Chapter 2. Transactional Application

**Table of Contents**

<span class="sect1"> [Application Overview](txnapp.md#appoverview) </span>

<span class="sect1"> [Program Listing](simpleprogramlisting.md) </span>

<span class="sect2"> [Class: RepConfig](simpleprogramlisting.md#repconfiginfo_cxx) </span>

<span class="sect2"> [Class: SimpleTxn](simpleprogramlisting.md#simpletxnusage_java) </span>

<span class="sect2"> [Method: SimpleTxn.main()](simpleprogramlisting.md#simpletxnmain_java) </span>

<span class="sect2"> [Method: SimpleTxn.init()](simpleprogramlisting.md#simpletxn_init_java) </span>

<span class="sect2"> [Method: SimpleTxn.doloop()](simpleprogramlisting.md#doloop_java) </span>

<span class="sect2"> [Method: SimpleTxn.printStocks()](simpleprogramlisting.md#printstocks_c) </span>

In this chapter, we build a simple transaction-protected DB application. Throughout the remainder of this book, we will add replication to this example. We do this to underscore the concepts that we are presenting in this book; the first being that you should start with a working transactional program and then add replication to it.

Note that this book assumes you already know how to write a transaction-protected DB application, so we will not be covering those concepts in this book. To learn how to write a transaction-protected application, see the *Berkeley DB Getting Started with Transaction Processing* guide.

## Application Overview

Our application maintains a stock market quotes database. This database contains records whose key is the stock market symbol and whose data is the stock's price.

The application operates by presenting you with a command line prompt. You then enter the stock symbol and its value, separated by a space. The application takes this information and writes it to the database.

To see the contents of the database, simply press `return` at the command prompt.

To quit the application, type 'quit' or 'exit' at the command prompt.

For example, the following illustrates the application's usage. In it, we use entirely fictitious stock market symbols and price values.

``` c
> java db.repquote_gsg.SimpleTxn -h env_home_dir
QUOTESERVER> stock1 88
QUOTESERVER> stock2 .08
QUOTESERVER> 
        Symbol  Price
        ======  =====
        stock1  88

QUOTESERVER> stock1 88.9
QUOTESERVER> 
        Symbol  Price
        ======  =====
        stock1  88.9
        stock2  .08

QUOTESERVER> quit 
>
```
