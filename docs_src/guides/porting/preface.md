---
title: "Preface"
api-name: "Preface"
source: docs/porting/preface.html
---
## Preface

**Table of Contents**

<span class="sect1"> [Conventions Used in this Book](preface.md#conventions) </span>

<span class="sect1"> [Audience](audience.md) </span>

<span class="sect1"> [For More Information](moreinfo.md) </span>

<span class="sect2"> [Contact Us](moreinfo.md#contact_us) </span>

The Berkeley DB family of open source, embeddable databases provides developers with fast, reliable persistence with zero administration. Often deployed as "edge" databases, the Berkeley DB family provides very high performance, reliability, scalability, and availability for application use cases that do not require SQL.

As an open source database, Berkeley DB works on many different platforms, from Wind River's Tornado system, to VMS, to Windows NT and Windows 95, and most existing UNIX platforms. It runs on 32 and 64-bit machines, little or big-endian.

<span class="emphasis">*Berkeley DB Porting Guide*</span> provides the information you need to port Berkeley DB 11<span class="emphasis">*g*</span> Release 2 (library version 11.2.5.3) to additional platforms.

## Conventions Used in this Book

The following typographical conventions are used within in this manual:

Structure names are represented in `monospaced font`, as are `method names`. For example: "`DB->open()` is a method on a `DB` handle."

Variable or non-literal text is presented in <span class="emphasis">*italics*</span>. For example: "Go to your <span class="emphasis">*DB_INSTALL*</span> directory."

Program examples are displayed in a `monospaced font` on a shaded background. For example:

``` c
/* File: gettingstarted_common.h */
typedef struct stock_dbs {
    DB *inventory_dbp; /* Database containing inventory information */
    DB *vendor_dbp;    /* Database containing vendor information */

    char *db_home_dir;       /* Directory containing the database files */
    char *inventory_db_name; /* Name of the inventory database */
    char *vendor_db_name;    /* Name of the vendor database */
} STOCK_DBS; 
```

### Note

Finally, notes of interest are represented using a note block such as this.
