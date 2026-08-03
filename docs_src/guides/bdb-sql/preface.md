---
title: "Preface"
api-name: "Preface"
source: docs/bdb-sql/preface.html
---
## Preface

**Table of Contents**

<span class="sect1"> [Conventions Used in this Book](preface.md#conventions) </span>

<span class="sect1"> [For More Information](moreinfo.md) </span>

<span class="sect2"> [Contact Us](moreinfo.md#contact_us) </span>

Welcome to the Berkeley DB SQL interface. This manual describes how to configure and use the SQL interface to Berkeley DB 11<span class="emphasis">*g*</span> Release 2. This manual also describes common administrative tasks, such as backup and restore, database dump and load, and data migration when using the BDB SQL interface.

This manual is intended for anyone who wants to use the BDB SQL interface. Because usage of the BDB SQL interface is very nearly identical to SQLite, prior knowledge of SQLite is assumed by this manual. No prior knowledge of Berkeley DB is necessary, but it is helpful.

To learn about SQLite, see the official SQLite website at: <a href="http://www.sqlite.org/" class="ulink" target="_top">http://www.sqlite.org</a>

## Conventions Used in this Book

The following typographical conventions are used within in this manual:

Keywords or literal text that you are expected to type is presented in a `monospaced font`. For example: "Use the `DB_HOME` environment variable to identify the location of your environment directory."

Variable or non-literal text is presented in <span class="emphasis">*italics*</span>. For example: "Go to your <span class="emphasis">*DB_INSTALL*</span> directory."

Program examples and literal text that you might type are displayed in a `monospaced font` on a shaded background. For example:

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
