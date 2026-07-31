---
title: "Chapter 4.  Access Method Wrapup"
api-name: "Chapter 4.  Access Method Wrapup"
source: docs/programmer_reference/am_misc.html
---
## Chapter 4.  Access Method Wrapup

**Table of Contents**

<span class="sect1"> [Data alignment](am_misc.md#am_misc_align) </span>

<span class="sect1"> [Retrieving and updating records in bulk](am_misc_bulk.md) </span>

<span class="sect2"> [Bulk retrieval](am_misc_bulk.md#am_misc_bulk_get) </span>

<span class="sect2"> [Bulk updates](am_misc_bulk.md#am_misc_bulk_put) </span>

<span class="sect2"> [Bulk deletes](am_misc_bulk.md#am_misc_bulk_del) </span>

<span class="sect1"> [Partial record storage and retrieval](am_misc_partial.md) </span>

<span class="sect1"> [Storing C/C++ structures/objects](am_misc_struct.md) </span>

<span class="sect1"> [Retrieved key/data permanence for C/C++](am_misc_perm.md) </span>

<span class="sect1"> [Error support](am_misc_error.md) </span>

<span class="sect1"> [Cursor stability](am_misc_stability.md) </span>

<span class="sect1"> [Database limits](am_misc_dbsizes.md) </span>

<span class="sect1"> [Disk space requirements](am_misc_diskspace.md) </span>

<span class="sect2"> [Btree](am_misc_diskspace.md#idp51253016) </span>

<span class="sect2"> [Hash](am_misc_diskspace.md#idp51253080) </span>

<span class="sect1"> [Specifying a Berkeley DB schema using SQL DDL](am_misc_db_sql.md) </span>

<span class="sect1"> [Access method tuning](am_misc_tune.md) </span>

<span class="sect1"> [Access method FAQ](am_misc_faq.md) </span>

## Data alignment

The Berkeley DB access methods provide no guarantees about byte alignment for returned key/data pairs, or callback functions which take <a href="../../api/c/dbt.md" class="olink">DBT</a> references as arguments, and applications are responsible for arranging any necessary alignment. The <a href="../../api/c/dbt.md#dbt_DB_DBT_MALLOC" class="olink">DB_DBT_MALLOC</a>, <a href="../../api/c/dbt.md#dbt_DB_DBT_REALLOC" class="olink">DB_DBT_REALLOC</a>, and <a href="../../api/c/dbt.md#dbt_DB_DBT_USERMEM" class="olink">DB_DBT_USERMEM</a> flags may be used to store returned items in memory of arbitrary alignment.
