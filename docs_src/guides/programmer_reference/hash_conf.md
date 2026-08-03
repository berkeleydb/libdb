---
title: "Hash access method specific configuration"
api-name: "Hash access method specific configuration"
source: docs/programmer_reference/hash_conf.html
---
## Hash access method specific configuration

<span class="sect2"> [Page fill factor](hash_conf.md#am_conf_h_ffactor) </span>

<span class="sect2"> [Specifying a database hash](hash_conf.md#am_conf_h_hash) </span>

<span class="sect2"> [Hash table size](hash_conf.md#am_conf_h_nelem) </span>

There are a series of configuration tasks which you can perform when using the Hash access method. They are described in the following sections.

### Page fill factor

The density, or page fill factor, is an approximation of the number of keys allowed to accumulate in any one bucket, determining when the hash table grows or shrinks. If you know the average sizes of the keys and data in your data set, setting the fill factor can enhance performance. A reasonable rule to use to compute fill factor is:

``` c
(pagesize - 32) / (average_key_size + average_data_size + 8)
```

The desired density within the hash table can be specified by calling the <a href="../../api/c/dbset_h_ffactor.md" class="olink">DB-&gt;set_h_ffactor()</a> method. If no density is specified, one will be selected dynamically as pages are filled.

### Specifying a database hash

The database hash determines in which bucket a particular key will reside. The goal of hashing keys is to distribute keys equally across the database pages, therefore it is important that the hash function work well with the specified keys so that the resulting bucket usage is relatively uniform. A hash function that does not work well can effectively turn into a sequential list.

No hash performs equally well on all possible data sets. It is possible that applications may find that the default hash function performs poorly with a particular set of keys. The distribution resulting from the hash function can be checked using the <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility. By comparing the number of hash buckets and the number of keys, one can decide if the entries are hashing in a well-distributed manner.

The hash function for the hash table can be specified by calling the <a href="../../api/c/dbset_h_hash.md" class="olink">DB-&gt;set_h_hash()</a> method. If no hash function is specified, a default function will be used. Any application-specified hash function must take a reference to a <a href="../../api/c/db.md" class="olink">DB</a> object, a pointer to a byte string and its length, as arguments and return an unsigned, 32-bit hash value.

### Hash table size

When setting up the hash database, knowing the expected number of elements that will be stored in the hash table is useful. This value can be used by the Hash access method implementation to more accurately construct the necessary number of buckets that the database will eventually require.

The anticipated number of elements in the hash table can be specified by calling the <a href="../../api/c/dbset_h_nelem.md" class="olink">DB-&gt;set_h_nelem()</a> method. If not specified, or set too low, hash tables will expand gracefully as keys are entered, although a slight performance degradation may be noticed. In order for the estimated number of elements to be a useful value to Berkeley DB, the <a href="../../api/c/dbset_h_ffactor.md" class="olink">DB-&gt;set_h_ffactor()</a> method must also be called to set the page fill factor.
