---
title: "Sequence Support in the SQL Layer"
api-name: "Sequence Support in the SQL Layer"
source: docs/installation/upgrade_11gr2_52_seq_sql.html
---
## Sequence Support in the SQL Layer

<span class="sect2"> [New Functions](upgrade_11gr2_52_seq_sql.md#idp963480) </span>

A partial implementation of the sequence API defined in the SQL 2003 specification has been added to the SQL layer. A sequence is created using the syntax:

``` c
SELECT create_sequence("sequence_name"...) 
```

The sequence numbers are accessed using

``` c
SELECT nextval("sequence_name") 
```

and

``` c
SELECT currval("sequence_name")
```

Finally, a sequence can be dropped using

``` c
SELECT drop_sequence("sequence_name")
```

### New Functions

The four new functions, which have to be called as part of a `SELECT` statement, are describe in more detail in <a href="../../guides/bdb-sql/sequencesupport.md" class="olink">Using Sequences</a> in the *Berkeley DB Getting Started with the SQL APIs* guide.

- <a href="../../guides/bdb-sql/sequencesupport.md#create_sequence" class="olink">create_sequence</a>
- <a href="../../guides/bdb-sql/sequencesupport.md#seq_nextval" class="olink">seq_nextval</a>
- <a href="../../guides/bdb-sql/sequencesupport.md#seq_currval" class="olink">seq_currval</a>
- <a href="../../guides/bdb-sql/sequencesupport.md#seq_drop_sequence" class="olink">seq_drop_sequence</a>.
