---
title: "Storing records"
api-name: "Storing records"
source: docs/programmer_reference/am_put.html
---
## Storing records

The <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a> method stores records into the database. In general, <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a> takes a key and stores the associated data into the database.

There are a few flags that you can set to customize storage:

<span class="term"> <a href="../../api/c/dbput.md#dbput_DB_APPEND" class="olink">DB_APPEND</a> </span>  
Simply append the data to the end of the database, treating the database much like a simple log. This flag is only valid for the Heap, Queue and Recno access methods. This flag is required if you are creating a new record in a Heap database.

<span class="term"> <a href="../../api/c/dbput.md#put_DB_NOOVERWRITE" class="olink">DB_NOOVERWRITE</a> </span>  
Only store the data item if the key does not already appear in the database.

If the database has been configured to support duplicate records, the <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a> method will add the new data value at the end of the duplicate set. If the database supports sorted duplicates, the new data value is inserted at the correct sorted location.

### Note

If you are using the Heap access method and you are creating a new record in the database, then the key that you provide to the <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a> method should be empty. The <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a> method will return the record's ID (RID) in the key. The RID is automatically created for you when Heap database records are created.
