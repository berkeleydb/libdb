---
title: "Deleting records"
api-name: "Deleting records"
source: docs/programmer_reference/am_delete.html
---
## Deleting records

The <a href="../../api/c/dbdel.md" class="olink">DB-&gt;del()</a> method deletes records from the database. In general, <a href="../../api/c/dbdel.md" class="olink">DB-&gt;del()</a> takes a key and deletes the data item associated with it from the database.

If the database has been configured to support duplicate records, the <a href="../../api/c/dbdel.md" class="olink">DB-&gt;del()</a> method will remove all of the duplicate records. To remove individual duplicate records, you must use a Berkeley DB cursor interface.
