---
title: "DBcursor->c_put"
api-name: "DBcursor->c_put"
source: docs/upgrading/upgrade_4_3_cput.html
---
## DBcursor-\>c_put

The 4.3 release disallows the <a href="../../api/c/dbcget.md#dbcget_DB_CURRENT" class="olink">DB_CURRENT</a> flag to the <a href="../../api/c/dbcput.md" class="olink">DBC-&gt;put()</a> method after the current item referenced by the cursor has been deleted. Applications using this sequence of operations should be changed to do the put without first deleting the item.
