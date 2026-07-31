---
title: "Chapter 3.  The DBcursor Handle"
api-name: "Chapter 3.  The DBcursor Handle"
source: docs/api_reference/C/dbc.html
---
## Chapter 3.  The DBcursor Handle

A DBcursor object is a handle for a cursor into a Berkeley DB database.

DBcursor handles are not free-threaded. Cursor handles may be shared by multiple threads if access is serialized by the application.

You create a DBcursor using the <a href="dbcursor.md" class="xref" title="DB-&gt;cursor()">DB-&gt;cursor()</a> method.

If the cursor is to be used to perform operations on behalf of a transaction, the cursor must be opened and closed within the context of that single transaction.

Once <a href="dbcclose.md" class="xref" title="DBcursor-&gt;close()">DBcursor-&gt;close()</a> has been called, the handle may not be accessed again, regardless of the method's return.

## Database Cursors and Related Methods

| Database Cursors and Related Methods | Description |
|----|----|
| <a href="dbcursor.md" class="xref" title="DB-&gt;cursor()">DB-&gt;cursor()</a> | Create a cursor handle |
| <a href="dbcclose.md" class="xref" title="DBcursor-&gt;close()">DBcursor-&gt;close()</a> | Close a cursor handle |
| <a href="dbccmp.md" class="xref" title="DBcursor-&gt;cmp()">DBcursor-&gt;cmp()</a> | Compare two cursors for equality. |
| <a href="dbccount.md" class="xref" title="DBcursor-&gt;count()">DBcursor-&gt;count()</a> | Return count of duplicates for current key |
| <a href="dbcdel.md" class="xref" title="DBcursor-&gt;del()">DBcursor-&gt;del()</a> | Delete current key/data pair |
| <a href="dbcdup.md" class="xref" title="DBcursor-&gt;dup()">DBcursor-&gt;dup()</a> | Duplicate the cursor handle |
| <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> | Retrieve by cursor |
| <a href="dbcput.md" class="xref" title="DBcursor-&gt;put()">DBcursor-&gt;put()</a> | Store by cursor |
| <a href="dbcset_priority.md" class="xref" title="DBcursor-&gt;set_priority()">DBcursor-&gt;set_priority()</a>, <a href="dbcget_priority.md" class="xref" title="DBcursor-&gt;get_priority()">DBcursor-&gt;get_priority()</a> | Set/get the cursor's cache priority |
