---
title: "Chapter 11.  The DB_SEQUENCE Handle"
api-name: "Chapter 11.  The DB_SEQUENCE Handle"
source: docs/api_reference/C/seq.html
---
## Chapter 11.  The DB_SEQUENCE Handle

Sequences provide an arbitrary number of persistent objects that return an increasing or decreasing sequence of integers. Opening a sequence handle associates it with a record in a database. The handle can maintain a cache of values from the database so that a database update is not needed as the application allocates a value.

A sequence is stored as a record pair in a database. The database may be of any type, but must not have been configured to support duplicate data items. The sequence is referenced by the key used when the sequence is created, therefore the key must be compatible with the underlying access method. If the database stores fixed-length records, the record size must be at least 64 bytes long.

You create a sequence using the <a href="seqcreate.md" class="xref" title="db_sequence_create">db_sequence_create</a> method.

For more information on sequences, see the *Berkeley DB Programmer's Reference Guide* guide.

## Sequences and Related Methods

<table data-border="1" width="80%">
<thead>
<tr>
<th>Sequences and Related Methods</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><a href="seqcreate.md" class="xref" title="db_sequence_create">db_sequence_create</a></td>
<td>Create a sequence handle</td>
</tr>
<tr>
<td><a href="seqclose.md" class="xref" title="DB_SEQUENCE-&gt;close()">DB_SEQUENCE-&gt;close()</a></td>
<td>Close a sequence</td>
</tr>
<tr>
<td><a href="seqget.md" class="xref" title="DB_SEQUENCE-&gt;get()">DB_SEQUENCE-&gt;get()</a></td>
<td>Get the next sequence element(s)</td>
</tr>
<tr>
<td><a href="seqget_dbp.md" class="xref" title="DB_SEQUENCE-&gt;get_dbp()">DB_SEQUENCE-&gt;get_dbp()</a></td>
<td>Return a handle for the underlying sequence database</td>
</tr>
<tr>
<td><a href="seqget_key.md" class="xref" title="DB_SEQUENCE-&gt;get_key()">DB_SEQUENCE-&gt;get_key()</a></td>
<td>Return the key for a sequence</td>
</tr>
<tr>
<td><a href="seqinitial_value.md" class="xref" title="DB_SEQUENCE-&gt;initial_value()">DB_SEQUENCE-&gt;initial_value()</a></td>
<td>Set the initial value of a sequence</td>
</tr>
<tr>
<td><a href="seqopen.md" class="xref" title="DB_SEQUENCE-&gt;open()">DB_SEQUENCE-&gt;open()</a></td>
<td>Open a sequence</td>
</tr>
<tr>
<td><a href="seqremove.md" class="xref" title="DB_SEQUENCE-&gt;remove()">DB_SEQUENCE-&gt;remove()</a></td>
<td>Remove a sequence</td>
</tr>
<tr>
<td><a href="seqstat.md" class="xref" title="DB_SEQUENCE-&gt;stat()">DB_SEQUENCE-&gt;stat()</a></td>
<td>Return sequence statistics</td>
</tr>
<tr>
<td><a href="seqstat_print.md" class="xref" title="DB_SEQUENCE-&gt;stat_print()">DB_SEQUENCE-&gt;stat_print()</a></td>
<td>Print sequence statistics</td>
</tr>
<tr>
<td colspan="2"><strong>Sequences Configuration</strong></td>
</tr>
<tr>
<td><a href="seqset_cachesize.md" class="xref" title="DB_SEQUENCE-&gt;set_cachesize()">DB_SEQUENCE-&gt;set_cachesize()</a>, <a href="seqget_cachesize.md" class="xref" title="DB_SEQUENCE-&gt;get_cachesize()">DB_SEQUENCE-&gt;get_cachesize()</a></td>
<td>Set/get the cache size of a sequence</td>
</tr>
<tr>
<td><a href="seqset_flags.md" class="xref" title="DB_SEQUENCE-&gt;set_flags()">DB_SEQUENCE-&gt;set_flags()</a>, <a href="seqget_flags.md" class="xref" title="DB_SEQUENCE-&gt;get_flags()">DB_SEQUENCE-&gt;get_flags()</a></td>
<td>Set/get the flags for a sequence</td>
</tr>
<tr>
<td><a href="seqset_range.md" class="xref" title="DB_SEQUENCE-&gt;set_range()">DB_SEQUENCE-&gt;set_range()</a>, <a href="seqget_range.md" class="xref" title="DB_SEQUENCE-&gt;get_range()">DB_SEQUENCE-&gt;get_range()</a></td>
<td>Set/get the range for a sequence</td>
</tr>
</tbody>
</table>
