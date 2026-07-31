---
title: "DBINFO structure"
api-name: "DBINFO structure"
source: docs/upgrading/upgrade_3_0_dbinfo.html
---
## DBINFO structure

The DB_INFO structure has been removed from the Berkeley DB 3.0 release. Accesses to any fields within that structure by the application should be replaced with method calls on the <a href="../../api/c/db.md" class="olink">DB</a> handle. The following example illustrates this using the historic db_cachesize structure field. In the Berkeley DB 2.X releases, applications could set the size of an underlying database cache using code similar to the following:

``` c
DB_INFO dbinfo;

memset(dbinfo, 0, sizeof(dbinfo));
dbinfo.db_cachesize = 1024 * 1024;
```

in the Berkeley DB 3.X releases, this should be done using the <a href="../../api/c/dbset_cachesize.md" class="olink">DB-&gt;set_cachesize()</a> method, as follows:

``` c
DB *db;
int ret;

ret = db->set_cachesize(db, 0, 1024 * 1024, 0);
```

The DB_INFO structure is no longer used in any way by the Berkeley DB 3.0 release, and should be removed from the application.

The following table lists the DB_INFO fields previously used by applications and the methods that should now be used to set them. Because these calls provide configuration for the database open, they must precede the call to <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a>. Calling them after the call to <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> will return an error.

<table data-border="1" width="80%">
<thead>
<tr>
<th>DB_INFO field</th>
<th>Berkeley DB 3.X method</th>
</tr>
</thead>
<tbody>
<tr>
<td>bt_compare</td>
<td><a href="../../api/c/dbset_bt_compare.md" class="olink">DB-&gt;set_bt_compare()</a></td>
</tr>
<tr>
<td>bt_minkey</td>
<td><a href="../../api/c/dbset_bt_minkey.md" class="olink">DB-&gt;set_bt_minkey()</a></td>
</tr>
<tr>
<td>bt_prefix</td>
<td><a href="../../api/c/dbset_bt_prefix.md" class="olink">DB-&gt;set_bt_prefix()</a></td>
</tr>
<tr>
<td>db_cachesize</td>
<td><a href="../../api/c/dbset_cachesize.md" class="olink">DB-&gt;set_cachesize()</a>
<p>Note: the <a href="../../api/c/dbset_cachesize.md" class="olink">DB-&gt;set_cachesize()</a> function takes additional arguments. Setting both the second argument (the number of GB in the pool) and the last argument (the number of memory pools to create) to 0 will result in behavior that is backward-compatible with previous Berkeley DB releases.</p></td>
</tr>
<tr>
<td>db_lorder</td>
<td><a href="../../api/c/dbset_lorder.md" class="olink">DB-&gt;set_lorder()</a></td>
</tr>
<tr>
<td>db_malloc</td>
<td>DB-&gt;set_malloc</td>
</tr>
<tr>
<td>db_pagesize</td>
<td><a href="../../api/c/dbset_pagesize.md" class="olink">DB-&gt;set_pagesize()</a></td>
</tr>
<tr>
<td>dup_compare</td>
<td><a href="../../api/c/dbset_dup_compare.md" class="olink">DB-&gt;set_dup_compare()</a></td>
</tr>
<tr>
<td>flags</td>
<td><a href="../../api/c/dbset_flags.md" class="olink">DB-&gt;set_flags()</a>
<p>Note: the DB_DELIMITER, DB_FIXEDLEN and DB_PAD flags no longer need to be set as there are specific methods off the <a href="../../api/c/db.md" class="olink">DB</a> handle that set the file delimiter, the length of fixed-length records and the fixed-length record pad character. They should simply be discarded from the application.</p></td>
</tr>
<tr>
<td>h_ffactor</td>
<td><a href="../../api/c/dbset_h_ffactor.md" class="olink">DB-&gt;set_h_ffactor()</a></td>
</tr>
<tr>
<td>h_hash</td>
<td><a href="../../api/c/dbset_h_hash.md" class="olink">DB-&gt;set_h_hash()</a></td>
</tr>
<tr>
<td>h_nelem</td>
<td><a href="../../api/c/dbset_h_nelem.md" class="olink">DB-&gt;set_h_nelem()</a></td>
</tr>
<tr>
<td>re_delim</td>
<td><a href="../../api/c/dbset_re_delim.md" class="olink">DB-&gt;set_re_delim()</a></td>
</tr>
<tr>
<td>re_len</td>
<td><a href="../../api/c/dbset_re_len.md" class="olink">DB-&gt;set_re_len()</a></td>
</tr>
<tr>
<td>re_pad</td>
<td><a href="../../api/c/dbset_re_pad.md" class="olink">DB-&gt;set_re_pad()</a></td>
</tr>
<tr>
<td>re_source</td>
<td><a href="../../api/c/dbset_re_source.md" class="olink">DB-&gt;set_re_source()</a></td>
</tr>
</tbody>
</table>
