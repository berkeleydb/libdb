---
title: "DB_ENV structure"
api-name: "DB_ENV structure"
source: docs/upgrading/upgrade_3_0_dbenv.html
---
## DB_ENV structure

The <a href="../../api/c/env.md" class="olink">DB_ENV</a> structure is now opaque for applications in the Berkeley DB 3.0 release. Accesses to any fields within that structure by the application should be replaced with method calls. The following example illustrates this using the historic errpfx structure field. In the Berkeley DB 2.X releases, applications set error prefixes using code similar to the following:

``` c
DB_ENV *dbenv;

dbenv->errpfx = "my prefix";
```

in the Berkeley DB 3.X releases, this should be done using the <a href="../../api/c/envset_errpfx.md" class="olink">DB_ENV-&gt;set_errpfx()</a> method, as follows:

``` c
DB_ENV *dbenv;

dbenv->set_errpfx(dbenv, "my prefix");
```

The following table lists the <a href="../../api/c/env.md" class="olink">DB_ENV</a> fields previously used by applications and the methods that should now be used to set them.

<table data-border="1" width="80%">
<thead>
<tr>
<th><a href="../../api/c/env.md" class="olink">DB_ENV</a> field</th>
<th>Berkeley DB 3.X method</th>
</tr>
</thead>
<tbody>
<tr>
<td>db_errcall</td>
<td><a href="../../api/c/envset_errcall.md" class="olink">DB_ENV-&gt;set_errcall()</a></td>
</tr>
<tr>
<td>db_errfile</td>
<td><a href="../../api/c/envset_errfile.md" class="olink">DB_ENV-&gt;set_errfile()</a></td>
</tr>
<tr>
<td>db_errpfx</td>
<td><a href="../../api/c/envset_errpfx.md" class="olink">DB_ENV-&gt;set_errpfx()</a></td>
</tr>
<tr>
<td>db_lorder</td>
<td>This field was removed from the <a href="../../api/c/env.md" class="olink">DB_ENV</a> structure in the Berkeley DB 3.0 release as no application should have ever used it. Any code using it should be evaluated for potential bugs.</td>
</tr>
<tr>
<td>db_paniccall</td>
<td>DB_ENV-&gt;set_paniccall</td>
</tr>
<tr>
<td>db_verbose</td>
<td><a href="../../api/c/envset_verbose.md" class="olink">DB_ENV-&gt;set_verbose()</a>
<p>Note: the db_verbose field was a simple boolean toggle, the <a href="../../api/c/envset_verbose.md" class="olink">DB_ENV-&gt;set_verbose()</a> method takes arguments that specify exactly which verbose messages are desired.</p></td>
</tr>
<tr>
<td>lg_max</td>
<td><a href="../../api/c/envset_lg_max.md" class="olink">DB_ENV-&gt;set_lg_max()</a></td>
</tr>
<tr>
<td>lk_conflicts</td>
<td><a href="../../api/c/envset_lk_conflicts.md" class="olink">DB_ENV-&gt;set_lk_conflicts()</a></td>
</tr>
<tr>
<td>lk_detect</td>
<td><a href="../../api/c/envset_lk_detect.md" class="olink">DB_ENV-&gt;set_lk_detect()</a></td>
</tr>
<tr>
<td>lk_max</td>
<td>dbenv-&gt;set_lk_max</td>
</tr>
<tr>
<td>lk_modes</td>
<td><a href="../../api/c/envset_lk_conflicts.md" class="olink">DB_ENV-&gt;set_lk_conflicts()</a></td>
</tr>
<tr>
<td>mp_mmapsize</td>
<td><a href="../../api/c/envset_mp_mmapsize.md" class="olink">DB_ENV-&gt;set_mp_mmapsize()</a></td>
</tr>
<tr>
<td>mp_size</td>
<td><a href="../../api/c/envset_cachesize.md" class="olink">DB_ENV-&gt;set_cachesize()</a>
<p>Note: the <a href="../../api/c/envset_cachesize.md" class="olink">DB_ENV-&gt;set_cachesize()</a> function takes additional arguments. Setting both the second argument (the number of GB in the pool) and the last argument (the number of memory pools to create) to 0 will result in behavior that is backward-compatible with previous Berkeley DB releases.</p></td>
</tr>
<tr>
<td>tx_info</td>
<td>This field was used by applications as an argument to the transaction subsystem functions. As those functions take references to a <a href="../../api/c/env.md" class="olink">DB_ENV</a> structure as arguments in the Berkeley DB 3.0 release, it should no longer be used by any application.</td>
</tr>
<tr>
<td>tx_max</td>
<td><a href="../../api/c/envset_tx_max.md" class="olink">DB_ENV-&gt;set_tx_max()</a></td>
</tr>
<tr>
<td>tx_recover</td>
<td>dbenv-&gt;set_tx_recover</td>
</tr>
</tbody>
</table>
