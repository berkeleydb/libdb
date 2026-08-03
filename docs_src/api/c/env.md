---
title: "Chapter 5.  The DB_ENV Handle"
api-name: "Chapter 5.  The DB_ENV Handle"
source: docs/api_reference/C/env.html
---
## Chapter 5.  The DB_ENV Handle

The `DB_ENV` object is the handle for a Berkeley DB environment — a collection including support for some or all of caching, locking, logging and transaction subsystems, as well as databases and log files. Methods of the `DB_ENV` handle are used to configure the environment as well as to operate on subsystems and databases in the environment.

`DB_ENV` handles are created using the <a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a> method, and are opened using the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.

When you are done using your environment, close it using the <a href="envclose.md" class="xref" title="DB_ENV-&gt;close()">DB_ENV-&gt;close()</a> method. Before closing your environment, make sure all open database handles are closed first. See the <a href="dbclose.md" class="xref" title="DB-&gt;close()">DB-&gt;close()</a> method for more information.

## Database Environments and Related Methods

<table data-border="1" width="80%">
<thead>
<tr>
<th>Database Environment Operations</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><a href="envbackup.md" class="xref" title="DB_ENV-&gt;backup()">DB_ENV-&gt;backup()</a></td>
<td>Hot back up an entire environment</td>
</tr>
<tr>
<td><a href="envclose.md" class="xref" title="DB_ENV-&gt;close()">DB_ENV-&gt;close()</a></td>
<td>Close an environment</td>
</tr>
<tr>
<td><a href="envcreate.md" class="xref" title="db_env_create">db_env_create</a></td>
<td>Create an environment handle</td>
</tr>
<tr>
<td><a href="envdbbackup.md" class="xref" title="DB_ENV-&gt;dbbackup()">DB_ENV-&gt;dbbackup()</a></td>
<td>Hot back up a single environment file</td>
</tr>
<tr>
<td><a href="envdbremove.md" class="xref" title="DB_ENV-&gt;dbremove()">DB_ENV-&gt;dbremove()</a></td>
<td>Remove a database</td>
</tr>
<tr>
<td><a href="envdbrename.md" class="xref" title="DB_ENV-&gt;dbrename()">DB_ENV-&gt;dbrename()</a></td>
<td>Rename a database</td>
</tr>
<tr>
<td><a href="enverr.md" class="xref" title="DB_ENV-&gt;err()">DB_ENV-&gt;err()</a></td>
<td>Error message</td>
</tr>
<tr>
<td><a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a></td>
<td>Check for thread failure</td>
</tr>
<tr>
<td><a href="envfileid_reset.md" class="xref" title="DB_ENV-&gt;fileid_reset()">DB_ENV-&gt;fileid_reset()</a></td>
<td>Reset database file IDs</td>
</tr>
<tr>
<td><a href="envfullversion.md" class="xref" title="db_full_version">db_full_version</a></td>
<td>Return full version information</td>
</tr>
<tr>
<td><a href="dbgetenv.md" class="xref" title="DB-&gt;get_env()">DB-&gt;get_env()</a></td>
<td>Return the DB's underlying DB_ENV handle</td>
</tr>
<tr>
<td><a href="envget_home.md" class="xref" title="DB_ENV-&gt;get_home()">DB_ENV-&gt;get_home()</a></td>
<td>Return environment's home directory</td>
</tr>
<tr>
<td><a href="envget_open_flags.md" class="xref" title="DB_ENV-&gt;get_open_flags()">DB_ENV-&gt;get_open_flags()</a></td>
<td>Return flags with which the environment was opened</td>
</tr>
<tr>
<td><a href="envlog_verify.md" class="xref" title="DB_ENV-&gt;log_verify()">DB_ENV-&gt;log_verify()</a></td>
<td>Verify log files of an environment.</td>
</tr>
<tr>
<td><a href="envlsn_reset.md" class="xref" title="DB_ENV-&gt;lsn_reset()">DB_ENV-&gt;lsn_reset()</a></td>
<td>Reset database file LSNs</td>
</tr>
<tr>
<td><a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a></td>
<td>Open an environment</td>
</tr>
<tr>
<td><a href="envremove.md" class="xref" title="DB_ENV-&gt;remove()">DB_ENV-&gt;remove()</a></td>
<td>Remove an environment</td>
</tr>
<tr>
<td><a href="envstat.md" class="xref" title="DB_ENV-&gt;stat_print()">DB_ENV-&gt;stat_print()</a></td>
<td>Environment statistics</td>
</tr>
<tr>
<td><a href="envstrerror.md" class="xref" title="db_strerror">db_strerror</a></td>
<td>Error strings</td>
</tr>
<tr>
<td><a href="envversion.md" class="xref" title="db_version">db_version</a></td>
<td>Return version information</td>
</tr>
<tr>
<td colspan="2"><strong>Environment Configuration</strong></td>
</tr>
<tr>
<td><a href="envadd_data_dir.md" class="xref" title="DB_ENV-&gt;add_data_dir()">DB_ENV-&gt;add_data_dir()</a></td>
<td>Add an environment data directory</td>
</tr>
<tr>
<td><a href="envset_alloc.md" class="xref" title="DB_ENV-&gt;set_alloc()">DB_ENV-&gt;set_alloc()</a></td>
<td>Set local space allocation functions</td>
</tr>
<tr>
<td><a href="envset_app_dispatch.md" class="xref" title="DB_ENV-&gt;set_app_dispatch()">DB_ENV-&gt;set_app_dispatch()</a></td>
<td>Configure application recovery callback</td>
</tr>
<tr>
<td><a href="envset_backup_callbacks.md" class="xref" title="DB_ENV-&gt;set_backup_callbacks()">DB_ENV-&gt;set_backup_callbacks()</a>, <a href="envget_backup_callbacks.md" class="xref" title="DB_ENV-&gt;get_backup_callbacks()">DB_ENV-&gt;get_backup_callbacks()</a></td>
<td>Set/get callbacks used for environment hot backups</td>
</tr>
<tr>
<td><a href="envset_backup_config.md" class="xref" title="DB_ENV-&gt;set_backup_config()">DB_ENV-&gt;set_backup_config()</a>, <a href="envget_backup_config.md" class="xref" title="DB_ENV-&gt;get_backup_config()">DB_ENV-&gt;get_backup_config()</a></td>
<td>Set/get environment hot backup configuration options</td>
</tr>
<tr>
<td><a href="envset_data_dir.md" class="xref" title="DB_ENV-&gt;set_data_dir()">DB_ENV-&gt;set_data_dir()</a>, <a href="envget_data_dirs.md" class="xref" title="DB_ENV-&gt;get_data_dirs()">DB_ENV-&gt;get_data_dirs()</a></td>
<td>Set/get the environment data directory</td>
</tr>
<tr>
<td><a href="envset_data_len.md" class="xref" title="DB_ENV-&gt;set_data_len()">DB_ENV-&gt;set_data_len()</a>, <a href="envget_data_len.md" class="xref" title="DB_ENV-&gt;get_data_len()">DB_ENV-&gt;get_data_len()</a></td>
<td>Set/get the command line utility byte limit</td>
</tr>
<tr>
<td><a href="envset_create_dir.md" class="xref" title="DB_ENV-&gt;set_create_dir()">DB_ENV-&gt;set_create_dir()</a>, <a href="envget_create_dir.md" class="xref" title="DB_ENV-&gt;get_create_dir()">DB_ENV-&gt;get_create_dir()</a></td>
<td>Add an environment data directory</td>
</tr>
<tr>
<td><a href="envset_encrypt.md" class="xref" title="DB_ENV-&gt;set_encrypt()">DB_ENV-&gt;set_encrypt()</a>, <a href="envget_encrypt_flags.md" class="xref" title="DB_ENV-&gt;get_encrypt_flags()">DB_ENV-&gt;get_encrypt_flags()</a></td>
<td>Set/get the environment cryptographic key</td>
</tr>
<tr>
<td><a href="envevent_notify.md" class="xref" title="DB_ENV-&gt;set_event_notify()">DB_ENV-&gt;set_event_notify()</a></td>
<td>Set event notification callback</td>
</tr>
<tr>
<td><a href="envset_errcall.md" class="xref" title="DB_ENV-&gt;set_errcall()">DB_ENV-&gt;set_errcall()</a></td>
<td>Set error message callbacks</td>
</tr>
<tr>
<td><a href="envset_errfile.md" class="xref" title="DB_ENV-&gt;set_errfile()">DB_ENV-&gt;set_errfile()</a>, <a href="envget_errfile.md" class="xref" title="DB_ENV-&gt;get_errfile()">DB_ENV-&gt;get_errfile()</a></td>
<td>Set/get error message FILE</td>
</tr>
<tr>
<td><a href="envset_errpfx.md" class="xref" title="DB_ENV-&gt;set_errpfx()">DB_ENV-&gt;set_errpfx()</a>, <a href="envget_errpfx.md" class="xref" title="DB_ENV-&gt;get_errpfx()">DB_ENV-&gt;get_errpfx()</a></td>
<td>Set/get error message prefix</td>
</tr>
<tr>
<td><a href="envset_feedback.md" class="xref" title="DB_ENV-&gt;set_feedback()">DB_ENV-&gt;set_feedback()</a></td>
<td>Set feedback callback</td>
</tr>
<tr>
<td><a href="envset_flags.md" class="xref" title="DB_ENV-&gt;set_flags()">DB_ENV-&gt;set_flags()</a>, <a href="envget_flags.md" class="xref" title="DB_ENV-&gt;get_flags()">DB_ENV-&gt;get_flags()</a></td>
<td>Environment configuration</td>
</tr>
<tr>
<td><a href="envset_intermediate_dir_mode.md" class="xref" title="DB_ENV-&gt;set_intermediate_dir_mode()">DB_ENV-&gt;set_intermediate_dir_mode()</a>, <a href="envget_intermediate_dir_mode.md" class="xref" title="DB_ENV-&gt;get_intermediate_dir_mode()">DB_ENV-&gt;get_intermediate_dir_mode()</a></td>
<td>Set/get intermediate directory creation mode</td>
</tr>
<tr>
<td><a href="envset_isalive.md" class="xref" title="DB_ENV-&gt;set_isalive()">DB_ENV-&gt;set_isalive()</a></td>
<td>Set thread is-alive callback</td>
</tr>
<tr>
<td><a href="envset_memory_init.md" class="xref" title="DB_ENV-&gt;set_memory_init()">DB_ENV-&gt;set_memory_init()</a>, <a href="envget_memory_init.md" class="xref" title="DB_ENV-&gt;get_memory_init()">DB_ENV-&gt;get_memory_init()</a></td>
<td>Set/get initial memory allocation</td>
</tr>
<tr>
<td><a href="envset_memory_max.md" class="xref" title="DB_ENV-&gt;set_memory_max()">DB_ENV-&gt;set_memory_max()</a>, <a href="envget_memory_max.md" class="xref" title="DB_ENV-&gt;get_memory_max()">DB_ENV-&gt;get_memory_max()</a></td>
<td>Set/get maximum memory allocation</td>
</tr>
<tr>
<td><a href="envset_metadata_dir.md" class="xref" title="DB_ENV-&gt;set_metadata_dir()">DB_ENV-&gt;set_metadata_dir()</a>, <a href="envget_metadata_dir.md" class="xref" title="DB_ENV-&gt;get_metadata_dir()">DB_ENV-&gt;get_metadata_dir()</a></td>
<td>Set/get the directory containing environment metadata</td>
</tr>
<tr>
<td><a href="envset_msgcall.md" class="xref" title="DB_ENV-&gt;set_msgcall()">DB_ENV-&gt;set_msgcall()</a></td>
<td>Set informational message callback</td>
</tr>
<tr>
<td><a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a>, <a href="envget_msgfile.md" class="xref" title="DB_ENV-&gt;get_msgfile()">DB_ENV-&gt;get_msgfile()</a></td>
<td>Set/get informational message FILE</td>
</tr>
<tr>
<td><a href="envset_shm_key.md" class="xref" title="DB_ENV-&gt;set_shm_key()">DB_ENV-&gt;set_shm_key()</a>, <a href="envget_shm_key.md" class="xref" title="DB_ENV-&gt;get_shm_key()">DB_ENV-&gt;get_shm_key()</a></td>
<td>Set/get system memory shared segment ID</td>
</tr>
<tr>
<td><a href="envset_thread_count.md" class="xref" title="DB_ENV-&gt;set_thread_count()">DB_ENV-&gt;set_thread_count()</a>, <a href="envget_thread_count.md" class="xref" title="DB_ENV-&gt;get_thread_count()">DB_ENV-&gt;get_thread_count()</a></td>
<td>Set/get approximate thread count</td>
</tr>
<tr>
<td><a href="envset_thread_id.md" class="xref" title="DB_ENV-&gt;set_thread_id()">DB_ENV-&gt;set_thread_id()</a></td>
<td>Set thread of control ID function</td>
</tr>
<tr>
<td><a href="envset_thread_id_string.md" class="xref" title="DB_ENV-&gt;set_thread_id_string()">DB_ENV-&gt;set_thread_id_string()</a></td>
<td>Set thread of control ID format function</td>
</tr>
<tr>
<td><a href="envset_timeout.md" class="xref" title="DB_ENV-&gt;set_timeout()">DB_ENV-&gt;set_timeout()</a>, <a href="envget_timeout.md" class="xref" title="DB_ENV-&gt;get_timeout()">DB_ENV-&gt;get_timeout()</a></td>
<td>Set/get lock and transaction timeout</td>
</tr>
<tr>
<td><a href="envset_tmp_dir.md" class="xref" title="DB_ENV-&gt;set_tmp_dir()">DB_ENV-&gt;set_tmp_dir()</a>, <a href="envget_tmp_dir.md" class="xref" title="DB_ENV-&gt;get_tmp_dir()">DB_ENV-&gt;get_tmp_dir()</a></td>
<td>Set/get the environment temporary file directory</td>
</tr>
<tr>
<td><a href="envset_verbose.md" class="xref" title="DB_ENV-&gt;set_verbose()">DB_ENV-&gt;set_verbose()</a>, <a href="envget_verbose.md" class="xref" title="DB_ENV-&gt;get_verbose()">DB_ENV-&gt;get_verbose()</a></td>
<td>Set/get verbose messages</td>
</tr>
<tr>
<td><a href="envset_cachesize.md" class="xref" title="DB_ENV-&gt;set_cachesize()">DB_ENV-&gt;set_cachesize()</a>, <a href="envget_cachesize.md" class="xref" title="DB_ENV-&gt;get_cachesize()">DB_ENV-&gt;get_cachesize()</a></td>
<td>Set/get the environment cache size</td>
</tr>
</tbody>
</table>
