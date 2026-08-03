---
title: "Chapter 2. Enabling Transactions"
api-name: "Chapter 2. Enabling Transactions"
source: docs/gsg_txn/JAVA/enabletxn.html
---
## Chapter 2. Enabling Transactions

**Table of Contents**

<span class="sect1"> [Environments](enabletxn.md#environments) </span>

<span class="sect2"> [File Naming](enabletxn.md#filenaming) </span>

<span class="sect2"> [Error Support](enabletxn.md#errorsupport) </span>

<span class="sect2"> [Shared Memory Regions](enabletxn.md#sharedmemory) </span>

<span class="sect2"> [Security Considerations](enabletxn.md#security) </span>

<span class="sect1"> [Opening a Transactional Environment and Store or Database](envopen.md) </span>

In order to use transactions with your application, you must turn them on. To do this you must:

- Use an environment (see <a href="enabletxn.md#environments" class="xref" title="Environments">Environments</a> for details).

- Turn on transactions for your environment. You do this by using the `EnvironmentConfig.setTransactional()` method. Note that initializing the transactional subsystem implies that the logging subsystem is also initialized. Also, note that if you do not initialize transactions when you first create your environment, then you cannot use transactions for that environment after that. This is because DB allocates certain structures needed for transactional locking that are not available if the environment is created without transactional support.

- Initialize the in-memory cache by passing `true` to the `EnvironmentConfig.setInitializeCache()` method.

- Initialize the locking subsystem. This is what provides locking for concurrent applications. It also is used to perform deadlock detection. See <a href="txnconcurrency.md" class="xref" title="Chapter 4. Concurrency">Concurrency</a> for more information.

  You initialize the locking subsystem by passing `true` to the `EnvironmentConfig.setInitializeLocking()` method.

- If you are using the DPL, transaction-enable your stores. You do this by using the `StoreConfig.setTransactional() method.`

-  Transaction-enable your databases. If you are using the base API, transaction-enable your databases. You do this by using the `DatabaseConfig.setTransactional()` method, and then opening the database from within a transaction. Note that the common practice is for auto commit to be used to transaction-protect the database open. To use auto-commit, you must still enable transactions as described here, but you do not have to explicitly use a transaction when you open your database. An example of this is given in the next section.

## Environments

<span class="sect2"> [File Naming](enabletxn.md#filenaming) </span>

<span class="sect2"> [Error Support](enabletxn.md#errorsupport) </span>

<span class="sect2"> [Shared Memory Regions](enabletxn.md#sharedmemory) </span>

<span class="sect2"> [Security Considerations](enabletxn.md#security) </span>

For simple DB applications, environments are optional. However, in order to transaction protect your database operations, you must use an environment.

An <span class="emphasis">*environment*</span>, represents an encapsulation of one or more databases and any associated log and region files. They are used to support multi-threaded and multi-process applications by allowing different threads of control to share the in-memory cache, the locking tables, the logging subsystem, and the file namespace. By sharing these things, your concurrent application is more efficient than if each thread of control had to manage these resources on its own.

By default all DB databases are backed by files on disk. In addition to these files, transactional DB applications create logs that are also by default stored on disk (they can optionally be backed using shared memory). Finally, transactional DB applications also create and use shared-memory regions that are also typically backed by the filesystem. But like databases and logs, the regions can be maintained strictly in-memory if your application requires it. For an example of an application that manages all environment files in-memory, see <a href="inmem_txnexample_java.md" class="xref" title="Base API In-Memory Transaction Example">Base API In-Memory Transaction Example</a>.

### Warning

Using environments with some journaling filesystems might result in log file corruption. This can occur if the operating system experiences an unclean shutdown when a log file is being created. Please see <a href="../../programmer_reference/transapp_journal.html" class="olink">Using Recovery on Journaling Filesystems</a> in the *Berkeley DB Programmer's Reference Guide* for more information.

### File Naming

In order to operate, your DB application must be able to locate its database files, log files, and region files. If these are stored in the filesystem, then you must tell DB where they are located (a number of mechanisms exist that allow you to identify the location of these files – see below). Otherwise, by default they are located in the current working directory.

#### Specifying the Environment Home Directory

The environment home directory is used to determine where DB files are located. Its location is identified using one of the following mechanisms, in the following order of priority:

- If no information is given as to where to put the environment home, then the current working directory is used.

- If a home directory is specified on the `Environment()` constructor, then that location is always used for the environment home.

- If a home directory is not supplied to `Environment()`, then the directory identified by the `DB_HOME` environment variable is used <span class="emphasis">*if*</span> you specify `true` to either the `EnvironmentConfig.setUseEnvironment()` or `EnvironmentConfig.setUseEnvironmentRoot()` method. Both methods allow you to identify the path to the environment's home directory using `DB_HOME`. However, `EnvironmentConfig.setUseEnvironmentRoot()` is honored only if the process is run with root or administrative privileges.

#### Specifying File Locations

By default, all DB files are created relative to the environment home directory. For example, suppose your environment home is in `/export/myAppHome`. Also suppose you name your database `data/myDatabase.db`. Then in this case, the database is placed in: `/export/myAppHome/data/myDatabase.db`.

That said, DB always defers to absolute pathnames. This means that if you provide an absolute filename when you name your database, then that file is <span class="emphasis">*not*</span> placed relative to the environment home directory. Instead, it is placed in the exact location that you specified for the filename.

On UNIX systems, an absolute pathname is a name that begins with a forward slash ('/'). On Windows systems, an absolute pathname is a name that begins with one of the following:

- A backslash ('\\).

- Any alphabetic letter, followed by a colon (':'), followed by a backslash ('\\).

### Note

Try not to use absolute path names for your environment's files. Under certain recovery scenarios, absolute path names can render your environment unrecoverable. This occurs if you are attempting to recover your environment on a system that does not support the absolute path name that you used.

#### Identifying Specific File Locations

As described in the previous sections, DB will place all its files in or relative to the environment home directory. You can also cause a specific database file to be placed in a particular location by using an absolute path name for its name. In this situation, the environment's home directory is not considered when naming the file.

It is frequently desirable to place database, log, and region files on separate disk drives. By spreading I/O across multiple drives, you can increase parallelism and improve throughput. Additionally, by placing log files and database files on separate drives, you improve your application's reliability by providing your application with a greater chance of surviving a disk failure.

You can cause DB's files to be placed in specific locations using the following mechanisms:

<table data-border="1" width="80%">
<thead>
<tr>
<th>File Type</th>
<th>To Override</th>
</tr>
</thead>
<tbody>
<tr>
<td>database files</td>
<td><p>You can cause database files to be created in a directory other than the environment home by using the <code class="methodname">EnvironmentConfig.addDataDir()</code> method. The directory identified here must exist. If a relative path is provided, then the directory location is resolved relative to the environment's home directory.</p>
<p>This method modifies the directory used for database files created and managed by a single environment handle; it does not configure the entire environment.</p>
<p>You can also set a default data location that is used by the entire environment by using the <code class="literal">add_data_dir</code> parameter in the environment's <code class="literal">DB_CONFIG</code> file. Note that the <code class="literal">add_data_dir</code> parameter overrides any value set by the <code class="methodname">EnvironmentConfig.addDataDir()</code> method.</p></td>
</tr>
<tr>
<td>Log files</td>
<td><p>You can cause log files to be created in a directory other than the environment home directory by using the <code class="methodname">EnvironmentConfig.LogDirectory()</code> method. The directory identified here must exist. If a relative path is provided, then the directory location is resolved relative to the environment's home directory.</p>
<p>This method modifies the directory used for database files created and managed by a single environment handle; it does not configure the entire environment.</p>
<p>You can also set a default log file location that is used by the entire environment by using the <code class="literal">set_lg_dir</code> parameter in the environment's <code class="literal">DB_CONFIG</code> file. Note that the <code class="literal">set_lg_dir</code> parameter overrides any value set by the <code class="methodname">EnvironmentConfig.setLogDirectory()</code> method.</p></td>
</tr>
<tr>
<td>Temporary files</td>
<td><p>You can cause temporary files required by the environment to be created in a directory other than the environment home directory by using the <code class="methodname">EnvironmentConfig.setTemporaryDirectory()</code> method. The directory identified here must exist. If a relative path is provided, then the directory location is resolved relative to the environment's home directory.</p>
<p>You can also set a temporary file location by using the <code class="literal">set_tmp_dir</code> parameter in the environment's <code class="literal">DB_CONFIG</code> file. Note that the <code class="literal">set_tmp_dir</code> parameter overrides any value set by the <code class="methodname">EnvironmentConfig.setTemporaryDirectory()</code> method.</p></td>
</tr>
<tr>
<td>Metadata files</td>
<td><p>You can cause persistent metadata files required by the replicated applications to be created in a directory other than the environment home directory by using the <code class="methodname">EnvironmentConfig.setMetadataDir()</code> method. The directory identified here must exist. If a relative path is provided, then the directory location is resolved relative to the environment's home directory.</p>
<p>You can also set a metadata directory location by using the <code class="literal">set_metadata_dir</code> parameter in the environment's <code class="literal">DB_CONFIG</code> file. Note that the <code class="literal">set_metadata_dir</code> parameter overrides any value set by the <code class="methodname">EnvironmentConfig.setMetadataDir()</code> method.</p></td>
</tr>
<tr>
<td>Region files</td>
<td>If backed by the filesystem, region files are always placed in the environment home directory.</td>
</tr>
</tbody>
</table>

Note that the `DB_CONFIG` must reside in the environment home directory. Parameters are specified in it one parameter to a line. Each parameter is followed by a space, which is followed by the parameter value. For example:

``` c
    add_data_dir /export1/db/env_data_files 
```

### Error Support

To simplify error handling and to aid in application debugging, environments offer several useful methods. Note that many of these methods are identical to the error handling methods available for the DatabaseConfig class. They are:

- `EnvironmentConfig.setErrorStream()`

  Sets the Java `OutputStream` to be used for displaying error messages issued by the DB library.

- `EnvironmentConfig.setErrorHandler()`

  Defines the message handler that is called when an error message is issued by DB. The error prefix and message are passed to this callback. It is up to the application to display this information correctly.

  Note that the message handler must be an implementation of the `com.sleepycat.db.ErrorHandler` interface.

  This is the recommended way to get error messages from DB.

- `EnvironmentConfig.setErrorPrefix()`

  Sets the prefix used to for any error messages issued by the DB library.

### Shared Memory Regions

The subsystems that you enable for an environment (in our case, transaction, logging, locking, and the memory pool) are described by one or more regions. The regions contain all of the state information that needs to be shared among threads and/or processes using the environment.

Regions may be backed by the file system, by heap memory, or by system shared memory.

### Note

When DB dynamically obtains memory, it uses memory outside of the JVM. Normally the amount of memory that DB obtains is trivial, a few bytes here and there, so you might not notice it. However, if heap or system memory is used to back your region files, then this can represent a significant amount of memory being used by DB above and beyond the memory required by the JVM process. As a result, the JVM process may appear to be using more memory than you told the process it could use.

#### Regions Backed by Files

By default, shared memory regions are created as files in the environment's home directory (<span class="emphasis">*not*</span> the environment's data directory). If it is available, the POSIX `mmap` interface is used to map these files into your application's address space. If `mmap` is not available, then the UNIX `shmget` interfaces are used instead (again, if they are available).

In this default case, the region files are named `__db.###` (for example, `__db.001`, `__db.002`, and so on).

#### Regions Backed by Heap Memory

If heap memory is used to back your shared memory regions, then you can only open a single handle for the environment. This means that the environment cannot be accessed by multiple processes. In this case, the regions are managed only in memory, and they are not written to the filesystem. You indicate that heap memory is to be used for the region files by specifying `true` to the `EnvironmentConfig.setPrivate()` method.

Note that you can also set this flag by using the `set_open_flags` parameter in the `DB_CONFIG` file. See the *Berkeley DB C API Reference Guide* for more information.

(For an example of an entirely in-memory transactional application, see <a href="inmem_txnexample_java.md" class="xref" title="Base API In-Memory Transaction Example">Base API In-Memory Transaction Example</a>.)

#### Regions Backed by System Memory

Finally, you can cause system memory to be used for your regions instead of memory-mapped files. You do this by providing `true` to the `EnvironmentConfig.setSystemMemory()` method.

When region files are backed by system memory, DB creates a single file in the environment's home directory. This file contains information necessary to identify the system shared memory in use by the environment. By creating this file, DB enables multiple processes to share the environment.

The system memory that is used is architecture-dependent. For example, on systems supporting X/Open-style shared memory interfaces, such as UNIX systems, the `shmget(2)` and related System V IPC interfaces are used. Additionally, VxWorks systems use system memory. In these cases, an initial segment ID must be specified by the application to ensure that applications do not overwrite each other's environments, so that the number of segments created does not grow without bounds. See the `EnvironmentConfig.setSegmentId()` method for more information.

On Windows platforms, the use of system memory for the region files is problematic because the operating system uses reference counting to clean up shared objects in the paging file automatically. In addition, the default access permissions for shared objects are different from files, which may cause problems when an environment is accessed by multiple processes running as different users. See <a href="" class="ulink" target="_top">Windows notes</a> or more information.

### Security Considerations

When using environments, there are some security considerations to keep in mind:

- Database environment permissions

  The directory used for the environment should have its permissions set to ensure that files in the environment are not accessible to users without appropriate permissions. Applications that add to the user's permissions (for example, UNIX `setuid` or `setgid` applications), must be carefully checked to not permit illegal use of those permissions such as general file access in the environment directory.

- Environment variables

  Setting `true` for `EnvironmentConfig.setUseEnvironment()` or `EnvironmentConfig.setUseEnvironmentRoot()` so that environment variables can be used during file naming can be dangerous. Setting those flags in DB applications with additional permissions (for example, UNIX `setuid` or `setgid` applications) could potentially allow users to read and write databases to which they would not normally have access.

  For example, suppose you write a DB application that runs `setuid`. This means that when the application runs, it does so under a userid different than that of the application's caller. This is especially problematic if the application is granting stronger privileges to a user than the user might ordinarily have.

  Now, if `true` is specified for `EnvironmentConfig.setUseEnvironment()` or `EnvironmentConfig.setUseEnvironmentRoot()`, then the environment that the application is using is modifiable using the `DB_HOME` environment variable. In this scenario, if the uid used by the application has sufficiently broad privileges, then the application's caller can read and/or write databases owned by another user simply by setting his `DB_HOME` environment variable to the environment used by that other user.

  Note that this scenario need not be malicious; the wrong environment could be used by the application simply by inadvertently specifying the wrong path to `DB_HOME`.

  As always, you should use `setuid` sparingly, if at all. But if you do use `setuid`, then you should refrain from specifying `true` for `EnvironmentConfig.setUseEnvironment()` or `EnvironmentConfig.setUseEnvironmentRoot()` for the environment open. And, of course, if you must use `setuid`, then make sure you use the weakest uid possible – preferably one that is used only by the application itself.

- File permissions

  By default, DB always creates database and log files readable and writable by the owner and the group (that is, `S_IRUSR`, `S_IWUSR`, `S_IRGRP` and `S_IWGRP`; or octal mode 0660 on historic UNIX systems). The group ownership of created files is based on the system and directory defaults, and is not further specified by DB.

- Temporary backing files

  If an unnamed database is created and the cache is too small to hold the database in memory, Berkeley DB will create a temporary physical file to enable it to page the database to disk as needed. In this case, environment variables such as `TMPDIR` may be used to specify the location of that temporary file. Although temporary backing files are created readable and writable by the owner only (`S_IRUSR` and `S_IWUSR`, or octal mode 0600 on historic UNIX systems), some filesystems may not sufficiently protect temporary files created in random directories from improper access. To be absolutely safe, applications storing sensitive data in unnamed databases should use the `EnvironmentConfig.setTemporaryDirectory()` method to specify a temporary directory with known permissions.
