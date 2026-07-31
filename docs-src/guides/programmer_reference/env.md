---
title: "Chapter 9.  The Berkeley DB Environment"
api-name: "Chapter 9.  The Berkeley DB Environment"
source: docs/programmer_reference/env.html
---
## Chapter 9.  The Berkeley DB Environment

**Table of Contents**

<span class="sect1"> [Database environment introduction](env.md#env_intro) </span>

<span class="sect1"> [Creating a database environment](env_create.md) </span>

<span class="sect1"> [Sizing a database environment](env_size.md) </span>

<span class="sect1"> [Opening databases within the environment](env_open.md) </span>

<span class="sect1"> [Error support](env_error.md) </span>

<span class="sect1"> [DB_CONFIG configuration file](env_db_config.md) </span>

<span class="sect1"> [File naming](env_naming.md) </span>

<span class="sect2"> [Specifying file naming to Berkeley DB](env_naming.md#idp51749352) </span>

<span class="sect2"> [Filename resolution in Berkeley DB](env_naming.md#idp51763728) </span>

<span class="sect2"> [Examples](env_naming.md#idp51756464) </span>

<span class="sect1"> [Shared memory regions](env_region.md) </span>

<span class="sect1"> [Security](env_security.md) </span>

<span class="sect1"> [Encryption](env_encrypt.md) </span>

<span class="sect1"> [Remote filesystems](env_remote.md) </span>

<span class="sect1"> [Environment FAQ](env_faq.md) </span>

## Database environment introduction

A Berkeley DB environment is an encapsulation of one or more databases, log files and region files. Region files are the shared memory areas that contain information about the database environment such as memory pool cache pages. Only databases are byte-order independent and only database files can be moved between machines of different byte orders. Log files can be moved between machines of the same byte order. Region files are usually unique to a specific machine and potentially to a specific operating system release.

The simplest way to administer a Berkeley DB application environment is to create a single **home** directory that stores the files for the applications that will share the environment. The environment home directory must be created before any Berkeley DB applications are run. Berkeley DB itself never creates the environment home directory. The environment can then be identified by the name of that directory.

An environment may be shared by any number of processes, as well as by any number of threads within those processes. It is possible for an environment to include resources from other directories on the system, and applications often choose to distribute resources to other directories or disks for performance or other reasons. However, by default, the databases, shared regions (the locking, logging, memory pool, and transaction shared memory areas) and log files will be stored in a single directory hierarchy.

It is important to realize that all applications sharing a database environment implicitly trust each other. They have access to each other's data as it resides in the shared regions, and they will share resources such as buffer space and locks. At the same time, any applications using the same databases **must** share an environment if consistency is to be maintained between them.

For more information on the operations supported by the database environment handle, see the <a href="../../api/c/env.md#envlist" class="olink">Database Environments and Related Methods</a> section in the *Berkeley DB C API Reference Guide.*
