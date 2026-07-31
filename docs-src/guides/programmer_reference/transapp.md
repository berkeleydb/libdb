---
title: "Chapter 11.  Berkeley DB Transactional Data Store Applications"
api-name: "Chapter 11.  Berkeley DB Transactional Data Store Applications"
source: docs/programmer_reference/transapp.html
---
## Chapter 11.  Berkeley DB Transactional Data Store Applications

**Table of Contents**

<span class="sect1"> [Transactional Data Store introduction](transapp.md#transapp_intro) </span>

<span class="sect1"> [Why transactions?](transapp_why.md) </span>

<span class="sect1"> [Terminology](transapp_term.md) </span>

<span class="sect1"> [Handling failure in Transactional Data Store applications](transapp_fail.md) </span>

<span class="sect1"> [Architecting Transactional Data Store applications](transapp_app.md) </span>

<span class="sect1"> [Opening the environment](transapp_env_open.md) </span>

<span class="sect1"> [Opening the databases](transapp_data_open.md) </span>

<span class="sect1"> [Recoverability and deadlock handling](transapp_put.md) </span>

<span class="sect1"> [Atomicity](transapp_atomicity.md) </span>

<span class="sect1"> [Isolation](transapp_inc.md) </span>

<span class="sect1"> [Degrees of isolation](transapp_read.md) </span>

<span class="sect2"> [Snapshot Isolation](transapp_read.md#snapshot_isolation) </span>

<span class="sect1"> [Transactional cursors](transapp_cursor.md) </span>

<span class="sect1"> [Nested transactions](transapp_nested.md) </span>

<span class="sect1"> [Environment infrastructure](transapp_admin.md) </span>

<span class="sect1"> [Deadlock detection](transapp_deadlock.md) </span>

<span class="sect1"> [Checkpoints](transapp_checkpoint.md) </span>

<span class="sect1"> [Database and log file archival](transapp_archival.md) </span>

<span class="sect1"> [Log file removal](transapp_logfile.md) </span>

<span class="sect1"> [Recovery procedures](transapp_recovery.md) </span>

<span class="sect1"> [Hot failover](transapp_hotfail.md) </span>

<span class="sect1"> [Using Recovery on Journaling Filesystems](transapp_journal.md) </span>

<span class="sect1"> [Recovery and filesystem operations](transapp_filesys.md) </span>

<span class="sect1"> [Berkeley DB recoverability](transapp_reclimit.md) </span>

<span class="sect1"> [Transaction tuning](transapp_tune.md) </span>

<span class="sect1"> [Transaction throughput](transapp_throughput.md) </span>

<span class="sect1"> [Transaction FAQ](transapp_faq.md) </span>

## Transactional Data Store introduction

It is difficult to write a useful transactional tutorial and still keep within reasonable bounds of documentation; that is, without writing a book on transactional programming. We have two goals in this section: to familiarize readers with the transactional interfaces of Berkeley DB and to provide code building blocks that will be useful for creating applications.

We have not attempted to present this information using a real-world application. First, transactional applications are often complex and time-consuming to explain. Also, one of our goals is to give you an understanding of the wide variety of tools Berkeley DB makes available to you, and no single application would use most of the interfaces included in the Berkeley DB library. For these reasons, we have chosen to simply present the Berkeley DB data structures and programming solutions, using examples that differ from page to page. All the examples are included in a standalone program you can examine, modify, and run; and from which you will be able to extract code blocks for your own applications. Fragments of the program will be presented throughout this chapter, and the complete text of the <a href="transapp.cs" class="ulink" target="_top">example program</a> for IEEE/ANSI Std 1003.1 (POSIX) standard systems is included in the Berkeley DB distribution.
