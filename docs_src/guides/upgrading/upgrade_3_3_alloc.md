---
title: "DB->set_malloc, DB->set_realloc"
api-name: "DB->set_malloc, DB->set_realloc"
source: docs/upgrading/upgrade_3_3_alloc.html
---
## DB-\>set_malloc, DB-\>set_realloc

There are two new methods in the Berkeley DB 3.3 release: <a href="../../api/c/envset_alloc.md" class="olink">DB_ENV-&gt;set_alloc()</a>. These functions allow applications to specify a set of allocation functions for the Berkeley DB library to use when allocating memory to be owned by the application and when freeing memory that was originally allocated by the application.

The new methods affect or replace the following historic methods:

<span class="term">DB-\>set_malloc</span>  
The DB-\>set_malloc method has been replaced in its entirety. Applications using this method should replace the call with a call to <a href="../../api/c/dbset_alloc.md" class="olink">DB-&gt;set_alloc()</a>.

<span class="term">DB-\>set_realloc</span>  
The DB-\>set_realloc method has been replaced in its entirety. Applications using this method should replace the call with a call to <a href="../../api/c/dbset_alloc.md" class="olink">DB-&gt;set_alloc()</a>.

<span class="term"><a href="../../api/c/dbstat.md" class="olink">DB-&gt;stat()</a> method</span>  
has been replaced. Applications using this method should do as follows: if the argument is NULL, it should simply be removed. If non-NULL, it should be replaced with a call to <a href="../../api/c/dbset_alloc.md" class="olink">DB-&gt;set_alloc()</a>.

<span class="term">lock_stat</span>  
The historic **db_malloc** argument to the lock_stat function has been replaced. Applications using this function should do as follows: if the argument is NULL, it should simply be removed. If non-NULL, it should be replaced with a call to <a href="../../api/c/envset_alloc.md" class="olink">DB_ENV-&gt;set_alloc()</a>.

<span class="term">log_archive</span>  
The historic **db_malloc** argument to the log_archive function has been replaced. Applications using this function should do as follows: if the argument is NULL, it should simply be removed. If non-NULL, it should be replaced with a call to <a href="../../api/c/envset_alloc.md" class="olink">DB_ENV-&gt;set_alloc()</a>.

<span class="term">log_stat</span>  
The historic **db_malloc** argument to the log_stat function has been replaced. Applications using this function should do as follows: if the argument is NULL, it should simply be removed. If non-NULL, it should be replaced with a call to <a href="../../api/c/envset_alloc.md" class="olink">DB_ENV-&gt;set_alloc()</a>.

<span class="term">memp_stat</span>  
The historic **db_malloc** argument to the memp_stat function has been replaced. Applications using this function should do as follows: if the argument is NULL, it should simply be removed. If non-NULL, it should be replaced with a call to <a href="../../api/c/envset_alloc.md" class="olink">DB_ENV-&gt;set_alloc()</a>.

<span class="term">txn_stat</span>  
The historic **db_malloc** argument to the txn_stat function has been replaced. Applications using this function should do as follows: if the argument is NULL, it should simply be removed. If non-NULL, it should be replaced with a call to <a href="../../api/c/envset_alloc.md" class="olink">DB_ENV-&gt;set_alloc()</a>.

One potential incompatibility for historic applications is that the allocation functions for a database environment must now be set before the environment is opened. Historically, Berkeley DB applications could open the environment first, and subsequently call the DB-\>set_malloc and DB-\>set_realloc methods; that use is no longer supported.
