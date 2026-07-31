---
title: "Terminology"
api-name: "Terminology"
source: docs/programmer_reference/transapp_term.html
---
## Terminology

Here are some definitions that will be helpful in understanding transactions:

<span class="term">Thread of control</span>  
Berkeley DB is indifferent to the type or style of threads being used by the application; or, for that matter, if threads are being used at all — because Berkeley DB supports multiprocess access. In the Berkeley DB documentation, any time we refer to a <span class="emphasis">*thread of control*</span>, it can be read as a true thread (one of many in an application's address space) or a process.

<span class="term">Free-threaded</span>  
A Berkeley DB handle that can be used by multiple threads simultaneously without any application-level synchronization is called <span class="emphasis">*free-threaded*</span>.

<span class="term">Transaction</span>  
A <span class="emphasis">*transaction*</span> is a one or more operations on one or more databases that should be treated as a single unit of work. For example, changes to a set of databases, in which either all of the changes must be applied to the database(s) or none of them should. Applications specify when each transaction starts, what database operations are included in it, and when it ends.

<span class="term">Transaction abort/commit</span>  
Every transaction ends by <span class="emphasis">*committing*</span> or <span class="emphasis">*aborting*</span>. If a transaction commits, Berkeley DB guarantees that any database changes included in the transaction will never be lost, even after system or application failure. If a transaction aborts, or is uncommitted when the system or application fails, then the changes involved will never appear in the database.

<span class="term">System or application failure</span>  
<span class="emphasis">*System or application failure*</span> is the phrase we use to describe something bad happening near your data. It can be an application dumping core, being interrupted by a signal, the disk filling up, or the entire system crashing. In any case, for whatever reason, the application can no longer make forward progress, and its databases are left in an unknown state.

<span class="term">Recovery</span>  
<span class="emphasis">*Recovery*</span> is what makes the database consistent after a system or application failure. The recovery process includes review of log files and databases to ensure that the changes from each committed transaction appear in the database, and that no changes from an unfinished (or aborted) transaction do. Whenever system or application failure occurs, applications must usually run recovery.

<span class="term">Deadlock</span>  
<span class="emphasis">*Deadlock*</span>, in its simplest form, happens when one thread of control owns resource A, but needs resource B; while another thread of control owns resource B, but needs resource A. Neither thread of control can make progress, and so one has to give up and release all its resources, at which time the remaining thread of control can make forward progress.
