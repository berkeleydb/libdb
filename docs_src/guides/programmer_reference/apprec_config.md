---
title: "Application configuration"
api-name: "Application configuration"
source: docs/programmer_reference/apprec_config.html
---
## Application configuration

The application should include a dispatch function that dispatches to appropriate printing and/or recovery functions based on the log record type and the operation code. The dispatch function should take the same arguments as the recovery function, and should call the appropriate recovery and/or printing functions based on the log record type and the operation code. For example, the ex_apprec dispatch function is as follows:

``` c
int
apprec_dispatch(dbenv, dbt, lsn, op)
    DB_ENV *dbenv;
    DBT *dbt;
    DB_LSN *lsn;
    db_recops op;
{
    u_int32_t rectype;
    /* Pull the record type out of the log record. */
    memcpy(&rectype, dbt->data, sizeof(rectype));
    switch (rectype) {
    case DB_ex_apprec_mkdir:
        return (ex_apprec_mkdir_recover(dbenv, dbt, lsn, op));
    default:
        /*
         * We've hit an unexpected, allegedly user-defined record
         * type.
         */
        dbenv->errx(dbenv, "Unexpected log record type encountered");
        return (EINVAL);
    }
}
```

Applications use this dispatch function and the automatically generated functions as follows:

1.  When the application starts, call the <a href="../../api/c/envset_app_dispatch.md" class="olink">DB_ENV-&gt;set_app_dispatch()</a> with your dispatch function.
2.  Issue a <a href="../../api/c/txnbegin.md" class="olink">DB_ENV-&gt;txn_begin()</a> call before any operations you want to be transaction-protected.
3.  Before accessing any data, issue the appropriate lock call to lock the data (either for reading or writing).
4.  Before modifying any data that is transaction-protected, issue a call to the appropriate log function.
5.  Call <a href="../../api/c/txncommit.md" class="olink">DB_TXN-&gt;commit()</a> to cancel all of the modifications.

The recovery functions are called in the three following cases:

1.  During recovery after application or system failure, with op set to <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_FORWARD_ROLL" class="olink">DB_TXN_FORWARD_ROLL</a> or <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_BACKWARD_ROLL" class="olink">DB_TXN_BACKWARD_ROLL</a>.
2.  During transaction abort, with op set to <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_ABORT" class="olink">DB_TXN_ABORT</a>.
3.  On a replicated client to apply updates from the master, with op set to <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_APPLY" class="olink">DB_TXN_APPLY</a>.

For each log record type you declare, you must write the appropriate function to undo and redo the modifications. The shell of these functions will be generated for you automatically, but you must fill in the details.

Your code must be able to detect whether the described modifications have been applied to the data. The function will be called with the "op" parameter set to <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_ABORT" class="olink">DB_TXN_ABORT</a> when a transaction that wrote the log record aborts, with <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_FORWARD_ROLL" class="olink">DB_TXN_FORWARD_ROLL</a> and <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_BACKWARD_ROLL" class="olink">DB_TXN_BACKWARD_ROLL</a> during recovery, and with <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_APPLY" class="olink">DB_TXN_APPLY</a> on a replicated client.

The actions for <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_ABORT" class="olink">DB_TXN_ABORT</a> and <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_BACKWARD_ROLL" class="olink">DB_TXN_BACKWARD_ROLL</a> should generally be the same, and the actions for <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_FORWARD_ROLL" class="olink">DB_TXN_FORWARD_ROLL</a> and <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_APPLY" class="olink">DB_TXN_APPLY</a> should generally be the same. However, if the application is using Berkeley DB replication and another thread of control may be performing read operations while log records are applied on a replication client, the recovery function should perform appropriate locking during <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_APPLY" class="olink">DB_TXN_APPLY</a> operations. In this case, the recovery function may encounter deadlocks when issuing locking calls. The application should run with the deadlock detector, and the recovery function should simply return <a href="program_errorret.md#program_errorret.DB_LOCK_DEADLOCK" class="link">DB_LOCK_DEADLOCK</a> if a deadlock is detected and a locking operation fails with that error.

The <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_PRINT" class="olink">DB_TXN_PRINT</a> operation should print the log record, typically using the auto-generated print function; it is not used in the Berkeley DB library, but may be useful for debugging, as in the <a href="../../api/c/db_printlog.md" class="olink">db_printlog</a> utility. Applications may safely ignore this operation code, they may handle printing from the recovery function, or they may dispatch directly to the auto-generated print function.

One common way to determine whether operations need to be undone or redone is the use of log sequence numbers (LSNs). For example, each access method database page contains the LSN of the most recent log record that describes a modification to the page. When the access method changes a page, it writes a log record describing the change and including the LSN that was on the page before the change. This LSN is referred to as the previous LSN. The recovery functions read the page described by a log record, and compare the LSN on the page to the LSN they were passed.

If the page LSN is less than the passed LSN and the operation is an undo, no action is necessary (because the modifications have not been written to the page). If the page LSN is the same as the previous LSN and the operation is a redo, the actions described are reapplied to the page. If the page LSN is equal to the passed LSN and the operation is an undo, the actions are removed from the page; if the page LSN is greater than the passed LSN and the operation is a redo, no further action is necessary. If the action is a redo and the LSN on the page is less than the previous LSN in the log record, it is an error because it could happen only if some previous log record was not processed.

Examples of other recovery functions can be found in the Berkeley DB library recovery functions (found in files named XXX_rec.c) and in the application-specific recovery example (specifically, ex_apprec_rec.c).

Finally, applications need to ensure that any data modifications they have made, that were part of a committed transaction, must be written to stable storage before calling the <a href="../../api/c/txncheckpoint.md" class="olink">DB_ENV-&gt;txn_checkpoint()</a> method. This is to allow the periodic removal of database environment log files.
