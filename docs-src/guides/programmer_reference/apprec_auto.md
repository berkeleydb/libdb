---
title: "Automatically generated functions"
api-name: "Automatically generated functions"
source: docs/programmer_reference/apprec_auto.html
---
## Automatically generated functions

The XXX.src file is processed using the gen_rec.awk script included in the dist directory of the Berkeley DB distribution. This is an awk script that is executed from with the following command line:

``` c
awk -f gen_rec.awk \
    -v source_file=C_FILE \
    -v header_file=H_FILE \
    -v print_file=P_FILE \
    -v template_file=TMP_FILE < XXX.src
```

where <span class="emphasis">*C_FILE*</span> is the name of the file into which to place the automatically generated C code, <span class="emphasis">*H_FILE*</span> is the name of the file into which to place the automatically generated data structures and declarations, <span class="emphasis">*P_FILE*</span> is the name of the file into which to place the automatically generated C code that prints log records, and <span class="emphasis">*TMP_FILE*</span> is the name of the file into which to place a template for the recovery routines.

Because the gen_rec.awk script uses sources files located relative to the Berkeley DB dist directory, it must be run from the dist directory. For example, in building the Berkeley DB logging and recovery routines for ex_apprec, the following script is used to rebuild the automatically generated files:

``` c
E=../examples_c/ex_apprec

cd ../../dist
awk -f gen_rec.awk \
    -v source_file=$E/ex_apprec_auto.c \
    -v header_file=$E/ex_apprec_auto.h \
    -v print_file=$E/ex_apprec_autop.c \
    -v template_file=$E/ex_apprec_template < $E/ex_apprec.src
```

For each log record description found in the XXX.src file, the following structure declarations and \#defines will be created in the file <span class="emphasis">*header_file*</span>:

``` c
#define DB_PREFIX_RECORD_TYPE        /* Integer ID number */

typedef struct _PREFIX_RECORD_TYPE_args {
    /*
     * These three fields are generated for every record.
     */
    u_int32_t type;      /* Record type used for dispatch. */

    /*
     * Transaction handle that identifies the transaction on whose
     * behalf the record is being logged.
     */
    DB_TXN *txnid;

    /*
     * The log sequence number returned by the previous call to log_put
     * for this transaction.
     */
    DB_LSN *prev_lsn;

    /*
     * The rest of the structure contains one field for each of
     * the entries in the record statement.
     */
};
```

Thus, the auto-generated ex_apprec_mkdir_args structure looks as follows:

``` c
typedef struct _ex_apprec_mkdir_args {
    u_int32_t type;
    DB_TXN *txnid;
    DB_LSN prev_lsn;
    DBT dirname;
} ex_apprec_mkdir_args;
```

The template_file will contain a template for a recovery function. The recovery function is called on each record read from the log during system recovery, transaction abort, or the application of log records on a replication client, and is expected to redo or undo the operations described by that record. The details of the recovery function will be specific to the record being logged and need to be written manually, but the template provides a good starting point. (See ex_apprec_template and ex_apprec_rec.c for an example of both the template produced and the resulting recovery function.)

The template file should be copied to a source file in the application (but not the automatically generated source_file, as that will get overwritten each time gen_rec.awk is run) and fully developed there. The recovery function takes the following parameters:

> <span class="term">dbenv</span>  
> The environment in which recovery is running.
>
> <span class="term">rec</span>  
> The record being recovered.
>
> <span class="term">lsn</span>  
> The log sequence number of the record being recovered. The prev_lsn field, automatically included in every auto-generated log record, should be returned through this argument. The prev_lsn field is used to chain log records together to allow transaction aborts; because the recovery function is the only place that a log record gets parsed, the responsibility for returning this value lies with the recovery function writer.
>
> <span class="term">op</span>  
> A parameter of type db_recops, which indicates what operation is being run (<a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_ABORT" class="olink">DB_TXN_ABORT</a>, <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_APPLY" class="olink">DB_TXN_APPLY</a>, <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_BACKWARD_ROLL" class="olink">DB_TXN_BACKWARD_ROLL</a>, <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_FORWARD_ROLL" class="olink">DB_TXN_FORWARD_ROLL</a> or <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_PRINT" class="olink">DB_TXN_PRINT</a>).

In addition to the header_file and template_file, a source_file is created, containing a log, read, recovery, and print function for each record type.

The log function marshalls the parameters into a buffer, and calls <a href="../../api/c/logput.md" class="olink">DB_ENV-&gt;log_put()</a> on that buffer returning 0 on success and non-zero on failure. The log function takes the following parameters:

> <span class="term">dbenv</span>  
> The environment in which recovery is running.
>
> <span class="term">txnid</span>  
> The transaction identifier for the transaction handle returned by <a href="../../api/c/txnbegin.md" class="olink">DB_ENV-&gt;txn_begin()</a>.
>
> <span class="term">lsnp</span>  
> A pointer to storage for a log sequence number into which the log sequence number of the new log record will be returned.
>
> <span class="term">syncflag</span>  
> A flag indicating whether the record must be written synchronously. Valid values are 0 and <a href="../../api/c/logput.md#put_DB_FLUSH" class="olink">DB_FLUSH</a>.
>
> <span class="term">args</span>  
> The remaining parameters to the log message are the fields described in the XXX.src file, in order.

The read function takes a buffer and unmarshalls its contents into a structure of the appropriate type. It returns 0 on success and non-zero on error. After the fields of the structure have been used, the pointer returned from the read function should be freed. The read function takes the following parameters:

> <span class="term">dbenv</span>  
> The environment in which recovery is running.
>
> <span class="term">recbuf</span>  
> A buffer.
>
> <span class="term">argp</span>  
> A pointer to a structure of the appropriate type.

The print function displays the contents of the record. The print function takes the same parameters as the recovery function described previously. Although some of the parameters are unused by the print function, taking the same parameters allows a single dispatch loop to dispatch to a variety of functions. The print function takes the following parameters:

> <span class="term">dbenv</span>  
> The environment in which recovery is running.
>
> <span class="term">rec</span>  
> The record being recovered.
>
> <span class="term">lsn</span>  
> The log sequence number of the record being recovered.
>
> <span class="term">op</span>  
> Unused.

Finally, the source file will contain a function (named XXX_init_print, where XXX is replaced by the prefix) which should be added to the initialization part of the standalone <a href="../../api/c/db_printlog.md" class="olink">db_printlog</a> utility code so that utility can be used to display application-specific log records.
