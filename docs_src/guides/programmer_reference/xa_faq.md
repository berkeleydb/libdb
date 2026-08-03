---
title: "XA: Frequently Asked Questions"
api-name: "XA: Frequently Asked Questions"
source: docs/programmer_reference/xa_faq.html
---
## XA: Frequently Asked Questions

1.  **Is it possible to mix XA and non-XA transactions?**

    Yes. It is also possible for XA and non-XA transactions to coexist in the same Berkeley DB environment. To do this, specify the same environment to the non-XA <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> calls as was specified in the Tuxedo configuration file.

2.  **Does converting an application to run within XA change any of the already existing C/C++ API calls it does?**

    When converting an application to run under XA, the application's Berkeley DB calls are unchanged, with three exceptions:

    1.  The application must specify the <a href="../../api/c/dbcreate.md#dbcreate_DB_XA_CREATE" class="olink">DB_XA_CREATE</a> flag to the <a href="../../api/c/dbcreate.md" class="olink">db_create()</a> function.
    2.  Unless the application is performing an operation for a non-XA transaction, the application should never explicitly call <a href="../../api/c/txncommit.md" class="olink">DB_TXN-&gt;commit()</a>, <a href="../../api/c/txnabort.md" class="olink">DB_TXN-&gt;abort()</a>, and <a href="../../api/c/txnbegin.md" class="olink">DB_ENV-&gt;txn_begin()</a>, and those calls should be replaced by calls into the Tuxedo transaction manager.
    3.  Unless the application is performing an operation for a non-XA transaction, the application should specify a transaction argument of NULL to Berkeley DB methods taking transaction arguments (for example, <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a> or <a href="../../api/c/dbcursor.md" class="olink">DB-&gt;cursor()</a>).

    Otherwise, the application should be unchanged.

3.  **How does Berkeley DB recovery interact with recovery by the Tuxedo transaction manager?**

    Recovery is completed in two steps. First, each resource manager should recover its environment(s). This can be done via a program that calls <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> or by calling the <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility. If using the <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility, then the option should be specified so that the regions that are recovered persist after the utility exits. Any transactions that were prepared, but neither completed nor aborted, are restored to their prepared state so that they may be aborted or committed via the Tuxedo recovery mechanisms. After each resource manager has recovered, then Tuxedo recovery may begin. Tuxedo will interact with each resource manager via the \_\_db_xa_recover function which returns the list of prepared, but not yet completed transactions. It should issue a commit or abort for each one, and only after having completed each transaction will normal processing resume.

    Finally, standard log file archival and catastrophic recovery procedures should occur independently of XA operation.

4.  **Does Berkeley DB provide multi-threaded support for XA transactions?**

    Yes. For information on how to build multi-threaded servers for XA transactions, see <a href="http://download.oracle.com/docs/cd/E13161_01/tuxedo/docs10gr3/pgc/pgthr.html" class="ulink" target="_top">http://download.oracle.com/docs/cd/E13161_01/tuxedo/docs10gr3/pgc/pgthr.html</a>. All databases used by servers should be opened with handles created with the <a href="../../api/c/dbcreate.md#dbcreate_DB_XA_CREATE" class="olink">DB_XA_CREATE</a> flag in the <a href="../../api/c/dbcreate.md" class="olink">db_create()</a> method and must be opened in the tpsvrinit routine. Note that the environment parameter of the <a href="../../api/c/dbcreate.md" class="olink">db_create()</a> method must be assigned NULL. For more information on the tpsvrinit routine, see <a href="http://download.oracle.com/docs/cd/E13161_01/tuxedo/docs10gr3/pgc/pgthr.html" class="ulink" target="_top">http://download.oracle.com/docs/cd/E13161_01/tuxedo/docs10gr3/pgc/pgthr.html</a>.
