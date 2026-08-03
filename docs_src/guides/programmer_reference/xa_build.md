---
title: "Building a Global Transaction Manager"
api-name: "Building a Global Transaction Manager"
source: docs/programmer_reference/xa_build.html
---
## Building a Global Transaction Manager

<span class="sect2"> [Communicating with multiple Berkeley DB environments](xa_build.md#idp52778488) </span>

<span class="sect2"> [Recovering from GTM failure](xa_build.md#idp52779432) </span>

<span class="sect2"> [Managing the Global Transaction ID (GID) name space](xa_build.md#idp52703176) </span>

<span class="sect2"> [Maintaining state for each distributed transaction.](xa_build.md#idp52758336) </span>

<span class="sect2"> [Recovering from the failure of a single environment](xa_build.md#idp52777008) </span>

<span class="sect2"> [Recovering from GTM failure](xa_build.md#idp52779896) </span>

Managing distributed transactions and using the two-phase commit protocol of Berkeley DB from an application requires the application to provide the functionality of a global transaction manager (GTM). The GTM is responsible for the following:

- Communicating with the multiple environments (potentially on separate systems).
- Managing the global transaction ID name space.
- Maintaining state information about each distributed transaction.
- Recovering from failures of individual environments.
- Recovering the global transaction state after failure of the global transaction manager.

### Communicating with multiple Berkeley DB environments

Two-phase commit is required if a transaction spans operations in multiple Berkeley DB environments or across operations in Berkeley DB and some other database systems. If the environments reside on the same machine, the application can communicate with each environment through its own address space with no additional complexity. If the environments reside on separate machines, the application may use its own messaging capability, translating messages on the remote machine into calls into the Berkeley DB library (including the recovery calls).

### Recovering from GTM failure

If the GTM fails, it must first recover its local state. Assuming the GTM uses Berkeley DB tables to maintain state, it should run the <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility (or the DB_RECOVER option to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>) upon startup. Once the GTM is back up and running, it needs to review all its outstanding global transactions, that is, all transactions that are recorded but not yet completed.

The global transactions that have not yet reached the prepare phase should be aborted. For each global transaction that has not yet prepared, the GTM should send a message to each participant telling it to abort its transaction.

Next the GTM should review its log to identify all participating environments that have transactions in the preparing, aborting, or committing states. For each such participant, the GTM should issue a <a href="../../api/c/txnrecover.md" class="olink">DB_ENV-&gt;txn_recover()</a> call. Upon receiving responses from each participant, the GTM must decide the fate of each transaction and issue appropriate calls. The correct behavior is defined as follows:

<span class="term">preparing</span>  
if all participating environments return the transaction in the list of prepared but not yet completed transactions, then the GTM should commit the transaction. If any participating environment fails to return the transaction in this list, then the GTM must issue an abort to all environments participating in that global transaction.

<span class="term">committing</span>  
the GTM should send a commit to any environment that returned this transaction in its list of prepared but not yet completed transactions.

<span class="term">aborting</span>  
the GTM should send an abort to any environment that returned this transaction in its list of prepared but not yet completed transactions.

### Managing the Global Transaction ID (GID) name space

A global transaction is a transaction that spans multiple environments. Each global transaction must have a unique transaction ID. This unique ID is the global transaction ID (GID). In Berkeley DB, global transaction IDs must be represented by the character array, `DB_GID_SIZE` (currently 128 bytes). It is the responsibility of the global transaction manager to assign GIDs, guarantee their uniqueness, and manage the mapping of local transactions to GID. That is, for each GID, the GTM should know which local transaction managers participated. The Berkeley DB logging system or a Berkeley DB table could be used to record this information.

### Maintaining state for each distributed transaction.

In addition to knowing which local environments participate in each global transaction, the GTM must also know the state of each active global transaction. As soon as a transaction becomes distributed (that is, a second environment participates), the GTM must record the existence of the global transaction and all participants (whether this must reside on stable storage or not depends on the exact configuration of the system). As new environments participate, the GTM must keep this information up to date.

When the GTM is ready to begin commit processing, it should issue <a href="../../api/c/txnprepare.md" class="olink">DB_TXN-&gt;prepare()</a> calls to each participating environment, indicating the GID of the global transaction. Once all the participants have successfully prepared, then the GTM must record that the global transaction will be committed. This record should go to stable storage. Once written to stable storage, the GTM can send <a href="../../api/c/txncommit.md" class="olink">DB_TXN-&gt;commit()</a> requests to each participating environment. Once all environments have successfully completed the commit, the GTM can either record the successful commit or can somehow "forget" the global transaction.

If an application uses nested transactions (that is, the parent parameter is non-NULL in a call to <a href="../../api/c/txnbegin.md" class="olink">DB_ENV-&gt;txn_begin()</a>) then, only the parent transaction should call <a href="../../api/c/txnprepare.md" class="olink">DB_TXN-&gt;prepare()</a>, not any of the child transactions.

Should any participant fail to prepare, then the GTM must abort the global transaction. The fact that the transaction is going to be aborted should be written to stable storage. Once written, the GTM can then issue <a href="../../api/c/txnabort.md" class="olink">DB_TXN-&gt;abort()</a> requests to each environment. When all aborts have returned successfully, the GTM can either record the successful abort or "forget" the global transaction.

In summary, for each transaction, the GTM must maintain the following:

- A list of participating environments
- The current state of each transaction (pre-prepare, preparing, committing, aborting, done)

### Recovering from the failure of a single environment

If a single environment fails, there is no need to bring down or recover other environments (the only exception to this is if all environments are managed in the same application address space and there is a risk that the failure of the environment corrupted other environments). Instead, once the failing environment comes back up, it should be recovered (that is, conventional recovery, via the <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility or by specifying the <a href="../../api/c/envopen.md#envopen_DB_RECOVER" class="olink">DB_RECOVER</a> flag to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> should be run). If the <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility is used, then the -e option must be specified. In this case, the application will almost certainly want to specify environmental parameters via a <a href="env_db_config.md" class="xref" title="DB_CONFIG configuration file">DB_CONFIG configuration file</a> in the environment's home directory, so that the <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility can create an appropriately configured environment. If the <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility is not used, then the GTM should call <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> specifying the <a href="../../api/c/envopen.md#envopen_DB_RECOVER" class="olink">DB_RECOVER</a> flag. It should then call <a href="../../api/c/txnrecover.md" class="olink">DB_ENV-&gt;txn_recover()</a>, which will return an array of DB_TXN handles for the set of prepared, but not yet completed transactions. For each transaction, the GTM should combine this knowledge with its transaction state table and call either <a href="../../api/c/txncommit.md" class="olink">DB_TXN-&gt;commit()</a> or <a href="../../api/c/txnabort.md" class="olink">DB_TXN-&gt;abort()</a>. After that process is complete, the environment is ready to participate in new transactions.

If the GTM is running in a system with multiple GTMs, it is possible that some of the transactions returned via <a href="../../api/c/txnrecover.md" class="olink">DB_ENV-&gt;txn_recover()</a> do not belong to the current environment. The GTM should detect this and call <a href="../../api/c/txndiscard.md" class="olink">DB_TXN-&gt;discard()</a> on each such transaction handle. Furthermore, it is important to note the environment does not retain information about which GTM has issued <a href="../../api/c/txnrecover.md" class="olink">DB_ENV-&gt;txn_recover()</a> operations. Therefore, each GTM should issue all its <a href="../../api/c/txnrecover.md" class="olink">DB_ENV-&gt;txn_recover()</a> calls before another GTM issues its calls. If the calls are interleaved, each GTM may not get a complete and consistent set of transactions. The simplest way to enforce this is for each GTM to make sure it can receive all its outstanding transactions in a single <a href="../../api/c/txnrecover.md" class="olink">DB_ENV-&gt;txn_recover()</a> call. The maximum number of possible outstanding transactions is roughly the maximum number of active transactions in the environment (whose value can be obtained using the <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility). To simplify this procedure, the caller should allocate an array large enough to hold the list of transactions (for example, allocate an array able to hold three times the maximum number of transactions). If that is not possible, callers should check that the array was not completely filled in when <a href="../../api/c/txnrecover.md" class="olink">DB_ENV-&gt;txn_recover()</a> returns. If the array was completely filled in, each transaction should be explicitly discarded, and the call repeated with a larger array.

The newly recovered environment will forbid any new transactions from being started until the prepared but not yet completed transactions have been resolved. In the multiple GTM case, this means that all GTMs must recover before any GTM can begin issuing new transactions.

The GTM must determine how long it needs to retain global transaction commit and abort records. If the participating environments are following a DB_TXN_SYNC policy, that is, they are forcing commit and abort records to disk before replying to the GTM, then once the GTM has heard from all participants, it need not retain its persistent log records. However, if participating environments are running at weaker durability levels, such as <a href="../../api/c/envset_flags.md#set_flags_DB_TXN_WRITE_NOSYNC" class="olink">DB_TXN_WRITE_NOSYNC</a> or <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a>, then the GTM must retain all commit and abort records until all participants have completed a checkpoint following the completion of a transaction.

### Recovering from GTM failure

If the GTM fails, it must first recover its local state. Assuming the GTM uses Berkeley DB tables to maintain state, it should run the <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility (or the DB_RECOVER option to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>) upon startup. Once the GTM is back up and running, it needs to review all its outstanding global transactions, that is, all transactions that are recorded but not yet completed.

The global transactions that have not yet reached the prepare phase should be aborted. For each global transaction that has not yet prepared, the GTM should send a message to each participant telling it to abort its transaction.

Next the GTM should review its log to identify all participating environments that have transactions in the preparing, aborting, or committing states. For each such participant, the GTM should issue a <a href="../../api/c/txnrecover.md" class="olink">DB_ENV-&gt;txn_recover()</a> call. Upon receiving responses from each participant, the GTM must decide the fate of each transaction and issue appropriate calls. The correct behavior is defined as follows:

<span class="term">preparing</span>  
if all participating environments return the transaction in the list of prepared but not yet completed transactions, then the GTM should commit the transaction. If any participating environment fails to return the transaction in this list, then the GTM must issue an abort to all environments participating in that global transaction.

<span class="term">committing</span>  
the GTM should send a commit to any environment that returned this transaction in its list of prepared but not yet completed transactions.

<span class="term">aborting</span>  
the GTM should send an abort to any environment that returned this transaction in its list of prepared but not yet completed transactions.
