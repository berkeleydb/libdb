---
title: "Read your writes consistency"
api-name: "Read your writes consistency"
source: docs/programmer_reference/rep_ryw.html
---
## Read your writes consistency

<span class="sect2"> [Getting a token](rep_ryw.md#gettoken) </span>

<span class="sect2"> [Token handling](rep_ryw.md#tokenhandling) </span>

<span class="sect2"> [Using a token to check or wait for a transaction](rep_ryw.md#usingtoken) </span>

Some applications require the ability to read replicated data at a client site, and determine whether it is consistent with data that has been written previously at the master site.

For example, a web application may be backed by multiple database environments, linked to form a replication group, in order to share the workload. Web requests that update data must be served by the replication master, but any site in the group may serve a read-only request. Consider a work flow of a series of web requests from one specific user at a web browser: the first request generates a database update, but the second request merely reads data. If the read-only request is served by a replication client database environment, it may be important to make sure that the updated data has been replicated to the client before performing the read (or to wait until it has been replicated) in order to show this user a consistent view of the data.

Berkeley DB supports this requirement through the use of transaction "tokens". A token is a form of identification for a transaction within the scope of the replication group. The application may request a copy of the transaction's token at the master site during the execution of the transaction. Later, the application running on a client site can use a copy of the token to determine whether the transaction has been applied at that site.

It is the application's responsibility to keep track of the token during the interim. In the web example, the token might be sent to the browser as a "cookie", or stored on the application server in the user's session context.

The operations described here are supported both for Replication Manager applications and for applications that use the replication Base API.

### Getting a token

In order to get a token, the application must supply a small memory buffer, using the <a href="../../api/c/txnset_commit_token.md" class="olink">DB_TXN-&gt;set_commit_token()</a> method.

Note that a token is generated only upon a successful commit operation, and therefore the token buffer content is valid only after a successful commit. Also, if a transaction does not perform any update operations it does not generate a useful token.

In the Berkeley DB Java and C# API, getting a token is simpler. The application need only invoke the <a href="../java/com/sleepycat/db/Transaction.html#getCommitToken()" class="ulink" target="_top">Transaction.getCommitToken()</a> method, after the transaction has committed.

### Token handling

The application should not try to interpret the content of the token buffer, but may store and/or transmit it freely between systems. However, since the buffer contains binary data it may be necessary to apply some encoding for transmission (e.g., base 64).

The data is resilient to differences in byte order between different systems. It does not expire: it may be retained indefinitely for later use, even across Berkeley DB version upgrades.

### Using a token to check or wait for a transaction

The <a href="../../api/c/envtxn_applied.md" class="olink">DB_ENV-&gt;txn_applied()</a> method takes a copy of a token, and determines whether the corresponding transaction is currently applied at the local site. The timeout argument allows the application to block for a bounded amount of time for cases where the transaction has not yet been applied.

Depending on the transaction durability levels implemented or configured by the application, it is sometimes possible for a transaction to disappear from a replication group if an original master site fails and a different site becomes the new master without having received the transaction. When the <a href="../../api/c/envtxn_applied.md" class="olink">DB_ENV-&gt;txn_applied()</a> method discovers this, it produces the `DB_NOTFOUND` return code.

This means that the results of <a href="../../api/c/envtxn_applied.md" class="olink">DB_ENV-&gt;txn_applied()</a> are not guaranteed forever. Even after a successful call to <a href="../../api/c/envtxn_applied.md" class="olink">DB_ENV-&gt;txn_applied()</a>, it is possible that by the time the application tries to read the data, the transaction and its data could have disappeared.

To avoid this problem the application should do the read operations in the context of a transaction, and hold the transaction handle open during the <a href="../../api/c/envtxn_applied.md" class="olink">DB_ENV-&gt;txn_applied()</a> call. The <a href="../../api/c/envtxn_applied.md" class="olink">DB_ENV-&gt;txn_applied()</a> method itself does not actually execute in the context of the transaction; but no rollbacks due to new master synchronization ever occur while a transaction is active, even a read-only transaction at a client site.

Note that the <a href="../../api/c/envtxn_applied.md" class="olink">DB_ENV-&gt;txn_applied()</a> method can return `DB_LOCK_DEADLOCK`. The application should respond to this situation just as it does for any other normal operation: abort any existing transaction, and then pause briefly before retrying.
