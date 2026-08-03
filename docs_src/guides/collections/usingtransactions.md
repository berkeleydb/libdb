---
title: "Using Transactions"
api-name: "Using Transactions"
source: docs/collections/tutorial/usingtransactions.html
---
## Using Transactions

DB transactional applications have standard transactional characteristics: recoverability, atomicity and integrity (this is sometimes also referred to generically as <span class="emphasis">*ACID properties*</span>). The DB Java Collections API provides these transactional capabilities using a <span class="emphasis">*transaction-per-thread*</span> model. Once a transaction is begun, it is implicitly associated with the current thread until it is committed or aborted. This model is used for the following reasons.

- The transaction-per-thread model is commonly used in other Java APIs such as J2EE.

- Since the Java collections API is used for data access, there is no way to pass a transaction object to methods such as <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/util/Map.html#put(K,%20V)" class="ulink" target="_top">Map.put</a>.

The DB Java Collections API provides two transaction APIs. The lower-level API is the <a href="../../java/com/sleepycat/collections/CurrentTransaction.html" class="ulink" target="_top">CurrentTransaction</a> class. It provides a way to get the transaction for the current thread, and to begin, commit and abort transactions. It also provides access to the Berkeley DB core API <a href="../../java/com/sleepycat/db/Transaction.html" class="ulink" target="_top">Transaction</a> object. With <a href="../../java/com/sleepycat/collections/CurrentTransaction.html" class="ulink" target="_top">CurrentTransaction</a>, just as in the com.sleepycat.db API, the application is responsible for beginning, committing and aborting transactions, and for handling deadlock exceptions and retrying operations. This API may be needed for some applications, but it is not used in the example.

The example uses the higher-level <a href="../../java/com/sleepycat/collections/TransactionRunner.html" class="ulink" target="_top">TransactionRunner</a> and <a href="../../java/com/sleepycat/collections/TransactionWorker.html" class="ulink" target="_top">TransactionWorker</a> APIs, which are build on top of <a href="../../java/com/sleepycat/collections/CurrentTransaction.html" class="ulink" target="_top">CurrentTransaction</a>. `TransactionRunner.run()` automatically begins a transaction and then calls the `TransactionWorker.doWork()` method, which is implemented by the application.

The `TransactionRunner.run()` method automatically detects deadlock exceptions and performs retries by repeatedly calling the `TransactionWorker.doWork()` method until the operation succeeds or the maximum retry count is reached. If the maximum retry count is reached or if another exception (other than <a href="../../java/com/sleepycat/db/DeadlockException.html" class="ulink" target="_top">DeadlockException</a>) is thrown by `TransactionWorker.doWork()`, then the transaction will be automatically aborted. Otherwise, the transaction will be automatically committed.

Using this high-level API, if `TransactionRunner.run()` throws an exception, the application can assume that the operation failed and the transaction was aborted; otherwise, when an exception is not thrown, the application can assume the operation succeeded and the transaction was committed.

The `Sample.run()` method creates a `TransactionRunner` object and calls its `run()` method.

``` c
import com.sleepycat.collections.TransactionRunner;
import com.sleepycat.collections.TransactionWorker;
...
public class Sample
{
    private SampleDatabase db;
    ...
    private void run()
        throws Exception
    {
        TransactionRunner runner = 
            new TransactionRunner(db.getEnvironment());
        runner.run(new PopulateDatabase());
        runner.run(new PrintDatabase());
    }
    ...
    private class PopulateDatabase implements TransactionWorker
    {
        public void doWork()
            throws Exception
        {
        }
    }

    private class PrintDatabase implements TransactionWorker
    {
        public void doWork()
            throws Exception
        {
        }
    }
} 
```

The `run()` method is called by `main()` and was outlined in the previous section. It first creates a `TransactionRunner`, passing the database environment to its constructor.

It then calls `TransactionRunner.run()` to execute two transactions, passing instances of the application-defined `PopulateDatabase` and `PrintDatabase` nested classes. These classes implement the `TransactionWorker.doWork()` method and will be fully described in the next two sections.

For each call to `TransactionRunner.run()`, a separate transaction will be performed. The use of two transactions in the example — one for populating the database and another for printing its contents — is arbitrary. A real-life application should be designed to create transactions for each group of operations that should have ACID properties, while also taking into account the impact of transactions on performance.

The advantage of using `TransactionRunner` is that deadlock retries and transaction begin, commit and abort are handled automatically. However, a `TransactionWorker` class must be implemented for each type of transaction. If desired, anonymous inner classes can be used to implement the `TransactionWorker` interface.
