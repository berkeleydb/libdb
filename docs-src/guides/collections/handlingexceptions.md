---
title: "Handling Exceptions"
api-name: "Handling Exceptions"
source: docs/collections/tutorial/handlingexceptions.html
---
## Handling Exceptions

Exception handling was illustrated previously in <a href="implementingmain.md" class="xref" title="Implementing the Main Program">Implementing the Main Program</a> and <a href="usingtransactions.md" class="xref" title="Using Transactions">Using Transactions</a> exception handling in a DB Java Collections API application in more detail.

There are two exceptions that must be treated specially: <a href="../../java/com/sleepycat/db/RunRecoveryException.html" class="ulink" target="_top">RunRecoveryException</a> and <a href="../../java/com/sleepycat/db/DeadlockException.html" class="ulink" target="_top">DeadlockException</a>.

<a href="../../java/com/sleepycat/db/RunRecoveryException.html" class="ulink" target="_top">RunRecoveryException</a> is thrown when the only solution is to shut down the application and run recovery. All applications must catch this exception and follow the recovery procedure.

When <a href="../../java/com/sleepycat/db/DeadlockException.html" class="ulink" target="_top">DeadlockException</a> is thrown, the application should normally retry the operation. If a deadlock continues to occur for some maximum number of retries, the application should give up and try again later or take other corrective actions. The DB Java Collections API provides two APIs for transaction execution.

- When using the <a href="../../java/com/sleepycat/collections/CurrentTransaction.html" class="ulink" target="_top">CurrentTransaction</a> class directly, the application must catch <a href="../../java/com/sleepycat/db/DeadlockException.html" class="ulink" target="_top">DeadlockException</a> and follow the procedure described previously.

- When using the <a href="../../java/com/sleepycat/collections/TransactionRunner.html" class="ulink" target="_top">TransactionRunner</a> class, retries are performed automatically and the application need only handle the case where the maximum number of retries has been reached. In that case, <a href="../../java/com/sleepycat/collections/TransactionRunner.html#run(com.sleepycat.collections.TransactionWorker)" class="ulink" target="_top">TransactionRunner.run</a> will throw <a href="../../java/com/sleepycat/db/DeadlockException.html" class="ulink" target="_top">DeadlockException</a>.

When using the <a href="../../java/com/sleepycat/collections/TransactionRunner.html" class="ulink" target="_top">TransactionRunner</a> class there are two other considerations.

- First, if the application-defined <a href="../../java/com/sleepycat/collections/TransactionWorker.html#doWork()" class="ulink" target="_top">TransactionWorker.doWork</a> method throws an exception the transaction will automatically be aborted, and otherwise the transaction will automatically be committed. Applications should design their transaction processing with this in mind.

- Second, please be aware that <a href="../../java/com/sleepycat/collections/TransactionRunner.html#run(com.sleepycat.collections.TransactionWorker)" class="ulink" target="_top">TransactionRunner.run</a> unwraps exceptions in order to discover whether a nested exception is a <a href="../../java/com/sleepycat/db/DeadlockException.html" class="ulink" target="_top">DeadlockException</a>. This is particularly important since all Berkeley DB exceptions that occur while calling a stored collection method are wrapped with a <a href="../../java/com/sleepycat/util/RuntimeExceptionWrapper.html" class="ulink" target="_top">RuntimeExceptionWrapper</a>. This wrapping is necessary because Berkeley DB exceptions are checked exceptions, and the Java collections API does not allow such exceptions to be thrown.

When calling <a href="../../java/com/sleepycat/collections/TransactionRunner.html#run(com.sleepycat.collections.TransactionWorker)" class="ulink" target="_top">TransactionRunner.run</a>, the unwrapped (nested) exception will be unwrapped and thrown automatically. If you are not using <a href="../../java/com/sleepycat/collections/TransactionRunner.html" class="ulink" target="_top">TransactionRunner</a> or if you are handling exceptions directly for some other reason, use the <a href="../../java/com/sleepycat/util/ExceptionUnwrapper.html#unwrap(java.lang.Exception)" class="ulink" target="_top">ExceptionUnwrapper.unwrap</a> method to get the nested exception. For example, this can be used to discover that an exception is a <a href="../../java/com/sleepycat/db/RunRecoveryException.html" class="ulink" target="_top">RunRecoveryException</a> as shown below.

``` c
import com.sleepycat.db.RunRecoveryException;
import com.sleepycat.util.ExceptionUnwrapper;
...
    catch (Exception e)
    {
        e = ExceptionUnwrapper.unwrap(e);
        if (e instanceof RunRecoveryException)
        {
            // follow recovery procedure
        }
    } 
```
