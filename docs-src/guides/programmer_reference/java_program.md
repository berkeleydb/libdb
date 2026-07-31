---
title: "Java programming notes"
api-name: "Java programming notes"
source: docs/programmer_reference/java_program.html
---
## Java programming notes

Although the Java API parallels the Berkeley DB C++/C interface in many ways, it differs where the Java language requires. For example, the handle method names are camel-cased and conform to Java naming patterns. (The C++/C method names are currently provided, but are deprecated.)

1.  The Java runtime does not automatically close Berkeley DB objects on finalization. There are several reasons for this. One is that finalization is generally run only when garbage collection occurs, and there is no guarantee that this occurs at all, even on exit. Allowing specific Berkeley DB actions to occur in ways that cannot be replicated seems wrong. Second, finalization of objects may happen in an arbitrary order, so we would have to do extra bookkeeping to make sure that everything was closed in the proper order. The best word of advice is to always do a close() for any matching open() call. Specifically, the Berkeley DB package requires that you explicitly call close on each individual <a href="../java/com/sleepycat/db/Database.html" class="ulink" target="_top">Database</a> and <a href="../java/com/sleepycat/db/Cursor.html" class="ulink" target="_top">Cursor</a> object that you opened. Your database activity may not be synchronized to disk unless you do so.

2.  Some methods in the Java API have no return type, and throw a <a href="../java/com/sleepycat/db/DatabaseException.html" class="ulink" target="_top">DatabaseException</a> when an severe error arises. There are some notable methods that do have a return value, and can also throw an exception. The "get" methods in <a href="../java/com/sleepycat/db/Database.html" class="ulink" target="_top">Database</a> and <a href="../java/com/sleepycat/db/Cursor.html" class="ulink" target="_top">Cursor</a> both return 0 when a get succeeds, <a href="program_errorret.md#program_errorret.DB_NOTFOUND" class="link">DB_NOTFOUND</a> when the key is not found, and throw an error when there is a severe error. This approach allows the programmer to check for typical data-driven errors by watching return values without special casing exceptions.

    An object of type <a href="../java/com/sleepycat/db/MemoryException.html" class="ulink" target="_top">MemoryException</a> is thrown when a Dbt is too small to hold the corresponding key or data item.

    An object of type <a href="../java/com/sleepycat/db/DeadlockException.html" class="ulink" target="_top">DeadlockException</a> is thrown when a deadlock would occur.

    An object of type <a href="../java/com/sleepycat/db/RunRecoveryException.html" class="ulink" target="_top">RunRecoveryException</a>, a subclass of <a href="../java/com/sleepycat/db/DatabaseException.html" class="ulink" target="_top">DatabaseException</a>, is thrown when there is an error that requires a recovery of the database using <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility.

    An object of type <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/IllegalArgumentException.html" class="ulink" target="_top">IllegalArgumentException</a> a standard Java Language exception, is thrown when there is an error in method arguments.

    An object of type <a href="http://download.oracle.com/javase/1.5.0/docs/api/java/lang/OutOfMemoryError.html" class="ulink" target="_top">OutOfMemoryError</a> is thrown when the system cannot provide enough memory to complete the operation (the ENOMEM system error on UNIX).

3.  If there are embedded nulls in the **curslist** argument for <a href="../java/com/sleepycat/db/Database.html#join(com.sleepycat.db.Cursor%5B%5D,%20com.sleepycat.db.JoinConfig)" class="ulink" target="_top">Database.join(com.sleepycat.db.Cursor[], com.sleepycat.db.JoinConfig)</a>, they will be treated as the end of the list of cursors, even if you may have allocated a longer array. Fill in all the cursors in your array unless you intend to cut it short.

4.  If you are using custom class loaders in your application, make sure that the Berkeley DB classes are loaded by the system class loader, not a custom class loader. This is due to a JVM bug that can cause an access violation during finalization (see the bug 4238486 in Sun Microsystem's Java Bug Database).
