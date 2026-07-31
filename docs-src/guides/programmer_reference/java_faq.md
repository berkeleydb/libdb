---
title: "Java FAQ"
api-name: "Java FAQ"
source: docs/programmer_reference/java_faq.html
---
## Java FAQ

1.  **On what platforms is the Berkeley DB Java API supported?**

    All platforms supported by Berkeley DB that have a JVM compatible with J2SE 1.4 or above.

2.  **How does the Berkeley DB Java API relate to the J2EE standard?**

    The Berkeley DB Java API does not currently implement any part of the J2EE standard. That said, it does implement the implicit standard for Java <a href="http://download.oracle.com/javase/1.5.0/docs/guide/collections/" class="ulink" target="_top">Java Collections</a>. The concept of a transaction exists in several Java packages (J2EE, XA, JINI to name a few). Support for these APIs will be added based on demand in future versions of Berkeley DB.

3.  **How should I incorporate db.jar and the db native library into a Tomcat or other J2EE application servers?**

    Tomcat and other J2EE application servers have the ability to rebuild and reload code automatically. When using Tomcat this is the case when "reloadable" is set to "true". If your WAR file includes the db.jar it too will be reloaded each time your code is reloaded. This causes exceptions as the native library can't be loaded more than once and there is no way to unload native code. The solution is to place the db.jar in \$TOMCAT_HOME/common/lib and let Tomcat load that library once at start time rather than putting it into the WAR that gets reloaded over and over.

4.  **Can I use the Berkeley DB Java API from within a EJB, a Servlet or a JSP page?**

    Yes. The Berkeley DB Java API can be used from within all the popular J2EE application servers in many different ways.

5.  **During one of the first calls to the Berkeley DB Java API, a DbException is thrown with a "Bad file number" or "Bad file descriptor" message.**

    There are known large-file support bugs under JNI in various releases of the JDK. Please upgrade to the latest release of the JDK, and, if that does not solve the problem, disable big file support using the --disable-largefile configuration option.

6.  **How can I use native methods from a debug build of the Java library?**

    Set Java's library path so that the debug version of Berkeley DB's Java library appears, but the release version does not. Berkeley DB tries to load the release library first, and if that fails tries the debug library.

7.  **Why is ClassNotFoundException thrown when adding a record to the database, when a SerialBinding is used?**

    This problem occurs if you copy the db.jar file into the Java extensions (ext) directory. This will cause the database code to run under the System class loader, and it won't be able to find your application classes.

    You'll have to actually remove db.jar from the Java extension directory. If you have more than one installation of Java, be sure to remove it from all of them. This is necessary even if db.jar is specified in the classpath.

    An example of the exception is:

    ``` c
    collections.ship.basic.SupplierKey
    at java.net.URLClassLoader$1.run(Unknown Source)
    at java.security.AccessController.doPrivileged(Native Method)
    at java.net.URLClassLoader.findClass(Unknown Source)
    at java.lang.ClassLoader.loadClass(Unknown Source)
    at java.lang.ClassLoader.loadClass(Unknown Source)
    at java.lang.ClassLoader.loadClassInternal(Unknown Source)
    at java.lang.Class.forName0(Native Method)
    at java.lang.Class.forName(Unknown Source)
    at com.sleepycat.bind.serial.StoredClassCatalog.
    getClassInfo(StoredClassCatalog.java:211)
    ...
    ```

8.  **I'm upgrading my Java application to Berkeley DB 4.3. Can I use the com.sleepycat.db.internal package rather than porting my code to the new API?**

    While it is possible to use the low-level API from applications, there are some caveats that should be considered when upgrading. The first is that the internal API depends on some classes in the public API such as DatabaseEntry.

    In addition, the internal API is closer to the C API and doesn't have some of the default settings that were part of the earlier Java API. For example, applications will need to set the DB_THREAD flag explicitly if handles are to be used from multiple threads, or subtle errors may occur.
