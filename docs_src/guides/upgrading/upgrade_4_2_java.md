---
title: "Java"
api-name: "Java"
source: docs/upgrading/upgrade_4_2_java.html
---
## Java

There are a number of major changes to the Java support in Berkeley DB in this release. Despite that we have tried to make this a bridge release, a release where we don't require you to change anything. We've done this using the standard approach to deprecation in Java. If you do not compile with deprecation warnings on, your existing sources should work with this new release with only minor changes despite the large number of changes. Expect that in a future release we will remove all the deprecated API and only support the new API names.

This is a list of areas where we have broken compatibility with the 4.1 release. In most cases it is a name change in an interface class.

- **DbAppDispatch.app_dispatch(DbEnv,Dbt,DbLsn,int)**

  is now: **DbAppDispatch.appDispatch(DbEnv,Dbt,DbLsn,int)**

- **DbAppendRecno.db_append_recno(Db,Dbt,int)**

  is now: **DbAppendRecno.dbAppendRecno(Db,Dbt,int)**

- **DbBtreeCompare.bt_compare(Db,Dbt,Dbt)**

  is now: **DbBtreeCompare.compare(Db,Dbt,Dbt)**

- **DbBtreeCompare.dup_compare(Db,Dbt,Dbt)**

  is now: **DbBtreeCompare.compareDuplicates(Db,Dbt,Dbt)**

- **DbBtreePrefix.bt_prefix(Db,Dbt,Dbt)**

  is now: **DbBtreePrefix.prefix(Db,Dbt,Dbt)**

- **DbSecondaryKeyCreate.secondary_key_create(Db,Dbt,Dbt,Dbt)**

  is now: **DbSecondaryKeyCreate.secondaryKeyCreate(Db,Dbt,Dbt,Dbt)**

The 4.2 release of Berkeley DB requires at minimum a J2SE 1.3.1 certified Java virtual machine and associated classes to properly build and execute. To determine what version virtual machine you are running, enter:

``` c
java -version
```

at a command line and look for the version number. If you need to deploy to a version 1.1 or 1.0 Java environment, it may be possible to do so by not including the classes in the com.sleepycat.bdb package in the Java build process (however, that workaround has not been tested by us).

A few inconsistent methods have been cleaned up (for example, Db.close now returns void; previously, it returned an int which was always zero). The synchronized attributed has been toggled on some methods -- this is an attempt to prevent multithreaded applications from calling close or similar methods concurrently from multiple threads.

The Berkeley DB API has up until now been consistent across all language APIs. Although consistency has is benefits, it made our Java API look strange to Java programmers. Many methods have been renamed in this release of the Java API to conform with Java naming conventions. Sometimes this renaming was simply "camel casing", sometimes it required rewording. The mapping file for these name changes is in `dist/camel.pm`. The Perl script we use to convert code to the new names is called `dist/camelize.pl`, and may help with updating Java applications written for earlier versions of Berkeley DB.

Berkeley DB has a number of places where as a C library it uses function pointers to move into custom code for the purpose of notification of some event. In Java the best parallel is the registration of some class which implements an interface. In this version of Berkeley DB we have made an effort to make those interfaces more uniform and predictable. Specifically, DbEnvFeedback is now DbEnvFeedbackHandler, DbErrcall is DbErrorHandler and DbFeedback is DbFeedbackHandler. In every case we have kept the older interfaces and the older registration methods so as to allow for backward compatibility in this release. Expect them to be removed in future releases.

As you upgrade to this release of Berkeley DB you will notice that we have added an entirely new layer inside the package com.sleepycat.bdb. This was formerly the Greybird project by Mark Hayes. Sleepycat Software and Mark worked together to incorporate his work. We have done this in hopes of reducing the learning curve when using Berkeley DB in a Java project. When you upgrade you should consider switching to this layer as over time the historical classes and the new bdb package classes will be more and more integrated providing a simple yet powerful interface from Java into the Berkeley DB library.

Berkeley DB's Java API is now generated with <a href="http://www.swig.org" class="ulink" target="_top">SWIG</a>. The new Java API is significantly faster for many operations.

Some internal methods and constructors that were previously public have been hidden or removed.

Packages found under com.sleepycat are considered different APIs into the Berkeley DB system. These include the core db api (com.sleepycat.db), the collections style access layer (com.sleepycat.bdb) and the now relocated XA system (com.sleepycat.xa).
