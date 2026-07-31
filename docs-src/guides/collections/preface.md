---
title: "Preface"
api-name: "Preface"
source: docs/collections/tutorial/preface.html
---
## Preface

**Table of Contents**

<span class="sect1"> [Conventions Used in this Book](preface.md#conventions) </span>

<span class="sect1"> [For More Information](moreinfo.md) </span>

<span class="sect2"> [Contact Us](moreinfo.md#contact_us) </span>

Welcome to the Berkeley DB (DB) Collections API. This document provides a tutorial that introduces the collections API. The goal of this document is to provide you with an efficient mechanism with which you can quickly become efficient with this API. As such, this document is intended for Java developers and senior software architects who are looking for transactionally-protected backing of their Java collections. No prior experience with DB technologies is expected or required.

This document reflects Berkeley DB 11<span class="emphasis">*g*</span> Release 2, which provides DB library version 11.2.5.3.

## Conventions Used in this Book

The following typographical conventions are used within in this manual:

Class names are represented in `monospaced font`, as are `method names`. For example: "The `Environment.openDatabase()` method returns a `Database` class object."

Variable or non-literal text is presented in <span class="emphasis">*italics*</span>. For example: "Go to your <span class="emphasis">*DB_INSTALLATION_HOME*</span> directory."

Program examples are displayed in a monospaced font on a shaded background. For example:

``` c
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import java.io.File;

...

// Open the environment. Allow it to be created if it does not already 
// exist.
Environment myDbEnvironment;
```

In situations in this book, programming examples are updated from one chapter to the next in this book. When this occurs, the new code is presented in **`monospaced bold`** font. For example:

``` c
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import java.io.File;

...

// Open the environment. Allow it to be created if it does not already 
// exist.
Environment myDbEnv;
EnvironmentConfig envConfig = new EnvironmentConfig();
envConfig.setAllowCreate(true);
myDbEnv = new Environment(new File("/export/dbEnv"), envConfig); 
```
