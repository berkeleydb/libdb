---
title: "Building the Java API"
api-name: "Building the Java API"
source: docs/installation/build_win_java.html
---
## Building the Java API

Java support is not built automatically. The following instructions assume that you have installed the Sun Java Development Kit in `d:\java`. Of course, if you installed elsewhere or have different Java software, you will need to adjust the pathnames accordingly.

1.  Set your include directories.
    - In Visual Studio 2005/Visual Studio 2008 - Choose <span class="emphasis">*Tools -\> Options -\> Projects -\> VC++ Directories*</span>. Under the "Show directories for" pull-down, select "Include files". Add the full pathnames for the `d:\java\include` and `d:\java\include\win32` directories. Then click OK.
    - In Visual Studio 2010 - Right-click db_java project, choose <span class="emphasis">*Properties-\>Configuration Properties-\> VC++ Directories-\>Include Directories*</span>. Add the full pathnames for the `d:\java\include` and `d:\java\include\win32` directories. Then click OK.

    These are the directories needed when including jni.h.

2.  Set the executable files directories.
    - In Visual Studio 2005/Visual Studio 2008 - Choose <span class="emphasis">*Tools -\> Options -\> Projects -\> VC++ Directories*</span>. Under the "Show directories for" pull-down, select "Executable files". Add the full pathname for the `d:\java\bin` directory, then click OK.
    - In Visual Studio 2010 - Right-click db_java project, choose <span class="emphasis">*Properties-\>Configuration Properties-\> VC++ Directories-\>Executable Directories*</span>. Add the full pathnames for the `d:\java\bin` directories. Then click OK.

    This is the directory needed to find javac.

3.  Set the build type to Release or Debug in the drop-down on the tool bar.

4.  To build, right-click on db_java and select Build. This builds the Java support library for Berkeley DB and compiles all the java files, placing the resulting `db.jar` and `dbexamples.jar` files in one of the following Berkeley DB subdirectories, depending on the configuration that you chose:

    |                               |
    |-------------------------------|
    | `build_windows\Win32\Debug`   |
    | `build_windows\Win32\Release` |
