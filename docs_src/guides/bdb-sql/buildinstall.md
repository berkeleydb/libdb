---
title: "Getting and Installing BDB SQL"
api-name: "Getting and Installing BDB SQL"
source: docs/bdb-sql/buildinstall.html
---
## Getting and Installing BDB SQL

<span class="sect2"> [On Windows Systems](buildinstall.md#onwin) </span>

<span class="sect2"> [On Unix](buildinstall.md#onunix) </span>

<span class="sect2"> [The BDB SQL ADO.NET Interface](buildinstall.md#ado_net) </span>

The BDB SQL interface comes as a part of the Oracle Berkeley DB download. This can be downloaded from the <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/index.html" class="ulink" target="_top">Oracle Berkeley DB download page</a>.

### On Windows Systems

The BDB SQL interface is automatically built and installed whenever you build or install Berkeley DB for a Windows system. The BDB SQL interface `dll`s and the command line interpreter have names that differ from a standard SQLite distribution as follows:

- `dbsql.exe`

  This is the command line shell. It operates identically to the SQLite <span class="command">**sqlite3.exe**</span> shell.

- `libdb_sql50.dll`

  This is the library that provides the BDB SQL interface. It is the equivalent of the SQLite `sqlite3.dll` library.

### On Unix

In order to build the BDB SQL interface, you download and build Berkeley DB, configuring it so that the BDB SQL interface is also built. Be aware that it is not built by default. Instead, you need to tell the Berkeley DB `configure` script to also build the BDB SQL interface. For instructions on building the BDB SQL interface, see <a href="../../guides/installation/build_unix.md#build_unix_sqlinter" class="olink">Building the DB SQL Interface</a> in the *Berkeley DB Installation and Build Guide*.

The library and application names used when building the BDB SQL interface are different than those used by SQLite. If you want library and command shell names that are consistent with the names used by SQLite, configure the BDB SQL interface build using the compatibility (`--enable-sql_compat`) option.

### Warning

The compatibility option can break other applications on your platform that rely on standard SQLite. This is especially true of Mac OS X, which uses standard SQLite for a number of default applications.

<span class="emphasis"> *Use the compatibility option only if you know exactly what you are doing.* </span>

Unless you built the BDB SQL interface with the compatibility option, libraries and a command line shell are built with the following names:

- <a href="../../api/c/dbsql.md" class="olink">dbsql</a>

  This is the command line shell. It operates identically to the SQLite <span class="command">**sqlite3**</span> shell.

- `libdb_sql`

  This is the library that provides the BDB SQL interface. It is the equivalent of the SQLite `libsqlite3` library.

### The BDB SQL ADO.NET Interface

Download the ADO.NET package from the <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/index.html" class="ulink" target="_top">Oracle Berkeley DB download page</a>.

#### Prerequisites For Building The ADO.NET Package

- To build the Linq package, you will need to install `Microsoft .NET Framework 3.5 SP1`.
- To build SQLite.Designer, you will need to install the `Microsoft Visual Studio SDK`.
- To build on Windows Mobile you will need to install the `Microsoft Windows Mobile 6.5.3 Developer Tool Kit (DTK)`.
- To build on Windows Mobile you will need to use Visual Studio 2008.

#### Building BDB SQL ADO.NET Interface For Windows

- The package contains Visual Studio solution files:

  - `SQLite.NET.2008.sln` and `SQLite.NET.2010.sln`

    For use by with Visual Studio 2008 or 2010. Note that these solution files do not build support for Linq or SQLite Designer.

  - `SQLite.NET.2008.MSBuild.sln` and `SQLite.NET.2010.MSBuild.sln`

    For use with MSBuild (Microsoft Build Engine). These can also be used with Visual Studio. These solutions exclude SQLite Designer and CompactFramework. By default, these do not build support for Linq.

- Change the current platform target to `ReleaseNativeOnly` choose either `Win32` or `x64` depending on your target platform.

- Build the solution.

#### Building BDB SQL ADO.NET Interface For Windows Mobile

Building BDB SQL ADO.NET for Windows Mobile requires Windows Mobile 6.5.3 Professional DTK. Typical requirements for installing this toolkit are:

- Visual Studio 2005 SP1 or Later

- ActiveSync 4.5

- .NET CompactFramework 2.0 SP1

- Windows Mobile 6 SDK

To build BDB SQL ADO.NET for Windows Mobile, do the following:

- Open the `SQLite.NET.2008.WinCE.sln` solution file in Visual Studio 2008.
- Select `Load Project Normally`
- Change the current platform to `ReleaseNativeOnly`.
- Select `Configuration Manager`-\>`new`, then type or select the platform `Windows Mobile 6.5.3 Professional DTK (ARMV4I)`. Choose to copy settings from `Pocket PC 2003 (ARMV4I)`
- Build the solution.
