---
title: "Appendix A. Using the BFILE Extension"
api-name: "Appendix A. Using the BFILE Extension"
source: docs/bdb-sql/bfile-extension.html
---
## Appendix A. Using the BFILE Extension

The BFILE data type allows the BDB SQL interface to access binary files that are stored in the file system outside of the database. The binary file can be queried in exactly the same way as any other data type stored in the database, but Berkeley DB is able to save space in the database file by not embedding a large amount of binary data in it. This also helps overall database performance.

Internally, a BFILE column or attribute stores a BFILE locater, which serves as a pointer to the binary file. The locater maintains the directory alias and the filename. You can change the path of BFILE without affecting the base table by using the BFILENAME function. BFILE is somewhat like the BLOB data type, but it does not participate in transactions and it is not recoverable. Instead, the underlying operating system is expected to provide file integrity and durability.

The remainder of this section describes the various objects and functions that the BFILE extension makes available to you. In addition, complete examples of using these extensions are available with your Berkeley DB distribution. They are placed in the following location:

``` c
<db-dist>/lang/sql/sqlite/ext/bfile/examples
```

## Supported Platforms and Languages

The BFILE extension is currently only supported for \*nix platforms.

The BFILE extension it is not available in your library by default. Instead, you must enable the extension when you compile Berkeley DB. See the *Berkeley DB Installation and Build Guide* for information on how to enable this extension when you build Berkeley DB. Once you have enabled the extension, applications will also need to load the BFILE library file: `libbfile_ext.so`.

By default, the BFILE extension provides support for additional SQL statements. With some extra configuration at Berkeley DB compile time, you can also obtain support for extensions to the SQLite C/C++ interface. Both the SQL extensions and the extensions to the SQLite C/C++ interface are described in the following sections.
