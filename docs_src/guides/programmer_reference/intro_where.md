---
title: "Where does Berkeley DB run?"
api-name: "Where does Berkeley DB run?"
source: docs/programmer_reference/intro_where.html
---
## Where does Berkeley DB run?

Berkeley DB requires only underlying IEEE/ANSI Std 1003.1 (POSIX) system calls and can be ported easily to new architectures by adding stub routines to connect the native system interfaces to the Berkeley DB POSIX-style system calls. See the Berkeley DB Porting Guide for more information.

Berkeley DB will autoconfigure and run on almost any modern UNIX, POSIX or Linux systems, and on most historical UNIX platforms. Berkeley DB will autoconfigure and run on almost any GNU gcc toolchain-based embedded platform, including Cygwin, OpenLinux and others. See the Berkeley DB Installation and Build Guide for more information.

The Berkeley DB distribution includes support for QNX Neutrino. See the Berkeley DB Installation and Build Guide for more information.

The Berkeley DB distribution includes support for Windows, via the Microsoft Visual C++ development environment. See the Berkeley DB Installation and Build Guide for more information.

Support for VxWorks and for Windows CE was present in earlier releases and has been removed; neither platform is supported by this release.
