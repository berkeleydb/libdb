---
title: "Recno backing text source files"
api-name: "Recno backing text source files"
source: docs/upgrading/upgrade_4_5_source.html
---
## Recno backing text source files

In previous releases of Berkeley DB, Recno access method backing source text files were opened using the ANSI C fopen function with the "r" and "w" modes. This caused Windows systems to translate carriage-return and linefeed characters on input and output and could lead to database corruption.

In the current release, Berkeley DB opens the backing source text files using the "rb" and "wb" modes, consequently carriage-return and linefeed characters will not be translated on Windows systems.

Applications using the backing source text file feature on systems where the "r/w" and "rb/wb" modes differ should evaluate their application as part of upgrading to the 4.5 release. There is the possibility that characters have been translated or stripped and the backing source file has been corrupted. (Applications on other systems, for example, POSIX-like systems, should not require any changes related to this issue.)
