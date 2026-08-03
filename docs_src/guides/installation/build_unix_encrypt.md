---
title: "Cryptography Support"
api-name: "Cryptography Support"
source: docs/installation/build_unix_encrypt.html
---
## Cryptography Support

In this release, the configuration options, --disable-cryptography and --enable-cryptography are deprecated. --disable-cryptography is replaced by --with-cryptography=no and --enable-cryptography is replaced by --with-cryptography=yes.

To build Berkeley DB with support for cryptography, enter --with-cryptography=yes as an argument to configure instead of --enable-cryptography.

To build Berkeley DB without support for cryptography, enter --with-cryptography=no as an argument to configure instead of --disable-cryptography.

Berkeley DB now supports encryption using Intel's Performance Primitive (IPP) on Linux. To build Berkeley DB with support for cryptography using Intel's Performance Primitive (IPP) library, enter --with-cryptography=ipp as an argument to configure.

Note: The --with-cryptography=ipp argument works only on Linux.
