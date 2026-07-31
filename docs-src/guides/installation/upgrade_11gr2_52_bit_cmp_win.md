---
title: "32bit/64bit Compatibility on Windows"
api-name: "32bit/64bit Compatibility on Windows"
source: docs/installation/upgrade_11gr2_52_bit_cmp_win.html
---
## 32bit/64bit Compatibility on Windows

Berkeley DB can now be compiled on Windows so that 32 bit and 64 bit applications can concurrently access a BDB environment. To enable this feature, build both the 32 bit BDB library and application and the 64 bit library and application with the flag `/D HAVE_MIXED_SIZE_ADDRESSING`. Note that private environments are disabled under the compatibility mode.
