---
title: "--enable-pthread_self"
api-name: "--enable-pthread_self"
source: docs/upgrading/upgrade_4_5_config.html
---
## --enable-pthread_self

In previous releases, the --enable-pthread_self configuration option was used to force Berkeley DB to use the POSIX pthread pthread_self function to identify threads of control (even when Berkeley DB was configured for test-and-set mutexes). In the 4.5 release, the --enable-pthread_self option has been replaced with the --enable-pthread_api option. This option has the same effect as the previous option, but configures the Berkeley DB build for a POSIX pthread application in other ways (for example, configuring Berkeley DB to use the pthread_self function).
