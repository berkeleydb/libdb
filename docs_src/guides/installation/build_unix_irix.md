---
title: "IRIX"
api-name: "IRIX"
source: docs/installation/build_unix_irix.html
---
## IRIX

1.  **I can't compile and run multithreaded applications.**

    Special compile-time flags are required when compiling threaded applications on IRIX. If you are compiling a threaded application, you must compile with the \_SGI_MP_SOURCE flag:

    ``` c
    cc -D_SGI_MP_SOURCE ...
    ```

    The Berkeley DB library will automatically build with the correct options.
