---
title: "Cross-Compiling on Unix"
api-name: "Cross-Compiling on Unix"
source: docs/installation/cross_compile_unix.html
---
## Cross-Compiling on Unix

The purpose of cross-compiling is to build a Berkeley DB library on one platform that is to be used on a different platform. This section demonstrates how to build a library compatible with the ARM platform from a 32-bit x86 Linux platform. You will need to adjust the build tools and options to match your particular platforms.

1.  Download, install, and test a toolchain that can build the desired target binaries. In this example, we use the `arm-linux-gnueabi-gcc` package.

2.  Decide on your configuration options, and set up your environment to use the appropriate compiler and compiler tools. It is often easiest to set this up as a small shell script. For example:

    ``` c
    #!/bin/sh -f
    env \
    CC=/usr/bin/arm-linux-gnueabi-gcc \
    STRIP=/usr/bin/arm-linux-gnueabi-strip \
    ../dist/configure \
    --build=i686-pc-linux-gnu \
    --host=arm-linux-gnueabi \
    --enable-java \
    --enable-sql \
    --enable-jdbc \
    --enable-smallbuild \
    --enable-shared \
    --enable-stripped_messages \
    --prefix=$HOME/ARM-linux/install \
    $* 
    ```

    The `--build` flag indicates the system on which you are compiling and the `--host` flag indicates the target platform. Adjust or omit the `--enable` and `--prefix` flag settings as dictated by your own needs.

3.  Unpack your Berkeley DB distribution and go to the `build_unix` directory. Alternatively, you may create a more appropriately-named directory at the same level and build in it. For example, `build_arm`.

4.  In your build directory, configure by executing your script, then `make` and `make install`. Review any compiler warnings and fix if necessary, repeating the `configure` and `make` steps until you are satisfied. The resulting libraries and executables can then be transferred to your target system.
