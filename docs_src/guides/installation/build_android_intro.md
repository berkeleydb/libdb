---
title: "Chapter 4. Building Berkeley DB for Android"
api-name: "Chapter 4. Building Berkeley DB for Android"
source: docs/installation/build_android_intro.html
---
## Chapter 4. Building Berkeley DB for Android

**Table of Contents**

<span class="sect1"> [Building the Drop-In Replacement for Android](build_android_intro.md#build_android) </span>

<span class="sect2"> [Migrating from SQLite to Berkeley DB](build_android_intro.md#build_android_migrate) </span>

<span class="sect1"> [Building the Android JDBC Driver](build_android_jdbc.md) </span>

<span class="sect1"> [Android Configuration Options](build_android_config.md) </span>

Berkeley DB provides support for the Android platform enabling you to develop and deploy a wide range of mobile applications and services. Android provides SQLite as the default database for developing applications that need database support. Berkeley DB SQL API is fully compatible with SQLite and can be used as a replacement. The `build_android` directory in the Berkeley DB distribution contains a makefile, `Android.mk`, for building a drop-in replacement for SQLite.

Oracle offers two different solutions for building the BDB SQL API for Android. The first creates a library that can be used as a drop-in replacement for SQLite on Android. The second creates a JDBC driver for Android.

## Building the Drop-In Replacement for Android

<span class="sect2"> [Migrating from SQLite to Berkeley DB](build_android_intro.md#build_android_migrate) </span>

This section describes how to build a library that can be used as a drop-in replacement for SQLite on Android.

1.  Download and compile the Android source tree.

    The compiling process takes time but is a one time activity. For information on downloading and compiling the Android source code, see <a href="http://source.android.com/source/download.html" class="ulink" target="_top">http://source.android.com/source/download.html</a>.

2.  Copy the Berkeley DB code into the Android build tree.

    ``` c
    $ cd <root>/external/sqlite/dist
    $ tar zxf ${DB_PATH} 
    ```

    where \<root\> is the root of the Android source tree and \${DB_PATH} is the path where you saved the `db-xx.tar.gz` version of the Berkeley DB distribution.

3.  Update the Android build file to identify Berkeley DB.

    Replace the `Android.mk` file with the one from the Berkeley DB source tree by doing the following:

    ``` c
    $ cd <root>/external/sqlite/dist
    $ mv Android.mk Android.mk.sqlite
    $ cp ${DB_INSTALL}/build_android/Android.mk ./ 
    ```

    where \${DB_INSTALL} is the directory into which you installed the Berkeley DB library.

4.  Tuning parameters.

    The configuration options for performance tuning can be added/edited in the `Android.mk` file by modifying `LOCAL_CFLAGS` located in the `build libsqlite replacement` section. For more information, see <a href="build_android_config.md" class="xref" title="Android Configuration Options">Android Configuration Options</a>.

    It is also possible to change these settings using PRAGMA commands or through the <a href="../../api/c/configuration_reference.md" class="olink">DB_CONFIG</a> file.

5.  Build the new Android image.

    To build the Android image with Berkeley DB SQL included, do the following:

    ``` c
    $ cd <root>
    $ . build/envsetup.sh
    $ make clean-libsqlite
    $ mmm -B external/sqlite/dist
    $ make snod
    ```

    You can locate the new image in `<root>/out/target/product/generic`.

### Migrating from SQLite to Berkeley DB

This section describes how to enable automatic conversion of SQLite format databases to Berkeley DB SQL when they are opened. To do this, you must first make sure that the `-DBDBSQL_CONVERT_SQLITE` option is added to `LOCAL_CFLAGS` when you configure your Berkeley DB database build.

1.  Build a static SQLite shell for Android platform.

    Create a script, build_sqlite3_shell.sh, in the \<root\>/external/sqlite/dist directory.

    ``` c
    #!/bin/bash
    # This script shows how to use built-in toolchain to build
    # sqlite3 shell, which is required by Berkeley DB SQL 
    # on-the-fly migration feature.

    # Note: these variables should be set per active Android source tree
    # We assume $PWD=$ROOT/external/sqlite/dist
    ROOT=${PWD}/../../..
    TOOLCHAIN=${ROOT}/prebuilt/linux-x86/toolchain/arm-eabi-4.4.0
    CC=${TOOLCHAIN}/bin/arm-eabi-gcc
    LIB="${ROOT}/out/target/product/generic/obj/lib"
    INCLUDE="${ROOT}/ndk/build/platforms/android-8/arch-arm/usr/include"

    # CFLAGS should be set per Android.mk.sqlite (the original 
    # version of SQLite's Android.mk)
    CFLAGS="-DHAVE_USLEEP=1 -DSQLITE_THREADSAFE=1 -DNDEBUG=1 \
     -DSQLITE_DEFAULT_JOURNAL_SIZE_LIMIT=1048576 \
     -DSQLITE_ENABLE_MEMORY_MANAGEMENT=1 \
     -DSQLITE_DEFAULT_AUTOVACUUM=1 \
     -DSQLITE_TEMP_STORE=3 -DSQLITE_ENABLE_FTS3 \
     -DSQLITE_ENABLE_FTS3_BACKWARDS -DTHREADSAFE=1"
    CFLAGS="${CFLAGS} -I${INCLUDE}"

    LDFLAGS="-ldl -nostdlib -Wl,--gc-sections -lc -llog -lgcc \
     -Wl,--no-undefined,-z,nocopyreloc ${LIB}/crtend_android.o \
     ${LIB}/crtbegin_dynamic.o -L${LIB} -Wl,-rpath,${LIB}"

    ${CC} -DANDROID -DOS_ANDROID --sysroot="${SYSROOT}" -mandroid \
          -fvisibility=hidden -ffunction-sections -fdata-sections \
          -fPIC ${LDFLAGS} ${CFLAGS} \
          sqlite3.c shell.c -o sqlite3orig
    ```

    Ensure you adjust the variables as per your actual Android environment. This script is suited for Android 2.2.

2.  Execute the build_sqlite3_shell.sh script and to get the static sqlite3 shell utility - sqlite3orig.

3.  Change the system image file.

    Use the `xyaffs2` utiltiy to decompress the `system.img` and get the directory system.

    ``` c
    $ xyaffs2 ./system.img system
    ```

    Add static sqlite3 shell utility.

    ``` c
    $ cp <root>/external/sqlite/dist/sqlite3orig \
                          system/xbin/sqlite3orig
    ```

    Use the `mkyaffs2image` utility to rebuild `system.img` from the changed directory system.

    ``` c
    $ mkyaffs2image -f $PWD/system system.img
    ```

    ### Note

    To open the database in the `SQLite` format use the `sqlite3orig` command.
