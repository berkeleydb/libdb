---
title: "Apple iOS (iPhone OS)"
api-name: "Apple iOS (iPhone OS)"
source: docs/installation/build_unix_iphone.html
---
## Apple iOS (iPhone OS)

Building Berkeley DB in Apple iOS (known as iPhone OS previously) is the same as building for a conventional UNIX platform. This section lists the commands for building Berkeley DB in both the iPhone simulator (a software simulator included in the iPhone SDK that you can use to test your application without using the iPhone/iPod Touch) and the iPhone device.

Prior to building BDB in an iPhone simulator/iPhone device, set the required environment variables for iOS (iPhone OS). For a simulator/iPhone version 4.2 or older, set the LDFLAGS variable as follows:

``` c
export LDFLAGS="-L$SDKROOT/usr/lib/"
```

Otherwise, set LDFLAGS as follows:

``` c
export LDFLAGS="-L$SDKROOT/usr/lib/system/"
```

The steps to build BDB in an iPhone simulator are as follows:

``` c
export CFLAGS="-arch i386 -pipe -no-cpp-precomp --sysroot=$SDKROOT"
export CXXFLAGS="-arch i386 -pipe -no-cpp-precomp --sysroot=$SDKROOT"
cd $BDB_HOME/build_unix
../dist/configure --host=i386-apple-darwin\
                 --prefix=$SDKROOT ...
make
```

The steps to build BDB in an iPhone device are as follows:

``` c
export CFLAGS="-arch armv6 -pipe -Os -gdwarf-2\
               -no-cpp-precomp -mthumb -isysroot $SDKROOT "
export CXXFLAGS="-arch armv6 -pipe -Os -gdwarf-2\
                 -no-cpp-precomp -mthumb -isysroot $SDKROOT "
cd $BDB_HOME/build_unix
../dist/configure --host=arm-apple-darwin9\
                  --prefix=$SDKROOT ...           
make
```

Both sets of commands create the BDB dynamic library - libdb-5.3.dylib. To build the static library, libdb-5.3.a, add the `--enable-shared=no` option while configuring.
