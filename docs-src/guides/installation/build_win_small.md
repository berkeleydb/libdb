---
title: "Building a small memory footprint library"
api-name: "Building a small memory footprint library"
source: docs/installation/build_win_small.html
---
## Building a small memory footprint library

For applications that don't require all of the functionality of the full Berkeley DB library, an option is provided to build a static library with certain functionality disabled. In particular, cryptography, hash and queue access methods, replication and verification are all turned off. In addition, all message text is stripped from the library. This can reduce the memory footprint of Berkeley DB significantly.

### Note

If your library has stripped messages, you can get an idea of what text should be issued for a given error message by using the <a href="../articles/mssgtxt/index.html" class="olink">Message Reference for Stripped Libraries</a> guide.

In general on Windows systems, you will want to evaluate the size of the final application, not the library build. The Microsoft LIB file format (like UNIX archives) includes copies of all of the object files and additional information. The linker rearranges symbols and strips out the overhead, and the resulting application is much smaller than the library. There is also a Visual C++ optimization to "Minimize size" that will reduce the library size by a few percent.

A Visual C++ project file called `db_small` is provided for this small memory configuration. During a build, static libraries are created in `Release` or `Debug`, respectively. The library name is `libdb_small48sd.lib` for the debug build, or `libdb_small48s.lib` for the release build.

For assistance in further reducing the size of the Berkeley DB library, or in building small memory footprint libraries on other systems, please contact Berkeley DB support.
