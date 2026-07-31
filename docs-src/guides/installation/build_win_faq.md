---
title: "Windows FAQ"
api-name: "Windows FAQ"
source: docs/installation/build_win_faq.html
---
## Windows FAQ

1.  **My Win\* C/C++ application crashes in the Berkeley DB library when Berkeley DB calls fprintf (or some other standard C library function).**

    You should be using the "Debug Multithreaded DLL" compiler option in your application when you link with the build_windows\Debug\libdb48d.lib library (this .lib file is actually a stub for libdb48d.DLL). To check this setting in Visual C++, choose the <span class="emphasis">*Project/Settings*</span> menu item and select <span class="emphasis">*Code Generation*</span> under the tab marked <span class="emphasis">*C/C++*</span>; and see the box marked <span class="emphasis">*Use runtime library*</span>. This should be set to <span class="emphasis">*Debug Multithreaded DLL*</span>. If your application is linked against the static library, build_windows\Debug\libdb48sd.lib; then, you will want to set <span class="emphasis">*Use runtime library*</span> to <span class="emphasis">*Debug Multithreaded*</span>.

    Setting this option incorrectly can cause multiple versions of the standard libraries to be linked into your application (one on behalf of your application, and one on behalf of the Berkeley DB library). That violates assumptions made by these libraries, and traps can result.

    Also, using different Visual Studio compilers in the application and libraries can lead to a crash. So rebuild the application with the same Visual C++ version as that of the library.

2.  **Why are the build options for DB_DLL marked as "Use MFC in a Shared DLL"? Does Berkeley DB use MFC?**

    Berkeley DB does not use MFC at all. It does however, call malloc and free and other facilities provided by the Microsoft C runtime library. We found in our work that many applications and libraries are built assuming MFC, and specifying this for Berkeley DB solves various interoperation issues, and guarantees that the right runtime libraries are selected. Note that because we do not use MFC facilities, the MFC library DLL is not marked as a dependency for libdb.dll, but the appropriate Microsoft C runtime is.

3.  **How can I build Berkeley DB for <a href="http://www.mingw.org" class="ulink" target="_top">MinGW</a>?**

    Follow the instructions in <a href="build_unix.md#build_unix_intro" class="xref" title="Building for UNIX/POSIX">Building for UNIX/POSIX</a>, and specify the --enable-mingw option to the configuration script. This configuration option currently only builds static versions of the library, it does not yet build a DLL version of the library, and file sizes are limited to 2GB (2^32 bytes.)

4.  **How can I build a Berkeley DB for Windows 98/ME?**

    Windows 98/ME is no longer supported by Berkeley DB. The following is therefore only of interest to historical users of Berkeley DB.

    By default on Windows, Berkeley DB supports internationalized filenames by treating all directory paths and filenames passed to Berkeley DB methods as UTF-8 encoded strings. All paths are internally converted to wide character strings and passed to the wide character variants of Windows system calls.

    This allows applications to create and open databases with names that cannot be represented with ASCII names while maintaining compatibility with applications that work purely with ASCII paths.

    Windows 98 and ME do not support Unicode paths directly. To build for those versions of Windows, either:

    - Follow the instructions at <a href="http://msdn.microsoft.com/en-us/goglobal/bb688166.aspx" class="ulink" target="_top">Microsoft's web site</a>.

    - Open the workspace or solution file with Visual Studio. Then open the Project properties/settings section for the project you need to build (at least db_dll). In the <span class="emphasis">*C/C++-\>Preprocessor-\>Preprocessor Definitions*</span> section, remove <span class="emphasis">*\_UNICODE*</span> and <span class="emphasis">*UNICODE*</span> entries. Add in an entry of <span class="emphasis">*\_MBCS*</span>. Build the project as normal.

    The ASCII builds will also work on Windows NT/2K/XP/2003 and Windows7, but will not translate paths to wide character strings.
