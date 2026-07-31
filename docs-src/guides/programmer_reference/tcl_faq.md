---
title: "Tcl FAQ"
api-name: "Tcl FAQ"
source: docs/programmer_reference/tcl_faq.html
---
## Tcl FAQ

1.  **I have several versions of Tcl installed. How do I configure Berkeley DB to use a particular version?**

    To compile the Tcl interface with a particular version of Tcl, use the --with-tcl option to specify the Tcl installation directory that contains the tclConfig.sh file. See the <a href="../../guides/installation/build_unix_flags.md" class="olink">Changing compile or load options</a> section in the Berkeley DB Installation and Build Guide for more information.

2.  **Berkeley DB was configured using --enable-tcl or --with-tcl and fails to build.**

    The Berkeley DB Tcl interface requires Tcl version 8.5 or greater.

3.  **Berkeley DB was configured using --enable-tcl or --with-tcl and fails to build.**

    If the Tcl installation was moved after it was configured and installed, try reconfiguring and reinstalling Tcl.

    Also, some systems do not search for shared libraries by default, or do not search for shared libraries named the way the Tcl installation names them, or are searching for a different kind of library than those in your Tcl installation. For example, Linux systems often require linking "libtcl.a" to "libtcl#.#.a", whereas AIX systems often require adding the "-brtl" flag to the linker. A simpler solution that almost always works on all systems is to create a link from "libtcl.#.#.a" or "libtcl.so" (or whatever you happen to have) to "libtcl.a" and reconfigure.

4.  **Loading the Berkeley DB library into Tcl on AIX causes a core dump.**

    In some versions of Tcl, the "tclConfig.sh" autoconfiguration script created by the Tcl installation does not work properly under AIX, and you may have to modify values in the tclConfig.sh file to in order to load the Berkeley DB library into Tcl. Specifically, the TCL_LIB_SPEC variable should contain sufficient linker flags to find and link against the installed libtcl library. In some circumstances, the tclConfig.sh file built by Tcl does not.
