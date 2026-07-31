---
title: "Additional build options"
api-name: "Additional build options"
source: docs/installation/win_additional_options.html
---
## Additional build options

There are several build options that you can configure when building Berkeley DB on Windows. To specify these, select `Project Properties`-\>`C/C++`-\>`Command Line` and add the property.

These are some of the additional properties that you can specify when you are building Berkeley DB on Windows:

- **/D HAVE_LOCALIZATION**

  Enable localized error message text, if available. This option should not be used when enabling stripped messages.

- **/D HAVE_MIXED_SIZE_ADDRESSING**

  Allows for the sharing of the BDB database environment between 32-bit and 64-bit applications. Note that if you use this macro to rebuild your Berkeley DB library, then you need to also rebuild both your 32-bit and 64-bit applications using **/D HAVE_MIXED_SIZE_ADDRESSING**.

  Note that use of this macro means that private environments are disabled for the library.

- **/D HAVE_STRIPPED_MESSAGES**

  Causes all error messages to be stripped of their textual information. This option should not be used when enabling localization support. Use of this property can reduce your library footprint by up to 42KB (for DLLs) or 98KB (for a .lib).

  Note that this option is automatically enabled if you build using the `db_small` project name. For more information on building a small library, see <a href="build_win_small.md" class="xref" title="Building a small memory footprint library">Building a small memory footprint library</a>.

  If you use this build option, you can get an idea of what text should be issued for a given error message by using the <a href="../articles/mssgtxt/index.html" class="olink">Message Reference for Stripped Libraries</a> guide.
