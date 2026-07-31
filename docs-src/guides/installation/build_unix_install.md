---
title: "Installing Berkeley DB"
api-name: "Installing Berkeley DB"
source: docs/installation/build_unix_install.html
---
## Installing Berkeley DB

Berkeley DB installs the following files into the following locations, with the following default values:

| Configuration Variables | Default value                             |
|-------------------------|-------------------------------------------|
| --prefix                | /usr/local/BerkeleyDB.**Major**.**Minor** |
| --exec_prefix           | \$(prefix)                                |
| --bindir                | \$(exec_prefix)/bin                       |
| --includedir            | \$(prefix)/include                        |
| --libdir                | \$(exec_prefix)/lib                       |
| docdir                  | \$(prefix)/docs                           |

| Files         | Default location |
|---------------|------------------|
| include files | \$(includedir)   |
| libraries     | \$(libdir)       |
| utilities     | \$(bindir)       |
| documentation | \$(docdir)       |

With one exception, this follows the GNU Autoconf and GNU Coding Standards installation guidelines; please see that documentation for more information and rationale.

The single exception is the Berkeley DB documentation. The Berkeley DB documentation is provided in HTML format, not in UNIX-style man or GNU info format. For this reason, Berkeley DB configuration does not support **--infodir** or **--mandir**. To change the default installation location for the Berkeley DB documentation, modify the Makefile variable, **docdir**.

When installing Berkeley DB on filesystems shared by machines of different architectures, please note that although Berkeley DB include files are installed based on the value of \$(prefix), rather than \$(exec_prefix), the Berkeley DB include files are not always architecture independent.

To move the entire installation tree to somewhere besides **/usr/local**, change the value of **prefix**.

To move the binaries and libraries to a different location, change the value of **exec_prefix**. The values of **includedir** and **libdir** may be similarly changed.

Any of these values except for **docdir** may be set as part of the configuration:

``` c
prompt: ../dist/configure --bindir=/usr/local/bin
```

Any of these values, including **docdir**, may be changed when doing the install itself:

``` c
prompt: make prefix=/usr/contrib/bdb install
```

The Berkeley DB installation process will attempt to create any directories that do not already exist on the system.
