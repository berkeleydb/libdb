---
title: "Defining application-specific log records"
api-name: "Defining application-specific log records"
source: docs/programmer_reference/apprec_def.html
---
## Defining application-specific log records

By convention, log records are described in files named `XXX.src`, where "XXX" is typically a descriptive name for a subsystem or other logical group of logging functions. These files contain interface definition language descriptions for each type of log record that is used by the subsystem.

All blank lines and lines beginning with a hash ("#") character in the XXX.src files are ignored.

The first non-comment line in the file should begin with the keyword PREFIX, followed by a string that will be prepended to every generated function name. Frequently, the PREFIX is either identical or similar to the name of the `XXX.src` file. For example, the Berkeley DB application-specific recovery example uses the file `ex_apprec.src`, which begins with the following PREFIX line:

``` c
PREFIX ex_apprec
```

Following the PREFIX line are the include files required by the automatically generated functions. The include files should be listed in order, prefixed by the keyword INCLUDE. For example, the Berkeley DB application-specific recovery example lists the following include files:

``` c
INCLUDE #include "ex_apprec.h"
```

The rest of the XXX.src file consists of log record descriptions. Each log record description begins with one of the following lines:

``` c
BEGIN RECORD_NAME DB_VERSION_NUMBER RECORD_NUMBER
```

``` c
BEGIN_COMPAT RECORD_NAME DB_VERSION_NUMBER RECORD_NUMBER
```

and ends with the line:

``` c
END
```

The <span class="emphasis">*BEGIN*</span> line should be used for most record types.

The <span class="emphasis">*BEGIN_COMPAT*</span> is used for log record compatibility to facilitate online upgrades of replication groups. Records created with this keyword will produce reading and printing routines, but no logging routines. The recovery routines are retrieved from older releases, so no recovery templates will be generated for these records.

The <span class="emphasis">*DB_VERSION_NUMBER*</span> variable should be replaced with the current major and minor version of Berkeley DB, with all punctuation removed. For example, Berkeley DB version 4.2 should be 42, version 4.5 should be 45.

The <span class="emphasis">*RECORD_NAME*</span> variable should be replaced with a record name for this log record. The <span class="emphasis">*RECORD_NUMBER*</span> variable should be replaced with a record number.

The combination of PREFIX name and <span class="emphasis">*RECORD_NAME*</span>, and the <span class="emphasis">*RECORD_NUMBER*</span> must be unique for the application, that is, values for application-specific and Berkeley DB log records may not overlap. Further, because record numbers are stored in log files, which are usually portable across application and Berkeley DB releases, any change to the record numbers or log record format or should be handled as described in the section on log format changes in the <a href="../../guides/upgrading/upgrade_process.md" class="olink">Upgrading Berkeley DB installations</a> chapter of the Berkeley DB Installation and Build Guide. The record number space below 10,000 is reserved for Berkeley DB itself; applications should choose record number values equal to or greater than 10,000.

Between the BEGIN and END keywords there should be one optional <span class="emphasis">*DUPLICATE*</span> line and one line for each data item logged as part of this log record.

The <span class="emphasis">*DUPLICATE*</span> line is of the form:

``` c
DUPLICATE RECORD_NAME DB_VERSION_NUMBER RECORD_NUMBER
```

The <span class="emphasis">*DUPLICATE*</span> specifier should be used when creating a record that requires its own record number but can use the argument structure, reading and printing routines from another record. In this case, we will create a new log record type, but use the enclosing log record type for the argument structure and the log reading and printing routines.

The format of lines for each data item logged is as follows:

``` c
ARG | DBT | POINTER   variable_name    variable_type    printf_format
```

The keyword ARG indicates that the argument is a simple parameter of the type specified. For example, a file ID might be logged as:

``` c
ARG   fileID  int d
```

The keyword DBT indicates that the argument is a Berkeley DB DBT structure, containing a length and pointer to a byte string. The keyword POINTER indicates that the argument is a pointer to the data type specified (of course the data type, not the pointer, is what is logged).

The <span class="emphasis">*variable_name*</span> is the field name within the structure that will be used to refer to this item. The <span class="emphasis">*variable_type*</span> is the C-language type of the variable, and the printf format is the C-language format string, without the leading percent ("%") character, that should be used to display the contents of the field (for example, "s" for string, "d" for signed integral type, "u" for unsigned integral type, "ld" for signed long integral type, "lu" for long unsigned integral type, and so on).

For example, ex_apprec.src defines a single log record type, used to log a directory name that has been stored in a DBT:

``` c
BEGIN mkdir       10000
DBT dirname     DBT     s
END
```

As the name suggests, this example of an application-defined log record will be used to log the creation of a directory. There are many more examples of XXX.src files in the Berkeley DB distribution. For example, the file btree/btree.src contains the definitions for the log records supported by the Berkeley DB Btree access method.
