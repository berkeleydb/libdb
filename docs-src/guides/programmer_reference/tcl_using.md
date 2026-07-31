---
title: "Using Berkeley DB with Tcl"
api-name: "Using Berkeley DB with Tcl"
source: docs/programmer_reference/tcl_using.html
---
## Using Berkeley DB with Tcl

All commands in the Berkeley DB Tcl interface are in the following form:

``` c
command_handle operation options
```

The <span class="emphasis">*command handle*</span> is **berkdb** or one of the additional commands that may be created. The <span class="emphasis">*operation*</span> is what you want to do to that handle, and the <span class="emphasis">*options*</span> apply to the operation. Commands that get created on behalf of the application have their own sets of operations. Generally, any calls in DB that result in new object handles will translate into a new command handle in Tcl. Then, the user can access the operations of the handle via the new Tcl command handle.

Newly created commands are named with an abbreviated form of their objects, followed by a number. Some created commands are subcommands of other created commands and will be the first command, followed by a period (.), and then followed by the new subcommand. For example, suppose that you have a database already existing called my_data.db. The following example shows the commands created when you open the database and when you open a cursor:

``` c
# First open the database and get a database command handle
% berkdb open my_data.db
db0
#Get some data from that database
% db0 get my_key
{{my_key my_data0}{my_key my_data1}}
#Open a cursor in this database, get a new cursor handle
% db0 cursor
db0.c0
#Get the first data from the cursor
% db0.c0 get -first
{{first_key first_data}}
```

All commands in the library support a special option **-?** that will list the correct operations for a command or the correct options.

A list of commands and operations can be found in the <a href="../api_reference/TCL/index.html" class="olink">Tcl API</a> documentation.
