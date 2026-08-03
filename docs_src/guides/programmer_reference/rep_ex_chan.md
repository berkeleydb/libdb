---
title: "Ex_rep_chan: a Replication Manager channel example"
api-name: "Ex_rep_chan: a Replication Manager channel example"
source: docs/programmer_reference/rep_ex_chan.html
---
## Ex_rep_chan: a Replication Manager channel example

Ex_rep_chan, found in the `examples/c/ex_rep_chan` subdirectory of the Berkeley DB distribution, is a simple but complete demonstration of a replicated application that uses the Replication Manager feature of channels to perform write forwarding. The application is a mock stock ticker. Although similar to the ex_rep_mgr example program, this example differs in that it provides an example of using Replication Manager message channels. Any site accepts a command to write data to the database. If the site is a client, then, using the channels feature, the application forwards the request to the master site. If the site is a master then the request is automatically handled locally. You can read and write stock values at any site without needing to know what site is currently the master.

The set of supported commands can be viewed with either the **help** or the **?** command. Several commands work with key/data pairs where the key is a stock symbol and the data is its value.

The command to retrieve and print the current site's database contents is **print** or simply an empty input line. To read the contents of the master's database from any site use the **get key key ...** command. That command will forward the read request to the master if necessary and return the key/data pairs for all given keys.

There are two commands to put data into the database. Both commands take one or more key/data pairs, all of which are written into the database in a single transaction at the master site. The **put** command sends the data to the master site, and simply waits for a status response. The **put_sync** command sends the data to the master site, and uses a transaction token returned by the master to wait for the contents of that put to be available on the local site. This serves as a demonstration of the <a href="rep_ryw.md" class="link" title="Read your writes consistency">read your writes</a> consistency feature.

The Makefile created by the standard UNIX configuration will build the ex_rep_chan example on most platforms. Enter "make ex_rep_chan" to build it.

The synopsis for the program is as follows:

`ex_rep_chan `**`-h home`**` `**`-l host:port`**` [`**`-MC`**`] [`**`-r host:port`**`] [`**`-R host:port`**`] [`**`-p priority`**`] [`**`-v`**`]`

The options are as follows:

<span class="term"> **-h** </span>  
Specify a home directory for the database environment.

<span class="term"> **-l** </span>  
Listen on local host "host" at port "port" for incoming connections.

<span class="term"> **-M** </span>  
Configure this process as a master.

<span class="term"> **-C** </span>  
Configure this process as a client.

<span class="term"> **-r** </span>  
Identifies the helper site used for joining the group.

<span class="term"> **-R** </span>  
Identifies a remote peer to be used for joining the group. This peer is used for syncing purposes. See <a href="rep_mastersync.md#rep_c2c_sync" class="xref" title="Client-to-client synchronization">Client-to-client synchronization</a> for more information.

<span class="term"> **-p** </span>  
Set the election priority. See <a href="rep_elect.md" class="xref" title="Elections">Elections</a> for more information.

<span class="term"> **-v** </span>  
Indicates that additional informational and debugging output should be enabled.

A typical ex_rep_chan session begins with a command such as the following, to start a master:

``` c
ex_rep_chan -M -h DIR1 -l localhost:30100
```

and several clients:

``` c
ex_rep_chan -C -h DIR2 -l localhost:30101 -r localhost:30100
ex_rep_chan -C -h DIR3 -l localhost:30102 -r localhost:30100
ex_rep_chan -C -h DIR4 -l localhost:30103 -r localhost:30100
```
