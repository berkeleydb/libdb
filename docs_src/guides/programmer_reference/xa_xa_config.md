---
title: "Configuring Berkeley DB with the Tuxedo System"
api-name: "Configuring Berkeley DB with the Tuxedo System"
source: docs/programmer_reference/xa_xa_config.html
---
## Configuring Berkeley DB with the Tuxedo System

<span class="sect2"> [Update the Resource Manager File in Tuxedo](xa_xa_config.md#idp52786896) </span>

<span class="sect2"> [Build the Transaction Manager Server](xa_xa_config.md#idp52812512) </span>

<span class="sect2"> [Update the UBBCONFIG File](xa_xa_config.md#idp52759288) </span>

To configure the Tuxedo system to use Berkeley DB resource managers, do the following:

### Update the Resource Manager File in Tuxedo

For the purposes of this discussion, assume that the Tuxedo home directory is in

``` c
/home/tuxedo
```

In that case, the resource manager file will be located in

``` c
/home/tuxedo/udataobj/RM
```

Edit the resource manager file to identify the Berkeley DB resource manager, the name of the resource manager switch, and the name of the library for the resource manager.

For example, on a RedHat Linux Enterprise (64-bit) installation of Oracle Tuxedo 11gR1, you can update the resource manager file by adding the following line:

``` c
BERKELEY-DB:db_xa_switch:-L${DB_INSTALL}/lib -ldb 
```

where `${DB_INSTALL}` is the directory into which you installed the Berkeley DB library.

Note that the load options may differ depending on the platform of your system.

### Build the Transaction Manager Server

To do this, use the Tuxedo **buildtms(1)** utility. The **buildtms** command will create the `Berkeley-DB` resource manager in the directory from which it was run. The parameters to the **buildtms** command should be:

``` c
buildtms -v -o DBRM -r BERKELEY-DB
```

This will create an executable transaction manager server, `DBRM`, which is called by Tuxedo to process begins, commits, and aborts.

### Update the UBBCONFIG File

You must make sure that your TUXCONFIG environment variable identifies an UBBCONFIG file that properly identifies your resource managers. In the GROUPS section of the UBBCONFIG file, you should identify the group's LMID and GRPNO, as well as the transaction manager server name "TMSNAME=DBRM." You must also specify the OPENINFO parameter, setting it equal to the string

``` c
rm_name:dir
```

where rm_name is the resource name specified in the RM file (that is, BERKELEY-DB) and dir is the directory for the Berkeley DB home environment (see <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> for a discussion of Berkeley DB environments).

Because Tuxedo resource manager startup accepts only a single string for configuration, any environment customization that might have been done via the config parameter to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> must instead be done by placing a <a href="env_db_config.md" class="xref" title="DB_CONFIG configuration file">DB_CONFIG configuration file</a> in the Berkeley DB environment directory. See <a href="env_naming.md" class="xref" title="File naming">File naming</a> for further information.

Consider the following configuration. We have built a transaction manager server, as described previously. We want the Berkeley DB environment to be `/home/dbhome`, our database files to be maintained in `/home/datafiles`, our log files to be maintained in `/home/log`, and we want a duplexed server.

The GROUPS section of the ubb file might look like the following:

``` c
group_tm LMID=myname GRPNO=1 TMSNAME=DBRM TMSCOUNT=2 \
    OPENINFO="BERKELEY-DB:/home/dbhome"
```

There would be a <a href="env_db_config.md" class="xref" title="DB_CONFIG configuration file">DB_CONFIG configuration file</a> in the directory `/home/dbhome` that contained the following two lines:

``` c
set_data_dir    /home/datafiles
set_lg_dir  /home/log
```

Finally, the UBBCONFIG file must be translated into a binary version using Tuxedo's **tmloadcf**(1) utility, and then the pathname of that binary file must be specified as your TUXCONFIG environment variable.

At this point, your system is properly initialized to use the Berkeley DB resource manager.

See <a href="../../api/c/db.md" class="olink">DB class</a> for further information on accessing data files using XA.
