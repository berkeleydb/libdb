---
title: "Using Berkeley DB with PHP"
api-name: "Using Berkeley DB with PHP"
source: docs/programmer_reference/ext_php.html
---
## Using Berkeley DB with PHP

A PHP 4 extension for this release of Berkeley DB is included in the distribution package. It can either either link directly against the installed Berkeley DB library (which is necessary for running in a non-Apache/mod_php4 environment), or against mod_db4, which provides additional safety when running under Apache/mod_php4.

For installation instructions, see the `INSTALL` file that resides in the `lang/php_db4` directory in your Berkeley DB distribution.

The PHP extension provides the following classes, which mirror the standard Berkeley DB C++ API.

``` c
class Db4Env {
    function Db4Env($flags = 0) {}
    function close($flags = 0) {}
    function dbremove($txn, $filename, $database = null, $flags = 0) {}
    function dbrename($txn, $file, $database, $new_database, 
                      $flags = 0) {}
    function open($home, $flags = DB_CREATE  | DB_INIT_LOCK | 
                  DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 
                  $mode = 0666) {}
    function remove($home, $flags = 0) {}
    function set_data_dir($directory) {}
    function txn_begin($parent_txn = null, $flags = 0) {}
    function txn_checkpoint($kbytes, $minutes, $flags = 0) {}
}

class Db4 {
    function Db4($dbenv = null) {}  // create a new Db4 object using 
                                    // the optional DbEnv
    function open($txn = null, $file = null, $database = null, 
                  $flags = DB_CREATE, $mode = 0) {}
    function close() {}
    function del($key, $txn = null) {}
    function get($key, $txn = null, $flags = 0) {}
    function pget($key, &$pkey, $txn = null, $flags = 0) {}
    function get_type() {} // returns the stringified 
                           // database type name
    function stat($txn = null, $flags = 0) {} // returns statistics as
                                              // an as
    function join($cursor_list, $flags = 0) {}
    function sync() {}
    function truncate($txn = null, $flags = 0) {}
    function cursor($txn = null, flags = 0) {}
}

class Db4Txn {
    function abort() {}
    function commit() {}
    function discard() {}
    function id() {}
    function set_timeout($timeout, $flags = 0) {}
}

class Db4Cursor {
    function close() {}
    function count() {}
    function del() {}
    function dup($flags = 0) {}
    function get($key, $flags = 0) {}
    function pget($key, &$primary_key, $flags = 0) {}
    function put($key, $data, $flags = 0) {}
}
```

The PHP extension attempts to be "smart" for you by:

1.  Auto-committing operations on transactional databases if no explicit Db4Txn object is specified.
2.  Performing reference and dependency checking to insure that all resources are closed in the correct order.
3.  Supplying default values for flags.
