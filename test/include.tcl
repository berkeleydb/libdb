set tclsh_path @TCL_TCLSH@
set tcllib .libs/libdb_tcl-@DB_VERSION_MAJOR@.@DB_VERSION_MINOR@.@SOSUFFIX@
set rpc_server localhost
set test_path @srcdir@/../test

set KILL "@db_cv_path_kill@"

# DO NOT EDIT BELOW THIS LINE: automatically built by dist/distrib.

global dict
global testdir
set testdir ./TESTDIR

set is_windows_test 0
