dnl @(#)tcl.m4	11.1 (Sleepycat) 7/25/99

dnl Try to find the Tcl/Tk library.
AC_DEFUN(AM_TCL_LOAD, [

AC_CACHE_CHECK([for Tcl libraries], db_cv_tcllibs, [dnl
db_cv_tcllibs=no

if test "$db_cv_tcllibs" = no; then
	AC_TRY_LINK([#include <tcl.h>], [Tcl_Init(0);], [db_cv_tcllibs="yes"])
fi

if test "$db_cv_tcllibs" = no; then
	saved_libs="$LIBS";
	LIBS="$LIBS -ltcl"
	AC_TRY_LINK([#include <tcl.h>], [Tcl_Init(0);], [db_cv_tcllibs="-ltcl"])
	LIBS="$saved_libs"
fi

if test "$db_cv_tcllibs" = no; then
	saved_libs="$LIBS";
	LIBS="$LIBS -ltcl -ldl"
	AC_TRY_LINK([#include <tcl.h>],
	    [Tcl_Init(0);], [db_cv_tcllibs="-ltcl -ldl"])
	LIBS="$saved_libs"
fi

if test "$db_cv_tcllibs" = no; then
	saved_libs="$LIBS";
	LIBS="$LIBS -ltcl -ldl -lm"
	AC_TRY_LINK([#include <tcl.h>],
	    [Tcl_Init(0);], [db_cv_tcllibs="-ltcl -ldl -lm"])
	LIBS="$saved_libs"
fi

dnl Solaris requires lots 'o stuff.
if test "$db_cv_tcllibs" = no; then
	saved_libs="$LIBS";
	LIBS="$LIBS -ltcl -lsocket -ldl -lm -lnsl"
	AC_TRY_LINK([#include <tcl.h>],
	    [Tcl_Init(0);], [db_cv_tcllibs="-ltcl -lsocket -ldl -lm -lnsl"])
	LIBS="$saved_libs"
fi
])

if test "$db_cv_tcllibs" = "no"; then
	AC_MSG_ERROR([No Tcl library found.])
else
	if test "$db_cv_tcllibs" != "yes"; then
		LIBS="$LIBS $db_cv_tcllibs"
	fi
fi

])dnl
