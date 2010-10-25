
# Process sql API specific settings.
AC_DEFUN(AC_SQL_CONFIG, [
# Setup the SQLite debug build.
mkdir -p sql
if test "$with_tclconfig" != "no"; then
	db_cv_sql_config_tclconfig="--with-tcl=$with_tclconfig"
else
	db_cv_sql_config_tclconfig=	
fi

# It would be nice to use AC_CONFIG_SUBDIRS here, but it does not allow for
# tweaking of command line options, so hard code things instead.
#
# !!! BEGIN COPIED from autoconf distribution
# Modified to not repeat CPPFLAGS or readline settings

  # Remove --cache-file, --srcdir, and --disable-option-checking arguments
  # so they do not pile up.
  ac_sub_configure_args=
  ac_prev=
  eval "set x $ac_configure_args"
  shift
  for ac_arg
  do
    if test -n "$ac_prev"; then
      ac_prev=
      continue
    fi
    case $ac_arg in
    -cache-file | --cache-file | --cache-fil | --cache-fi \
    | --cache-f | --cache- | --cache | --cach | --cac | --ca | --c)
      ac_prev=cache_file ;;
    -cache-file=* | --cache-file=* | --cache-fil=* | --cache-fi=* \
    | --cache-f=* | --cache-=* | --cache=* | --cach=* | --cac=* | --ca=* \
    | --c=*)
      ;;
    --config-cache | -C)
      ;;
    -srcdir | --srcdir | --srcdi | --srcd | --src | --sr)
      ac_prev=srcdir ;;
    -srcdir=* | --srcdir=* | --srcdi=* | --srcd=* | --src=* | --sr=*)
      ;;
    -prefix | --prefix | --prefi | --pref | --pre | --pr | --p)
      ac_prev=prefix ;;
    -prefix=* | --prefix=* | --prefi=* | --pref=* | --pre=* | --pr=* | --p=*)
      ;;
    --disable-option-checking)
      ;;
    CPPFLAGS=* | *readline*)
      ;;
    *)
      case $ac_arg in
      *\'*) ac_arg=`AS_ECHO(["$ac_arg"]) | sed "s/'/'\\\\\\\\''/g"` ;;
      esac
      ac_sub_configure_args="$ac_sub_configure_args '$ac_arg'" ;;
    esac
  done

  # Always prepend --prefix to ensure using the same prefix
  # in subdir configurations.
  ac_arg="--prefix=$prefix"
  case $ac_arg in
  *\'*) ac_arg=`AS_ECHO(["$ac_arg"]) | sed "s/'/'\\\\\\\\''/g"` ;;
  esac
  ac_sub_configure_args="'$ac_arg' $ac_sub_configure_args"

  # Pass --silent
  if test "$silent" = yes; then
    ac_sub_configure_args="--silent $ac_sub_configure_args"
  fi

  # Always prepend --disable-option-checking to silence warnings, since
  # different subdirs can have different --enable and --with options.
  ac_sub_configure_args="--disable-option-checking $ac_sub_configure_args"

# !!! END COPIED from autoconf distribution

sqlite_dir=`cd $srcdir/../sql/sqlite && /bin/pwd`
(cd sql && eval "\$SHELL $sqlite_dir/configure --disable-option-checking $ac_sub_configure_args CPPFLAGS=\"-I.. $CPPFLAGS\" --enable-amalgamation=$db_cv_sql_amalgamation --enable-readline=$with_readline" && cat build_config.h >> config.h)
])
