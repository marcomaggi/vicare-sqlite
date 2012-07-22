dnl @configure_input@
dnl

AC_PREREQ([2.68])
AC_INIT([Vicare/SQLite],[0.1d0],[marco.maggi-ipsu@poste.it],
  [vicare-sqlite],[http://marcomaggi.github.com/vicare.html])
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([-Wall])
dnl AM_MAINTAINER_MODE
AC_CONFIG_SRCDIR([src/])
AC_CONFIG_MACRO_DIR([m4])
AC_CANONICAL_HOST

#page
#### compiler

AC_PROG_CC
AC_PROG_CC_C_O

AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

LT_PREREQ([2.4])
LT_INIT([shared disable-static])

dnl AC_LANG([C])
dnl AC_DEFINE([_GNU_SOURCE],[1],[enable everything GNU])

#page
#### vicare configuration

AC_CHECK_HEADERS([vicare.h],,[AC_MSG_FAILURE([missing vicare.h header],[2])])
AC_CHECK_PROG([VICARE],[vicare],[vicare],[:])

#page
#### sqlite configuration

AC_CHECK_HEADERS([sqlite3.h],,[AC_MSG_FAILURE([cannot find SQLite main header],[2])])
AC_CHECK_HEADERS([sqlite3ext.h],,[AC_MSG_FAILURE([cannot find SQLite extension header],[2])])
AC_CHECK_LIB([sqlite3],[sqlite3_version],,[AC_MSG_FAILURE([cannot find SQLite library],[2])])

vicare_sqlite3_VERSION_INTERFACE_CURRENT=0
vicare_sqlite3_VERSION_INTERFACE_REVISION=0
vicare_sqlite3_VERSION_INTERFACE_AGE=0
AC_DEFINE_UNQUOTED([vicare_sqlite3_VERSION_INTERFACE_CURRENT],
  [$vicare_sqlite3_VERSION_INTERFACE_CURRENT],
  [current interface number])
AC_DEFINE_UNQUOTED([vicare_sqlite3_VERSION_INTERFACE_REVISION],
  [$vicare_sqlite3_VERSION_INTERFACE_REVISION],
  [current interface implementation number])
AC_DEFINE_UNQUOTED([vicare_sqlite3_VERSION_INTERFACE_AGE],
  [$vicare_sqlite3_VERSION_INTERFACE_AGE],
  [current interface age number])
AC_DEFINE_UNQUOTED([vicare_sqlite3_VERSION_INTERFACE_STRING],
  ["$vicare_sqlite3_VERSION_INTERFACE_CURRENT.$vicare_sqlite3_VERSION_INTERFACE_REVISION"],
  [library interface version])
AC_SUBST([vicare_sqlite3_VERSION_INTERFACE_CURRENT])
AC_SUBST([vicare_sqlite3_VERSION_INTERFACE_REVISION])
AC_SUBST([vicare_sqlite3_VERSION_INTERFACE_AGE])

#page
#### symbols

m4_define([VICARE_INCLUDES],[
AC_INCLUDES_DEFAULT
#ifdef HAVE_VICARE_H
#  include <vicare.h>
#endif
#ifdef HAVE_SQLITE3_H
#  include <sqlite3.h>
#endif
#ifdef HAVE_SQLITE3EXT_H
#  include <sqlite3ext.h>
#endif
])

dnl Miscellaneous string constants
VICARE_STRING_CONSTANT_TESTS([SQLITE_VERSION SQLITE_SOURCE_ID])

dnl Miscellaneous integer constants
VICARE_CONSTANT_TESTS([SQLITE_VERSION_NUMBER])


#page
#### finish

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile]
        [src/Makefile]
        [doc/Makefile]
        [tests/Makefile]
        [lib/Makefile]
        [lib/vicare/sqlite3/constants.sls]
        [meta/slackware/doinst.sh])
AC_OUTPUT

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^#page"
# End:
