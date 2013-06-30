### custom macros

dnl Common initialisation for the Vicare Scheme environment.
AC_DEFUN([VICARE_SCHEME],
  [AC_CHECK_HEADERS([vicare.h],,[AC_MSG_FAILURE([missing vicare.h header],[2])])
   AC_CHECK_PROG([VICARE],[vicare],[vicare],[:])
   AS_VAR_SET_IF(VFLAGS,,[AS_VAR_SET(VFLAGS,["-O2"])])])

AC_DEFUN([VICARE_OUTPUT],
  [AC_SUBST([VFLAGS])])

dnl page
dnl Configuration options.

AC_DEFUN([VICARE_OPTION_DEBUGGING_MODE],
  [VICARE_DEBUG=no
   AC_MSG_CHECKING([whether debugging features are included in compiled Scheme libraries])
   AC_ARG_ENABLE([debug],
     [AS_HELP_STRING([--enable-debug],
        [enable compilation of Scheme libraries with debugging features (default is disable)])],
     [AC_MSG_RESULT([$enableval])
     if test "z$enableval" = zno
     then VICARE_DEBUG=no
     else VICARE_DEBUG=yes
     fi],
     [VICARE_DEBUG=no
      AC_MSG_RESULT([no])])])

dnl $1 - "yes", "no" or "check"
dnl
dnl Notice that the AM_CONDITIONAL definition for WANT_NAUSICAA
dnl must go in "configure.ac", else Automake will not see it.
AC_DEFUN([VICARE_OPTION_NAUSICAA],
  [AC_ARG_WITH([nausicaa],
     AS_HELP_STRING([--with-nausicaa],[install Nausicaa libraries (default is $1)]),
     [true],[with_nausicaa=$1])
   vicare_with_nausicaa=no
   if test x$with_nausicaa = xyes || test x$with_nausicaa = xcheck
   then
     VICARE_CHECK_LIBRARY([NAUSICAA],[(nausicaa)],
       [vicare_have_nausicaa=yes],[vicare_have_nausicaa=no])
     if test "$vicare_have_nausicaa" = yes || test "$vicare_cv_have_NAUSICAA" = yes
     then
       # Nausicaa library was found: success!
       AC_MSG_NOTICE([Nausicaa support enabled])
       vicare_with_nausicaa=yes
     elif test "$with_nausicaa" = yes
     then
       # The user  requested Nausicaa support  or die, but the  library was
       # not found.
       AC_MSG_ERROR([Nausicaa cannot be found])
     else
       # The  user requested  a check  for optional  Nausicaa  support, but
       # the library was not found.
       AC_MSG_WARN([Nausicaa not found -- support disabled])
     fi
   fi])

dnl page
dnl C language helper library preparations.

dnl $1 - library name
dnl $2 - interface current version number
dnl $3 - interface revision version number
dnl $4 - interface age version number
AC_DEFUN([VICARE_EXTENSION_LIBRARY_VERSION],
  [vicare_$1_VERSION_INTERFACE_CURRENT=$2
   vicare_$1_VERSION_INTERFACE_REVISION=$3
   vicare_$1_VERSION_INTERFACE_AGE=$4
   AC_DEFINE_UNQUOTED([vicare_$1_VERSION_INTERFACE_CURRENT],
     [$vicare_$1_VERSION_INTERFACE_CURRENT],
     [current interface number])
   AC_DEFINE_UNQUOTED([vicare_$1_VERSION_INTERFACE_REVISION],
     [$vicare_$1_VERSION_INTERFACE_REVISION],
     [current interface implementation number])
   AC_DEFINE_UNQUOTED([vicare_$1_VERSION_INTERFACE_AGE],
     [$vicare_$1_VERSION_INTERFACE_AGE],
     [current interface age number])
   AC_DEFINE_UNQUOTED([vicare_$1_VERSION_INTERFACE_STRING],
     ["$vicare_$1_VERSION_INTERFACE_CURRENT.$vicare_$1_VERSION_INTERFACE_REVISION"],
     [library interface version])
   AC_SUBST([vicare_$1_VERSION_INTERFACE_CURRENT])
   AC_SUBST([vicare_$1_VERSION_INTERFACE_REVISION])
   AC_SUBST([vicare_$1_VERSION_INTERFACE_AGE])])

dnl page
dnl Exact integer constants inspection.

AC_DEFUN([VICARE_VALUEOF_TEST],[
  VALUEOF_$1="#f"
  AC_CACHE_CHECK([the value of '$2'],
    [vicare_cv_valueof_$1],
    [AC_COMPUTE_INT([vicare_cv_valueof_$1],
       [$2],
       [VICARE_INCLUDES],
       [vicare_cv_valueof_$1="#f"])])
   VALUEOF_$1="$vicare_cv_valueof_$1"
   AC_SUBST([VALUEOF_$1])])

AC_DEFUN([VICARE_CONSTANT_TEST],[VICARE_VALUEOF_TEST([$1],[$1])])
AC_DEFUN([VICARE_CONSTANT_TESTS],[m4_map_args_w($1,[VICARE_CONSTANT_TEST(],[)])])

AC_DEFUN([VICARE_CONSTANT_FALSE],
  [VALUEOF_$1="#f"
   AC_SUBST([VALUEOF_$1])])
AC_DEFUN([VICARE_CONSTANT_FALSES],[m4_map_args_w($1,[VICARE_CONSTANT_FALSE(],[)])])

dnl page
dnl String constants inspection.

AC_DEFUN([VICARE_STRINGOF_TEST],
  [VALUEOF_$1=""
   AC_CACHE_CHECK([the string value of '$1'],
     [vicare_cv_stringof_$1],
     [AC_RUN_IFELSE([AC_LANG_SOURCE([VICARE_INCLUDES
        int main (void)
        {
           FILE *f = fopen ("conftest.val", "w");
           fprintf(f, "%s", $2);
           return ferror (f) || fclose (f) != 0;
        }])],
        [vicare_cv_stringof_$1=`cat conftest.val`],
        [vicare_cv_stringof_$1=""],
	[vicare_cv_stringof_$1=""])
      rm -f conftest.val])
   VALUEOF_$1="$vicare_cv_stringof_$1"
   AC_SUBST([VALUEOF_$1])])

AC_DEFUN([VICARE_STRING_CONSTANT_TEST],[VICARE_STRINGOF_TEST([$1],[$1])])
AC_DEFUN([VICARE_STRING_CONSTANT_TESTS],[m4_map_args_w($1,[VICARE_STRING_CONSTANT_TEST(],[)])])

dnl page
dnl Floating point constants inspection.

AC_DEFUN([VICARE_DOUBLEOF_TEST],
  [VALUEOF_$1=""
   AC_CACHE_CHECK([the floating point value of '$1'],
     [vicare_cv_doubleof_$1],
     [AC_RUN_IFELSE([AC_LANG_SOURCE([VICARE_INCLUDES
        int main (void)
        {
           FILE *f = fopen ("conftest.val", "w");
           fprintf(f, "%f", $1);
           return ferror (f) || fclose (f) != 0;
        }])],
        [vicare_cv_doubleof_$1=`cat conftest.val`],
        [vicare_cv_doubleof_$1=""],
	[vicare_cv_doubleof_$1="0.0"])
      rm -f conftest.val])
   VALUEOF_$1="$vicare_cv_doubleof_$1"
   AC_SUBST([VALUEOF_$1])])

AC_DEFUN([VICARE_DOUBLEOF_TESTS],[m4_map_args_w($1,[VICARE_DOUBLEOF_TEST(],[)])])

dnl page
dnl Checking functions.

dnl $1 - function name
dnl $2 - program body
dnl $3 - includes
AC_DEFUN([VICARE_CHECK_CPP_FUNC],
  [AS_LINENO_PUSH([$1])
   AC_LANG_PUSH([C++])
   AC_CACHE_CHECK([for $1],
     [vicare_cv_func_$1],
     [AC_LINK_IFELSE([AC_LANG_PROGRAM([$3],[$2])],
        [AS_VAR_SET([vicare_cv_func_$1],[yes])],
        [AS_VAR_SET([vicare_cv_func_$1],[no])])])
   dnl Prepare the autoheader snippet for the function.
   _AH_CHECK_FUNC([$1])
   AS_VAR_IF(vicare_cv_func_$1,[yes],
     [AC_DEFINE_UNQUOTED(AS_TR_CPP([HAVE_]$1))])
   AC_LANG_POP([C++])
   AS_LINENO_POP])

dnl page
dnl 1 WITH_TEMP_FILE_CHUNK
dnl 2 AFTER_CHUNK
AC_DEFUN([VICARE_WITH_TMPFILE],
  [: ${TMPDIR=/tmp}
   {
     vicare_private_TMPDIR=$((umask 077 && mktemp -d "$TMPDIR/fooXXXXXX") 2>/dev/null) &&
       test -n "${vicare_private_TMPDIR}" && test -d "${vicare_private_TMPDIR}"
   } || {
     vicare_private_TMPDIR=${TMPDIR}/foo$$-$RANDOM
     (umask 077 && mkdir "${vicare_private_TMPDIR}")
   } || exit $?
   vicare_TMPFILE=${vicare_private_TMPDIR}/temporary.txt
   dnl Chunk with temporary file usage.
   $1
   rm -fr "${vicare_private_TMPDIR}"
   dnl Chunk after temporary file usage.
   $2
   ])

AC_DEFUN([WITH_OUTPUT_FROM_VICARE_SCRIPT],
  [VICARE_WITH_TMPFILE([vicare_ANSWER=`echo '$1' >"${vicare_TMPFILE}"
    "${VICARE}" "${vicare_TMPFILE}" $2`],[$3])])

dnl 1 OUTPUT_VARIABLE_COMPONENT_NAME
dnl 2 LIBRARY_IMPORT_SPEC
dnl 3 OPTIONAL_ACTION_IF_FOUND
dnl 4 OPTIONAL_ACTION_IF_FOUND
AC_DEFUN([VICARE_CHECK_LIBRARY],
  [AC_CACHE_CHECK([availability of Vicare library $2],
     [vicare_cv_schemelib_$1],
     [WITH_OUTPUT_FROM_VICARE_SCRIPT([(import (rnrs) (rnrs eval (6)))
       (with-exception-handler
          (lambda (ex)
            (display "no\n")
            (flush-output-port (current-output-port))
            (exit))
          (lambda ()
            (environment (quote $2))
            (display "yes\n")
            (flush-output-port (current-output-port))))],,
         [AS_VAR_SET([vicare_cv_schemelib_$1],[$vicare_ANSWER])
          if test "$vicare_ANSWER" = yes ; then
            dnl action if found
            :
            $3
          else
            dnl action if not found
            AC_MSG_WARN([Vicare Scheme could not find library $2])
            $4
          fi])])])

### end of file
# Local Variables:
# mode: autoconf
# End:
