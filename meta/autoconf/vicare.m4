### custom macros

dnl Common initialisation for the Vicare Scheme environment.
AC_DEFUN([VICARE_SCHEME],
  [AC_CHECK_HEADERS([vicare.h],,[AC_MSG_FAILURE([missing vicare.h header],[2])])
   AC_PATH_PROG([VICARE],[vicare])
   AS_VAR_SET_IF(VFLAGS,,[AS_VAR_SET(VFLAGS,["-O2"])])])

AC_DEFUN([VICARE_OUTPUT],
  [AC_SUBST([VFLAGS])])

dnl page
dnl Configuration options.

dnl Wrapper for AC_ARG_ENABLE which adds  verbose messages and defines a
dnl shell variable "vicare_enable_$1" set to "yes" or "no".
dnl
dnl $1 - upper case option name
dnl $2 - command line option name "--enable-$2"
dnl $3 - default (yes, no)
dnl $4 - text for the "checking option... " message
dnl $5 - text for the "enable option... " message
AC_DEFUN([VICARE_ENABLE_OPTION],
  [vicare_enable_$1=$3
   AC_MSG_CHECKING([$4])
   AC_ARG_ENABLE([$2],
     [AS_HELP_STRING([--enable-$2],
        [$5 (default is $3)])],
     [AS_CASE([$enableval],
        [yes],[vicare_enable_$1=yes],
        [no], [vicare_enable_$1=no],
        [AC_MSG_ERROR([bad value $enableval for --enable-$2])])],
     [vicare_enable_$1=$3])
   AC_MSG_RESULT([$vicare_enable_$1])])

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
     if test "$vicare_have_nausicaa" = yes || test "$vicare_cv_schemelib_NAUSICAA" = yes
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
       test -n "$vicare_private_TMPDIR" && test -d "$vicare_private_TMPDIR"
   } || {
     vicare_private_TMPDIR=$TMPDIR/foo$$-$RANDOM
     (umask 077 && mkdir "$vicare_private_TMPDIR")
   } || exit $?
   vicare_TMPFILE=$vicare_private_TMPDIR/temporary.txt
   dnl Chunk with temporary file usage.
   $1
   rm -fr "$vicare_private_TMPDIR"
   dnl Chunk after temporary file usage.
   $2
   ])

dnl 1 VICARE_SCRIPT_CONTENTS
dnl 2 VICARE_COMMAND_LINE_OPTIONS
dnl 3 SHELL_CODE_BLOCK
AC_DEFUN([WITH_OUTPUT_FROM_VICARE_SCRIPT],
  [VICARE_WITH_TMPFILE([vicare_ANSWER=`echo '$1' >"$vicare_TMPFILE"
    "$VICARE" --r6rs-script "$vicare_TMPFILE" $2`],[$3])])

dnl Set the shell variable "vicare_cv_schemelib_$1" to "yes" or "no".
dnl
dnl 1 OUTPUT_VARIABLE_COMPONENT_NAME
dnl 2 LIBRARY_IMPORT_SPEC
dnl
dnl Usage example:
dnl
dnl   VICARE_CHECK_LIBRARY([VICARE],[(vicare (0 4 (>= 2015) (>= 5) (>= 19)))])
dnl   AS_IF([test "$vicare_cv_schemelib_VICARE" = no],[AC_MSG_ERROR([wrong Vicare version],1)])
dnl
AC_DEFUN([VICARE_CHECK_LIBRARY],
  [AC_CACHE_CHECK([availability of Vicare library $2],
     [vicare_cv_schemelib_$1],
     [WITH_OUTPUT_FROM_VICARE_SCRIPT([(import (vicare))
       ;;ENVIRONMENT is classified as having no side effects,
       ;;so we have to make sure the compiler does not
       ;;remove it while optimising the program.
       (if (environment?
              (with-exception-handler
                  (lambda (ex)
                    (display "no\n")
                    (flush-output-port (current-output-port))
                    (exit 0))
                (lambda ()
                  (environment (quote $2)))))
           (display "yes\n")
         (display "no\n"))
         (flush-output-port (current-output-port))
       (exit 0)],,
         [AS_VAR_SET([vicare_cv_schemelib_$1],[$vicare_ANSWER])
          AS_IF([test "$vicare_ANSWER" = yes],[vicare_cv_schemelib_$1=yes],[vicare_cv_schemelib_$1=no])])])])

### end of file
# Local Variables:
# mode: autoconf
# End:
