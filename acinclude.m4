### custom macros

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
  [AC_CACHE_CHECK([availability of Vicare library $2],[vicare_cv_have_$1],
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
         [vicare_cv_have_$1=$vicare_ANSWER
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
