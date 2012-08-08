/*
  Part of: Vicare/SQLite
  Contents: miscellaneous functions
  Date: Tue Jul 31, 2012

  Abstract



  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either version  3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See the  GNU
  General Public License for more details.

  You should  have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include <vicare-sqlite-internals.h>


/** --------------------------------------------------------------------
 ** Error codes and error messages.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_extended_result_codes (ikptr s_conn, ikptr s_boolean, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_EXTENDED_RESULT_CODES
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);
  int		rv;
  rv = sqlite3_extended_result_codes(conn, (false_object == s_boolean)? 0 : 1);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_errcode (ikptr s_conn, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_ERRCODE
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);
  int		rv;
  rv = sqlite3_errcode(conn);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_extended_errcode (ikptr s_conn, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_EXTENDED_ERRCODE
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);
  int		rv;
  rv = sqlite3_extended_errcode(conn);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_errmsg (ikptr s_conn, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_ERRMSG
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);
  const char *	rv;
  rv = sqlite3_errmsg(conn);
  return ika_bytevector_from_cstring(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_errmsg16 (ikptr s_conn, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_ERRMSG16
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);
  const void *	rv;
  rv = sqlite3_errmsg16(conn);
  return ika_bytevector_from_cstring(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SQL snippets validation.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_complete (ikptr s_sql_snippet)
{
#ifdef HAVE_SQLITE3_COMPLETE
  const char *	sql_snippet = IK_BYTEVECTOR_DATA_CHARP(s_sql_snippet);
  return (sqlite3_complete(sql_snippet))? true_object : false_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_complete16 (ikptr s_sql_snippet)
{
#ifdef HAVE_SQLITE3_COMPLETE16
  const char *	sql_snippet = IK_BYTEVECTOR_DATA_CHARP(s_sql_snippet);
  int		rv;
  rv = sqlite3_complete16(sql_snippet);
  /* fprintf(stderr, "%s: %d\n", __func__, rv); */
  return (rv)? true_object : false_object;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Miscellaneous functions.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_sleep (ikptr s_milliseconds, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SLEEP
  int	milliseconds = ik_integer_to_int(s_milliseconds);
  int	rv;
  rv = sqlite3_sleep(milliseconds);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_log (ikptr s_error_code, ikptr s_message, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_LOG
  int		error_code = ik_integer_to_int(s_error_code);
  const char *	message    = IK_BYTEVECTOR_DATA_CHARP(s_message);
  sqlite3_log(error_code, message);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_randomness (ikptr s_bytevector, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RANDOMNESS
  int		len = IK_BYTEVECTOR_LENGTH(s_bytevector);
  uint8_t *	ptr = IK_BYTEVECTOR_DATA_VOIDP(s_bytevector);
  sqlite3_randomness(len, ptr);
  return s_bytevector;
#else
  feature_failure(__func__);
#endif
}

/* end of file */
