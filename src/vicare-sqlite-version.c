/*
  Part of: Vicare/SQLite
  Contents: Vicare backend for SQLite
  Date: Thu Feb  2, 2012

  Abstract



  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either  version 3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <vicare.h>
#include <sqlite3.h>

/* #define SL_DB_POINTER_OBJECT(PARSER)	(PARSER) */
/* #define SL_DB(PARSER)			\ */
/*   IK_POINTER_DATA_VOIDP(SL_DB_POINTER_OBJECT(PARSER)) */

/* #define SL_CALLBACK(CALLBACK)	IK_POINTER_DATA_VOIDP(CALLBACK) */


/** --------------------------------------------------------------------
 ** Support for missing functions.
 ** ----------------------------------------------------------------- */

static IK_UNUSED void
feature_failure_ (const char * funcname)
{
  ik_abort("called POSIX specific function, %s\n", funcname);
}

#define feature_failure(FN)     { feature_failure_(FN); return void_object; }


/** --------------------------------------------------------------------
 ** Vicare/SQLite version functions.
 ** ----------------------------------------------------------------- */

ikptr
vicare_sqlite3_version_string (ikpcb * pcb)
{
  return ika_bytevector_from_cstring(pcb, vicare_sqlite3_VERSION_INTERFACE_STRING);
}
ikptr
vicare_sqlite3_version_interface_current (void)
{
  return IK_FIX(vicare_sqlite3_VERSION_INTERFACE_CURRENT);
}
ikptr
vicare_sqlite3_version_interface_revision (void)
{
  return IK_FIX(vicare_sqlite3_VERSION_INTERFACE_REVISION);
}
ikptr
vicare_sqlite3_version_interface_age (void)
{
  return IK_FIX(vicare_sqlite3_VERSION_INTERFACE_AGE);
}


/** --------------------------------------------------------------------
 ** SQLite version functions.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_libversion (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_LIBVERSION
  return ika_bytevector_from_cstring(pcb, sqlite3_libversion());
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_sourceid (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SOURCEID
  return ika_bytevector_from_cstring(pcb, sqlite3_sourceid());
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_libversion_number (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_LIBVERSION_NUMBER
  return ika_integer_from_int(pcb, sqlite3_libversion_number());
#else
  feature_failure(__func__);
#endif
}

/* end of file */
