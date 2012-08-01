/*
  Part of: Vicare/SQLite
  Contents: internal header file for Vicare/SQLite
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

#ifndef VICARE_SQLITE_INTERNALS_H
#define VICARE_SQLITE_INTERNALS_H 1


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <vicare.h>
#include <string.h>
#include <sqlite3.h>


/** --------------------------------------------------------------------
 ** Handling of Scheme objects.
 ** ----------------------------------------------------------------- */

/* At present, all  the SQLite error codes  fit in a fixnum,  but in the
   future who knows?  (Marco Maggi; Aug 1, 2012) */
#define ika_integer_from_sqlite_errcode(PCB,CODE)	IK_FIX(CODE)
   /* ika_integer_from_int((PCB),(CODE)) */

#define IK_SQLITE_CALLBACK(S_CALLBACK)	IK_POINTER_DATA_VOIDP(S_CALLBACK)

/* Accessors for the fields of the Scheme structure "sqlite3". */
#define IK_SQLITE_SQLITE3_POINTER(CONN)		IK_FIELD((CONN),0)
#define IK_SQLITE_SQLITE3_PATHNAME(CONN)	IK_FIELD((CONN),1)
#define IK_SQLITE_CONNECTION(CONN)	\
  IK_POINTER_DATA_VOIDP(IK_SQLITE_SQLITE3_POINTER(CONN))

/* Accessors for the fields of the Scheme structure "sqlite3-stmt". */
#define IK_SQLITE_STMT_CONNECTION(STMT)		IK_FIELD((STMT),0)
#define IK_SQLITE_STMT_POINTER(STMT)		IK_FIELD((STMT),1)
#define IK_SQLITE_STMT_SQLBV(STMT)		IK_FIELD((STMT),2)
#define IK_SQLITE_STMT_ENCODING(STMT)		IK_FIELD((STMT),3)
#define IK_SQLITE_STATEMENT(STMT)	\
  IK_POINTER_DATA_VOIDP(IK_SQLITE_STMT_POINTER(STMT))


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
 ** Done.
 ** ----------------------------------------------------------------- */

#endif /* VICARE_SQLITE_INTERNALS_H */

/* end of file */
