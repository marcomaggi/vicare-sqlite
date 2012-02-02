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

#define SL_DB_POINTER_OBJECT(PARSER)	(PARSER)
#define SL_DB(PARSER)			\
  IK_POINTER_DATA_VOIDP(SL_DB_POINTER_OBJECT(PARSER))

#define SL_CALLBACK(CALLBACK)	IK_POINTER_DATA_VOIDP(CALLBACK)


/** --------------------------------------------------------------------
 ** Version functions.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_libversion (ikpcb * pcb)
{
  return ika_bytevector_from_cstring(pcb, sqlite3_libversion());
}


/** --------------------------------------------------------------------
 ** Callbacks.
 ** ----------------------------------------------------------------- */

#define DECLARE_SINGLE_CALLBACK_SETTER(IK_SUFFIX,EX_FUNCTION)	\
  ikptr								\
  ik_sqlite_ ## IK_SUFFIX (ikptr s_parser, ikptr s_callback)	\
  {								\
    EX_FUNCTION(EX_PARSER(s_parser), EX_CALLBACK(s_callback));	\
    return void_object;						\
  }

#define DECLARE_DOUBLE_CALLBACK_SETTER(IK_SUFFIX,EX_FUNCTION)		\
  ikptr									\
  ik_sqlite_ ## IK_SUFFIX (ikptr s_parser, ikptr s_start, ikptr s_end)	\
  {									\
    EX_FUNCTION(EX_PARSER(s_parser), EX_CALLBACK(s_start), EX_CALLBACK(s_end));	\
    return void_object;							\
  }

// DECLARE_SINGLE_CALLBACK_SETTER(set_element_decl_handler, XML_SetElementDeclHandler)


/* end of file */
