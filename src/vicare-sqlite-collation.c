/*
  Part of: Vicare/SQLite
  Contents: collation functions
  Date: Wed Aug 29, 2012

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
 ** Creating a collation.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_create_collation (ikptr s_conn, ikptr s_collation_name, ikptr s_encoding,
			     ikptr s_custom_data, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_COLLATION
  typedef int (*compare_t) (void*, int, const void*, int, const void*);
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  const char *	collation_name	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER(s_collation_name);
  int		encoding	= ik_integer_to_int(s_encoding);
  void *	custom_data	= IK_POINTER_FROM_POINTER_OR_FALSE(s_custom_data);
  compare_t	cb		= IK_POINTER_FROM_POINTER_OR_FALSE(s_callback);
  int		rv;
  rv = sqlite3_create_collation(conn, collation_name, encoding, custom_data, cb);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_create_collation_v2 (ikptr s_conn, ikptr s_collation_name, ikptr s_encoding,
				ikptr s_custom_data, ikptr s_callback, ikptr s_destructor,
				ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_COLLATION_V2
  typedef int (*compare_t) (void*, int, const void*, int, const void*);
  typedef void (*destroy_t) (void*);
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  const char *	collation_name	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER(s_collation_name);
  int		encoding	= ik_integer_to_int(s_encoding);
  void *	custom_data	= IK_POINTER_FROM_POINTER_OR_FALSE(s_custom_data);
  compare_t	cb		= IK_POINTER_FROM_POINTER_OR_FALSE(s_callback);
  destroy_t	destroy		= IK_POINTER_FROM_POINTER_OR_FALSE(s_destructor);
  int		rv;
  rv = sqlite3_create_collation_v2(conn, collation_name, encoding, custom_data, cb, destroy);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_create_collation16 (ikptr s_conn, ikptr s_collation_name, ikptr s_encoding,
			       ikptr s_custom_data, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_COLLATION16
  typedef int (*compare_t) (void*, int, const void*, int, const void*);
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  const void *	collation_name	= IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER(s_collation_name);
  int		encoding	= ik_integer_to_int(s_encoding);
  void *	custom_data	= IK_POINTER_FROM_POINTER_OR_FALSE(s_custom_data);
  compare_t	cb		= IK_POINTER_FROM_POINTER_OR_FALSE(s_callback);
  int		rv;
  rv = sqlite3_create_collation16(conn, collation_name, encoding, custom_data, cb);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Accepting collation requests.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_collation_needed (ikptr s_conn, ikptr s_custom_data, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLLATION_NEEDED
  typedef void (*needed_t) (void*, sqlite3*, int eTextRep, const char*);
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  void *	custom_data	= IK_POINTER_FROM_POINTER_OR_FALSE(s_custom_data);
  needed_t	cb		= IK_POINTER_FROM_POINTER_OR_FALSE(s_callback);
  int		rv;
  rv = sqlite3_collation_needed(conn, custom_data, cb);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_collation_needed16 (ikptr s_conn, ikptr s_custom_data, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLLATION_NEEDED16
  typedef void (*needed_t) (void*, sqlite3*, int eTextRep, const void*);
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  void *	custom_data	= IK_POINTER_FROM_POINTER_OR_FALSE(s_custom_data);
  needed_t	cb		= IK_POINTER_FROM_POINTER_OR_FALSE(s_callback);
  int		rv;
  rv = sqlite3_collation_needed16(conn, custom_data, cb);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}

/* end of file */
