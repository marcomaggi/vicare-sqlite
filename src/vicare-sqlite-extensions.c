/*
  Part of: Vicare/SQLite
  Contents: loading extensions
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
 ** SQLite extensions.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_load_extension (ikptr s_conn, ikptr s_pathname, ikptr s_procname, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_LOAD_EXTENSION
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);;
  const char *	pathname	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_pathname);
  const char *	procname	= \
    IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(s_procname);
  char *	error_message = NULL;
  int		rv;
  rv = sqlite3_load_extension(conn, pathname, procname, &error_message);
  if (SQLITE_OK == rv) {
    return ika_integer_from_sqlite_errcode(pcb, rv);
  } else {
    ikptr	s_pair = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_sqlite_errcode(pcb, rv));
      if (error_message) {
	IK_ASS(IK_CAR(s_pair), ika_bytevector_from_cstring(pcb, error_message));
	sqlite3_free(error_message);
      } else
	IK_CAR(s_pair) = IK_FALSE_OBJECT;
    }
    pcb->root0 = NULL;
    return s_pair;
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_enable_load_extension (ikptr s_conn, ikptr s_onoff, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_ENABLE_LOAD_EXTENSION
  sqlite3 *	conn	= IK_SQLITE_CONNECTION(s_conn);;
  int		onoff	= (IK_FALSE_OBJECT == s_onoff)? 0 : 1;
  int		rv;
  rv = sqlite3_enable_load_extension(conn, onoff);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_auto_extension (ikptr s_entry_point, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_AUTO_EXTENSION
  typedef void (*entry_point_t) (void);
  entry_point_t	cb = IK_POINTER_DATA_VOIDP(s_entry_point);
  int		rv;
  rv = sqlite3_auto_extension(cb);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_reset_auto_extension (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESET_AUTO_EXTENSION
  sqlite3_reset_auto_extension();
  return IK_VOID_OBJECT;
#else
  feature_failure(__func__);
#endif
}

/* end of file */
