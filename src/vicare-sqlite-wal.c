/*
  Part of: Vicare/SQLite
  Contents: WAL functions
  Date: Tue Sep  4, 2012

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
 ** Write ahead log.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_wal_hook (ikptr s_conn, ikptr s_callback, ikptr s_custom_data, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_WAL_HOOK
  typedef int cb_t (void *,sqlite3*,const char*,int);
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  cb_t *	callback	= IK_VOIDP_FROM_POINTER_OR_FALSE(s_custom_data);
  void *	custom_data	= IK_VOIDP_FROM_POINTER_OR_FALSE(s_custom_data);
  void *	rv;
  rv = sqlite3_wal_hook(conn, callback, custom_data);
  return (rv)? ika_pointer_alloc(pcb, (ik_ulong)rv) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_wal_autocheckpoint (ikptr s_conn, ikptr s_number_of_frames, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_WAL_AUTOCHECKPOINT
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  int		number_of_frames= ik_integer_to_int(s_number_of_frames);
  int		rv;
  rv = sqlite3_wal_autocheckpoint(conn, number_of_frames);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_wal_checkpoint (ikptr s_conn, ikptr s_database_name, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_WAL_CHECKPOINT
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  char *	database_name	= \
    IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(s_database_name);
  int		rv;
  rv = sqlite3_wal_checkpoint(conn, database_name);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_wal_checkpoint_v2 (ikptr s_conn, ikptr s_database_name,
			      ikptr s_checkpoint_mode, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_WAL_CHECKPOINT_V2
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  char *	database_name	= \
    IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(s_database_name);
  int		checkpoint_mode	= ik_integer_to_int(s_checkpoint_mode);
  int		size_of_wal_log;
  int		number_of_checkpointed_frames;
  int		rv;
  rv = sqlite3_wal_checkpoint_v2(conn, database_name, checkpoint_mode,
				 &size_of_wal_log, &number_of_checkpointed_frames);
  if (SQLITE_OK == rv) {
    ikptr	s_vec = ika_vector_alloc_and_init(pcb, 3);
    pcb->root0 = &s_vec;
    {
      IK_ASS(IK_ITEM(s_vec, 0), ika_integer_from_sqlite_errcode(pcb, rv));
      IK_ASS(IK_ITEM(s_vec, 1), ika_integer_from_int(pcb, size_of_wal_log));
      IK_ASS(IK_ITEM(s_vec, 2), ika_integer_from_int(pcb, number_of_checkpointed_frames));
    }
    pcb->root0 = NULL;
    return s_vec;
  } else
    return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}

/* end of file */
