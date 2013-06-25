/*
  Part of: Vicare/SQLite
  Contents: backup functions
  Date: Tue Aug 28, 2012

  Abstract



  Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>

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
 ** Initialisation and finalisation.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_backup_init (ikptr s_dst_conn, ikptr s_dst_name,
			ikptr s_src_conn, ikptr s_src_name,
			ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BACKUP_INIT
  sqlite3 *		dst_conn = IK_SQLITE_CONNECTION(s_dst_conn);
  const char *		dst_name = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_dst_name);
  sqlite3 *		src_conn = IK_SQLITE_CONNECTION(s_src_conn);
  const char *		src_name = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_src_name);
  sqlite3_backup *	backup;
  /* fprintf(stderr, "%s: %p, %s, %p, %s\n", __func__, */
  /* 	  dst_conn, dst_name, src_conn, src_name); */
  backup = sqlite3_backup_init(dst_conn, dst_name, src_conn, src_name);
  return (backup)? ika_pointer_alloc(pcb, (ik_ulong)backup) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_backup_finish (ikptr s_backup, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BACKUP_FINISH
  ikptr			s_pointer = IK_SQLITE_BACKUP_POINTER(s_backup);
  sqlite3_backup *	backup    = IK_POINTER_DATA_VOIDP(s_pointer);
  if (backup) {
    int		rv;
    ik_enter_c_function(pcb);
    {
      rv = sqlite3_backup_finish(backup);
    }
    ik_leave_c_function(pcb);
    if (SQLITE_OK == rv) {
      IK_POINTER_SET_NULL(s_pointer);
    }
    return ika_integer_from_sqlite_errcode(pcb, rv);
  } else
    return ika_integer_from_sqlite_errcode(pcb, SQLITE_OK);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Stepping.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_backup_step (ikptr s_backup, ikptr s_number_of_pages, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BACKUP_STEP
  sqlite3_backup *	backup = IK_SQLITE_BACKUP(s_backup);
  int			npages = ik_integer_to_int(s_number_of_pages);
  int			rv;
  ik_enter_c_function(pcb);
  {
    rv = sqlite3_backup_step(backup, npages);
  }
  ik_leave_c_function(pcb);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Inspection.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_backup_remaining (ikptr s_backup, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BACKUP_REMAINING
  sqlite3_backup *	backup = IK_SQLITE_BACKUP(s_backup);
  int			rv;
  rv = sqlite3_backup_remaining(backup);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_backup_pagecount (ikptr s_backup, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BACKUP_PAGECOUNT
  sqlite3_backup *	backup = IK_SQLITE_BACKUP(s_backup);
  int			rv;
  rv = sqlite3_backup_pagecount(backup);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */


/* end of file */
