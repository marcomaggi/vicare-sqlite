/*
  Part of: Vicare/SQLite
  Contents: functions still to be implemented
  Date: Sun Aug  5, 2012

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
 ** Still to be implemented.
 ** ----------------------------------------------------------------- */

#if 0
ikptr
ik_sqlite3_create_module (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_MODULE
  sqlite3_create_module();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_create_module_v2 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_MODULE_V2
  sqlite3_create_module_v2();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_declare_vtab (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DECLARE_VTAB
  sqlite3_declare_vtab();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_overload_function (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_OVERLOAD_FUNCTION
  sqlite3_overload_function();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_vfs_find (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VFS_FIND
  sqlite3_vfs_find();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_vfs_register (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VFS_REGISTER
  sqlite3_vfs_register();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_vfs_unregister (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VFS_UNREGISTER
  sqlite3_vfs_unregister();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_mutex_alloc (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_MUTEX_ALLOC
  sqlite3_mutex_alloc();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_mutex_free (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_MUTEX_FREE
  sqlite3_mutex_free();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_mutex_enter (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_MUTEX_ENTER
  sqlite3_mutex_enter();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_mutex_try (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_MUTEX_TRY
  sqlite3_mutex_try();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_mutex_leave (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_MUTEX_LEAVE
  sqlite3_mutex_leave();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_mutex_held (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_MUTEX_HELD
  sqlite3_mutex_held();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_mutex_notheld (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_MUTEX_NOTHELD
  sqlite3_mutex_notheld();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_db_mutex (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DB_MUTEX
  sqlite3_db_mutex();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_file_control (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_FILE_CONTROL
  sqlite3_file_control();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_test_control (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_TEST_CONTROL
  sqlite3_test_control();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_unlock_notify (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_UNLOCK_NOTIFY
  sqlite3_unlock_notify();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_stricmp (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_STRICMP
  sqlite3_stricmp();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_strnicmp (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_STRNICMP
  sqlite3_strnicmp();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_vtab_config (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VTAB_CONFIG
  sqlite3_vtab_config();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_vtab_on_conflict (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VTAB_ON_CONFLICT
  sqlite3_vtab_on_conflict();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_rtree_geometry_callback (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RTREE_GEOMETRY_CALLBACK
  sqlite3_rtree_geometry_callback();
#else
  feature_failure(__func__);
#endif
}
#endif

/* end of file */
