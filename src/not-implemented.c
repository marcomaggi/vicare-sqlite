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
ik_sqlite3_set_authorizer (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SET_AUTHORIZER
  sqlite3_set_authorizer();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_profile (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_PROFILE
  sqlite3_profile();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_uri_parameter (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_URI_PARAMETER
  sqlite3_uri_parameter();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_uri_boolean (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_URI_BOOLEAN
  sqlite3_uri_boolean();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_uri_int64 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_URI_INT64
  sqlite3_uri_int64();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_create_collation (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_COLLATION
  sqlite3_create_collation();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_create_collation_v2 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_COLLATION_V2
  sqlite3_create_collation_v2();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_create_collation16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_COLLATION16
  sqlite3_create_collation16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_collation_needed (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLLATION_NEEDED
  sqlite3_collation_needed();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_collation_needed16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLLATION_NEEDED16
  sqlite3_collation_needed16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_key (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_KEY
  sqlite3_key();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_rekey (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_REKEY
  sqlite3_rekey();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_activate_see (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_ACTIVATE_SEE
  sqlite3_activate_see();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_activate_cerod (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_ACTIVATE_CEROD
  sqlite3_activate_cerod();
#else
  feature_failure(__func__);
#endif
}
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
ik_sqlite3_status (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_STATUS
  sqlite3_status();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_db_status (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DB_STATUS
  sqlite3_db_status();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_stmt_status (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_STMT_STATUS
  sqlite3_stmt_status();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_backup_init (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BACKUP_INIT
  sqlite3_backup_init();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_backup_step (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BACKUP_STEP
  sqlite3_backup_step();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_backup_finish (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BACKUP_FINISH
  sqlite3_backup_finish();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_backup_remaining (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BACKUP_REMAINING
  sqlite3_backup_remaining();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_backup_pagecount (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BACKUP_PAGECOUNT
  sqlite3_backup_pagecount();
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
ik_sqlite3_wal_hook (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_WAL_HOOK
  sqlite3_wal_hook();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_wal_autocheckpoint (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_WAL_AUTOCHECKPOINT
  sqlite3_wal_autocheckpoint();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_wal_checkpoint (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_WAL_CHECKPOINT
  sqlite3_wal_checkpoint();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_wal_checkpoint_v2 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_WAL_CHECKPOINT_V2
  sqlite3_wal_checkpoint_v2();
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
