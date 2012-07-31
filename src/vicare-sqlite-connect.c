/*
  Part of: Vicare/SQLite
  Contents: database connections
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
 ** Opening and closing connections.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_close (ikptr s_conn, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CLOSE
  ikptr		s_pointer	= IK_FIELD(s_conn, 0);
  sqlite3 *	conn		= IK_POINTER_DATA_VOIDP(s_pointer);
  if (conn) {
    int		rv;
    rv = sqlite3_close(conn);
    if (SQLITE_OK == rv) {
      IK_POINTER_SET_NULL(s_pointer);
    }
    return IK_FIX(rv);
  } else
    return IK_FIX(SQLITE_OK);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_open (ikptr s_pathname, ikptr s_conn, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_OPEN
  const char *	pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  sqlite3 *	conn;
  int		rv;
  rv = sqlite3_open(pathname, &conn);
  if (SQLITE_OK == rv) {
    pcb->root0 = &s_conn;
    {
      IK_ASS(IK_FIELD(s_conn, 0), ika_pointer_alloc(pcb, (ik_ulong)conn));
    }
    pcb->root0 = NULL;
    return IK_FIX(rv);
  } else {
    /* When "sqlite3_open()" fails:  it still may have  allocated a data
       structure and it is our responsibility to release it. */
    if (conn)
      sqlite3_close(conn);
    return IK_FIX(rv);
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_open16 (ikptr s_pathname, ikptr s_conn, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_OPEN16
  long		pathlen = IK_BYTEVECTOR_LENGTH(s_pathname)+1;
  char		pathname[pathlen];
  sqlite3 *	conn;
  int		rv;
  memcpy(pathname, IK_BYTEVECTOR_DATA_CHARP(s_pathname), pathlen);
  /*
  fprintf(stderr, "%s: pathname utf-16 len %ld, buf: ", __func__,
	  IK_BYTEVECTOR_LENGTH(s_pathname));
  fwrite(pathname, pathlen, 1, stderr);
  fprintf(stderr, ", end %d %d %d\n",
	  pathname[pathlen-3],
	  pathname[pathlen-2],
	  pathname[pathlen-1]);
  */
  rv = sqlite3_open16(pathname, &conn);
  if (SQLITE_OK == rv) {
    pcb->root0 = &s_conn;
    {
      IK_ASS(IK_FIELD(s_conn, 0), ika_pointer_alloc(pcb, (ik_ulong)conn));
    }
    pcb->root0 = NULL;
    return IK_FIX(rv);
  } else {
    /* When "sqlite3_open16()" fails: it still may have allocated a data
       structure and it is our responsibility to release it. */
    if (conn)
      sqlite3_close(conn);
    return IK_FIX(rv);
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_open_v2 (ikptr s_pathname, ikptr s_conn, ikptr s_flags, ikptr s_vfs_module,
		    ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_OPEN_V2
  const char *	pathname	= IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  int		flags		= ik_integer_to_int(s_flags);
  const char *	vfs_module;
  sqlite3 *	conn;
  int		rv;
  vfs_module = (false_object == s_vfs_module)? NULL : IK_BYTEVECTOR_DATA_CHARP(s_vfs_module);
  rv = sqlite3_open_v2(pathname, &conn, flags, vfs_module);
  if (SQLITE_OK == rv) {
    pcb->root0 = &s_conn;
    {
      IK_ASS(IK_FIELD(s_conn, 0), ika_pointer_alloc(pcb, (ik_ulong)conn));
    }
    pcb->root0 = NULL;
    return IK_FIX(rv);
  } else {
    /* When  "sqlite3_open_v2()" fails:  it still  may have  allocated a
       data structure and it is our responsibility to release it. */
    if (conn)
      sqlite3_close(conn);
    return IK_FIX(rv);
  }
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Configuring connections.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_db_config (ikptr s_conn, ikptr s_option_identifier, ikptr s_args)
/* Interface to  the C function "sqlite3_db_config()";  this function is
   variadic.   For most  options: if  successful return  SQLITE_OK, else
   return a SQLITE_  error code; see the individual  option branches for
   special cases.

   This function accepts  a fixnum as second  argument, representing the
   option identifier  as one of  the SQLITE_DBCONFIG_ constants;  if the
   option  identifier  is  not  recognised here:  the  return  value  is
   SQLITE_ERROR.

   S_ARGS  must be  false,  if no  arguments where  given,  or a  vector
   holding the arguments. */
{
#ifdef HAVE_SQLITE3_DB_CONFIG
  sqlite3 *	conn			= IK_SQLITE_CONNECTION(s_conn);
  int		option_identifier	= IK_UNFIX(s_option_identifier);
  int		rv;
  switch (option_identifier) {
#ifdef SQLITE_DBCONFIG_LOOKASIDE
  case SQLITE_DBCONFIG_LOOKASIDE:
    if ((3 == IK_VECTOR_LENGTH(s_args)) &&
        ik_is_pointer(IK_ITEM(s_args, 0)) &&
	IK_IS_FIXNUM(IK_ITEM(s_args, 1)) &&
	IK_IS_FIXNUM(IK_ITEM(s_args, 2)))
      rv = sqlite3_db_config(conn, option_identifier,
                             IK_POINTER_DATA_VOIDP(IK_ITEM(s_args, 0)),
                             IK_UNFIX(IK_ITEM(s_args, 1)),
                             IK_UNFIX(IK_ITEM(s_args, 2)));
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_DBCONFIG_ENABLE_FKEY
  case SQLITE_DBCONFIG_ENABLE_FKEY:
    {
      int fk;
      if ((1 == IK_VECTOR_LENGTH(s_args)) && IK_IS_FIXNUM(IK_ITEM(s_args, 0))) {
        rv = sqlite3_db_config(conn, option_identifier, IK_UNFIX(IK_ITEM(s_args, 0)), &fk);
	return (fk)? true_object : false_object;
      } else
	rv = SQLITE_ERROR;
    }
    break;
#endif
#ifdef SQLITE_DBCONFIG_ENABLE_TRIGGER
  case SQLITE_DBCONFIG_ENABLE_TRIGGER:
    {
      int fk;
      if ((1 == IK_VECTOR_LENGTH(s_args)) && IK_IS_FIXNUM(IK_ITEM(s_args, 0))) {
        rv = sqlite3_db_config(conn, option_identifier, IK_UNFIX(IK_ITEM(s_args, 0)), &fk);
	return (fk)? true_object : false_object;
      } else
	rv = SQLITE_ERROR;
    }
    break;
#endif
  default:
    return IK_FIX(SQLITE_ERROR);
  }
  return IK_FIX(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Auxiliary functions.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_busy_handler (ikptr s_conn, ikptr s_callback /*, ikpcb * pcb */)
{
#ifdef HAVE_SQLITE3_BUSY_HANDLER
  typedef int (*ik_sqlite3_busy_handler_callback) (void*,int);
  sqlite3 *				conn;
  ik_sqlite3_busy_handler_callback	cb;
  int	rv;
  conn = IK_SQLITE_CONNECTION(s_conn);
  cb   = (false_object == s_callback)? NULL : IK_SQLITE_CALLBACK(s_callback);
  rv   = sqlite3_busy_handler(conn, cb, NULL);
  return IK_FIX(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_busy_timeout (ikptr s_conn, ikptr s_milliseconds /*, ikpcb * pcb */)
{
#ifdef HAVE_SQLITE3_BUSY_TIMEOUT
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);
  int		ms   = IK_UNFIX(s_milliseconds);
  int		rv;
  rv = sqlite3_busy_timeout(conn, ms);
  return IK_FIX(rv);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ik_sqlite3_progress_handler (ikptr s_conn, ikptr s_instruction_count, ikptr s_callback)
{
#ifdef HAVE_SQLITE3_PROGRESS_HANDLER
  typedef int (*ik_sqlite3_progress_handler_callback) (void*);
  sqlite3 *				conn;
  ik_sqlite3_progress_handler_callback	cb;
  int					instruction_count;
  conn			= IK_SQLITE_CONNECTION(s_conn);
  instruction_count	= ik_integer_to_int(s_instruction_count);
  cb			= (false_object == s_callback)? NULL : IK_SQLITE_CALLBACK(s_callback);
  sqlite3_progress_handler(conn, instruction_count, cb, NULL);
  return void_object;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */


/* end of file */
