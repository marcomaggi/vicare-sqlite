/*
  Part of: Vicare/SQLite
  Contents: database connections
  Date: Tue Jul 31, 2012

  Abstract



  Copyright (C) 2012, 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>

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

#include "vicare-sqlite-internals.h"


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
    /* Closing the connection may  cause invocation of Scheme callbacks,
       so we save the Scheme continuation. */
    ik_enter_c_function(pcb);
    {
      rv = sqlite3_close(conn);
    }
    ik_leave_c_function(pcb);
    if (SQLITE_OK == rv) {
      IK_POINTER_SET_NULL(s_pointer);
    }
    return ika_integer_from_sqlite_errcode(pcb,rv);
  } else
    return ika_integer_from_sqlite_errcode(pcb,SQLITE_OK);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_open (ikptr s_pathname, ikptr s_conn, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_OPEN
  const char *	pathname = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_pathname);
  sqlite3 *	conn;
  int		rv;
  rv = sqlite3_open(pathname, &conn);
  if (SQLITE_OK == rv) {
    pcb->root0 = &s_conn;
    {
      IK_ASS(IK_FIELD(s_conn, 0), ika_pointer_alloc(pcb, (ik_ulong)conn));
    }
    pcb->root0 = NULL;
    return ika_integer_from_sqlite_errcode(pcb,rv);
  } else {
    /* When "sqlite3_open()" fails:  it still may have  allocated a data
       structure and it is our responsibility to release it. */
    if (conn)
      sqlite3_close(conn);
    return ika_integer_from_sqlite_errcode(pcb,rv);
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_open16 (ikptr s_pathname, ikptr s_conn, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_OPEN16
  const char *	pathname = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_pathname);
  sqlite3 *	conn;
  int		rv;
  rv = sqlite3_open16(pathname, &conn);
  if (SQLITE_OK == rv) {
    pcb->root0 = &s_conn;
    {
      IK_ASS(IK_FIELD(s_conn, 0), ika_pointer_alloc(pcb, (ik_ulong)conn));
    }
    pcb->root0 = NULL;
    return ika_integer_from_sqlite_errcode(pcb,rv);
  } else {
    /* When "sqlite3_open16()" fails: it still may have allocated a data
       structure and it is our responsibility to release it. */
    if (conn)
      sqlite3_close(conn);
    return ika_integer_from_sqlite_errcode(pcb,rv);
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
  const char *	pathname	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_pathname);
  int		flags		= ik_integer_to_int(s_flags);
  const char *	vfs_module;
  sqlite3 *	conn;
  int		rv;
  vfs_module = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_FALSE(s_vfs_module);
  rv = sqlite3_open_v2(pathname, &conn, flags, vfs_module);
  if (SQLITE_OK == rv) {
    pcb->root0 = &s_conn;
    {
      IK_ASS(IK_FIELD(s_conn, 0), ika_pointer_alloc(pcb, (ik_ulong)conn));
    }
    pcb->root0 = NULL;
    return ika_integer_from_sqlite_errcode(pcb,rv);
  } else {
    /* When  "sqlite3_open_v2()" fails:  it still  may have  allocated a
       data structure and it is our responsibility to release it. */
    if (conn)
      sqlite3_close(conn);
    return ika_integer_from_sqlite_errcode(pcb,rv);
  }
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Configuring connections.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_db_config (ikptr s_conn, ikptr s_option_identifier, ikptr s_args, ikpcb * pcb)
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
	return (fk)? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
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
	return (fk)? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
      } else
	rv = SQLITE_ERROR;
    }
    break;
#endif
  default:
    return IK_FIX(SQLITE_ERROR);
  }
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_limit (ikptr s_conn, ikptr s_limit_identifier, ikptr s_limit_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_LIMIT
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  int		limit_identifier= ik_integer_to_int(s_limit_identifier);
  int		limit_value	= ik_integer_to_int(s_limit_value);
  int		rv;
  rv = sqlite3_limit(conn, limit_identifier, limit_value);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Auxiliary functions.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_busy_handler (ikptr s_conn, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BUSY_HANDLER
  typedef int (*ik_sqlite3_busy_handler_callback) (void*,int);
  sqlite3 *				conn;
  ik_sqlite3_busy_handler_callback	cb;
  int	rv;
  conn = IK_SQLITE_CONNECTION(s_conn);
  cb   = IK_POINTER_FROM_POINTER_OR_FALSE(s_callback);
  rv   = sqlite3_busy_handler(conn, cb, NULL);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_busy_timeout (ikptr s_conn, ikptr s_milliseconds, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BUSY_TIMEOUT
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);
  int		ms   = IK_UNFIX(s_milliseconds);
  int		rv;
  rv = sqlite3_busy_timeout(conn, ms);
  return ika_integer_from_sqlite_errcode(pcb,rv);
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
  cb			= IK_POINTER_FROM_POINTER_OR_FALSE(s_callback);
  sqlite3_progress_handler(conn, instruction_count, cb, NULL);
  return IK_VOID_OBJECT;
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ik_sqlite3_get_autocommit (ikptr s_conn)
{
#ifdef HAVE_SQLITE3_GET_AUTOCOMMIT
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);
  int		rv;
  rv = sqlite3_get_autocommit(conn);
  return (rv)? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_db_filename (ikptr s_conn, ikptr s_database, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DB_FILENAME
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  const char *	database	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_database);
  const char *	rv;
  rv = sqlite3_db_filename(conn, database);
  return (rv)? ika_bytevector_from_cstring(pcb, rv) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_db_filename_from_pointer (ikptr s_conn_pointer, ikptr s_database, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DB_FILENAME
  sqlite3 *	conn		= IK_POINTER_DATA_VOIDP(s_conn_pointer);
  const char *	database	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_database);
  const char *	rv;
  rv = sqlite3_db_filename(conn, database);
  return (rv)? ika_bytevector_from_cstring(pcb, rv) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_db_readonly (ikptr s_conn, ikptr s_database)
{
#ifdef HAVE_SQLITE3_DB_READONLY
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  const char *	database	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_database);
  int		rv;
  rv = sqlite3_db_readonly(conn, database);
  return IK_FIX(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_next_stmt (ikptr s_conn, ikptr s_statement, ikpcb * pcb)
/* Interface to  the C function "sqlite3_next_stmt()".   Return false or
   an  exact integer  representing a  pointer to  the next  statement in
   S_CONN, which must be an instance of "sqlite3".

   If S_STATEMENT  is false: the return  value is false or  a pointer to
   the  first  statement.   Else  S_STATEMENT must  be  an  instance  of
   "sqlite3-stmt"  and the  return value  is false  or an  exact integer
   representing a pointer to the next statement.

   The return value is false if there is no next statement. */
{
#ifdef HAVE_SQLITE3_NEXT_STMT
  sqlite3 *		conn;
  sqlite3_stmt *	statement;
  sqlite3_stmt *	next;
  conn		= IK_SQLITE_CONNECTION(s_conn);
  statement	= (IK_FALSE_OBJECT == s_statement)? NULL : IK_SQLITE_STATEMENT(s_statement);
  next = sqlite3_next_stmt(conn, statement);
  return (next)? ika_integer_from_ulong(pcb, (ik_ulong)next) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Hooks.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_commit_hook (ikptr s_conn, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COMMIT_HOOK
  typedef int (*commit_t) (void*);
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);;
  commit_t	cb   = IK_POINTER_FROM_POINTER_OR_FALSE(s_callback);
  void *	rv;
  rv = sqlite3_commit_hook(conn, cb, NULL);
  return ika_pointer_alloc(pcb, (ik_ulong)rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_rollback_hook (ikptr s_conn, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_ROLLBACK_HOOK
  typedef void (*rollback_t) (void*);
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);;
  rollback_t	cb   = IK_POINTER_FROM_POINTER_OR_FALSE(s_callback);
  void *	rv;
  rv = sqlite3_rollback_hook(conn, cb, NULL);
  return ika_pointer_alloc(pcb, (ik_ulong)rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_update_hook (ikptr s_conn, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_UPDATE_HOOK
  typedef void (*update_t) (void *,int ,char const *,char const *,sqlite3_int64);
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);;
  update_t	cb   = IK_POINTER_FROM_POINTER_OR_FALSE(s_callback);
  void *	rv;
  rv = sqlite3_update_hook(conn, cb, NULL);
  return ika_pointer_alloc(pcb, (ik_ulong)rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Inspection functions.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_table_column_metadata (ikptr s_conn, ikptr s_database_name,
				  ikptr s_table_name, ikptr s_column_name,
				  ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_TABLE_COLUMN_METADATA
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  const char *	database_name	= \
    IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(s_database_name);
  const char *	table_name	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER(s_table_name);
  const char *	column_name	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER(s_column_name);
  char const *	declared_data_type;
  char const *	collation_sequence_name;
  int		not_null_constraint_exists;
  int		column_is_part_of_primary_key;
  int		column_is_auto_increment;
  int		rv;
  rv = sqlite3_table_column_metadata(conn, database_name, table_name, column_name,
				     &declared_data_type,
				     &collation_sequence_name,
				     &not_null_constraint_exists,
				     &column_is_part_of_primary_key,
				     &column_is_auto_increment);
  if (SQLITE_OK == rv) {
    ikptr	s_result = ika_vector_alloc_and_init(pcb, 5);
    pcb->root0 = &s_result;
    {
      IK_ASS(IK_ITEM(s_result, 0), ika_bytevector_from_cstring(pcb, declared_data_type));
      IK_ASS(IK_ITEM(s_result, 1), ika_bytevector_from_cstring(pcb, collation_sequence_name));
      IK_ASS(IK_ITEM(s_result, 2),
	     (not_null_constraint_exists)? IK_TRUE_OBJECT : IK_FALSE_OBJECT);
      IK_ASS(IK_ITEM(s_result, 3),
	     (column_is_part_of_primary_key)? IK_TRUE_OBJECT : IK_FALSE_OBJECT);
      IK_ASS(IK_ITEM(s_result, 4),
	     (column_is_auto_increment)? IK_TRUE_OBJECT : IK_FALSE_OBJECT);
    }
    pcb->root0 = NULL;
    return s_result;
  } else
    return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Encrypted databases.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_key (ikptr s_conn, ikptr s_key, ikptr s_length, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_KEY
  sqlite3 *	conn	= IK_SQLITE_CONNECTION(s_conn);
  int		len;
  void *	ptr	= IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(s_key);
  int		rv;
  if (IK_IS_POINTER(s_key))
    len = ik_integer_to_int(s_length);
  else if (IK_IS_BYTEVECTOR(s_key))
    len = (int)IK_BYTEVECTOR_LENGTH(s_key);
  else if (IK_FALSE == s_key)
    len = 0;
  else /* We assume it is a "memory-block". */
    len = (int)IK_MBLOCK_SIZE_T(s_key);
  rv = sqlite3_key(conn, ptr, (int)len);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_rekey (ikptr s_conn, ikptr s_key, ikptr s_length,ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_REKEY
  sqlite3 *	conn	= IK_SQLITE_CONNECTION(s_conn);
  int		len;
  void *	ptr	= IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(s_key);
  int		rv;
  if (IK_IS_POINTER(s_key))
    len = ik_integer_to_int(s_length);
  else if (IK_IS_BYTEVECTOR(s_key))
    len = (int)IK_BYTEVECTOR_LENGTH(s_key);
  else if (IK_FALSE == s_key)
    len = 0;
  else /* We assume it is a "memory-block". */
    len = (int)IK_MBLOCK_SIZE_T(s_key);
  rv = sqlite3_rekey(conn, ptr, (int)len);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Miscellaneous functions related to connections.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_trace (ikptr s_conn, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_TRACE
  typedef void (*trace_t) (void*, const char*);
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);;
  trace_t	cb   = IK_POINTER_FROM_POINTER_OR_FALSE(s_callback);
  /* Ignore the  return value: undocumented  in the header  file (SQLite
     version 3.7.13), it  is probably the custom data pointer  used in a
     previous call to "sqlite3_trace()". */
  sqlite3_trace(conn, cb, NULL);
  return IK_VOID_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_profile (ikptr s_conn, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_PROFILE
  typedef void (*profile_t) (void*,const char*,sqlite3_uint64);
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);;
  profile_t	cb   = IK_POINTER_FROM_POINTER_OR_FALSE(s_callback);
  /* Ignore the  return value: undocumented  in the header  file (SQLite
     version 3.7.13), it  is probably the custom data pointer  used in a
     previous call to "sqlite3_profile()". */
  sqlite3_profile(conn, cb, NULL);
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_db_release_memory (ikptr s_conn, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DB_RELEASE_MEMORY
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);;
  int		rv;
  rv = sqlite3_db_release_memory(conn);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_set_authorizer (ikptr s_conn, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SET_AUTHORIZER
  typedef int (*authorizer_t) (void*,int,const char*,const char*,const char*,const char*);
  sqlite3 *	conn	= IK_SQLITE_CONNECTION(s_conn);;
  authorizer_t	cb	= IK_POINTER_FROM_POINTER_OR_FALSE(s_callback);
  int		rv;
  rv = sqlite3_set_authorizer(conn, cb, NULL);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_db_status (ikptr s_conn, ikptr s_opcode, ikptr s_reset, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DB_STATUS
  sqlite3 *	conn	= IK_SQLITE_CONNECTION(s_conn);
  int		opcode	= ik_integer_to_int(s_opcode);
  int		reset	= !(IK_FALSE == s_reset);
  int		current;
  int		highwater;
  int		rv;
  rv = sqlite3_db_status(conn, opcode, &current, &highwater, reset);
  if (SQLITE_OK == rv) {
    ikptr	s_vector = ika_vector_alloc_and_init(pcb, 3);
    pcb->root0 = &s_vector;
    {
      IK_ASS(IK_ITEM(s_vector, 0), ika_integer_from_sqlite_errcode(pcb, rv));
      IK_ASS(IK_ITEM(s_vector, 1), ika_integer_from_int(pcb, current));
      IK_ASS(IK_ITEM(s_vector, 2), ika_integer_from_int(pcb, highwater));
    }
    pcb->root0 = NULL;
    return s_vector;
  } else
    return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */


/* end of file */
