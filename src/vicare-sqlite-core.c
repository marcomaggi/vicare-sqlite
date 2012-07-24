/*
  Part of: Vicare/SQLite
  Contents: Vicare backend for SQLite
  Date: Mon Jul 23, 2012

  Abstract

	Core functions.

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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <vicare.h>
#include <sqlite3.h>

/* #define SL_DB_POINTER_OBJECT(PARSER)	(PARSER) */
/* #define SL_DB(PARSER)			\ */
/*   IK_POINTER_DATA_VOIDP(SL_DB_POINTER_OBJECT(PARSER)) */

/* #define SL_CALLBACK(CALLBACK)	IK_POINTER_DATA_VOIDP(CALLBACK) */


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
 ** Compile options.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_compileoption_used (ikptr s_option_name, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COMPILEOPTION_USED
  const char *	option_name = IK_BYTEVECTOR_DATA_CHARP(s_option_name);
  int		rv;
  rv = sqlite3_compileoption_used(option_name);
  return rv? true_object : false_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_compileoption_get (ikptr s_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COMPILEOPTION_GET
  int		idx = ik_integer_to_int(s_index);
  const char *	rv;
  rv = sqlite3_compileoption_get(idx);
  return rv? ika_bytevector_from_cstring(pcb, rv) : false_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_threadsafe (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_THREADSAFE
  return (sqlite3_threadsafe())? true_object : false_object;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Handling connections.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_close (ikptr s_sqlite3, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CLOSE
  ikptr		s_pointer	= IK_FIELD(s_sqlite3, 0);
  sqlite3 *	conn		= IK_POINTER_DATA_VOIDP(s_pointer);
  int		rv;
  rv = sqlite3_close(conn);
  if (SQLITE_OK == rv) {
    IK_POINTER_SET_NULL(s_pointer);
  }
  return IK_FIX(rv);
#else
  feature_failure(__func__);
#endif
}
#if 0
ikptr
ik_sqlite3_exec (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_EXEC
  sqlite3_exec();
#else
  feature_failure(__func__);
#endif
}
#endif


/** --------------------------------------------------------------------
 ** Still to be implemented.
 ** ----------------------------------------------------------------- */

/*
ikptr
ik_sqlite3_initialize (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_INITIALIZE
  sqlite3_initialize();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_shutdown (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SHUTDOWN
  sqlite3_shutdown();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_os_init (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_OS_INIT
  sqlite3_os_init();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_os_end (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_OS_END
  sqlite3_os_end();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_config (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CONFIG
  sqlite3_config();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_db_config (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DB_CONFIG
  sqlite3_db_config();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_extended_result_codes (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_EXTENDED_RESULT_CODES
  sqlite3_extended_result_codes();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_last_insert_rowid (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_LAST_INSERT_ROWID
  sqlite3_last_insert_rowid();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_changes (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CHANGES
  sqlite3_changes();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_total_changes (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_TOTAL_CHANGES
  sqlite3_total_changes();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_interrupt (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_INTERRUPT
  sqlite3_interrupt();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_complete (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COMPLETE
  sqlite3_complete();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_complete16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COMPLETE16
  sqlite3_complete16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_busy_handler (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BUSY_HANDLER
  sqlite3_busy_handler();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_busy_timeout (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BUSY_TIMEOUT
  sqlite3_busy_timeout();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_get_table (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_GET_TABLE
  sqlite3_get_table();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_free_table (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_FREE_TABLE
  sqlite3_free_table();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_memory_used (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_MEMORY_USED
  sqlite3_memory_used();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_memory_highwater (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_MEMORY_HIGHWATER
  sqlite3_memory_highwater();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_randomness (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RANDOMNESS
  sqlite3_randomness();
#else
  feature_failure(__func__);
#endif
}
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
ik_sqlite3_trace (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_TRACE
  sqlite3_trace();
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
ik_sqlite3_progress_handler (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_PROGRESS_HANDLER
  sqlite3_progress_handler();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_open (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_OPEN
  sqlite3_open();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_open16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_OPEN16
  sqlite3_open16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_open_v2 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_OPEN_V2
  sqlite3_open_v2();
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
ik_sqlite3_errcode (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_ERRCODE
  sqlite3_errcode();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_extended_errcode (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_EXTENDED_ERRCODE
  sqlite3_extended_errcode();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_errmsg (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_ERRMSG
  sqlite3_errmsg();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_errmsg16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_ERRMSG16
  sqlite3_errmsg16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_limit (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_LIMIT
  sqlite3_limit();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_prepare (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_PREPARE
  sqlite3_prepare();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_prepare_v2 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_PREPARE_V2
  sqlite3_prepare_v2();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_prepare16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_PREPARE16
  sqlite3_prepare16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_prepare16_v2 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_PREPARE16_V2
  sqlite3_prepare16_v2();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_sql (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SQL
  sqlite3_sql();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_stmt_readonly (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_STMT_READONLY
  sqlite3_stmt_readonly();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_stmt_busy (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_STMT_BUSY
  sqlite3_stmt_busy();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_blob (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_BLOB
  sqlite3_bind_blob();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_double (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_DOUBLE
  sqlite3_bind_double();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_int (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_INT
  sqlite3_bind_int();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_int64 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_INT64
  sqlite3_bind_int64();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_null (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_NULL
  sqlite3_bind_null();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_text (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_TEXT
  sqlite3_bind_text();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_text16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_TEXT16
  sqlite3_bind_text16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_value (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_VALUE
  sqlite3_bind_value();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_zeroblob (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_ZEROBLOB
  sqlite3_bind_zeroblob();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_parameter_count (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_PARAMETER_COUNT
  sqlite3_bind_parameter_count();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_parameter_name (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_PARAMETER_NAME
  sqlite3_bind_parameter_name();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_parameter_index (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_PARAMETER_INDEX
  sqlite3_bind_parameter_index();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_clear_bindings (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CLEAR_BINDINGS
  sqlite3_clear_bindings();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_count (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_COUNT
  sqlite3_column_count();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_name (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_NAME
  sqlite3_column_name();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_name16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_NAME16
  sqlite3_column_name16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_database_name (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_DATABASE_NAME
  sqlite3_column_database_name();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_database_name16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_DATABASE_NAME16
  sqlite3_column_database_name16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_table_name (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_TABLE_NAME
  sqlite3_column_table_name();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_table_name16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_TABLE_NAME16
  sqlite3_column_table_name16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_origin_name (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_ORIGIN_NAME
  sqlite3_column_origin_name();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_origin_name16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_ORIGIN_NAME16
  sqlite3_column_origin_name16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_decltype (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_DECLTYPE
  sqlite3_column_decltype();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_decltype16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_DECLTYPE16
  sqlite3_column_decltype16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_step (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_STEP
  sqlite3_step();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_data_count (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DATA_COUNT
  sqlite3_data_count();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_blob (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_BLOB
  sqlite3_column_blob();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_bytes (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_BYTES
  sqlite3_column_bytes();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_bytes16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_BYTES16
  sqlite3_column_bytes16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_double (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_DOUBLE
  sqlite3_column_double();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_int (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_INT
  sqlite3_column_int();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_int64 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_INT64
  sqlite3_column_int64();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_text (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_TEXT
  sqlite3_column_text();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_text16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_TEXT16
  sqlite3_column_text16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_type (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_TYPE
  sqlite3_column_type();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_value (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_VALUE
  sqlite3_column_value();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_finalize (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_FINALIZE
  sqlite3_finalize();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_reset (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESET
  sqlite3_reset();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_create_function (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_FUNCTION
  sqlite3_create_function();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_create_function16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_FUNCTION16
  sqlite3_create_function16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_create_function_v2 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_FUNCTION_V2
  sqlite3_create_function_v2();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_blob (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_BLOB
  sqlite3_value_blob();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_bytes (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_BYTES
  sqlite3_value_bytes();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_bytes16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_BYTES16
  sqlite3_value_bytes16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_double (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_DOUBLE
  sqlite3_value_double();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_int (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_INT
  sqlite3_value_int();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_int64 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_INT64
  sqlite3_value_int64();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_text (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_TEXT
  sqlite3_value_text();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_text16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_TEXT16
  sqlite3_value_text16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_text16le (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_TEXT16LE
  sqlite3_value_text16le();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_text16be (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_TEXT16BE
  sqlite3_value_text16be();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_type (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_TYPE
  sqlite3_value_type();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_numeric_type (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_NUMERIC_TYPE
  sqlite3_value_numeric_type();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_aggregate_context (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_AGGREGATE_CONTEXT
  sqlite3_aggregate_context();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_user_data (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_USER_DATA
  sqlite3_user_data();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_context_db_handle (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CONTEXT_DB_HANDLE
  sqlite3_context_db_handle();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_get_auxdata (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_GET_AUXDATA
  sqlite3_get_auxdata();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_set_auxdata (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SET_AUXDATA
  sqlite3_set_auxdata();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_blob (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_BLOB
  sqlite3_result_blob();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_double (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_DOUBLE
  sqlite3_result_double();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_error (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_ERROR
  sqlite3_result_error();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_error16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_ERROR16
  sqlite3_result_error16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_error_toobig (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_ERROR_TOOBIG
  sqlite3_result_error_toobig();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_error_nomem (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_ERROR_NOMEM
  sqlite3_result_error_nomem();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_error_code (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_ERROR_CODE
  sqlite3_result_error_code();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_int (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_INT
  sqlite3_result_int();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_int64 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_INT64
  sqlite3_result_int64();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_null (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_NULL
  sqlite3_result_null();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_text (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_TEXT
  sqlite3_result_text();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_text16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_TEXT16
  sqlite3_result_text16();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_text16le (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_TEXT16LE
  sqlite3_result_text16le();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_text16be (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_TEXT16BE
  sqlite3_result_text16be();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_value (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_VALUE
  sqlite3_result_value();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_zeroblob (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_ZEROBLOB
  sqlite3_result_zeroblob();
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
ik_sqlite3_sleep (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SLEEP
  sqlite3_sleep();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_get_autocommit (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_GET_AUTOCOMMIT
  sqlite3_get_autocommit();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_db_handle (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DB_HANDLE
  sqlite3_db_handle();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_db_filename (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DB_FILENAME
  sqlite3_db_filename();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_db_readonly (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DB_READONLY
  sqlite3_db_readonly();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_next_stmt (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_NEXT_STMT
  sqlite3_next_stmt();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_commit_hook (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COMMIT_HOOK
  sqlite3_commit_hook();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_rollback_hook (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_ROLLBACK_HOOK
  sqlite3_rollback_hook();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_update_hook (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_UPDATE_HOOK
  sqlite3_update_hook();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_enable_shared_cache (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_ENABLE_SHARED_CACHE
  sqlite3_enable_shared_cache();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_release_memory (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RELEASE_MEMORY
  sqlite3_release_memory();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_db_release_memory (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DB_RELEASE_MEMORY
  sqlite3_db_release_memory();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_soft_heap_limit64 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SOFT_HEAP_LIMIT64
  sqlite3_soft_heap_limit64();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_soft_heap_limit (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SOFT_HEAP_LIMIT
  sqlite3_soft_heap_limit();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_table_column_metadata (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_TABLE_COLUMN_METADATA
  sqlite3_table_column_metadata();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_load_extension (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_LOAD_EXTENSION
  sqlite3_load_extension();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_enable_load_extension (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_ENABLE_LOAD_EXTENSION
  sqlite3_enable_load_extension();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_auto_extension (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_AUTO_EXTENSION
  sqlite3_auto_extension();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_reset_auto_extension (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESET_AUTO_EXTENSION
  sqlite3_reset_auto_extension();
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
ik_sqlite3_blob_open (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BLOB_OPEN
  sqlite3_blob_open();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_blob_reopen (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BLOB_REOPEN
  sqlite3_blob_reopen();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_blob_close (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BLOB_CLOSE
  sqlite3_blob_close();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_blob_bytes (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BLOB_BYTES
  sqlite3_blob_bytes();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_blob_read (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BLOB_READ
  sqlite3_blob_read();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_blob_write (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BLOB_WRITE
  sqlite3_blob_write();
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
ik_sqlite3_log (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_LOG
  sqlite3_log();
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
*/


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

/* end of file */
