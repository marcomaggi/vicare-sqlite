/*
  Part of: Vicare/SQLite
  Contents: prepared SQL statements
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
 ** SQL prepared statements: finalisation.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_finalize (ikptr s_statement, ikpcb * pcb)
/* Interface  to  the  C   function  "sqlite3_finalize()".   Finalise  a
   prepared statement; return a SQLITE_ error code.

   S_STATEMENT must be an instance of "sqlite3-stmt".

   The pointer object referencing the statement  is reset to NULL, if it
   is not NULL already.  If is safe to call this function multiple times
   for the same instance of "sqlite3-stmt". */
{
#ifdef HAVE_SQLITE3_FINALIZE
  ikptr		s_pointer = IK_SQLITE_STMT_POINTER(s_statement);
  if (IK_FALSE_OBJECT == s_pointer) {
    /* The pointer field can be false  if the data structure was created
       but then the initialisation of the statement failed. */
    return SQLITE_OK;
  } else {
    sqlite3_stmt *statement = IK_POINTER_DATA_VOIDP(s_pointer);
    if (NULL == statement)
      return ika_integer_from_sqlite_errcode(pcb,SQLITE_OK);
    else {
      int	rv;
      ikptr	sk;
      sk = ik_enter_c_function(pcb);
      {
	rv = sqlite3_finalize(statement);
      }
      ik_leave_c_function(pcb, sk);
      /* Reset  the reference  to  the connection  in  terms of  "sqlite3"
	 instance. */
      IK_SQLITE_STMT_CONNECTION(s_statement) = IK_FALSE_OBJECT;
      /* Reset the  pointer to SQLite structure  to NULL, so that  we know
	 that this instance has been finalised already. */
      IK_POINTER_SET_NULL(s_pointer);
      return ika_integer_from_sqlite_errcode(pcb,rv);
    }
  }
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SQL prepared statements: preparing precompiled statements.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_prepare (ikptr s_conn, ikptr s_sql_snippet, ikptr s_sql_offset,
		    ikptr s_statement, ikptr s_store_sql_text,
		    ikpcb * pcb)
/* Interface  to   the  C   function  "sqlite3_prepare()".    Prepare  a
   precompiled SQL statement in the context of a database connection; if
   successful return a pair whose car  is SQLITE_OK and whose cdr is the
   number  of bytes  used  from the  input code,  else  return a  fixnum
   representing a SQLITE_ error code.

   S_CONN must be an instance of "sqlite3".

   S_SQL_SNIPPET must be a UTF-8 bytevector holding SQL code.

   S_SQL_OFFSET  must be  an  offset into  S_SQL_SNIPPET specifying  the
   start of the SQL statement.

   S_STATEMENT must be  an instance of "sqlite3-stmt",  whose fields are
   mutated to reference the results of this function call.

   S_STORE_SQL_TEXT  is interpreted  as boolean:  if true,  the SQL-CODE
   field of  the statement structure  is filled with a  UTF-8 bytevector
   holding the SQL code; else such field is set to false. */
{
#ifdef HAVE_SQLITE3_PREPARE
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  const char *	sql_snippet	= IK_BYTEVECTOR_DATA_CHARP(s_sql_snippet);
  int		sql_offset	= ik_integer_to_int(s_sql_offset);
  int		sql_length	= IK_BYTEVECTOR_LENGTH(s_sql_snippet) - sql_offset;
  sqlite3_stmt *statement;
  const char *	sql_unused;
  int		rv;
  ikptr		sk;
  sql_snippet += sql_offset;
  sk = ik_enter_c_function(pcb);
  {
    rv = sqlite3_prepare(conn, sql_snippet, sql_length, &statement, &sql_unused);
  }
  ik_leave_c_function(pcb, sk);
  if (SQLITE_OK == rv) {
    long	used_length	= (long)(sql_unused - sql_snippet);
    ikptr	s_pair		= ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_sqlite_errcode(pcb,rv));
      IK_ASS(IK_CDR(s_pair), ika_integer_from_long(pcb, used_length));
      IK_ASS(IK_SQLITE_STMT_POINTER(s_statement),
	     ika_pointer_alloc(pcb, (ik_ulong)statement));
      /* If  requested:  store  the  SQL  statement  code  in  the  data
	 structure. */
      if (IK_FALSE_OBJECT == s_store_sql_text) {
	IK_SQLITE_STMT_SQLBV(s_statement) = IK_FALSE_OBJECT;
      } else {
	IK_ASS(IK_SQLITE_STMT_SQLBV(s_statement),
	       ika_bytevector_from_cstring_len(pcb, sql_snippet, used_length));
      }
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_prepare_v2 (ikptr s_conn, ikptr s_sql_snippet, ikptr s_sql_offset,
		       ikptr s_statement, ikptr s_store_sql_text,
		       ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_PREPARE_V2
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  const char *	sql_snippet	= IK_BYTEVECTOR_DATA_CHARP(s_sql_snippet);
  int		sql_offset	= ik_integer_to_int(s_sql_offset);
  int		sql_length	= IK_BYTEVECTOR_LENGTH(s_sql_snippet) - sql_offset;
  sqlite3_stmt *statement;
  const char *	sql_unused;
  int		rv;
  ikptr		sk;
  sql_snippet += sql_offset;
  sk = ik_enter_c_function(pcb);
  {
    rv = sqlite3_prepare_v2(conn, sql_snippet, sql_length, &statement, &sql_unused);
  }
  ik_leave_c_function(pcb, sk);
  if (SQLITE_OK == rv) {
    long	used_length	= (long)(sql_unused - sql_snippet);
    ikptr	s_pair		= ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_sqlite_errcode(pcb,rv));
      IK_ASS(IK_CDR(s_pair), ika_integer_from_long(pcb, used_length));
      IK_ASS(IK_SQLITE_STMT_POINTER(s_statement),
	     ika_pointer_alloc(pcb, (ik_ulong)statement));
      /* If  requested:  store  the  SQL  statement  code  in  the  data
	 structure. */
      if (IK_FALSE_OBJECT == s_store_sql_text) {
	IK_SQLITE_STMT_SQLBV(s_statement) = IK_FALSE_OBJECT;
      } else {
	IK_ASS(IK_SQLITE_STMT_SQLBV(s_statement),
	       ika_bytevector_from_cstring_len(pcb, sql_snippet, used_length));
      }
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_prepare16 (ikptr s_conn, ikptr s_sql_snippet, ikptr s_sql_offset,
		      ikptr s_statement, ikptr s_store_sql_text,
		      ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_PREPARE16
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  void *	sql_snippet	= IK_BYTEVECTOR_DATA_VOIDP(s_sql_snippet);
  int		sql_offset	= ik_integer_to_int(s_sql_offset);
  int		sql_length	= IK_BYTEVECTOR_LENGTH(s_sql_snippet) - sql_offset;
  sqlite3_stmt *statement;
  void *	sql_unused;
  int		rv;
  ikptr		sk;
  sql_snippet += sql_offset;
  sk = ik_enter_c_function(pcb);
  {
    rv = sqlite3_prepare16(conn, sql_snippet, sql_length, &statement,
			   (void *)&sql_unused);
  }
  ik_leave_c_function(pcb, sk);
  if (SQLITE_OK == rv) {
    long	used_length	= (long)(((uint8_t *)sql_unused)-((uint8_t *)sql_snippet));
    ikptr	s_pair		= ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_sqlite_errcode(pcb,rv));
      IK_ASS(IK_CDR(s_pair), ika_integer_from_long(pcb, used_length));
      IK_ASS(IK_SQLITE_STMT_POINTER(s_statement),
	     ika_pointer_alloc(pcb, (ik_ulong)statement));
      /* If  requested:  store  the  SQL  statement  code  in  the  data
	 structure. */
      if (IK_FALSE_OBJECT == s_store_sql_text) {
	IK_SQLITE_STMT_SQLBV(s_statement) = IK_FALSE_OBJECT;
      } else {
	IK_ASS(IK_SQLITE_STMT_SQLBV(s_statement),
	       ika_bytevector_from_memory_block(pcb, sql_snippet, used_length));
      }
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_prepare16_v2 (ikptr s_conn, ikptr s_sql_snippet, ikptr s_sql_offset,
			 ikptr s_statement, ikptr s_store_sql_text,
			 ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_PREPARE16_V2
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  void *	sql_snippet	= IK_BYTEVECTOR_DATA_VOIDP(s_sql_snippet);
  int		sql_offset	= ik_integer_to_int(s_sql_offset);
  int		sql_length	= IK_BYTEVECTOR_LENGTH(s_sql_snippet) - sql_offset;
  sqlite3_stmt *statement;
  void *	sql_unused;
  int		rv;
  ikptr		sk;
  sql_snippet += sql_offset;
  sk = ik_enter_c_function(pcb);
  {
    rv = sqlite3_prepare16_v2(conn, sql_snippet, sql_length, &statement,
			      (void *)&sql_unused);
  }
  ik_leave_c_function(pcb, sk);
  if (SQLITE_OK == rv) {
    long	used_length	= (long)(((uint8_t *)sql_unused) - ((uint8_t *)sql_snippet));
    ikptr	s_pair		= ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_sqlite_errcode(pcb,rv));
      IK_ASS(IK_CDR(s_pair), ika_integer_from_long(pcb, used_length));
      IK_ASS(IK_SQLITE_STMT_POINTER(s_statement),
	     ika_pointer_alloc(pcb, (ik_ulong)statement));
      /* If  requested:  store  the  SQL  statement  code  in  the  data
	 structure. */
      if (IK_FALSE_OBJECT == s_store_sql_text) {
	IK_SQLITE_STMT_SQLBV(s_statement) = IK_FALSE_OBJECT;
      } else {
	IK_ASS(IK_SQLITE_STMT_SQLBV(s_statement),
	       ika_bytevector_from_memory_block(pcb, sql_snippet, used_length));
      }
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SQL prepared statements: executing code.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_step (ikptr s_statement, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_STEP
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			rv;
  ikptr			sk;
  sk = ik_enter_c_function(pcb);
  {
    rv = sqlite3_step(statement);
  }
  ik_leave_c_function(pcb, sk);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SQL prepared statements: auxiliary functions.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_sql (ikptr s_statement, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SQL
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  const char *		rv;
  rv = sqlite3_sql(statement);
  return ika_bytevector_from_cstring(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_stmt_readonly (ikptr s_statement)
{
#ifdef HAVE_SQLITE3_STMT_READONLY
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			rv;
  rv = sqlite3_stmt_readonly(statement);
  return (rv)? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_stmt_busy (ikptr s_statement)
{
#ifdef HAVE_SQLITE3_STMT_BUSY
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			rv;
  rv = sqlite3_stmt_busy(statement);
  return (rv)? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SQL prepared statements: binding parameters.
 ** ----------------------------------------------------------------- */

typedef void (*ik_sqlite_destructor) (void*);

ikptr
ik_sqlite3_bind_blob (ikptr s_statement, ikptr s_parameter_index,
		      ikptr s_blob_data, ikptr s_blob_start, ikptr s_blob_length,
		      ikptr s_blob_destructor, ikpcb * pcb)
/* Interface to the C  function "sqlite3_bind_blob()".  Bind a statement
   parameter to the supplied blob;  if successful return SQLITE_OK, else
   return a SQLITE_ error code.

   S_STATEMENT must be an instance of "sqlite3-stmt".

   S_PARAMETER_INDEX  must be  a  fixnum selecting  a  parameter in  the
   statement.

   S_BLOB_DATA must represent the blob's data: it can be a bytevector or
   a pointer object.

   S_BLOB_START must  be a  fixnum representing the  offset in  the blob
   data of the first byte to be used as parameter value.

   S_BLOB_LENGTH must be a fixnum  representing the number of bytes from
   the start in the blob data to be used as parameter value.

   S_BLOB_DESTRUCTOR must  be a pointer object  referencing a destructor
   function  for   the  blob;  special  values   are  SQLITE_STATIC  and
   SQLITE_TRANSIENT. */
{
#ifdef HAVE_SQLITE3_BIND_BLOB
  sqlite3_stmt *	statement;
  int			parameter_index;
  void *		data_ptr;
  int			data_start, data_length;
  ik_sqlite_destructor	blob_destructor;
  int			rv;
  statement		= IK_SQLITE_STATEMENT(s_statement);
  parameter_index	= IK_UNFIX(s_parameter_index);
  blob_destructor	= IK_POINTER_DATA_VOIDP(s_blob_destructor);
  data_start	= ik_integer_to_int(s_blob_start);
  data_length	= ik_integer_to_int(s_blob_length);
  data_ptr = data_start + ((IK_IS_BYTEVECTOR(s_blob_data))?	\
	      IK_BYTEVECTOR_DATA_VOIDP(s_blob_data) : IK_POINTER_DATA_VOIDP(s_blob_data));
  /* fprintf(stderr, "%s: parameter index %d\n", __func__, parameter_index); */
  rv = sqlite3_bind_blob(statement, parameter_index, data_ptr, data_length, blob_destructor);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_double (ikptr s_statement, ikptr s_parameter_index, ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_DOUBLE
  sqlite3_stmt *statement	= IK_SQLITE_STATEMENT(s_statement);
  int		parameter_index	= IK_UNFIX(s_parameter_index);
  double	value		= IK_FLONUM_DATA(s_value);
  int		rv;
  rv = sqlite3_bind_double(statement, parameter_index, value);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_int (ikptr s_statement, ikptr s_parameter_index, ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_INT
  sqlite3_stmt *statement	= IK_SQLITE_STATEMENT(s_statement);
  int		parameter_index	= IK_UNFIX(s_parameter_index);
  int		value		= ik_integer_to_int(s_value);
  int		rv;
  rv = sqlite3_bind_int(statement, parameter_index, value);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}

ikptr
ik_sqlite3_bind_int64 (ikptr s_statement, ikptr s_parameter_index, ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_INT64
  sqlite3_stmt *statement	= IK_SQLITE_STATEMENT(s_statement);
  int		parameter_index	= IK_UNFIX(s_parameter_index);
  sqlite_int64	value		= ik_integer_to_sint64(s_value);
  int		rv;
  rv = sqlite3_bind_int64(statement, parameter_index, value);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_null (ikptr s_statement, ikptr s_parameter_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_NULL
  sqlite3_stmt *statement	= IK_SQLITE_STATEMENT(s_statement);
  int		parameter_index	= IK_UNFIX(s_parameter_index);
  int		rv;
  rv = sqlite3_bind_null(statement, parameter_index);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_text (ikptr s_statement, ikptr s_parameter_index,
		      ikptr s_text_data, ikptr s_text_start, ikptr s_text_length,
		      ikptr s_text_destructor, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_TEXT
  sqlite3_stmt *	statement;
  int			parameter_index;
  void *		data_ptr;
  int			data_start, data_length;
  ik_sqlite_destructor	text_destructor;
  int			rv;
  statement		= IK_SQLITE_STATEMENT(s_statement);
  parameter_index	= IK_UNFIX(s_parameter_index);
  text_destructor	= IK_POINTER_DATA_VOIDP(s_text_destructor);
  data_start		= ik_integer_to_int(s_text_start);
  data_length		= ik_integer_to_int(s_text_length);
  data_ptr		= data_start + IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER(s_text_data);
  rv = sqlite3_bind_text(statement, parameter_index, data_ptr, data_length, text_destructor);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_text16 (ikptr s_statement, ikptr s_parameter_index,
			ikptr s_text_data, ikptr s_text_start, ikptr s_text_length,
			ikptr s_text_destructor, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_TEXT16
  sqlite3_stmt *	statement;
  int			parameter_index;
  void *		data_ptr;
  int			data_start, data_length;
  ik_sqlite_destructor	text_destructor;
  int			rv;
  statement		= IK_SQLITE_STATEMENT(s_statement);
  parameter_index	= IK_UNFIX(s_parameter_index);
  text_destructor	= IK_POINTER_DATA_VOIDP(s_text_destructor);
  data_start		= ik_integer_to_int(s_text_start);
  data_length		= ik_integer_to_int(s_text_length);
  data_ptr		= data_start + IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER(s_text_data);
  rv = sqlite3_bind_text16(statement, parameter_index, data_ptr, data_length, text_destructor);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_value (ikptr s_statement, ikptr s_parameter_index, ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_VALUE
  sqlite3_stmt *	statement	= IK_SQLITE_STATEMENT(s_statement);
  int			parameter_index	= IK_UNFIX(s_parameter_index);
  sqlite3_value *	value		= IK_POINTER_DATA_VOIDP(s_value);
  int			rv;
  rv = sqlite3_bind_value(statement, parameter_index, value);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_zeroblob (ikptr s_statement, ikptr s_parameter_index, ikptr s_blob_length,
			  ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_ZEROBLOB
  sqlite3_stmt *statement	= IK_SQLITE_STATEMENT(s_statement);
  int		parameter_index	= IK_UNFIX(s_parameter_index);
  int		blob_length	= ik_integer_to_int(s_blob_length);
  int		rv;
  rv = sqlite3_bind_zeroblob(statement, parameter_index, blob_length);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_parameter_count (ikptr s_statement, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_PARAMETER_COUNT
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			rv;
  rv = sqlite3_bind_parameter_count(statement);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_parameter_name (ikptr s_statement, ikptr s_parameter_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_PARAMETER_NAME
  sqlite3_stmt *statement = IK_SQLITE_STATEMENT(s_statement);
  int		parameter_index = IK_UNFIX(s_parameter_index);
  const char *	rv;
  rv = sqlite3_bind_parameter_name(statement, parameter_index);
  return (rv)? ika_bytevector_from_cstring(pcb, rv) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_bind_parameter_index (ikptr s_statement, ikptr s_parameter_name, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BIND_PARAMETER_INDEX
  sqlite3_stmt *statement	= IK_SQLITE_STATEMENT(s_statement);
  const char *	parameter_name	= IK_BYTEVECTOR_DATA_VOIDP(s_parameter_name);
  int		rv;
  rv = sqlite3_bind_parameter_index(statement, parameter_name);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}

ikptr
ik_sqlite3_clear_bindings (ikptr s_statement, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CLEAR_BINDINGS
  sqlite3_stmt *statement	= IK_SQLITE_STATEMENT(s_statement);
  int		rv;
  rv = sqlite3_clear_bindings(statement);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_reset (ikptr s_statement, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESET
  sqlite3_stmt *statement	= IK_SQLITE_STATEMENT(s_statement);
  int		rv;
  ikptr		sk;
  sk = ik_enter_c_function(pcb);
  {
    rv = sqlite3_reset(statement);
  }
  ik_leave_c_function(pcb, sk);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SQL prepared statements: result row inspection.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_column_count (ikptr s_statement, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_COUNT
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			rv;
  rv = sqlite3_column_count(statement);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ik_sqlite3_column_name (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_NAME
  sqlite3_stmt *statement    = IK_SQLITE_STATEMENT(s_statement);
  int		column_index = ik_integer_to_int(s_column_index);
  const char *	column_name;
  column_name = sqlite3_column_name(statement, column_index);
  return (column_name)? ika_bytevector_from_cstring(pcb, column_name) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_name16 (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_NAME16
  sqlite3_stmt *	statement    = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  const uint8_t *	column_name;
  column_name = sqlite3_column_name16(statement, column_index);
  return (column_name)? ik_bytevector_from_utf16z(pcb, column_name) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ik_sqlite3_column_database_name (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_DATABASE_NAME
  sqlite3_stmt *statement    = IK_SQLITE_STATEMENT(s_statement);
  int		column_index = ik_integer_to_int(s_column_index);
  const char *	database_name;
  database_name = sqlite3_column_database_name(statement, column_index);
  return (database_name)? ika_bytevector_from_cstring(pcb, database_name) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_database_name16 (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_DATABASE_NAME16
  sqlite3_stmt *	statement    = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  const uint8_t *	database_name;
  database_name = sqlite3_column_database_name16(statement, column_index);
  return (database_name)? ik_bytevector_from_utf16z(pcb, database_name) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ik_sqlite3_column_table_name (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_TABLE_NAME
  sqlite3_stmt *statement    = IK_SQLITE_STATEMENT(s_statement);
  int		column_index = ik_integer_to_int(s_column_index);
  const char *	table_name;
  table_name = sqlite3_column_table_name(statement, column_index);
  return (table_name)? ika_bytevector_from_cstring(pcb, table_name) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_table_name16 (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_TABLE_NAME16
  sqlite3_stmt *	statement    = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  const uint8_t *	table_name;
  table_name = sqlite3_column_table_name16(statement, column_index);
  return (table_name)? ik_bytevector_from_utf16z(pcb, table_name) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ik_sqlite3_column_origin_name (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_ORIGIN_NAME
  sqlite3_stmt *	statement    = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  const char *		origin_name;
  origin_name = sqlite3_column_origin_name(statement, column_index);
  return (origin_name)? ika_bytevector_from_cstring(pcb, origin_name) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_origin_name16 (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_ORIGIN_NAME16
  sqlite3_stmt *	statement    = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  const uint8_t *	origin_name;
  origin_name = sqlite3_column_origin_name16(statement, column_index);
  return (origin_name)? ik_bytevector_from_utf16z(pcb, origin_name) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ik_sqlite3_column_decltype (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_DECLTYPE
  sqlite3_stmt *statement    = IK_SQLITE_STATEMENT(s_statement);
  int		column_index = ik_integer_to_int(s_column_index);
  const char *	type_name;
  type_name = sqlite3_column_decltype(statement, column_index);
  return (type_name)? ika_bytevector_from_cstring(pcb, type_name) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_decltype16 (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_DECLTYPE16
  sqlite3_stmt *	statement    = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  const uint8_t *	type_name;
  type_name = sqlite3_column_decltype16(statement, column_index);
  return (type_name)? ik_bytevector_from_utf16z(pcb, type_name) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ik_sqlite3_data_count (ikptr s_statement, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DATA_COUNT
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			rv;
  rv = sqlite3_data_count(statement);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_type (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_TYPE
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  int			rv;
  rv = sqlite3_column_type(statement, column_index);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ik_sqlite3_column_blob (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_BLOB
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  const void *		ptr;
  int			len;
  ptr = sqlite3_column_blob(statement, column_index);
  if (ptr) {
    len = sqlite3_column_bytes(statement, column_index);
    return ika_bytevector_from_memory_block(pcb, ptr, len);
  } else
    return IK_FALSE_OBJECT; /* zero-length blob*/
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_bytes (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_BYTES
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  int			rv;
  rv = sqlite3_column_bytes(statement, column_index);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_bytes16 (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_BYTES16
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  int			rv;
  rv = sqlite3_column_bytes16(statement, column_index);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_double (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_DOUBLE
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  double		rv;
  rv = sqlite3_column_double(statement, column_index);
  return ika_flonum_from_double(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_int (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_INT
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  int			rv;
  rv = sqlite3_column_int(statement, column_index);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_int64 (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_INT64
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  sqlite3_int64		rv;
  rv = sqlite3_column_int64(statement, column_index);
  return ika_integer_from_sint64(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_text (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_TEXT
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  const char *		ptr;
  int			len;
  ptr = (const char *)sqlite3_column_text(statement, column_index);
  if (ptr) {
    len = sqlite3_column_bytes(statement, column_index);
    return ika_bytevector_from_cstring_len(pcb, ptr, len);
  } else
    return IK_FALSE_OBJECT; /* zero-length blob*/
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_text16 (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_TEXT16
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  const void *		ptr;
  int			len;
  ptr = sqlite3_column_text16(statement, column_index);
  if (ptr) {
    len = sqlite3_column_bytes16(statement, column_index);
    return ika_bytevector_from_memory_block(pcb, ptr, len);
  } else
    return IK_FALSE_OBJECT; /* zero-length blob*/
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_column_value (ikptr s_statement, ikptr s_column_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLUMN_VALUE
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  int			column_index = ik_integer_to_int(s_column_index);
  sqlite3_value *	rv;
  rv = sqlite3_column_value(statement, column_index);
  return ika_pointer_alloc(pcb, (ik_ulong)rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SQL prepared statements: inspection functions.
 ** ----------------------------------------------------------------- */

#if 0
ikptr
ik_sqlite3_db_handle (ikptr s_statement, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_DB_HANDLE
  sqlite3_stmt *	statement = IK_SQLITE_STATEMENT(s_statement);
  sqlite3 *		conn;
  conn = sqlite3_db_handle(statement);
  return ika_pointer_alloc(pcb, (ik_ulong)conn);
#else
  feature_failure(__func__);
#endif
}
#endif


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */


/* end of file */
