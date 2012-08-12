/*
  Part of: Vicare/SQLite
  Contents: extending SQL with custom functions
  Date: Fri Aug 10, 2012

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
 ** Creating functions.
 ** ----------------------------------------------------------------- */

typedef void (*xFunc) (sqlite3_context*,int,sqlite3_value**);
typedef void (*xStep) (sqlite3_context*,int,sqlite3_value**);
typedef void (*xFinal) (sqlite3_context*);
typedef void (*xDestroy) (void*);

ikptr
ik_sqlite3_create_function (ikptr s_conn, ikptr s_function_name, ikptr s_arity,
			    ikptr s_text_encoding, ikptr s_func, ikptr s_step, ikptr s_final,
			    ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_FUNCTION
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  const char *	function_name	= IK_BYTEVECTOR_DATA_CHARP(s_function_name);
  int		arity		= ik_integer_to_int(s_arity);
  int		text_encoding	= IK_UNFIX(s_text_encoding);
  void *	custom_data	= NULL;
  xFunc		func		= IK_SQLITE_CALLBACK_OR_NULL(s_func);
  xStep		step		= IK_SQLITE_CALLBACK_OR_NULL(s_step);
  xFinal	final		= IK_SQLITE_CALLBACK_OR_NULL(s_final);
  int		rv;
  rv = sqlite3_create_function(conn, function_name, arity, text_encoding, custom_data,
			       func, step, final);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_create_function16 (ikptr s_conn, ikptr s_function_name, ikptr s_arity,
			      ikptr s_text_encoding, ikptr s_func, ikptr s_step, ikptr s_final,
			      ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_FUNCTION16
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  const char *	function_name	= IK_BYTEVECTOR_DATA_CHARP(s_function_name);
  int		arity		= ik_integer_to_int(s_arity);
  int		text_encoding	= IK_UNFIX(s_text_encoding);
  void *	custom_data	= NULL;
  xFunc		func		= IK_SQLITE_CALLBACK_OR_NULL(s_func);
  xStep		step		= IK_SQLITE_CALLBACK_OR_NULL(s_step);
  xFinal	final		= IK_SQLITE_CALLBACK_OR_NULL(s_final);
  int		rv;
  rv = sqlite3_create_function16(conn, function_name, arity, text_encoding, custom_data,
				 func, step, final);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_create_function_v2 (ikptr s_conn, ikptr s_function_name, ikptr s_arity,
			       ikptr s_text_encoding,
			       ikptr s_func, ikptr s_step, ikptr s_final,
			       ikptr s_destroy, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_FUNCTION_V2
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  const char *	function_name	= IK_BYTEVECTOR_DATA_CHARP(s_function_name);
  int		arity		= ik_integer_to_int(s_arity);
  int		text_encoding	= IK_UNFIX(s_text_encoding);
  void *	custom_data	= NULL;
  xFunc		func		= IK_SQLITE_CALLBACK_OR_NULL(s_func);
  xStep		step		= IK_SQLITE_CALLBACK_OR_NULL(s_step);
  xFinal	final		= IK_SQLITE_CALLBACK_OR_NULL(s_final);
  xDestroy	destroy		= IK_SQLITE_CALLBACK_OR_NULL(s_destroy);
  int		rv;
  rv = sqlite3_create_function_v2(conn, function_name, arity, text_encoding, custom_data,
				  func, step, final, destroy);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** SQL arguments to Scheme values.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_value_blob (ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_BLOB
  sqlite3_value *	value = IK_POINTER_DATA_VOIDP(s_value);
  const void *		data;
  int			bytes;
  bytes = sqlite3_value_bytes(value);
  data  = sqlite3_value_blob(value);
  return ika_bytevector_from_memory_block(pcb, data, bytes);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_bytes (ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_BYTES
  sqlite3_value *	value = IK_POINTER_DATA_VOIDP(s_value);
  int			rv;
  rv = sqlite3_value_bytes(value);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_bytes16 (ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_BYTES16
  sqlite3_value *	value = IK_POINTER_DATA_VOIDP(s_value);
  int			rv;
  rv = sqlite3_value_bytes16(value);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_double (ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_DOUBLE
  sqlite3_value *	value = IK_POINTER_DATA_VOIDP(s_value);
  double		rv;
  rv = sqlite3_value_double(value);
  return ika_flonum_from_double(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_int (ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_INT
  sqlite3_value *	value = IK_POINTER_DATA_VOIDP(s_value);
  int			rv;
  rv = sqlite3_value_int(value);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_int64 (ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_INT64
  sqlite3_value *	value = IK_POINTER_DATA_VOIDP(s_value);
  sqlite3_int64		rv;
  rv = sqlite3_value_int64(value);
  return ika_integer_from_sint64(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_text (ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_TEXT
  sqlite3_value *	value = IK_POINTER_DATA_VOIDP(s_value);
  const void *		data;
  int			bytes;
  bytes = sqlite3_value_bytes(value);
  data  = sqlite3_value_text(value);
  return ika_bytevector_from_memory_block(pcb, data, bytes);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_text16 (ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_TEXT16
  sqlite3_value *	value = IK_POINTER_DATA_VOIDP(s_value);
  const void *		data;
  int			bytes;
  bytes = sqlite3_value_bytes16(value);
  data  = sqlite3_value_text16(value);
  return ika_bytevector_from_memory_block(pcb, data, bytes);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_text16le (ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_TEXT16LE
  sqlite3_value *	value = IK_POINTER_DATA_VOIDP(s_value);
  const void *		data;
  int			bytes;
  bytes = sqlite3_value_bytes16(value);
  data  = sqlite3_value_text16le(value);
  return ika_bytevector_from_memory_block(pcb, data, bytes);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_text16be (ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_TEXT16BE
  sqlite3_value *	value = IK_POINTER_DATA_VOIDP(s_value);
  const void *		data;
  int			bytes;
  bytes = sqlite3_value_bytes16(value);
  data  = sqlite3_value_text16be(value);
  return ika_bytevector_from_memory_block(pcb, data, bytes);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_type (ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_TYPE
  sqlite3_value *	value = IK_POINTER_DATA_VOIDP(s_value);
  int			rv;
  rv = sqlite3_value_type(value);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_value_numeric_type (ikptr s_value, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_VALUE_NUMERIC_TYPE
  sqlite3_value *	value = IK_POINTER_DATA_VOIDP(s_value);
  int			rv;
  rv = sqlite3_value_numeric_type(value);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Scheme return values to SQL values.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_result_blob (ikptr s_context, ikptr s_blob_data, ikptr s_blob_len,
			ikptr s_destructor, ikpcb * pcb)
/* Set the result in S_CONTEXT to a BLOB; return the void object.

   S_CONTEXT must  be a  pointer object referencing  a "sqlite3_context"
   instance.

   If S_BLOB_DATA  is a bytevector  and S_BLOB_LEN is the  false object:
   the BLOB is set to the whole bytevector data.

   If  S_BLOB_DATA is  a  bytevector  and S_BLOB_LEN  is  not the  false
   object, S_BLOB_LEN  must be an exact  integer in the range  of "int":
   the BLOB is set to the first S_BLOB_LEN bytes of bytevector data.

   If  S_BLOB_DATA is  a pointer  object,  S_BLOB_LEN must  be an  exact
   integer  in  the  range of  "int":  the  BLOB  is  set to  the  first
   S_BLOB_LEN bytes of the referenced memory block.

   If  S_DESTRUCTOR  is the  false  object:  the  destructor is  set  to
   SQLITE_TRANSIENT;  else   S_DESTRUCTOR  must  be  a   pointer  object
   referencing the destructor to be used to finalise the BLOB.
*/
{
#ifdef HAVE_SQLITE3_RESULT_BLOB
  sqlite3_context *		context = IK_POINTER_DATA_VOIDP(s_context);
  void *			ptr;
  int				len;
  sqlite3_destructor_type	destructor;
  if (IK_IS_BYTEVECTOR(s_blob_data)) {
    ptr = IK_BYTEVECTOR_DATA_VOIDP(s_blob_data);
    if (false_object == s_blob_len)
      len = IK_BYTEVECTOR_LENGTH(s_blob_data);
    else
      len = ik_integer_to_int(s_blob_len);
  } else {
    ptr = IK_POINTER_DATA_VOIDP(s_blob_data);
    len = ik_integer_to_int(s_blob_len);
  }
  destructor = (false_object == s_destructor)? \
    SQLITE_TRANSIENT : IK_SQLITE_CALLBACK(s_destructor);
  sqlite3_result_blob(context, ptr, len, destructor);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_double (ikptr s_context, ikptr s_retval, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_DOUBLE
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  double		retval	= IK_FLONUM_DATA(s_retval);
  sqlite3_result_double(context, retval);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_error (ikptr s_context, ikptr s_error_message, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_ERROR
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  const char *		error_message	= IK_BYTEVECTOR_DATA_CHARP(s_error_message);
  int			message_len	= IK_BYTEVECTOR_LENGTH(s_error_message);
  sqlite3_result_error(context, error_message, message_len);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_error16 (ikptr s_context, ikptr s_error_message, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_ERROR16
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  const void *		error_message	= IK_BYTEVECTOR_DATA_CHARP(s_error_message);
  int			message_len	= IK_BYTEVECTOR_LENGTH(s_error_message);
  sqlite3_result_error16(context, error_message, message_len);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_error_toobig (ikptr s_context, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_ERROR_TOOBIG
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  sqlite3_result_error_toobig(context);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_error_nomem (ikptr s_context, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_ERROR_NOMEM
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  sqlite3_result_error_nomem(context);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_error_code (ikptr s_context, ikptr s_errcode, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_ERROR_CODE
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  int			errcode	= ik_integer_to_int(s_errcode);
  sqlite3_result_error_code(context, errcode);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_int (ikptr s_context, ikptr s_retval, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_INT
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  int			retval	= ik_integer_to_int(s_retval);
  sqlite3_result_int(context, retval);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_int64 (ikptr s_context, ikptr s_retval, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_INT64
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  sqlite3_int64		retval	= ik_integer_to_sint64(s_retval);
  sqlite3_result_int64(context, retval);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_null (ikptr s_context, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_NULL
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  sqlite3_result_null(context);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_text (ikptr s_context, ikptr s_blob_data, ikptr s_blob_len,
			ikptr s_destructor, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_TEXT
  sqlite3_context *		context = IK_POINTER_DATA_VOIDP(s_context);
  void *			ptr;
  int				len;
  sqlite3_destructor_type	destructor;
  if (IK_IS_BYTEVECTOR(s_blob_data)) {
    ptr = IK_BYTEVECTOR_DATA_VOIDP(s_blob_data);
    if (false_object == s_blob_len)
      len = IK_BYTEVECTOR_LENGTH(s_blob_data);
    else
      len = ik_integer_to_int(s_blob_len);
  } else {
    ptr = IK_POINTER_DATA_VOIDP(s_blob_data);
    len = ik_integer_to_int(s_blob_len);
  }
  destructor = (false_object == s_destructor)? \
    SQLITE_TRANSIENT : IK_SQLITE_CALLBACK(s_destructor);
  sqlite3_result_text(context, ptr, len, destructor);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_text16 (ikptr s_context, ikptr s_blob_data, ikptr s_blob_len,
			  ikptr s_destructor, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_TEXT16
  sqlite3_context *		context = IK_POINTER_DATA_VOIDP(s_context);
  void *			ptr;
  int				len;
  sqlite3_destructor_type	destructor;
  if (IK_IS_BYTEVECTOR(s_blob_data)) {
    ptr = IK_BYTEVECTOR_DATA_VOIDP(s_blob_data);
    if (false_object == s_blob_len)
      len = IK_BYTEVECTOR_LENGTH(s_blob_data);
    else
      len = ik_integer_to_int(s_blob_len);
  } else {
    ptr = IK_POINTER_DATA_VOIDP(s_blob_data);
    len = ik_integer_to_int(s_blob_len);
  }
  destructor = (false_object == s_destructor)? \
    SQLITE_TRANSIENT : IK_SQLITE_CALLBACK(s_destructor);
  sqlite3_result_text16(context, ptr, len, destructor);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_text16le (ikptr s_context, ikptr s_blob_data, ikptr s_blob_len,
			    ikptr s_destructor, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_TEXT16LE
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  void *			ptr;
  int				len;
  sqlite3_destructor_type	destructor;
  if (IK_IS_BYTEVECTOR(s_blob_data)) {
    ptr = IK_BYTEVECTOR_DATA_VOIDP(s_blob_data);
    if (false_object == s_blob_len)
      len = IK_BYTEVECTOR_LENGTH(s_blob_data);
    else
      len = ik_integer_to_int(s_blob_len);
  } else {
    ptr = IK_POINTER_DATA_VOIDP(s_blob_data);
    len = ik_integer_to_int(s_blob_len);
  }
  destructor = (false_object == s_destructor)? \
    SQLITE_TRANSIENT : IK_SQLITE_CALLBACK(s_destructor);
  sqlite3_result_text16le(context, ptr, len, destructor);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_text16be (ikptr s_context, ikptr s_blob_data, ikptr s_blob_len,
			    ikptr s_destructor, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_TEXT16BE
  sqlite3_context *		context = IK_POINTER_DATA_VOIDP(s_context);
  void *			ptr;
  int				len;
  sqlite3_destructor_type	destructor;
  if (IK_IS_BYTEVECTOR(s_blob_data)) {
    ptr = IK_BYTEVECTOR_DATA_VOIDP(s_blob_data);
    if (false_object == s_blob_len)
      len = IK_BYTEVECTOR_LENGTH(s_blob_data);
    else
      len = ik_integer_to_int(s_blob_len);
  } else {
    ptr = IK_POINTER_DATA_VOIDP(s_blob_data);
    len = ik_integer_to_int(s_blob_len);
  }
  destructor = (false_object == s_destructor)? \
    SQLITE_TRANSIENT : IK_SQLITE_CALLBACK(s_destructor);
  sqlite3_result_text16be(context, ptr, len, destructor);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_value (ikptr s_context, ikptr s_retval, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_VALUE
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  sqlite3_value *	retval	= IK_POINTER_DATA_VOIDP(s_retval);
  sqlite3_result_value(context, retval);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_result_zeroblob (ikptr s_context, ikptr s_blob_len, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RESULT_ZEROBLOB
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  int			len	= ik_integer_to_int(s_blob_len);
  sqlite3_result_zeroblob(context, len);
  return void_object;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Auxiliary functions.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_aggregate_context (ikptr s_context, ikptr s_number_of_bytes, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_AGGREGATE_CONTEXT
  sqlite3_context *	context		= IK_POINTER_DATA_VOIDP(s_context);
  int			number_of_bytes = ik_integer_to_int(s_number_of_bytes);
  void *		rv;
  rv = sqlite3_aggregate_context(context, number_of_bytes);
  return (rv)? ika_pointer_alloc(pcb, (ik_ulong)rv) : false_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_user_data (ikptr s_context, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_USER_DATA
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  void *		rv;
  rv = sqlite3_user_data(context);
  return ika_pointer_alloc(pcb, (ik_ulong)rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_context_db_handle (ikptr s_context, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CONTEXT_DB_HANDLE
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  sqlite3 *		conn;
  conn = sqlite3_context_db_handle(context);
  return ika_pointer_alloc(pcb, (ik_ulong)conn);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_get_auxdata (ikptr s_context, ikptr s_argnum, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_GET_AUXDATA
  sqlite3_context *	context = IK_POINTER_DATA_VOIDP(s_context);
  int			argnum	= ik_integer_to_int(s_argnum);
  void *		aux_data;
  aux_data = sqlite3_get_auxdata(context, argnum);
  return ika_pointer_alloc(pcb, (ik_ulong)aux_data);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_set_auxdata (ikptr s_context, ikptr s_argnum,
			ikptr s_aux_data, ikptr s_destructor,
			ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SET_AUXDATA
  typedef void (*destructor_t)(void*);
  sqlite3_context *	context		= IK_POINTER_DATA_VOIDP(s_context);
  int			argnum		= ik_integer_to_int(s_argnum);
  void *		data		= IK_POINTER_DATA_VOIDP(s_aux_data);
  destructor_t		destructor	= IK_SQLITE_CALLBACK(s_destructor);
  sqlite3_set_auxdata(context, argnum, data, destructor);
  return void_object;
#else
  feature_failure(__func__);
#endif
}

/* end of file */