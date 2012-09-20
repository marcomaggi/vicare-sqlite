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

#include <vicare-sqlite-internals.h>


/** --------------------------------------------------------------------
 ** Convenience functions to execute SQL snippets: exec.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_exec (ikptr s_conn, ikptr s_sql_snippet, ikptr s_each_row_callback, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_EXEC
  typedef int ik_sqlite3_exec_callback (void*,int,char**,char**);
  sqlite3 *			conn;
  const char *			sql_snippet;
  ik_sqlite3_exec_callback *	each_row_callback;
  char *			error_message;
  ikptr				sk;
  int				rv;
  conn			= IK_SQLITE_CONNECTION(s_conn);
  sql_snippet		= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_sql_snippet);
  each_row_callback	= IK_POINTER_FROM_POINTER_OR_FALSE(s_each_row_callback);
  /* The  call  to  "sqlite3_exex()"  invokes Scheme  code  through  the
     callback,  so   we  protect   it  by   saving  and   restoring  the
     continuation. */
  sk = ik_enter_c_function(pcb);
  {
    rv = sqlite3_exec(conn, sql_snippet, each_row_callback,
		      NULL /* callback custom data */ , &error_message);
  }
  ik_leave_c_function(pcb, sk);
  if (SQLITE_OK) {
    return ika_integer_from_sqlite_errcode(pcb,rv);
  } else {
    if (error_message) {
      ikptr	s_pair = ika_pair_alloc(pcb);
      pcb->root0 = &s_pair;
      {
	IK_ASS(IK_CAR(s_pair), ika_integer_from_sqlite_errcode(pcb,rv));
	IK_ASS(IK_CDR(s_pair), ika_bytevector_from_cstring(pcb, error_message));
	sqlite3_free(error_message);
      }
      pcb->root0 = NULL;
      return s_pair;
    } else
      return ika_integer_from_sqlite_errcode(pcb,rv);
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_c_array_to_bytevectors (ikptr s_num_of_bvs, ikptr s_c_array, ikpcb * pcb)
/* This  is used  to convert  the  C arrays  handed to  the callback  by
   "sqlite3_exec()"  into  vectors  of bytevectors  in  UTF-8  encoding.
   Return a vector of UTF-8  bytevectors representing the entries of the
   given C array.

   S_NUM_OF_BVS is the number of entries  in the supplied C array and it
   must be the number of elements in the returned vector.

   S_C_ARRAY is  a pointer  object referencing an  array of  pointers to
   strings; some elements of the array may be NULL. */
{
  long		number_of_bytevectors	= IK_UNFIX(s_num_of_bvs);
  const char **	c_array			= IK_POINTER_DATA_VOIDP(s_c_array);
  ikptr		s_vector = ika_vector_alloc_and_init(pcb, number_of_bytevectors);
  pcb->root0 = &s_vector;
  {
    long	i;
    ikptr	s_bytevector;
    for (i=0; i<number_of_bytevectors; ++i) {
      s_bytevector = (c_array[i])? ika_bytevector_from_cstring(pcb, c_array[i]) \
	: IK_FALSE_OBJECT;
      IK_ITEM(s_vector, i) = s_bytevector;
    }
  }
  pcb->root0 = NULL;
  return s_vector;
}


/** --------------------------------------------------------------------
 ** Convenience functions to execute SQL snippets: get table.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_get_table (ikptr s_conn, ikptr s_sql_snippet, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_GET_TABLE
  sqlite3 *	conn;
  const char *	sql_snippet;
  char **	result;
  int		number_of_rows, number_of_columns;
  char *	error_message;
  int		rv;
  ikptr		sk;
  conn		= IK_SQLITE_CONNECTION(s_conn);
  sql_snippet	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_sql_snippet);
  sk = ik_enter_c_function(pcb);
  {
    rv = sqlite3_get_table(conn, sql_snippet,
			   &result, &number_of_rows, &number_of_columns,
			   &error_message);
  }
  ik_leave_c_function(pcb, sk);
  if (SQLITE_OK == rv) {
    /* Return a Scheme vector of  5 elements being: SQLITE_OK as fixnum,
       false object,  exact integer representing  the number of  rows in
       the result, exact  integer representing the number  of columns in
       the result, pointer object referencing the result structure. */
    ikptr	s_vector = ika_vector_alloc_and_init(pcb, 5);
    pcb->root0 = &s_vector;
    {
      IK_ASS(IK_ITEM(s_vector, 0), ika_integer_from_sqlite_errcode(pcb,rv));
      IK_ITEM(s_vector, 1) = IK_FALSE_OBJECT;
      IK_ASS(IK_ITEM(s_vector, 2), ika_integer_from_int(pcb, number_of_rows));
      IK_ASS(IK_ITEM(s_vector, 3), ika_integer_from_int(pcb, number_of_columns));
      IK_ASS(IK_ITEM(s_vector, 4), ika_pointer_alloc(pcb, (ik_ulong)result));
    }
    pcb->root0 = NULL;
    return s_vector;
  } else {
    /* Return a  Scheme vector  of 5 elements  being: the  SQLITE_ error
       code  as  fixnum,  a  UTF-8  bytevector  representing  the  error
       message, the fixnum  zero representing the number of  rows in the
       result, the fixnum zero representing the number of columns in the
       result, false object. */
    ikptr	s_vector = ika_vector_alloc_and_init(pcb, 5);
    pcb->root0 = &s_vector;
    {
      IK_ASS(IK_ITEM(s_vector, 0), ika_integer_from_sqlite_errcode(pcb,rv));
      if (error_message) {
	IK_ASS(IK_ITEM(s_vector, 1), ika_bytevector_from_cstring(pcb, error_message));
	sqlite3_free(error_message);
      } else
	IK_ASS(IK_ITEM(s_vector, 1), ika_bytevector_from_cstring(pcb, ""));
      IK_ITEM(s_vector, 2) = IK_FIX(0);
      IK_ITEM(s_vector, 3) = IK_FIX(0);
      IK_ITEM(s_vector, 4) = IK_FALSE_OBJECT;
    }
    pcb->root0 = NULL;
    return s_vector;
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_free_table (ikptr s_result)
{
#ifdef HAVE_SQLITE3_FREE_TABLE
  char **	result = IK_POINTER_DATA_VOIDP(s_result);
  sqlite3_free_table(result);
  return IK_VOID_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_table_to_vector (ikptr s_num_of_rows, ikptr s_num_of_cols,
			    ikptr s_table_pointer, ikpcb * pcb)
{
  int		num_of_rows	= ik_integer_to_int(s_num_of_rows);
  int		num_of_cols	= ik_integer_to_int(s_num_of_cols);
  const char **	table		= IK_POINTER_DATA_VOIDP(s_table_pointer);
  int		vector_len	= 1+num_of_rows;
  ikptr		s_vector	= ika_vector_alloc_and_init(pcb, vector_len);
  pcb->root0 = &s_vector;
  {
    ikptr	s_row = ika_vector_alloc_and_init(pcb, num_of_cols);
    int		vector_index=0, column_index, table_index=0;
    pcb->root1 = &s_row;
    {
      /* The first item  in S_VECTOR is a vector  of bytevectors holding
	 the column names.  False is used if no name is present. */
      for (column_index=0; column_index<num_of_cols; ++column_index, ++table_index) {
	/* fprintf(stderr, "%d %d %d\n", vector_index, column_index, table_index); */
	IK_ASS(IK_ITEM(s_row, column_index),
	       (table[table_index])?
	       ika_bytevector_from_cstring(pcb, table[table_index]) : IK_FALSE_OBJECT);
      }
      IK_ITEM(s_vector, vector_index++) = s_row;
    }
    pcb->root1 = NULL;
    /* Subsequent items  in S_VECTOR are vectors  of bytevectors holding
       the column values.  False is used if no value is present. */
    for (; vector_index<vector_len; ++vector_index) {
      s_row = ika_vector_alloc_and_init(pcb, num_of_cols);
      pcb->root1 = &s_row;
      {
	for (column_index=0; column_index<num_of_cols; ++column_index, ++table_index) {
	  /* fprintf(stderr, "%d %d %d\n", vector_index, column_index, table_index); */
	  IK_ASS(IK_ITEM(s_row, column_index),
		 (table[table_index])?
		 ika_bytevector_from_cstring(pcb, table[table_index]) : IK_FALSE_OBJECT);
	}
	IK_ITEM(s_vector, vector_index) = s_row;
      }
      pcb->root1 = NULL;
    }
  }
  pcb->root0 = NULL;
  return s_vector;
}


/** --------------------------------------------------------------------
 ** SQL execution auxiliary functions.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_last_insert_rowid (ikptr s_conn, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_LAST_INSERT_ROWID
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);
  sqlite_int64	rv;
  rv = sqlite3_last_insert_rowid(conn);
  return ika_integer_from_sint64(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_changes (ikptr s_conn, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CHANGES
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);
  int		rv;
  rv = sqlite3_changes(conn);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_total_changes (ikptr s_conn, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_TOTAL_CHANGES
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);
  int		rv;
  rv = sqlite3_total_changes(conn);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_interrupt (ikptr s_conn)
{
#ifdef HAVE_SQLITE3_INTERRUPT
  sqlite3 *	conn = IK_SQLITE_CONNECTION(s_conn);
  sqlite3_interrupt(conn);
  return IK_VOID_OBJECT;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

/* end of file */
