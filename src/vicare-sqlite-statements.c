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
ik_sqlite3_finalize (ikptr s_statement)
/* Interface  to  the  C   function  "sqlite3_finalize()".   Finalise  a
   prepared statement; return a SQLITE_ error code.

   S_STATEMENT must be an instance of "sqlite3-stmt".

   The pointer object referencing the statement  is reset to NULL, if it
   is not NULL already.  If is safe to call this function multiple times
   for the same instance of "sqlite3-stmt". */
{
#ifdef HAVE_SQLITE3_FINALIZE
  ikptr		s_pointer = IK_SQLITE_STMT_POINTER(s_statement);
  sqlite3_stmt *statement = IK_POINTER_DATA_VOIDP(s_pointer);
  if (NULL == statement)
    return IK_FIX(SQLITE_OK);
  else {
    int		rv;
    rv = sqlite3_finalize(statement);
    /* Reset  the reference  to  the connection  in  terms of  "sqlite3"
       instance. */
    IK_SQLITE_STMT_CONNECTION(s_statement) = false_object;
    /* Reset the  pointer to SQLite structure  to NULL, so that  we know
       that this instance has been finalised already. */
    IK_POINTER_SET_NULL(s_pointer);
    return IK_FIX(rv);
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
  sql_snippet += sql_offset;
  rv = sqlite3_prepare(conn, sql_snippet, sql_length, &statement, &sql_unused);
  if (SQLITE_OK == rv) {
    long	used_length	= (long)(sql_unused - sql_snippet);
    ikptr	s_pair		= ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_CAR(s_pair) = IK_FIX(rv);
      IK_ASS(IK_CDR(s_pair), ika_integer_from_long(pcb, used_length));
      IK_ASS(IK_SQLITE_STMT_POINTER(s_statement),
	     ika_pointer_alloc(pcb, (ik_ulong)statement));
      /* If  requested:  store  the  SQL  statement  code  in  the  data
	 structure. */
      if (false_object == s_store_sql_text) {
	IK_SQLITE_STMT_SQLBV(s_statement) = false_object;
      } else {
	IK_ASS(IK_SQLITE_STMT_SQLBV(s_statement),
	       ika_bytevector_from_cstring_len(pcb, sql_snippet, used_length));
      }
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return IK_FIX(rv);
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
  sql_snippet += sql_offset;
  rv = sqlite3_prepare_v2(conn, sql_snippet, sql_length, &statement, &sql_unused);
  if (SQLITE_OK == rv) {
    long	used_length	= (long)(sql_unused - sql_snippet);
    ikptr	s_pair		= ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_CAR(s_pair) = IK_FIX(rv);
      IK_ASS(IK_CDR(s_pair), ika_integer_from_long(pcb, used_length));
      IK_ASS(IK_SQLITE_STMT_POINTER(s_statement),
	     ika_pointer_alloc(pcb, (ik_ulong)statement));
      /* If  requested:  store  the  SQL  statement  code  in  the  data
	 structure. */
      if (false_object == s_store_sql_text) {
	IK_SQLITE_STMT_SQLBV(s_statement) = false_object;
      } else {
	IK_ASS(IK_SQLITE_STMT_SQLBV(s_statement),
	       ika_bytevector_from_cstring_len(pcb, sql_snippet, used_length));
      }
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return IK_FIX(rv);
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
  const char *	sql_snippet	= IK_BYTEVECTOR_DATA_CHARP(s_sql_snippet);
  int		sql_offset	= ik_integer_to_int(s_sql_offset);
  int		sql_length	= IK_BYTEVECTOR_LENGTH(s_sql_snippet) - sql_offset;
  sqlite3_stmt *statement;
  char *	sql_unused;
  int		rv;
  sql_snippet += sql_offset;
  rv = sqlite3_prepare16(conn, sql_snippet, sql_length, &statement,
			 (void *)&sql_unused);
  if (SQLITE_OK == rv) {
    long	used_length	= (long)(sql_unused - sql_snippet);
    ikptr	s_pair		= ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_CAR(s_pair) = IK_FIX(rv);
      IK_ASS(IK_CDR(s_pair), ika_integer_from_long(pcb, used_length));
      IK_ASS(IK_SQLITE_STMT_POINTER(s_statement),
	     ika_pointer_alloc(pcb, (ik_ulong)statement));
      /* If  requested:  store  the  SQL  statement  code  in  the  data
	 structure. */
      if (false_object == s_store_sql_text) {
	IK_SQLITE_STMT_SQLBV(s_statement) = false_object;
      } else {
	IK_ASS(IK_SQLITE_STMT_SQLBV(s_statement),
	       ika_bytevector_from_cstring_len(pcb, sql_snippet, used_length));
      }
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return IK_FIX(rv);
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
  const char *	sql_snippet	= IK_BYTEVECTOR_DATA_CHARP(s_sql_snippet);
  int		sql_offset	= ik_integer_to_int(s_sql_offset);
  int		sql_length	= IK_BYTEVECTOR_LENGTH(s_sql_snippet) - sql_offset;
  sqlite3_stmt *statement;
  char *	sql_unused;
  int		rv;
  sql_snippet += sql_offset;
  rv = sqlite3_prepare16_v2(conn, sql_snippet, sql_length, &statement,
			    (void *)&sql_unused);
  if (SQLITE_OK == rv) {
    long	used_length	= (long)(sql_unused - sql_snippet);
    ikptr	s_pair		= ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_CAR(s_pair) = IK_FIX(rv);
      IK_ASS(IK_CDR(s_pair), ika_integer_from_long(pcb, used_length));
      IK_ASS(IK_SQLITE_STMT_POINTER(s_statement),
	     ika_pointer_alloc(pcb, (ik_ulong)statement));
      /* If  requested:  store  the  SQL  statement  code  in  the  data
	 structure. */
      if (false_object == s_store_sql_text) {
	IK_SQLITE_STMT_SQLBV(s_statement) = false_object;
      } else {
	IK_ASS(IK_SQLITE_STMT_SQLBV(s_statement),
	       ika_bytevector_from_cstring_len(pcb, sql_snippet, used_length));
      }
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return IK_FIX(rv);
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


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */


/* end of file */
