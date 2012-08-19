/*
  Part of: Vicare/SQLite
  Contents: BLOBs for incremental input/output
  Date: Thu Aug  9, 2012

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
 ** Opening and closing BLOBs.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_blob_open (ikptr s_conn, ikptr s_database_name, ikptr s_table_name,
		      ikptr s_column_name, ikptr s_row_id, ikptr s_write_access,
		      ikptr s_blob, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BLOB_OPEN
  sqlite3 *	conn		= IK_SQLITE_CONNECTION(s_conn);
  const char *	database_name	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER(s_database_name);
  const char *	table_name	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER(s_table_name);
  const char *	column_name	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER(s_column_name);
  sqlite3_int64	row_id		= ik_integer_to_sint64(s_row_id);
  int		write_access	= (IK_FALSE_OBJECT == s_write_access)? 0 : 1;
  sqlite3_blob *blob;
  int		rv;
  rv = sqlite3_blob_open(conn, database_name, table_name, column_name, row_id,
			 write_access, &blob);
  if (SQLITE_OK == rv) {
    pcb->root0 = &s_blob;
    {
      IK_ASS(IK_SQLITE_BLOB_POINTER(s_blob),ika_pointer_alloc(pcb, (ik_ulong)blob));
    }
    pcb->root0 = NULL;
  }
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_blob_reopen (ikptr s_blob, ikptr s_row_id, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BLOB_REOPEN
  sqlite3_blob *	blob	= IK_SQLITE_BLOB(s_blob);
  sqlite3_int64		row_id	= ik_integer_to_sint64(s_row_id);
  int			rv;
  rv = sqlite3_blob_reopen(blob, row_id);
  if (SQLITE_OK == rv) {
    IK_SQLITE_BLOB_ROWID(s_blob) = s_row_id;
  }
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_blob_close (ikptr s_blob, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BLOB_CLOSE
  ikptr		s_pointer = IK_SQLITE_BLOB_POINTER(s_blob);
  if (ik_is_pointer(s_pointer)) {
    sqlite3_blob *blob = IK_POINTER_DATA_VOIDP(s_pointer);
    if (blob) {
      int	rv;
      rv = sqlite3_blob_close(blob);
      if (SQLITE_OK == rv) {
	IK_POINTER_SET_NULL(s_pointer);
      }
      return ika_integer_from_sqlite_errcode(pcb, rv);
    }
  }
  return ika_integer_from_sqlite_errcode(pcb, SQLITE_OK);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Operations on BLOBs.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_blob_bytes (ikptr s_blob, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BLOB_BYTES
  sqlite3_blob *	blob = IK_SQLITE_BLOB(s_blob);
  int			rv;
  rv = sqlite3_blob_bytes(blob);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_blob_read (ikptr s_src_blob,   ikptr s_src_offset,
		      ikptr s_dst_buffer, ikptr s_dst_offset,
		      ikptr s_number_of_bytes,
		      ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BLOB_READ
  sqlite3_blob *src_blob	= IK_SQLITE_BLOB(s_src_blob);
  uint8_t *	dst_buffer	= IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER(s_dst_buffer);
  int		src_offset	= ik_integer_to_int(s_src_offset);
  int		dst_offset	= ik_integer_to_int(s_dst_offset);
  int		size		= ik_integer_to_int(s_number_of_bytes);
  int		rv;
  rv = sqlite3_blob_read(src_blob, dst_buffer+dst_offset, size, src_offset);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_blob_write (ikptr s_dst_blob,   ikptr s_dst_offset,
		       ikptr s_src_buffer, ikptr s_src_offset,
		       ikptr s_number_of_bytes,
		       ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_BLOB_WRITE
  sqlite3_blob *dst_blob	= IK_SQLITE_BLOB(s_dst_blob);
  uint8_t *	src_buffer	= IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER(s_src_buffer);
  int		src_offset	= ik_integer_to_int(s_src_offset);
  int		dst_offset	= ik_integer_to_int(s_dst_offset);
  int		size		= ik_integer_to_int(s_number_of_bytes);
  int		rv;
  rv = sqlite3_blob_write(dst_blob, src_buffer+src_offset, size, dst_offset);
  return ika_integer_from_sqlite_errcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}

/* end of file */
