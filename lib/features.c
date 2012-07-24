/*
  Part of: Vicare/SQLite
  Contents: print platform features library
  Date: Tue Jul 24, 2012

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


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <stdlib.h>


int
main (int argc, const char *const argv[])
{
  printf(";;; -*- coding: utf-8-unix -*-\n\
;;;\n\
;;;Part of: Vicare/SQLite\n\
;;;Contents: static platform inspection\n\
;;;Date: Tue Jul 24, 2012\n\
;;;\n\
;;;Abstract\n\
;;;\n\
;;;\n\
;;;\n\
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>\n\
;;;\n\
;;;This program is free software:  you can redistribute it and/or modify\n\
;;;it under the terms of the  GNU General Public License as published by\n\
;;;the Free Software Foundation, either version 3 of the License, or (at\n\
;;;your option) any later version.\n\
;;;\n\
;;;This program is  distributed in the hope that it  will be useful, but\n\
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of\n\
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU\n\
;;;General Public License for more details.\n\
;;;\n\
;;;You should  have received a  copy of  the GNU General  Public License\n\
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\
;;;\n\
\n\
\n\
#!r6rs\n\
(library (vicare databases sqlite3 features)\n\
  (export\n\
    HAVE_SQLITE3_COMPILEOPTION_USED\n\
    HAVE_SQLITE3_COMPILEOPTION_GET\n\
    HAVE_SQLITE3_THREADSAFE\n\
    HAVE_SQLITE3_CLOSE\n\
    HAVE_SQLITE3_EXEC\n\
    HAVE_SQLITE3_INITIALIZE\n\
    HAVE_SQLITE3_SHUTDOWN\n\
    HAVE_SQLITE3_OS_INIT\n\
    HAVE_SQLITE3_OS_END\n\
    HAVE_SQLITE3_CONFIG\n\
    HAVE_SQLITE3_DB_CONFIG\n\
    HAVE_SQLITE3_EXTENDED_RESULT_CODES\n\
    HAVE_SQLITE3_LAST_INSERT_ROWID\n\
    HAVE_SQLITE3_CHANGES\n\
    HAVE_SQLITE3_TOTAL_CHANGES\n\
    HAVE_SQLITE3_INTERRUPT\n\
    HAVE_SQLITE3_COMPLETE\n\
    HAVE_SQLITE3_COMPLETE16\n\
    HAVE_SQLITE3_BUSY_HANDLER\n\
    HAVE_SQLITE3_BUSY_TIMEOUT\n\
    HAVE_SQLITE3_GET_TABLE\n\
    HAVE_SQLITE3_FREE_TABLE\n\
    HAVE_SQLITE3_MEMORY_USED\n\
    HAVE_SQLITE3_MEMORY_HIGHWATER\n\
    HAVE_SQLITE3_RANDOMNESS\n\
    HAVE_SQLITE3_SET_AUTHORIZER\n\
    HAVE_SQLITE3_TRACE\n\
    HAVE_SQLITE3_PROFILE\n\
    HAVE_SQLITE3_PROGRESS_HANDLER\n\
    HAVE_SQLITE3_OPEN\n\
    HAVE_SQLITE3_OPEN16\n\
    HAVE_SQLITE3_OPEN_V2\n\
    HAVE_SQLITE3_URI_PARAMETER\n\
    HAVE_SQLITE3_URI_BOOLEAN\n\
    HAVE_SQLITE3_URI_INT64\n\
    HAVE_SQLITE3_ERRCODE\n\
    HAVE_SQLITE3_EXTENDED_ERRCODE\n\
    HAVE_SQLITE3_ERRMSG\n\
    HAVE_SQLITE3_ERRMSG16\n\
    HAVE_SQLITE3_LIMIT\n\
    HAVE_SQLITE3_PREPARE\n\
    HAVE_SQLITE3_PREPARE_V2\n\
    HAVE_SQLITE3_PREPARE16\n\
    HAVE_SQLITE3_PREPARE16_V2\n\
    HAVE_SQLITE3_SQL\n\
    HAVE_SQLITE3_STMT_READONLY\n\
    HAVE_SQLITE3_STMT_BUSY\n\
    HAVE_SQLITE3_BIND_BLOB\n\
    HAVE_SQLITE3_BIND_DOUBLE\n\
    HAVE_SQLITE3_BIND_INT\n\
    HAVE_SQLITE3_BIND_INT64\n\
    HAVE_SQLITE3_BIND_NULL\n\
    HAVE_SQLITE3_BIND_TEXT\n\
    HAVE_SQLITE3_BIND_TEXT16\n\
    HAVE_SQLITE3_BIND_VALUE\n\
    HAVE_SQLITE3_BIND_ZEROBLOB\n\
    HAVE_SQLITE3_BIND_PARAMETER_COUNT\n\
    HAVE_SQLITE3_BIND_PARAMETER_NAME\n\
    HAVE_SQLITE3_BIND_PARAMETER_INDEX\n\
    HAVE_SQLITE3_CLEAR_BINDINGS\n\
    HAVE_SQLITE3_COLUMN_COUNT\n\
    HAVE_SQLITE3_COLUMN_NAME\n\
    HAVE_SQLITE3_COLUMN_NAME16\n\
    HAVE_SQLITE3_COLUMN_DATABASE_NAME\n\
    HAVE_SQLITE3_COLUMN_DATABASE_NAME16\n\
    HAVE_SQLITE3_COLUMN_TABLE_NAME\n\
    HAVE_SQLITE3_COLUMN_TABLE_NAME16\n\
    HAVE_SQLITE3_COLUMN_ORIGIN_NAME\n\
    HAVE_SQLITE3_COLUMN_ORIGIN_NAME16\n\
    HAVE_SQLITE3_COLUMN_DECLTYPE\n\
    HAVE_SQLITE3_COLUMN_DECLTYPE16\n\
    HAVE_SQLITE3_STEP\n\
    HAVE_SQLITE3_DATA_COUNT\n\
    HAVE_SQLITE3_COLUMN_BLOB\n\
    HAVE_SQLITE3_COLUMN_BYTES\n\
    HAVE_SQLITE3_COLUMN_BYTES16\n\
    HAVE_SQLITE3_COLUMN_DOUBLE\n\
    HAVE_SQLITE3_COLUMN_INT\n\
    HAVE_SQLITE3_COLUMN_INT64\n\
    HAVE_SQLITE3_COLUMN_TEXT\n\
    HAVE_SQLITE3_COLUMN_TEXT16\n\
    HAVE_SQLITE3_COLUMN_TYPE\n\
    HAVE_SQLITE3_COLUMN_VALUE\n\
    HAVE_SQLITE3_FINALIZE\n\
    HAVE_SQLITE3_RESET\n\
    HAVE_SQLITE3_CREATE_FUNCTION\n\
    HAVE_SQLITE3_CREATE_FUNCTION16\n\
    HAVE_SQLITE3_CREATE_FUNCTION_V2\n\
    HAVE_SQLITE3_VALUE_BLOB\n\
    HAVE_SQLITE3_VALUE_BYTES\n\
    HAVE_SQLITE3_VALUE_BYTES16\n\
    HAVE_SQLITE3_VALUE_DOUBLE\n\
    HAVE_SQLITE3_VALUE_INT\n\
    HAVE_SQLITE3_VALUE_INT64\n\
    HAVE_SQLITE3_VALUE_TEXT\n\
    HAVE_SQLITE3_VALUE_TEXT16\n\
    HAVE_SQLITE3_VALUE_TEXT16LE\n\
    HAVE_SQLITE3_VALUE_TEXT16BE\n\
    HAVE_SQLITE3_VALUE_TYPE\n\
    HAVE_SQLITE3_VALUE_NUMERIC_TYPE\n\
    HAVE_SQLITE3_AGGREGATE_CONTEXT\n\
    HAVE_SQLITE3_USER_DATA\n\
    HAVE_SQLITE3_CONTEXT_DB_HANDLE\n\
    HAVE_SQLITE3_GET_AUXDATA\n\
    HAVE_SQLITE3_SET_AUXDATA\n\
    HAVE_SQLITE3_RESULT_BLOB\n\
    HAVE_SQLITE3_RESULT_DOUBLE\n\
    HAVE_SQLITE3_RESULT_ERROR\n\
    HAVE_SQLITE3_RESULT_ERROR16\n\
    HAVE_SQLITE3_RESULT_ERROR_TOOBIG\n\
    HAVE_SQLITE3_RESULT_ERROR_NOMEM\n\
    HAVE_SQLITE3_RESULT_ERROR_CODE\n\
    HAVE_SQLITE3_RESULT_INT\n\
    HAVE_SQLITE3_RESULT_INT64\n\
    HAVE_SQLITE3_RESULT_NULL\n\
    HAVE_SQLITE3_RESULT_TEXT\n\
    HAVE_SQLITE3_RESULT_TEXT16\n\
    HAVE_SQLITE3_RESULT_TEXT16LE\n\
    HAVE_SQLITE3_RESULT_TEXT16BE\n\
    HAVE_SQLITE3_RESULT_VALUE\n\
    HAVE_SQLITE3_RESULT_ZEROBLOB\n\
    HAVE_SQLITE3_CREATE_COLLATION\n\
    HAVE_SQLITE3_CREATE_COLLATION_V2\n\
    HAVE_SQLITE3_CREATE_COLLATION16\n\
    HAVE_SQLITE3_COLLATION_NEEDED\n\
    HAVE_SQLITE3_COLLATION_NEEDED16\n\
    HAVE_SQLITE3_KEY\n\
    HAVE_SQLITE3_REKEY\n\
    HAVE_SQLITE3_ACTIVATE_SEE\n\
    HAVE_SQLITE3_ACTIVATE_CEROD\n\
    HAVE_SQLITE3_SLEEP\n\
    HAVE_SQLITE3_GET_AUTOCOMMIT\n\
    HAVE_SQLITE3_DB_HANDLE\n\
    HAVE_SQLITE3_DB_FILENAME\n\
    HAVE_SQLITE3_DB_READONLY\n\
    HAVE_SQLITE3_NEXT_STMT\n\
    HAVE_SQLITE3_COMMIT_HOOK\n\
    HAVE_SQLITE3_ROLLBACK_HOOK\n\
    HAVE_SQLITE3_UPDATE_HOOK\n\
    HAVE_SQLITE3_ENABLE_SHARED_CACHE\n\
    HAVE_SQLITE3_RELEASE_MEMORY\n\
    HAVE_SQLITE3_DB_RELEASE_MEMORY\n\
    HAVE_SQLITE3_SOFT_HEAP_LIMIT64\n\
    HAVE_SQLITE3_SOFT_HEAP_LIMIT\n\
    HAVE_SQLITE3_TABLE_COLUMN_METADATA\n\
    HAVE_SQLITE3_LOAD_EXTENSION\n\
    HAVE_SQLITE3_ENABLE_LOAD_EXTENSION\n\
    HAVE_SQLITE3_AUTO_EXTENSION\n\
    HAVE_SQLITE3_RESET_AUTO_EXTENSION\n\
    HAVE_SQLITE3_CREATE_MODULE\n\
    HAVE_SQLITE3_CREATE_MODULE_V2\n\
    HAVE_SQLITE3_DECLARE_VTAB\n\
    HAVE_SQLITE3_OVERLOAD_FUNCTION\n\
    HAVE_SQLITE3_BLOB_OPEN\n\
    HAVE_SQLITE3_BLOB_REOPEN\n\
    HAVE_SQLITE3_BLOB_CLOSE\n\
    HAVE_SQLITE3_BLOB_BYTES\n\
    HAVE_SQLITE3_BLOB_READ\n\
    HAVE_SQLITE3_BLOB_WRITE\n\
    HAVE_SQLITE3_VFS_FIND\n\
    HAVE_SQLITE3_VFS_REGISTER\n\
    HAVE_SQLITE3_VFS_UNREGISTER\n\
    HAVE_SQLITE3_MUTEX_ALLOC\n\
    HAVE_SQLITE3_MUTEX_FREE\n\
    HAVE_SQLITE3_MUTEX_ENTER\n\
    HAVE_SQLITE3_MUTEX_TRY\n\
    HAVE_SQLITE3_MUTEX_LEAVE\n\
    HAVE_SQLITE3_MUTEX_HELD\n\
    HAVE_SQLITE3_MUTEX_NOTHELD\n\
    HAVE_SQLITE3_DB_MUTEX\n\
    HAVE_SQLITE3_FILE_CONTROL\n\
    HAVE_SQLITE3_TEST_CONTROL\n\
    HAVE_SQLITE3_STATUS\n\
    HAVE_SQLITE3_DB_STATUS\n\
    HAVE_SQLITE3_STMT_STATUS\n\
    HAVE_SQLITE3_BACKUP_INIT\n\
    HAVE_SQLITE3_BACKUP_STEP\n\
    HAVE_SQLITE3_BACKUP_FINISH\n\
    HAVE_SQLITE3_BACKUP_REMAINING\n\
    HAVE_SQLITE3_BACKUP_PAGECOUNT\n\
    HAVE_SQLITE3_UNLOCK_NOTIFY\n\
    HAVE_SQLITE3_STRICMP\n\
    HAVE_SQLITE3_STRNICMP\n\
    HAVE_SQLITE3_LOG\n\
    HAVE_SQLITE3_WAL_HOOK\n\
    HAVE_SQLITE3_WAL_AUTOCHECKPOINT\n\
    HAVE_SQLITE3_WAL_CHECKPOINT\n\
    HAVE_SQLITE3_WAL_CHECKPOINT_V2\n\
    HAVE_SQLITE3_VTAB_CONFIG\n\
    HAVE_SQLITE3_VTAB_ON_CONFLICT\n\
    HAVE_SQLITE3_RTREE_GEOMETRY_CALLBACK\n\
    HAVE_SQLITE3_LIBVERSION\n\
    HAVE_SQLITE3_SOURCEID\n\
    HAVE_SQLITE3_LIBVERSION_NUMBER)\n\
  (import (rnrs))\n\
\n\
;;;; helpers\n\
\n\
(define-syntax define-inline-constant\n\
  (syntax-rules ()\n\
    ((_ ?name ?value)\n\
     (define-syntax ?name (identifier-syntax ?value)))))\n\
\n\
\n\
;;;; code\n\n");


printf("(define-inline-constant HAVE_SQLITE3_COMPILEOPTION_USED %s)\n",
#ifdef HAVE_SQLITE3_COMPILEOPTION_USED
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COMPILEOPTION_GET %s)\n",
#ifdef HAVE_SQLITE3_COMPILEOPTION_GET
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_THREADSAFE %s)\n",
#ifdef HAVE_SQLITE3_THREADSAFE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_CLOSE %s)\n",
#ifdef HAVE_SQLITE3_CLOSE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_EXEC %s)\n",
#ifdef HAVE_SQLITE3_EXEC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_INITIALIZE %s)\n",
#ifdef HAVE_SQLITE3_INITIALIZE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_SHUTDOWN %s)\n",
#ifdef HAVE_SQLITE3_SHUTDOWN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_OS_INIT %s)\n",
#ifdef HAVE_SQLITE3_OS_INIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_OS_END %s)\n",
#ifdef HAVE_SQLITE3_OS_END
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_CONFIG %s)\n",
#ifdef HAVE_SQLITE3_CONFIG
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_DB_CONFIG %s)\n",
#ifdef HAVE_SQLITE3_DB_CONFIG
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_EXTENDED_RESULT_CODES %s)\n",
#ifdef HAVE_SQLITE3_EXTENDED_RESULT_CODES
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_LAST_INSERT_ROWID %s)\n",
#ifdef HAVE_SQLITE3_LAST_INSERT_ROWID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_CHANGES %s)\n",
#ifdef HAVE_SQLITE3_CHANGES
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_TOTAL_CHANGES %s)\n",
#ifdef HAVE_SQLITE3_TOTAL_CHANGES
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_INTERRUPT %s)\n",
#ifdef HAVE_SQLITE3_INTERRUPT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COMPLETE %s)\n",
#ifdef HAVE_SQLITE3_COMPLETE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COMPLETE16 %s)\n",
#ifdef HAVE_SQLITE3_COMPLETE16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BUSY_HANDLER %s)\n",
#ifdef HAVE_SQLITE3_BUSY_HANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BUSY_TIMEOUT %s)\n",
#ifdef HAVE_SQLITE3_BUSY_TIMEOUT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_GET_TABLE %s)\n",
#ifdef HAVE_SQLITE3_GET_TABLE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_FREE_TABLE %s)\n",
#ifdef HAVE_SQLITE3_FREE_TABLE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_MEMORY_USED %s)\n",
#ifdef HAVE_SQLITE3_MEMORY_USED
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_MEMORY_HIGHWATER %s)\n",
#ifdef HAVE_SQLITE3_MEMORY_HIGHWATER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RANDOMNESS %s)\n",
#ifdef HAVE_SQLITE3_RANDOMNESS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_SET_AUTHORIZER %s)\n",
#ifdef HAVE_SQLITE3_SET_AUTHORIZER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_TRACE %s)\n",
#ifdef HAVE_SQLITE3_TRACE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_PROFILE %s)\n",
#ifdef HAVE_SQLITE3_PROFILE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_PROGRESS_HANDLER %s)\n",
#ifdef HAVE_SQLITE3_PROGRESS_HANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_OPEN %s)\n",
#ifdef HAVE_SQLITE3_OPEN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_OPEN16 %s)\n",
#ifdef HAVE_SQLITE3_OPEN16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_OPEN_V2 %s)\n",
#ifdef HAVE_SQLITE3_OPEN_V2
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_URI_PARAMETER %s)\n",
#ifdef HAVE_SQLITE3_URI_PARAMETER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_URI_BOOLEAN %s)\n",
#ifdef HAVE_SQLITE3_URI_BOOLEAN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_URI_INT64 %s)\n",
#ifdef HAVE_SQLITE3_URI_INT64
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_ERRCODE %s)\n",
#ifdef HAVE_SQLITE3_ERRCODE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_EXTENDED_ERRCODE %s)\n",
#ifdef HAVE_SQLITE3_EXTENDED_ERRCODE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_ERRMSG %s)\n",
#ifdef HAVE_SQLITE3_ERRMSG
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_ERRMSG16 %s)\n",
#ifdef HAVE_SQLITE3_ERRMSG16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_LIMIT %s)\n",
#ifdef HAVE_SQLITE3_LIMIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_PREPARE %s)\n",
#ifdef HAVE_SQLITE3_PREPARE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_PREPARE_V2 %s)\n",
#ifdef HAVE_SQLITE3_PREPARE_V2
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_PREPARE16 %s)\n",
#ifdef HAVE_SQLITE3_PREPARE16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_PREPARE16_V2 %s)\n",
#ifdef HAVE_SQLITE3_PREPARE16_V2
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_SQL %s)\n",
#ifdef HAVE_SQLITE3_SQL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_STMT_READONLY %s)\n",
#ifdef HAVE_SQLITE3_STMT_READONLY
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_STMT_BUSY %s)\n",
#ifdef HAVE_SQLITE3_STMT_BUSY
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BIND_BLOB %s)\n",
#ifdef HAVE_SQLITE3_BIND_BLOB
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BIND_DOUBLE %s)\n",
#ifdef HAVE_SQLITE3_BIND_DOUBLE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BIND_INT %s)\n",
#ifdef HAVE_SQLITE3_BIND_INT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BIND_INT64 %s)\n",
#ifdef HAVE_SQLITE3_BIND_INT64
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BIND_NULL %s)\n",
#ifdef HAVE_SQLITE3_BIND_NULL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BIND_TEXT %s)\n",
#ifdef HAVE_SQLITE3_BIND_TEXT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BIND_TEXT16 %s)\n",
#ifdef HAVE_SQLITE3_BIND_TEXT16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BIND_VALUE %s)\n",
#ifdef HAVE_SQLITE3_BIND_VALUE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BIND_ZEROBLOB %s)\n",
#ifdef HAVE_SQLITE3_BIND_ZEROBLOB
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BIND_PARAMETER_COUNT %s)\n",
#ifdef HAVE_SQLITE3_BIND_PARAMETER_COUNT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BIND_PARAMETER_NAME %s)\n",
#ifdef HAVE_SQLITE3_BIND_PARAMETER_NAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BIND_PARAMETER_INDEX %s)\n",
#ifdef HAVE_SQLITE3_BIND_PARAMETER_INDEX
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_CLEAR_BINDINGS %s)\n",
#ifdef HAVE_SQLITE3_CLEAR_BINDINGS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_COUNT %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_COUNT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_NAME %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_NAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_NAME16 %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_NAME16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_DATABASE_NAME %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_DATABASE_NAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_DATABASE_NAME16 %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_DATABASE_NAME16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_TABLE_NAME %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_TABLE_NAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_TABLE_NAME16 %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_TABLE_NAME16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_ORIGIN_NAME %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_ORIGIN_NAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_ORIGIN_NAME16 %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_ORIGIN_NAME16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_DECLTYPE %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_DECLTYPE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_DECLTYPE16 %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_DECLTYPE16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_STEP %s)\n",
#ifdef HAVE_SQLITE3_STEP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_DATA_COUNT %s)\n",
#ifdef HAVE_SQLITE3_DATA_COUNT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_BLOB %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_BLOB
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_BYTES %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_BYTES
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_BYTES16 %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_BYTES16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_DOUBLE %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_DOUBLE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_INT %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_INT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_INT64 %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_INT64
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_TEXT %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_TEXT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_TEXT16 %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_TEXT16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_TYPE %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_TYPE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLUMN_VALUE %s)\n",
#ifdef HAVE_SQLITE3_COLUMN_VALUE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_FINALIZE %s)\n",
#ifdef HAVE_SQLITE3_FINALIZE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESET %s)\n",
#ifdef HAVE_SQLITE3_RESET
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_CREATE_FUNCTION %s)\n",
#ifdef HAVE_SQLITE3_CREATE_FUNCTION
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_CREATE_FUNCTION16 %s)\n",
#ifdef HAVE_SQLITE3_CREATE_FUNCTION16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_CREATE_FUNCTION_V2 %s)\n",
#ifdef HAVE_SQLITE3_CREATE_FUNCTION_V2
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VALUE_BLOB %s)\n",
#ifdef HAVE_SQLITE3_VALUE_BLOB
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VALUE_BYTES %s)\n",
#ifdef HAVE_SQLITE3_VALUE_BYTES
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VALUE_BYTES16 %s)\n",
#ifdef HAVE_SQLITE3_VALUE_BYTES16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VALUE_DOUBLE %s)\n",
#ifdef HAVE_SQLITE3_VALUE_DOUBLE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VALUE_INT %s)\n",
#ifdef HAVE_SQLITE3_VALUE_INT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VALUE_INT64 %s)\n",
#ifdef HAVE_SQLITE3_VALUE_INT64
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VALUE_TEXT %s)\n",
#ifdef HAVE_SQLITE3_VALUE_TEXT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VALUE_TEXT16 %s)\n",
#ifdef HAVE_SQLITE3_VALUE_TEXT16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VALUE_TEXT16LE %s)\n",
#ifdef HAVE_SQLITE3_VALUE_TEXT16LE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VALUE_TEXT16BE %s)\n",
#ifdef HAVE_SQLITE3_VALUE_TEXT16BE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VALUE_TYPE %s)\n",
#ifdef HAVE_SQLITE3_VALUE_TYPE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VALUE_NUMERIC_TYPE %s)\n",
#ifdef HAVE_SQLITE3_VALUE_NUMERIC_TYPE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_AGGREGATE_CONTEXT %s)\n",
#ifdef HAVE_SQLITE3_AGGREGATE_CONTEXT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_USER_DATA %s)\n",
#ifdef HAVE_SQLITE3_USER_DATA
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_CONTEXT_DB_HANDLE %s)\n",
#ifdef HAVE_SQLITE3_CONTEXT_DB_HANDLE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_GET_AUXDATA %s)\n",
#ifdef HAVE_SQLITE3_GET_AUXDATA
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_SET_AUXDATA %s)\n",
#ifdef HAVE_SQLITE3_SET_AUXDATA
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_BLOB %s)\n",
#ifdef HAVE_SQLITE3_RESULT_BLOB
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_DOUBLE %s)\n",
#ifdef HAVE_SQLITE3_RESULT_DOUBLE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_ERROR %s)\n",
#ifdef HAVE_SQLITE3_RESULT_ERROR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_ERROR16 %s)\n",
#ifdef HAVE_SQLITE3_RESULT_ERROR16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_ERROR_TOOBIG %s)\n",
#ifdef HAVE_SQLITE3_RESULT_ERROR_TOOBIG
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_ERROR_NOMEM %s)\n",
#ifdef HAVE_SQLITE3_RESULT_ERROR_NOMEM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_ERROR_CODE %s)\n",
#ifdef HAVE_SQLITE3_RESULT_ERROR_CODE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_INT %s)\n",
#ifdef HAVE_SQLITE3_RESULT_INT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_INT64 %s)\n",
#ifdef HAVE_SQLITE3_RESULT_INT64
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_NULL %s)\n",
#ifdef HAVE_SQLITE3_RESULT_NULL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_TEXT %s)\n",
#ifdef HAVE_SQLITE3_RESULT_TEXT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_TEXT16 %s)\n",
#ifdef HAVE_SQLITE3_RESULT_TEXT16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_TEXT16LE %s)\n",
#ifdef HAVE_SQLITE3_RESULT_TEXT16LE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_TEXT16BE %s)\n",
#ifdef HAVE_SQLITE3_RESULT_TEXT16BE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_VALUE %s)\n",
#ifdef HAVE_SQLITE3_RESULT_VALUE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESULT_ZEROBLOB %s)\n",
#ifdef HAVE_SQLITE3_RESULT_ZEROBLOB
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_CREATE_COLLATION %s)\n",
#ifdef HAVE_SQLITE3_CREATE_COLLATION
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_CREATE_COLLATION_V2 %s)\n",
#ifdef HAVE_SQLITE3_CREATE_COLLATION_V2
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_CREATE_COLLATION16 %s)\n",
#ifdef HAVE_SQLITE3_CREATE_COLLATION16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLLATION_NEEDED %s)\n",
#ifdef HAVE_SQLITE3_COLLATION_NEEDED
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COLLATION_NEEDED16 %s)\n",
#ifdef HAVE_SQLITE3_COLLATION_NEEDED16
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_KEY %s)\n",
#ifdef HAVE_SQLITE3_KEY
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_REKEY %s)\n",
#ifdef HAVE_SQLITE3_REKEY
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_ACTIVATE_SEE %s)\n",
#ifdef HAVE_SQLITE3_ACTIVATE_SEE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_ACTIVATE_CEROD %s)\n",
#ifdef HAVE_SQLITE3_ACTIVATE_CEROD
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_SLEEP %s)\n",
#ifdef HAVE_SQLITE3_SLEEP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_GET_AUTOCOMMIT %s)\n",
#ifdef HAVE_SQLITE3_GET_AUTOCOMMIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_DB_HANDLE %s)\n",
#ifdef HAVE_SQLITE3_DB_HANDLE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_DB_FILENAME %s)\n",
#ifdef HAVE_SQLITE3_DB_FILENAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_DB_READONLY %s)\n",
#ifdef HAVE_SQLITE3_DB_READONLY
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_NEXT_STMT %s)\n",
#ifdef HAVE_SQLITE3_NEXT_STMT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_COMMIT_HOOK %s)\n",
#ifdef HAVE_SQLITE3_COMMIT_HOOK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_ROLLBACK_HOOK %s)\n",
#ifdef HAVE_SQLITE3_ROLLBACK_HOOK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_UPDATE_HOOK %s)\n",
#ifdef HAVE_SQLITE3_UPDATE_HOOK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_ENABLE_SHARED_CACHE %s)\n",
#ifdef HAVE_SQLITE3_ENABLE_SHARED_CACHE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RELEASE_MEMORY %s)\n",
#ifdef HAVE_SQLITE3_RELEASE_MEMORY
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_DB_RELEASE_MEMORY %s)\n",
#ifdef HAVE_SQLITE3_DB_RELEASE_MEMORY
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_SOFT_HEAP_LIMIT64 %s)\n",
#ifdef HAVE_SQLITE3_SOFT_HEAP_LIMIT64
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_SOFT_HEAP_LIMIT %s)\n",
#ifdef HAVE_SQLITE3_SOFT_HEAP_LIMIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_TABLE_COLUMN_METADATA %s)\n",
#ifdef HAVE_SQLITE3_TABLE_COLUMN_METADATA
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_LOAD_EXTENSION %s)\n",
#ifdef HAVE_SQLITE3_LOAD_EXTENSION
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_ENABLE_LOAD_EXTENSION %s)\n",
#ifdef HAVE_SQLITE3_ENABLE_LOAD_EXTENSION
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_AUTO_EXTENSION %s)\n",
#ifdef HAVE_SQLITE3_AUTO_EXTENSION
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RESET_AUTO_EXTENSION %s)\n",
#ifdef HAVE_SQLITE3_RESET_AUTO_EXTENSION
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_CREATE_MODULE %s)\n",
#ifdef HAVE_SQLITE3_CREATE_MODULE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_CREATE_MODULE_V2 %s)\n",
#ifdef HAVE_SQLITE3_CREATE_MODULE_V2
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_DECLARE_VTAB %s)\n",
#ifdef HAVE_SQLITE3_DECLARE_VTAB
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_OVERLOAD_FUNCTION %s)\n",
#ifdef HAVE_SQLITE3_OVERLOAD_FUNCTION
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BLOB_OPEN %s)\n",
#ifdef HAVE_SQLITE3_BLOB_OPEN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BLOB_REOPEN %s)\n",
#ifdef HAVE_SQLITE3_BLOB_REOPEN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BLOB_CLOSE %s)\n",
#ifdef HAVE_SQLITE3_BLOB_CLOSE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BLOB_BYTES %s)\n",
#ifdef HAVE_SQLITE3_BLOB_BYTES
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BLOB_READ %s)\n",
#ifdef HAVE_SQLITE3_BLOB_READ
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BLOB_WRITE %s)\n",
#ifdef HAVE_SQLITE3_BLOB_WRITE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VFS_FIND %s)\n",
#ifdef HAVE_SQLITE3_VFS_FIND
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VFS_REGISTER %s)\n",
#ifdef HAVE_SQLITE3_VFS_REGISTER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VFS_UNREGISTER %s)\n",
#ifdef HAVE_SQLITE3_VFS_UNREGISTER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_MUTEX_ALLOC %s)\n",
#ifdef HAVE_SQLITE3_MUTEX_ALLOC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_MUTEX_FREE %s)\n",
#ifdef HAVE_SQLITE3_MUTEX_FREE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_MUTEX_ENTER %s)\n",
#ifdef HAVE_SQLITE3_MUTEX_ENTER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_MUTEX_TRY %s)\n",
#ifdef HAVE_SQLITE3_MUTEX_TRY
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_MUTEX_LEAVE %s)\n",
#ifdef HAVE_SQLITE3_MUTEX_LEAVE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_MUTEX_HELD %s)\n",
#ifdef HAVE_SQLITE3_MUTEX_HELD
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_MUTEX_NOTHELD %s)\n",
#ifdef HAVE_SQLITE3_MUTEX_NOTHELD
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_DB_MUTEX %s)\n",
#ifdef HAVE_SQLITE3_DB_MUTEX
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_FILE_CONTROL %s)\n",
#ifdef HAVE_SQLITE3_FILE_CONTROL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_TEST_CONTROL %s)\n",
#ifdef HAVE_SQLITE3_TEST_CONTROL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_STATUS %s)\n",
#ifdef HAVE_SQLITE3_STATUS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_DB_STATUS %s)\n",
#ifdef HAVE_SQLITE3_DB_STATUS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_STMT_STATUS %s)\n",
#ifdef HAVE_SQLITE3_STMT_STATUS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BACKUP_INIT %s)\n",
#ifdef HAVE_SQLITE3_BACKUP_INIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BACKUP_STEP %s)\n",
#ifdef HAVE_SQLITE3_BACKUP_STEP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BACKUP_FINISH %s)\n",
#ifdef HAVE_SQLITE3_BACKUP_FINISH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BACKUP_REMAINING %s)\n",
#ifdef HAVE_SQLITE3_BACKUP_REMAINING
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_BACKUP_PAGECOUNT %s)\n",
#ifdef HAVE_SQLITE3_BACKUP_PAGECOUNT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_UNLOCK_NOTIFY %s)\n",
#ifdef HAVE_SQLITE3_UNLOCK_NOTIFY
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_STRICMP %s)\n",
#ifdef HAVE_SQLITE3_STRICMP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_STRNICMP %s)\n",
#ifdef HAVE_SQLITE3_STRNICMP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_LOG %s)\n",
#ifdef HAVE_SQLITE3_LOG
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_WAL_HOOK %s)\n",
#ifdef HAVE_SQLITE3_WAL_HOOK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_WAL_AUTOCHECKPOINT %s)\n",
#ifdef HAVE_SQLITE3_WAL_AUTOCHECKPOINT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_WAL_CHECKPOINT %s)\n",
#ifdef HAVE_SQLITE3_WAL_CHECKPOINT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_WAL_CHECKPOINT_V2 %s)\n",
#ifdef HAVE_SQLITE3_WAL_CHECKPOINT_V2
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VTAB_CONFIG %s)\n",
#ifdef HAVE_SQLITE3_VTAB_CONFIG
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_VTAB_ON_CONFLICT %s)\n",
#ifdef HAVE_SQLITE3_VTAB_ON_CONFLICT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_RTREE_GEOMETRY_CALLBACK %s)\n",
#ifdef HAVE_SQLITE3_RTREE_GEOMETRY_CALLBACK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_LIBVERSION %s)\n",
#ifdef HAVE_SQLITE3_LIBVERSION
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_SOURCEID %s)\n",
#ifdef HAVE_SQLITE3_SOURCEID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SQLITE3_LIBVERSION_NUMBER %s)\n",
#ifdef HAVE_SQLITE3_LIBVERSION_NUMBER
  "#t"
#else
  "#f"
#endif
  );


  printf("\n\
;;;; done\n\
\n\
)\n\
\n\
;;; end of file\n");
  exit(EXIT_SUCCESS);
}

/* end of file */
