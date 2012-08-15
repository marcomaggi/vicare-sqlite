dnl @configure_input@
dnl

AC_PREREQ([2.68])
AC_INIT([Vicare/SQLite],[0.1d0],[marco.maggi-ipsu@poste.it],
  [vicare-sqlite],[http://marcomaggi.github.com/vicare.html])
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([-Wall])
dnl AM_MAINTAINER_MODE
AC_CONFIG_SRCDIR([src/])
AC_CONFIG_MACRO_DIR([m4])
AC_CANONICAL_HOST

VICARE_DEBUG=no
AC_MSG_CHECKING([whether debugging features are included in compiled Scheme libraries])
AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],
     [enable compilation of Scheme libraries with debugging features (default is disable)])],
  [AC_MSG_RESULT([$enableval])
  if test "z$enableval" = zno
  then VICARE_DEBUG=no
  else VICARE_DEBUG=yes
  fi],
  [VICARE_DEBUG=no
   AC_MSG_RESULT([no])])
AM_CONDITIONAL([WANT_VICARE_DEBUG],[test x$VICARE_DEBUG = xyes])

#page
#### compiler

AC_PROG_CC
AC_PROG_CC_C_O

AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

LT_PREREQ([2.4])
LT_INIT([shared disable-static])

dnl AC_LANG([C])
dnl AC_DEFINE([_GNU_SOURCE],[1],[enable everything GNU])

#page
#### vicare configuration

AC_CHECK_HEADERS([vicare.h],,[AC_MSG_FAILURE([missing vicare.h header],[2])])
AC_CHECK_PROG([VICARE],[vicare],[vicare],[:])

#page
#### sqlite: configuration

AC_CHECK_HEADERS([sqlite3.h],,[AC_MSG_FAILURE([cannot find SQLite main header],[2])])
AC_CHECK_HEADERS([sqlite3ext.h],,[AC_MSG_FAILURE([cannot find SQLite extension header],[2])])
AC_CHECK_LIB([sqlite3],[sqlite3_version],,[AC_MSG_FAILURE([cannot find SQLite library],[2])])

vicare_sqlite3_VERSION_INTERFACE_CURRENT=0
vicare_sqlite3_VERSION_INTERFACE_REVISION=0
vicare_sqlite3_VERSION_INTERFACE_AGE=0
AC_DEFINE_UNQUOTED([vicare_sqlite3_VERSION_INTERFACE_CURRENT],
  [$vicare_sqlite3_VERSION_INTERFACE_CURRENT],
  [current interface number])
AC_DEFINE_UNQUOTED([vicare_sqlite3_VERSION_INTERFACE_REVISION],
  [$vicare_sqlite3_VERSION_INTERFACE_REVISION],
  [current interface implementation number])
AC_DEFINE_UNQUOTED([vicare_sqlite3_VERSION_INTERFACE_AGE],
  [$vicare_sqlite3_VERSION_INTERFACE_AGE],
  [current interface age number])
AC_DEFINE_UNQUOTED([vicare_sqlite3_VERSION_INTERFACE_STRING],
  ["$vicare_sqlite3_VERSION_INTERFACE_CURRENT.$vicare_sqlite3_VERSION_INTERFACE_REVISION"],
  [library interface version])
AC_SUBST([vicare_sqlite3_VERSION_INTERFACE_CURRENT])
AC_SUBST([vicare_sqlite3_VERSION_INTERFACE_REVISION])
AC_SUBST([vicare_sqlite3_VERSION_INTERFACE_AGE])

#page
#### sqlite: functions

AC_CHECK_FUNCS([sqlite3_libversion sqlite3_libversion_number sqlite3_sourceid])
AC_CHECK_FUNCS([sqlite3_compileoption_used sqlite3_compileoption_get])
AC_CHECK_FUNCS([sqlite3_threadsafe])
AC_CHECK_FUNCS([sqlite3_close sqlite3_exec sqlite3_initialize sqlite3_shutdown sqlite3_os_init sqlite3_os_end sqlite3_config sqlite3_db_config sqlite3_extended_result_codes sqlite3_last_insert_rowid sqlite3_changes sqlite3_total_changes sqlite3_interrupt sqlite3_complete sqlite3_complete16  sqlite3_busy_handler sqlite3_busy_timeout sqlite3_get_table sqlite3_free_table])
dnl AC_CHECK_FUNCS([sqlite3_mprintf sqlite3_vmprintf sqlite3_snprintf sqlite3_vsnprintf])
dnl AC_CHECK_FUNCS([sqlite3_malloc sqlite3_realloc void sqlite3_free])
AC_CHECK_FUNCS([sqlite3_memory_used sqlite3_memory_highwater])
AC_CHECK_FUNCS([sqlite3_randomness sqlite3_set_authorizer sqlite3_trace sqlite3_profile sqlite3_progress_handler sqlite3_open sqlite3_open16 sqlite3_open_v2 sqlite3_uri_parameter sqlite3_uri_boolean sqlite3_uri_int64 sqlite3_errcode sqlite3_extended_errcode sqlite3_errmsg sqlite3_errmsg16 sqlite3_limit sqlite3_prepare sqlite3_prepare_v2 sqlite3_prepare16 sqlite3_prepare16_v2 sqlite3_sql sqlite3_stmt_readonly sqlite3_stmt_busy])
AC_CHECK_FUNCS([sqlite3_bind_blob sqlite3_bind_double sqlite3_bind_int sqlite3_bind_int64 sqlite3_bind_null sqlite3_bind_text sqlite3_bind_text16 sqlite3_bind_value sqlite3_bind_zeroblob])
AC_CHECK_FUNCS([sqlite3_bind_parameter_count sqlite3_bind_parameter_name sqlite3_bind_parameter_index sqlite3_clear_bindings sqlite3_column_count sqlite3_column_name sqlite3_column_name16 sqlite3_column_database_name sqlite3_column_database_name16 sqlite3_column_table_name sqlite3_column_table_name16 sqlite3_column_origin_name sqlite3_column_origin_name16 sqlite3_column_decltype sqlite3_column_decltype16 sqlite3_step sqlite3_data_count])
AC_CHECK_FUNCS([sqlite3_column_blob sqlite3_column_bytes sqlite3_column_bytes16 sqlite3_column_double sqlite3_column_int sqlite3_column_int64 sqlite3_column_text sqlite3_column_text16 sqlite3_column_type sqlite3_column_value sqlite3_finalize sqlite3_reset sqlite3_create_function sqlite3_create_function16 sqlite3_create_function_v2])
AC_CHECK_FUNCS([sqlite3_value_blob sqlite3_value_bytes sqlite3_value_bytes16 sqlite3_value_double sqlite3_value_int sqlite3_value_int64 sqlite3_value_text sqlite3_value_text16 sqlite3_value_text16le sqlite3_value_text16be sqlite3_value_type sqlite3_value_numeric_type sqlite3_aggregate_context sqlite3_user_data sqlite3_context_db_handle sqlite3_get_auxdata sqlite3_set_auxdata])
AC_CHECK_FUNCS([sqlite3_result_blob sqlite3_result_double sqlite3_result_error sqlite3_result_error16 sqlite3_result_error_toobig sqlite3_result_error_nomem sqlite3_result_error_code sqlite3_result_int sqlite3_result_int64 sqlite3_result_null sqlite3_result_text sqlite3_result_text16 sqlite3_result_text16le sqlite3_result_text16be sqlite3_result_value sqlite3_result_zeroblob])
AC_CHECK_FUNCS([sqlite3_create_collation sqlite3_create_collation_v2 sqlite3_create_collation16 sqlite3_collation_needed sqlite3_collation_needed16])
AC_CHECK_FUNCS([sqlite3_key sqlite3_rekey sqlite3_activate_see])
AC_CHECK_FUNCS([sqlite3_activate_cerod])
AC_CHECK_FUNCS([sqlite3_sleep sqlite3_get_autocommit sqlite3_db_handle sqlite3_db_filename sqlite3_db_readonly sqlite3_next_stmt sqlite3_commit_hook sqlite3_rollback_hook sqlite3_update_hook sqlite3_enable_shared_cache sqlite3_release_memory sqlite3_db_release_memory sqlite3_soft_heap_limit64 sqlite3_soft_heap_limit sqlite3_table_column_metadata sqlite3_load_extension sqlite3_enable_load_extension sqlite3_auto_extension sqlite3_reset_auto_extension sqlite3_create_module sqlite3_create_module_v2 sqlite3_declare_vtab sqlite3_overload_function])
AC_CHECK_FUNCS([sqlite3_blob_open sqlite3_blob_reopen sqlite3_blob_close sqlite3_blob_bytes sqlite3_blob_read sqlite3_blob_write sqlite3_vfs_find sqlite3_vfs_register sqlite3_vfs_unregister])
AC_CHECK_FUNCS([sqlite3_mutex_alloc sqlite3_mutex_free sqlite3_mutex_enter sqlite3_mutex_try sqlite3_mutex_leave sqlite3_mutex_held sqlite3_mutex_notheld sqlite3_db_mutex])
AC_CHECK_FUNCS([sqlite3_file_control sqlite3_test_control sqlite3_status sqlite3_db_status sqlite3_stmt_status])
AC_CHECK_FUNCS([sqlite3_backup_init sqlite3_backup_step sqlite3_backup_finish sqlite3_backup_remaining sqlite3_backup_pagecount])
AC_CHECK_FUNCS([sqlite3_unlock_notify])
AC_CHECK_FUNCS([sqlite3_stricmp sqlite3_strnicmp])
AC_CHECK_FUNCS([sqlite3_log sqlite3_wal_hook sqlite3_wal_autocheckpoint sqlite3_wal_checkpoint sqlite3_wal_checkpoint_v2 sqlite3_vtab_config sqlite3_vtab_on_conflict])
AC_CHECK_FUNCS([sqlite3_rtree_geometry_callback])

#page
#### sqlite: symbols

m4_define([VICARE_INCLUDES],[
AC_INCLUDES_DEFAULT
#ifdef HAVE_VICARE_H
#  include <vicare.h>
#endif
#ifdef HAVE_SQLITE3_H
#  include <sqlite3.h>
#endif
#ifdef HAVE_SQLITE3EXT_H
#  include <sqlite3ext.h>
#endif
])

dnl Miscellaneous string constants
VICARE_STRING_CONSTANT_TESTS([SQLITE_VERSION SQLITE_SOURCE_ID])

dnl Miscellaneous integer constants
VICARE_CONSTANT_TESTS([SQLITE_VERSION_NUMBER])

dnl Result Codes
VICARE_CONSTANT_TESTS([SQLITE_OK SQLITE_ERROR SQLITE_INTERNAL SQLITE_PERM
  SQLITE_ABORT SQLITE_BUSY SQLITE_LOCKED SQLITE_NOMEM SQLITE_READONLY
  SQLITE_INTERRUPT SQLITE_IOERR SQLITE_CORRUPT SQLITE_NOTFOUND SQLITE_FULL
  SQLITE_CANTOPEN SQLITE_PROTOCOL SQLITE_EMPTY SQLITE_SCHEMA SQLITE_TOOBIG
  SQLITE_CONSTRAINT SQLITE_MISMATCH SQLITE_MISUSE SQLITE_NOLFS SQLITE_AUTH
  SQLITE_FORMAT SQLITE_RANGE SQLITE_NOTADB SQLITE_ROW SQLITE_DONE])

dnl Extended Result Codes
VICARE_CONSTANT_TESTS([SQLITE_IOERR_READ SQLITE_IOERR_SHORT_READ
  SQLITE_IOERR_WRITE SQLITE_IOERR_FSYNC SQLITE_IOERR_DIR_FSYNC
  SQLITE_IOERR_TRUNCATE SQLITE_IOERR_FSTAT SQLITE_IOERR_UNLOCK
  SQLITE_IOERR_RDLOCK SQLITE_IOERR_DELETE SQLITE_IOERR_BLOCKED
  SQLITE_IOERR_NOMEM SQLITE_IOERR_ACCESS SQLITE_IOERR_CHECKRESERVEDLOCK
  SQLITE_IOERR_LOCK SQLITE_IOERR_CLOSE SQLITE_IOERR_DIR_CLOSE
  SQLITE_IOERR_SHMOPEN SQLITE_IOERR_SHMSIZE SQLITE_IOERR_SHMLOCK
  SQLITE_IOERR_SHMMAP SQLITE_IOERR_SEEK SQLITE_LOCKED_SHAREDCACHE
  SQLITE_BUSY_RECOVERY SQLITE_CANTOPEN_NOTEMPDIR SQLITE_CANTOPEN_ISDIR
  SQLITE_CORRUPT_VTAB SQLITE_READONLY_RECOVERY SQLITE_READONLY_CANTLOCK
  SQLITE_ABORT_ROLLBACK])

dnl Flags For File Open Operations
VICARE_CONSTANT_TESTS([SQLITE_OPEN_READONLY SQLITE_OPEN_READWRITE
  SQLITE_OPEN_CREATE SQLITE_OPEN_DELETEONCLOSE SQLITE_OPEN_EXCLUSIVE
  SQLITE_OPEN_AUTOPROXY SQLITE_OPEN_URI SQLITE_OPEN_MEMORY
  SQLITE_OPEN_MAIN_DB SQLITE_OPEN_TEMP_DB SQLITE_OPEN_TRANSIENT_DB
  SQLITE_OPEN_MAIN_JOURNAL SQLITE_OPEN_TEMP_JOURNAL SQLITE_OPEN_SUBJOURNAL
  SQLITE_OPEN_MASTER_JOURNAL SQLITE_OPEN_NOMUTEX SQLITE_OPEN_FULLMUTEX
  SQLITE_OPEN_SHAREDCACHE SQLITE_OPEN_PRIVATECACHE SQLITE_OPEN_WAL])

dnl Device Characteristics
VICARE_CONSTANT_TESTS([SQLITE_IOCAP_ATOMIC SQLITE_IOCAP_ATOMIC512
  SQLITE_IOCAP_ATOMIC1K SQLITE_IOCAP_ATOMIC2K SQLITE_IOCAP_ATOMIC4K
  SQLITE_IOCAP_ATOMIC8K SQLITE_IOCAP_ATOMIC16K SQLITE_IOCAP_ATOMIC32K
  SQLITE_IOCAP_ATOMIC64K SQLITE_IOCAP_SAFE_APPEND SQLITE_IOCAP_SEQUENTIAL
  SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN SQLITE_IOCAP_POWERSAFE_OVERWRITE])

dnl File Locking Levels
VICARE_CONSTANT_TESTS([SQLITE_LOCK_NONE SQLITE_LOCK_SHARED
  SQLITE_LOCK_RESERVED SQLITE_LOCK_PENDING SQLITE_LOCK_EXCLUSIVE])

dnl Synchronization Type Flags
VICARE_CONSTANT_TESTS([SQLITE_SYNC_NORMAL SQLITE_SYNC_FULL SQLITE_SYNC_DATAONLY])

dnl Standard File Control Opcodes
VICARE_CONSTANT_TESTS([SQLITE_FCNTL_LOCKSTATE SQLITE_GET_LOCKPROXYFILE
  SQLITE_SET_LOCKPROXYFILE SQLITE_LAST_ERRNO SQLITE_FCNTL_SIZE_HINT
  SQLITE_FCNTL_CHUNK_SIZE SQLITE_FCNTL_FILE_POINTER SQLITE_FCNTL_SYNC_OMITTED
  SQLITE_FCNTL_WIN32_AV_RETRY SQLITE_FCNTL_PERSIST_WAL SQLITE_FCNTL_OVERWRITE
  SQLITE_FCNTL_VFSNAME SQLITE_FCNTL_POWERSAFE_OVERWRITE SQLITE_FCNTL_PRAGMA])

dnl Flags for the xAccess VFS method
VICARE_CONSTANT_TESTS([SQLITE_ACCESS_EXISTS SQLITE_ACCESS_READWRITE SQLITE_ACCESS_READ])

dnl Flags for the xShmLock VFS method
VICARE_CONSTANT_TESTS([SQLITE_SHM_UNLOCK SQLITE_SHM_LOCK SQLITE_SHM_SHARED
  SQLITE_SHM_EXCLUSIVE SQLITE_SHM_NLOCK])

dnl Configuration Options
VICARE_CONSTANT_TESTS([SQLITE_CONFIG_SINGLETHREAD SQLITE_CONFIG_MULTITHREAD
  SQLITE_CONFIG_SERIALIZED SQLITE_CONFIG_MALLOC SQLITE_CONFIG_GETMALLOC
  SQLITE_CONFIG_SCRATCH SQLITE_CONFIG_PAGECACHE SQLITE_CONFIG_HEAP
  SQLITE_CONFIG_MEMSTATUS SQLITE_CONFIG_MUTEX SQLITE_CONFIG_GETMUTEX
  SQLITE_CONFIG_LOOKASIDE SQLITE_CONFIG_PCACHE SQLITE_CONFIG_GETPCACHE
  SQLITE_CONFIG_LOG SQLITE_CONFIG_URI SQLITE_CONFIG_PCACHE2
  SQLITE_CONFIG_GETPCACHE2])

dnl Database Connection Configuration Options
VICARE_CONSTANT_TESTS([SQLITE_DBCONFIG_LOOKASIDE SQLITE_DBCONFIG_ENABLE_FKEY
  SQLITE_DBCONFIG_ENABLE_TRIGGER])

dnl Authorizer Return Codes
VICARE_CONSTANT_TESTS([SQLITE_DENY SQLITE_IGNORE])

dnl Authorizer Action Codes
VICARE_CONSTANT_TESTS([SQLITE_CREATE_INDEX SQLITE_CREATE_TABLE SQLITE_CREATE_TEMP_INDEX
  SQLITE_CREATE_TEMP_TABLE SQLITE_CREATE_TEMP_TRIGGER SQLITE_CREATE_TEMP_VIEW
  SQLITE_CREATE_TRIGGER SQLITE_CREATE_VIEW SQLITE_DELETE SQLITE_DROP_INDEX
  SQLITE_DROP_TABLE SQLITE_DROP_TEMP_INDEX SQLITE_DROP_TEMP_TABLE
  SQLITE_DROP_TEMP_TRIGGER SQLITE_DROP_TEMP_VIEW SQLITE_DROP_TRIGGER
  SQLITE_DROP_VIEW SQLITE_INSERT SQLITE_PRAGMA SQLITE_READ SQLITE_SELECT
  SQLITE_TRANSACTION SQLITE_UPDATE SQLITE_ATTACH SQLITE_DETACH SQLITE_ALTER_TABLE
  SQLITE_REINDEX SQLITE_ANALYZE SQLITE_CREATE_VTABLE SQLITE_DROP_VTABLE
  SQLITE_FUNCTION SQLITE_SAVEPOINT SQLITE_COPY])

dnl Run-Time Limit Categories
VICARE_CONSTANT_TESTS([SQLITE_LIMIT_LENGTH SQLITE_LIMIT_SQL_LENGTH
  SQLITE_LIMIT_COLUMN SQLITE_LIMIT_EXPR_DEPTH SQLITE_LIMIT_COMPOUND_SELECT
  SQLITE_LIMIT_VDBE_OP SQLITE_LIMIT_FUNCTION_ARG SQLITE_LIMIT_ATTACHED
  SQLITE_LIMIT_LIKE_PATTERN_LENGTH SQLITE_LIMIT_VARIABLE_NUMBER
  SQLITE_LIMIT_TRIGGER_DEPTH])

dnl Fundamental Datatypes
VICARE_CONSTANT_TESTS([SQLITE_INTEGER SQLITE_FLOAT SQLITE_BLOB SQLITE_NULL
  SQLITE_TEXT SQLITE3_TEXT])

dnl Text Encodings
VICARE_CONSTANT_TESTS([SQLITE_UTF8 SQLITE_UTF16LE SQLITE_UTF16BE SQLITE_UTF16
  SQLITE_ANY SQLITE_UTF16_ALIGNED])

dnl Constants Defining Special Destructor Behavior
VICARE_CONSTANT_TESTS([SQLITE_STATIC SQLITE_TRANSIENT])

dnl Virtual Table Constraint Operator Codes
VICARE_CONSTANT_TESTS([SQLITE_INDEX_CONSTRAINT_EQ SQLITE_INDEX_CONSTRAINT_GT
  SQLITE_INDEX_CONSTRAINT_LE SQLITE_INDEX_CONSTRAINT_LT
  SQLITE_INDEX_CONSTRAINT_GE SQLITE_INDEX_CONSTRAINT_MATCH])

dnl Mutex Types
VICARE_CONSTANT_TESTS([SQLITE_MUTEX_FAST SQLITE_MUTEX_RECURSIVE
  SQLITE_MUTEX_STATIC_MASTER SQLITE_MUTEX_STATIC_MEM SQLITE_MUTEX_STATIC_MEM2
  SQLITE_MUTEX_STATIC_OPEN SQLITE_MUTEX_STATIC_PRNG SQLITE_MUTEX_STATIC_LRU
  SQLITE_MUTEX_STATIC_LRU2 SQLITE_MUTEX_STATIC_PMEM])

dnl Testing Interface Operation Codes
VICARE_CONSTANT_TESTS([SQLITE_TESTCTRL_FIRST SQLITE_TESTCTRL_PRNG_SAVE
  SQLITE_TESTCTRL_PRNG_RESTORE SQLITE_TESTCTRL_PRNG_RESET
  SQLITE_TESTCTRL_BITVEC_TEST SQLITE_TESTCTRL_FAULT_INSTALL
  SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS SQLITE_TESTCTRL_PENDING_BYTE
  SQLITE_TESTCTRL_ASSERT SQLITE_TESTCTRL_ALWAYS SQLITE_TESTCTRL_RESERVE
  SQLITE_TESTCTRL_OPTIMIZATIONS SQLITE_TESTCTRL_ISKEYWORD
  SQLITE_TESTCTRL_SCRATCHMALLOC SQLITE_TESTCTRL_LOCALTIME_FAULT
  SQLITE_TESTCTRL_EXPLAIN_STMT SQLITE_TESTCTRL_LAST])

dnl Status Parameters
VICARE_CONSTANT_TESTS([SQLITE_STATUS_MEMORY_USED SQLITE_STATUS_PAGECACHE_USED
  SQLITE_STATUS_PAGECACHE_OVERFLOW SQLITE_STATUS_SCRATCH_USED
  SQLITE_STATUS_SCRATCH_OVERFLOW SQLITE_STATUS_MALLOC_SIZE
  SQLITE_STATUS_PARSER_STACK SQLITE_STATUS_PAGECACHE_SIZE
  SQLITE_STATUS_SCRATCH_SIZE SQLITE_STATUS_MALLOC_COUNT])

dnl Status Parameters for database connections
VICARE_CONSTANT_TESTS([SQLITE_DBSTATUS_LOOKASIDE_USED SQLITE_DBSTATUS_CACHE_USED
  SQLITE_DBSTATUS_SCHEMA_USED SQLITE_DBSTATUS_STMT_USED
  SQLITE_DBSTATUS_LOOKASIDE_HIT SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE
  SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL SQLITE_DBSTATUS_CACHE_HIT
  SQLITE_DBSTATUS_CACHE_MISS SQLITE_DBSTATUS_CACHE_WRITE SQLITE_DBSTATUS_MAX])

dnl Status Parameters for prepared statements
VICARE_CONSTANT_TESTS([SQLITE_STMTSTATUS_FULLSCAN_STEP
  SQLITE_STMTSTATUS_SORT SQLITE_STMTSTATUS_AUTOINDEX])

dnl Checkpoint operation parameters
VICARE_CONSTANT_TESTS([SQLITE_CHECKPOINT_PASSIVE SQLITE_CHECKPOINT_FULL
  SQLITE_CHECKPOINT_RESTART])

dnl Virtual Table Configuration Options
VICARE_CONSTANT_TESTS([SQLITE_VTAB_CONSTRAINT_SUPPORT])

dnl Conflict resolution modes
VICARE_CONSTANT_TESTS([SQLITE_ROLLBACK SQLITE_IGNORE SQLITE_FAIL
  SQLITE_ABORT SQLITE_REPLACE])

#page
#### finish

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile]
        [src/Makefile]
        [doc/Makefile]
        [tests/Makefile]
        [lib/Makefile]
        [lib/vicare/databases/sqlite3/constants.sls]
        [meta/slackware/doinst.sh])
AC_OUTPUT

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^#page"
# End:
