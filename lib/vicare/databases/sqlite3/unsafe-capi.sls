;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: unsafe interface to the C language API
;;;Date: Mon Jul 23, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare databases sqlite3 unsafe-capi)
  (export

    ;; library initialisation, finalisation, configuration and auxiliary
    sqlite3-initialize			sqlite3-shutdown
    sqlite3-os-init			sqlite3-os-end
    sqlite3-config
    sqlite3-memory-used			sqlite3-memory-highwater

    ;; version functions
    vicare-sqlite3-version-string
    vicare-sqlite3-version-interface-current
    vicare-sqlite3-version-interface-revision
    vicare-sqlite3-version-interface-age
    sqlite3-libversion
    sqlite3-libversion-number
    sqlite3-sourceid

    ;; error codes and error messages
    sqlite3-errcode			sqlite3-extended-errcode
    sqlite3-errmsg			sqlite3-errmsg16

    ;; compiled options
    sqlite3-compileoption-used		sqlite3-compileoption-get
    sqlite3-threadsafe

    ;; connection handling
    sqlite3-open			sqlite3-open16
    sqlite3-open-v2			sqlite3-close
    sqlite3-db-config			sqlite3-extended-result-codes
    sqlite3-busy-handler		sqlite3-busy-timeout

    ;; convenience execution of SQL snippets
    sqlite3-exec			%c-array->bytevectors
    sqlite3-get-table			sqlite3-free-table
    sqlite3-table-to-vector

    ;; SQL execution auxiliary functions
    sqlite3-last-insert-rowid
    sqlite3-changes			sqlite3-total-changes
    sqlite3-interrupt
    sqlite3-complete			sqlite3-complete16
    sqlite3-progress-handler

    ;; prepared SQL statements
    sqlite3-finalize
    sqlite3-prepare			sqlite3-prepare-v2
    sqlite3-prepare16			sqlite3-prepare16-v2

;;; --------------------------------------------------------------------
;;; still to be implemented

    sqlite3-randomness
    sqlite3-set-authorizer
    sqlite3-trace
    sqlite3-profile
    sqlite3-uri-parameter
    sqlite3-uri-boolean
    sqlite3-uri-int64
    sqlite3-limit
    sqlite3-sql
    sqlite3-stmt-readonly
    sqlite3-stmt-busy
    sqlite3-bind-blob
    sqlite3-bind-double
    sqlite3-bind-int
    sqlite3-bind-int64
    sqlite3-bind-null
    sqlite3-bind-text
    sqlite3-bind-text16
    sqlite3-bind-value
    sqlite3-bind-zeroblob
    sqlite3-bind-parameter-count
    sqlite3-bind-parameter-name
    sqlite3-bind-parameter-index
    sqlite3-clear-bindings
    sqlite3-column-count
    sqlite3-column-name
    sqlite3-column-name16
    sqlite3-column-database-name
    sqlite3-column-database-name16
    sqlite3-column-table-name
    sqlite3-column-table-name16
    sqlite3-column-origin-name
    sqlite3-column-origin-name16
    sqlite3-column-decltype
    sqlite3-column-decltype16
    sqlite3-step
    sqlite3-data-count
    sqlite3-column-blob
    sqlite3-column-bytes
    sqlite3-column-bytes16
    sqlite3-column-double
    sqlite3-column-int
    sqlite3-column-int64
    sqlite3-column-text
    sqlite3-column-text16
    sqlite3-column-type
    sqlite3-column-value
    sqlite3-reset
    sqlite3-create-function
    sqlite3-create-function16
    sqlite3-create-function-v2
    sqlite3-value-blob
    sqlite3-value-bytes
    sqlite3-value-bytes16
    sqlite3-value-double
    sqlite3-value-int
    sqlite3-value-int64
    sqlite3-value-text
    sqlite3-value-text16
    sqlite3-value-text16le
    sqlite3-value-text16be
    sqlite3-value-type
    sqlite3-value-numeric-type
    sqlite3-aggregate-context
    sqlite3-user-data
    sqlite3-context-db-handle
    sqlite3-get-auxdata
    sqlite3-set-auxdata
    sqlite3-result-blob
    sqlite3-result-double
    sqlite3-result-error
    sqlite3-result-error16
    sqlite3-result-error-toobig
    sqlite3-result-error-nomem
    sqlite3-result-error-code
    sqlite3-result-int
    sqlite3-result-int64
    sqlite3-result-null
    sqlite3-result-text
    sqlite3-result-text16
    sqlite3-result-text16le
    sqlite3-result-text16be
    sqlite3-result-value
    sqlite3-result-zeroblob
    sqlite3-create-collation
    sqlite3-create-collation-v2
    sqlite3-create-collation16
    sqlite3-collation-needed
    sqlite3-collation-needed16
    sqlite3-key
    sqlite3-rekey
    sqlite3-activate-see
    sqlite3_activate_cerod
    sqlite3-sleep
    sqlite3-get-autocommit
    sqlite3-db-handle
    sqlite3-db-filename
    sqlite3-db-readonly
    sqlite3-next-stmt
    sqlite3-commit-hook
    sqlite3-rollback-hook
    sqlite3-update-hook
    sqlite3-enable-shared-cache
    sqlite3-release-memory
    sqlite3-db-release-memory
    sqlite3-soft-heap-limit64
    sqlite3-soft-heap-limit
    sqlite3-table-column-metadata
    sqlite3-load-extension
    sqlite3-enable-load-extension
    sqlite3-auto-extension
    sqlite3-reset-auto-extension
    sqlite3-create-module
    sqlite3-create-module-v2
    sqlite3-declare-vtab
    sqlite3-overload-function
    sqlite3-blob-open
    sqlite3-blob-reopen
    sqlite3-blob-close
    sqlite3-blob-bytes
    sqlite3-blob-read
    sqlite3-blob-write
    sqlite3-vfs-find
    sqlite3-vfs-register
    sqlite3-vfs-unregister
    sqlite3-mutex-alloc
    sqlite3-mutex-free
    sqlite3-mutex-enter
    sqlite3-mutex-try
    sqlite3-mutex-leave
    sqlite3-mutex-held
    sqlite3-mutex-notheld
    sqlite3-db-mutex
    sqlite3-file-control
    sqlite3-test-control
    sqlite3-status
    sqlite3-db-status
    sqlite3-stmt-status
    sqlite3-backup-init
    sqlite3-backup-step
    sqlite3-backup-finish
    sqlite3-backup-remaining
    sqlite3-backup-pagecount
    sqlite3-unlock-notify
    sqlite3-stricmp
    sqlite3-strnicmp
    sqlite3-log
    sqlite3-wal-hook
    sqlite3-wal-autocheckpoint
    sqlite3-wal-checkpoint
    sqlite3-wal-checkpoint-v2
    sqlite3-vtab-config
    sqlite3-vtab-on-conflict
    sqlite3-rtree-geometry-callback
    )
  (import (vicare))


;;;; helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))


;;;; library initialisation, finalisation, configuration and auxiliary functions

(define-inline (sqlite3-initialize)
  (foreign-call "ik_sqlite3_initialize"))

(define-inline (sqlite3-shutdown)
  (foreign-call "ik_sqlite3_shutdown"))

(define-inline (sqlite3-os-init)
  (foreign-call "ik_sqlite3_os_init"))

(define-inline (sqlite3-os-end)
  (foreign-call "ik_sqlite3_os_end"))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-config option-identifier args)
  (foreign-call "ik_sqlite3_config" option-identifier args))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-memory-used)
  (foreign-call "ik_sqlite3_memory_used"))

(define-inline (sqlite3-memory-highwater reset)
  (foreign-call "ik_sqlite3_memory_highwater" reset))


;;;; version functions

(define-inline (vicare-sqlite3-version-string)
  (foreign-call "vicare_sqlite3_version_string"))

(define-inline (vicare-sqlite3-version-interface-current)
  (foreign-call "vicare_sqlite3_version_interface_current"))

(define-inline (vicare-sqlite3-version-interface-revision)
  (foreign-call "vicare_sqlite3_version_interface_revision"))

(define-inline (vicare-sqlite3-version-interface-age)
  (foreign-call "vicare_sqlite3_version_interface_age"))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-libversion)
  (foreign-call "ik_sqlite3_libversion"))

(define-inline (sqlite3-libversion-number)
  (foreign-call "ik_sqlite3_libversion_number"))

(define-inline (sqlite3-sourceid)
  (foreign-call "ik_sqlite3_sourceid"))


;;;; compiled options

(define-inline (sqlite3-compileoption-used option-name)
  (foreign-call "ik_sqlite3_compileoption_used" option-name))

(define-inline (sqlite3-compileoption-get option-index)
  (foreign-call "ik_sqlite3_compileoption_get" option-index))

(define-inline (sqlite3-threadsafe)
  (foreign-call "ik_sqlite3_threadsafe"))


;;;; error codes and error messages

(define-inline (sqlite3-errcode connection)
  (foreign-call "ik_sqlite3_errcode" connection))

(define-inline (sqlite3-extended-errcode connection)
  (foreign-call "ik_sqlite3_extended_errcode" connection))

(define-inline (sqlite3-errmsg connection)
  (foreign-call "ik_sqlite3_errmsg" connection))

(define-inline (sqlite3-errmsg16 connection)
  (foreign-call "ik_sqlite3_errmsg16" connection))


;;;; connection handling

(define-inline (sqlite3-close sqlite3)
  (foreign-call "ik_sqlite3_close" sqlite3))

(define-inline (sqlite3-open pathname connection)
  (foreign-call "ik_sqlite3_open" pathname connection))

(define-inline (sqlite3-open16 pathname connection)
  (foreign-call "ik_sqlite3_open16" pathname connection))

(define-inline (sqlite3-open-v2 pathname connection flags vfs-module)
  (foreign-call "ik_sqlite3_open_v2" pathname connection flags vfs-module))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-db-config connection option-identifier args)
  (foreign-call "ik_sqlite3_db_config" connection option-identifier args))

(define-inline (sqlite3-extended-result-codes connection boolean)
  (foreign-call "ik_sqlite3_extended_result_codes" connection boolean))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-busy-handler connection callback)
  (foreign-call "ik_sqlite3_busy_handler" connection callback))

(define-inline (sqlite3-busy-timeout connection milliseconds)
  (foreign-call "ik_sqlite3_busy_timeout" connection milliseconds))


;;;; convenience execution of SQL snippets

(define-inline (sqlite3-exec connection sql-snippet each-row-callback)
  (foreign-call "ik_sqlite3_exec" connection sql-snippet each-row-callback))

(define-inline (%c-array->bytevectors number-of-elements c-array)
  (foreign-call "ik_sqlite3_c_array_to_bytevectors" number-of-elements c-array))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-get-table connection sql-snippet)
  (foreign-call "ik_sqlite3_get_table" connection sql-snippet))

(define-inline (sqlite3-free-table result-pointer)
  (foreign-call "ik_sqlite3_free_table" result-pointer))

(define-inline (sqlite3-table-to-vector num-of-rows num-of-cols table-pointer)
  (foreign-call "ik_sqlite3_table_to_vector" num-of-rows num-of-cols table-pointer))


;;;; SQL execution auxiliary functions

(define-inline (sqlite3-last-insert-rowid connection)
  (foreign-call "ik_sqlite3_last_insert_rowid" connection))

(define-inline (sqlite3-changes connection)
  (foreign-call "ik_sqlite3_changes" connection))

(define-inline (sqlite3-total-changes connection)
  (foreign-call "ik_sqlite3_total_changes" connection))

(define-inline (sqlite3-interrupt connection)
  (foreign-call "ik_sqlite3_interrupt" connection))

(define-inline (sqlite3-complete sql-snippet)
  (foreign-call "ik_sqlite3_complete" sql-snippet))

(define-inline (sqlite3-complete16 sql-snippet)
  (foreign-call "ik_sqlite3_complete16" sql-snippet))

(define-inline (sqlite3-progress-handler connection instruction-count callback)
  (foreign-call "ik_sqlite3_progress_handler" connection instruction-count callback))


;;;; prepared SQL statements

(define-inline (sqlite3-finalize statement)
  (foreign-call "ik_sqlite3_finalize" statement))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-prepare connection sql-snippet sql-start statement store-sql-text?)
  (foreign-call "ik_sqlite3_prepare" connection sql-snippet sql-start
		statement store-sql-text?))

(define-inline (sqlite3-prepare-v2)
  (foreign-call "ik_sqlite3_prepare_v2"))

(define-inline (sqlite3-prepare16)
  (foreign-call "ik_sqlite3_prepare16"))

(define-inline (sqlite3-prepare16-v2)
  (foreign-call "ik_sqlite3_prepare16_v2"))




;;;; still to be implemented

(define-inline (sqlite3-randomness)
  (foreign-call "ik_sqlite3-randomness"))

(define-inline (sqlite3-set-authorizer)
  (foreign-call "ik_sqlite3_set_authorizer"))

(define-inline (sqlite3-trace)
  (foreign-call "ik_sqlite3_trace"))

(define-inline (sqlite3-profile)
  (foreign-call "ik_sqlite3_profile"))

(define-inline (sqlite3-uri-parameter)
  (foreign-call "ik_sqlite3_uri_parameter"))

(define-inline (sqlite3-uri-boolean)
  (foreign-call "ik_sqlite3_uri_boolean"))

(define-inline (sqlite3-uri-int64)
  (foreign-call "ik_sqlite3_uri_int64"))

(define-inline (sqlite3-limit)
  (foreign-call "ik_sqlite3_limit"))

(define-inline (sqlite3-sql)
  (foreign-call "ik_sqlite3_sql"))

(define-inline (sqlite3-stmt-readonly)
  (foreign-call "ik_sqlite3_stmt_readonly"))

(define-inline (sqlite3-stmt-busy)
  (foreign-call "ik_sqlite3_stmt_busy"))

(define-inline (sqlite3-bind-blob)
  (foreign-call "ik_sqlite3_bind_blob"))

(define-inline (sqlite3-bind-double)
  (foreign-call "ik_sqlite3_bind_double"))

(define-inline (sqlite3-bind-int)
  (foreign-call "ik_sqlite3_bind_int"))

(define-inline (sqlite3-bind-int64)
  (foreign-call "ik_sqlite3_bind_int64"))

(define-inline (sqlite3-bind-null)
  (foreign-call "ik_sqlite3_bind_null"))

(define-inline (sqlite3-bind-text)
  (foreign-call "ik_sqlite3_bind_text"))

(define-inline (sqlite3-bind-text16)
  (foreign-call "ik_sqlite3_bind_text16"))

(define-inline (sqlite3-bind-value)
  (foreign-call "ik_sqlite3_bind_value"))

(define-inline (sqlite3-bind-zeroblob)
  (foreign-call "ik_sqlite3_bind_zeroblob"))

(define-inline (sqlite3-bind-parameter-count)
  (foreign-call "ik_sqlite3_bind_parameter_count"))

(define-inline (sqlite3-bind-parameter-name)
  (foreign-call "ik_sqlite3_bind_parameter_name"))

(define-inline (sqlite3-bind-parameter-index)
  (foreign-call "ik_sqlite3_bind_parameter_index"))

(define-inline (sqlite3-clear-bindings)
  (foreign-call "ik_sqlite3_clear_bindings"))

(define-inline (sqlite3-column-count)
  (foreign-call "ik_sqlite3_column_count"))

(define-inline (sqlite3-column-name)
  (foreign-call "ik_sqlite3_column_name"))

(define-inline (sqlite3-column-name16)
  (foreign-call "ik_sqlite3_column_name16"))

(define-inline (sqlite3-column-database-name)
  (foreign-call "ik_sqlite3_column_database_name"))

(define-inline (sqlite3-column-database-name16)
  (foreign-call "ik_sqlite3_column_database_name16"))

(define-inline (sqlite3-column-table-name)
  (foreign-call "ik_sqlite3_column_table_name"))

(define-inline (sqlite3-column-table-name16)
  (foreign-call "ik_sqlite3_column_table_name16"))

(define-inline (sqlite3-column-origin-name)
  (foreign-call "ik_sqlite3_column_origin_name"))

(define-inline (sqlite3-column-origin-name16)
  (foreign-call "ik_sqlite3_column_origin_name16"))

(define-inline (sqlite3-column-decltype)
  (foreign-call "ik_sqlite3_column_decltype"))

(define-inline (sqlite3-column-decltype16)
  (foreign-call "ik_sqlite3_column_decltype16"))

(define-inline (sqlite3-step)
  (foreign-call "ik_sqlite3_step"))

(define-inline (sqlite3-data-count)
  (foreign-call "ik_sqlite3_data_count"))

(define-inline (sqlite3-column-blob)
  (foreign-call "ik_sqlite3_column_blob"))

(define-inline (sqlite3-column-bytes)
  (foreign-call "ik_sqlite3_column_bytes"))

(define-inline (sqlite3-column-bytes16)
  (foreign-call "ik_sqlite3_column_bytes16"))

(define-inline (sqlite3-column-double)
  (foreign-call "ik_sqlite3_column_double"))

(define-inline (sqlite3-column-int)
  (foreign-call "ik_sqlite3_column_int"))

(define-inline (sqlite3-column-int64)
  (foreign-call "ik_sqlite3_column_int64"))

(define-inline (sqlite3-column-text)
  (foreign-call "ik_sqlite3_column_text"))

(define-inline (sqlite3-column-text16)
  (foreign-call "ik_sqlite3_column_text16"))

(define-inline (sqlite3-column-type)
  (foreign-call "ik_sqlite3_column_type"))

(define-inline (sqlite3-column-value)
  (foreign-call "ik_sqlite3_column_value"))

(define-inline (sqlite3-reset)
  (foreign-call "ik_sqlite3_reset"))

(define-inline (sqlite3-create-function)
  (foreign-call "ik_sqlite3_create_function"))

(define-inline (sqlite3-create-function16)
  (foreign-call "ik_sqlite3_create_function16"))

(define-inline (sqlite3-create-function-v2)
  (foreign-call "ik_sqlite3_create_function_v2"))

(define-inline (sqlite3-value-blob)
  (foreign-call "ik_sqlite3_value_blob"))

(define-inline (sqlite3-value-bytes)
  (foreign-call "ik_sqlite3_value_bytes"))

(define-inline (sqlite3-value-bytes16)
  (foreign-call "ik_sqlite3_value_bytes16"))

(define-inline (sqlite3-value-double)
  (foreign-call "ik_sqlite3_value_double"))

(define-inline (sqlite3-value-int)
  (foreign-call "ik_sqlite3_value_int"))

(define-inline (sqlite3-value-int64)
  (foreign-call "ik_sqlite3_value_int64"))

(define-inline (sqlite3-value-text)
  (foreign-call "ik_sqlite3_value_text"))

(define-inline (sqlite3-value-text16)
  (foreign-call "ik_sqlite3_value_text16"))

(define-inline (sqlite3-value-text16le)
  (foreign-call "ik_sqlite3_value_text16le"))

(define-inline (sqlite3-value-text16be)
  (foreign-call "ik_sqlite3_value_text16be"))

(define-inline (sqlite3-value-type)
  (foreign-call "ik_sqlite3_value_type"))

(define-inline (sqlite3-value-numeric-type)
  (foreign-call "ik_sqlite3_value_numeric_type"))

(define-inline (sqlite3-aggregate-context)
  (foreign-call "ik_sqlite3_aggregate_context"))

(define-inline (sqlite3-user-data)
  (foreign-call "ik_sqlite3_user_data"))

(define-inline (sqlite3-context-db-handle)
  (foreign-call "ik_sqlite3_context_db_handle"))

(define-inline (sqlite3-get-auxdata)
  (foreign-call "ik_sqlite3_get_auxdata"))

(define-inline (sqlite3-set-auxdata)
  (foreign-call "ik_sqlite3_set_auxdata"))

(define-inline (sqlite3-result-blob)
  (foreign-call "ik_sqlite3_result_blob"))

(define-inline (sqlite3-result-double)
  (foreign-call "ik_sqlite3_result_double"))

(define-inline (sqlite3-result-error)
  (foreign-call "ik_sqlite3_result_error"))

(define-inline (sqlite3-result-error16)
  (foreign-call "ik_sqlite3_result_error16"))

(define-inline (sqlite3-result-error-toobig)
  (foreign-call "ik_sqlite3_result_error_toobig"))

(define-inline (sqlite3-result-error-nomem)
  (foreign-call "ik_sqlite3_result_error_nomem"))

(define-inline (sqlite3-result-error-code)
  (foreign-call "ik_sqlite3_result_error_code"))

(define-inline (sqlite3-result-int)
  (foreign-call "ik_sqlite3_result_int"))

(define-inline (sqlite3-result-int64)
  (foreign-call "ik_sqlite3_result_int64"))

(define-inline (sqlite3-result-null)
  (foreign-call "ik_sqlite3_result_null"))

(define-inline (sqlite3-result-text)
  (foreign-call "ik_sqlite3_result_text"))

(define-inline (sqlite3-result-text16)
  (foreign-call "ik_sqlite3_result_text16"))

(define-inline (sqlite3-result-text16le)
  (foreign-call "ik_sqlite3_result_text16le"))

(define-inline (sqlite3-result-text16be)
  (foreign-call "ik_sqlite3_result_text16be"))

(define-inline (sqlite3-result-value)
  (foreign-call "ik_sqlite3_result_value"))

(define-inline (sqlite3-result-zeroblob)
  (foreign-call "ik_sqlite3_result_zeroblob"))

(define-inline (sqlite3-create-collation)
  (foreign-call "ik_sqlite3_create_collation"))

(define-inline (sqlite3-create-collation-v2)
  (foreign-call "ik_sqlite3_create_collation_v2"))

(define-inline (sqlite3-create-collation16)
  (foreign-call "ik_sqlite3_create_collation16"))

(define-inline (sqlite3-collation-needed)
  (foreign-call "ik_sqlite3_collation_needed"))

(define-inline (sqlite3-collation-needed16)
  (foreign-call "ik_sqlite3_collation_needed16"))

(define-inline (sqlite3-key)
  (foreign-call "ik_sqlite3_key"))

(define-inline (sqlite3-rekey)
  (foreign-call "ik_sqlite3_rekey"))

(define-inline (sqlite3-activate-see)
  (foreign-call "ik_sqlite3_activate_see"))

(define-inline (sqlite3_activate_cerod)
  (foreign-call "ik_sqlite3_activate_cerod"))

(define-inline (sqlite3-sleep)
  (foreign-call "ik_sqlite3_sleep"))

(define-inline (sqlite3-get-autocommit)
  (foreign-call "ik_sqlite3_get_autocommit"))

(define-inline (sqlite3-db-handle)
  (foreign-call "ik_sqlite3_db_handle"))

(define-inline (sqlite3-db-filename)
  (foreign-call "ik_sqlite3_db_filename"))

(define-inline (sqlite3-db-readonly)
  (foreign-call "ik_sqlite3_db_readonly"))

(define-inline (sqlite3-next-stmt)
  (foreign-call "ik_sqlite3_next_stmt"))

(define-inline (sqlite3-commit-hook)
  (foreign-call "ik_sqlite3_commit_hook"))

(define-inline (sqlite3-rollback-hook)
  (foreign-call "ik_sqlite3_rollback_hook"))

(define-inline (sqlite3-update-hook)
  (foreign-call "ik_sqlite3_update_hook"))

(define-inline (sqlite3-enable-shared-cache)
  (foreign-call "ik_sqlite3_enable_shared_cache"))

(define-inline (sqlite3-release-memory)
  (foreign-call "ik_sqlite3_release_memory"))

(define-inline (sqlite3-db-release-memory)
  (foreign-call "ik_sqlite3_db_release_memory"))

(define-inline (sqlite3-soft-heap-limit64)
  (foreign-call "ik_sqlite3_soft_heap_limit64"))

(define-inline (sqlite3-soft-heap-limit)
  (foreign-call "ik_sqlite3_soft_heap_limit"))

(define-inline (sqlite3-table-column-metadata)
  (foreign-call "ik_sqlite3_table_column_metadata"))

(define-inline (sqlite3-load-extension)
  (foreign-call "ik_sqlite3_load_extension"))

(define-inline (sqlite3-enable-load-extension)
  (foreign-call "ik_sqlite3_enable_load_extension"))

(define-inline (sqlite3-auto-extension)
  (foreign-call "ik_sqlite3_auto_extension"))

(define-inline (sqlite3-reset-auto-extension)
  (foreign-call "ik_sqlite3_reset_auto_extension"))

(define-inline (sqlite3-create-module)
  (foreign-call "ik_sqlite3_create_module"))

(define-inline (sqlite3-create-module-v2)
  (foreign-call "ik_sqlite3_create_module_v2"))

(define-inline (sqlite3-declare-vtab)
  (foreign-call "ik_sqlite3_declare_vtab"))

(define-inline (sqlite3-overload-function)
  (foreign-call "ik_sqlite3_overload_function"))

(define-inline (sqlite3-blob-open)
  (foreign-call "ik_sqlite3_blob_open"))

(define-inline (sqlite3-blob-reopen)
  (foreign-call "ik_sqlite3_blob_reopen"))

(define-inline (sqlite3-blob-close)
  (foreign-call "ik_sqlite3_blob_close"))

(define-inline (sqlite3-blob-bytes)
  (foreign-call "ik_sqlite3_blob_bytes"))

(define-inline (sqlite3-blob-read)
  (foreign-call "ik_sqlite3_blob_read"))

(define-inline (sqlite3-blob-write)
  (foreign-call "ik_sqlite3_blob_write"))

(define-inline (sqlite3-vfs-find)
  (foreign-call "ik_sqlite3_vfs_find"))

(define-inline (sqlite3-vfs-register)
  (foreign-call "ik_sqlite3_vfs_register"))

(define-inline (sqlite3-vfs-unregister)
  (foreign-call "ik_sqlite3_vfs_unregister"))

(define-inline (sqlite3-mutex-alloc)
  (foreign-call "ik_sqlite3_mutex_alloc"))

(define-inline (sqlite3-mutex-free)
  (foreign-call "ik_sqlite3_mutex_free"))

(define-inline (sqlite3-mutex-enter)
  (foreign-call "ik_sqlite3_mutex_enter"))

(define-inline (sqlite3-mutex-try)
  (foreign-call "ik_sqlite3_mutex_try"))

(define-inline (sqlite3-mutex-leave)
  (foreign-call "ik_sqlite3_mutex_leave"))

(define-inline (sqlite3-mutex-held)
  (foreign-call "ik_sqlite3_mutex_held"))

(define-inline (sqlite3-mutex-notheld)
  (foreign-call "ik_sqlite3_mutex_notheld"))

(define-inline (sqlite3-db-mutex)
  (foreign-call "ik_sqlite3_db_mutex"))

(define-inline (sqlite3-file-control)
  (foreign-call "ik_sqlite3_file_control"))

(define-inline (sqlite3-test-control)
  (foreign-call "ik_sqlite3_test_control"))

(define-inline (sqlite3-status)
  (foreign-call "ik_sqlite3_status"))

(define-inline (sqlite3-db-status)
  (foreign-call "ik_sqlite3_db_status"))

(define-inline (sqlite3-stmt-status)
  (foreign-call "ik_sqlite3_stmt_status"))

(define-inline (sqlite3-backup-init)
  (foreign-call "ik_sqlite3_backup_init"))

(define-inline (sqlite3-backup-step)
  (foreign-call "ik_sqlite3_backup_step"))

(define-inline (sqlite3-backup-finish)
  (foreign-call "ik_sqlite3_backup_finish"))

(define-inline (sqlite3-backup-remaining)
  (foreign-call "ik_sqlite3_backup_remaining"))

(define-inline (sqlite3-backup-pagecount)
  (foreign-call "ik_sqlite3_backup_pagecount"))

(define-inline (sqlite3-unlock-notify)
  (foreign-call "ik_sqlite3_unlock_notify"))

(define-inline (sqlite3-stricmp)
  (foreign-call "ik_sqlite3_stricmp"))

(define-inline (sqlite3-strnicmp)
  (foreign-call "ik_sqlite3_strnicmp"))

(define-inline (sqlite3-log)
  (foreign-call "ik_sqlite3_log"))

(define-inline (sqlite3-wal-hook)
  (foreign-call "ik_sqlite3_wal_hook"))

(define-inline (sqlite3-wal-autocheckpoint)
  (foreign-call "ik_sqlite3_wal_autocheckpoint"))

(define-inline (sqlite3-wal-checkpoint)
  (foreign-call "ik_sqlite3_wal_checkpoint"))

(define-inline (sqlite3-wal-checkpoint-v2)
  (foreign-call "ik_sqlite3_wal_checkpoint_v2"))

(define-inline (sqlite3-vtab-config)
  (foreign-call "ik_sqlite3_vtab_config"))

(define-inline (sqlite3-vtab-on-conflict)
  (foreign-call "ik_sqlite3_vtab_on_conflict"))

(define-inline (sqlite3-rtree-geometry-callback)
  (foreign-call "ik_sqlite3_rtree_geometry_callback"))


;;;; done

)

;;; end of file
