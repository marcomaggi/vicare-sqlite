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
    sqlite3-enable-shared-cache		sqlite3-release-memory
    sqlite3-soft-heap-limit64		sqlite3-soft-heap-limit
    sqlite3-status

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
    sqlite3-limit			sqlite3-get-autocommit
    sqlite3-db-filename			sqlite3-db-filename-from-pointer
    sqlite3-db-readonly			sqlite3-next-stmt
    sqlite3-commit-hook			sqlite3-rollback-hook
    sqlite3-update-hook
    sqlite3-trace			sqlite3-profile
    sqlite3-db-release-memory		sqlite3-table-column-metadata
    sqlite3-set-authorizer		sqlite3-db-status

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

    ;; prepared SQL statements: initialisation, finalisation, etc
    sqlite3-finalize
    sqlite3-prepare			sqlite3-prepare-v2
    sqlite3-prepare16			sqlite3-prepare16-v2
    sqlite3-sql				sqlite3-stmt-readonly
    sqlite3-stmt-busy
    sqlite3-step			sqlite3-reset
    sqlite3-stmt-status

;;;Not interfaced.  SQLITE3-STMT-CONNECTION is used instead.
;;;
;;;sqlite3-db-handle

    ;; prepared SQL statements: binding parameters to values
    sqlite3-bind-blob			sqlite3-bind-double
    sqlite3-bind-int			sqlite3-bind-int64
    sqlite3-bind-null			sqlite3-bind-text
    sqlite3-bind-text16			sqlite3-bind-value
    sqlite3-bind-zeroblob
    sqlite3-bind-parameter-count	sqlite3-bind-parameter-name
    sqlite3-bind-parameter-index
    sqlite3-clear-bindings

    ;; prepared SQL statements: inspecting the resulting row
    sqlite3-column-count
    sqlite3-column-name			sqlite3-column-name16
    sqlite3-column-database-name	sqlite3-column-database-name16
    sqlite3-column-table-name		sqlite3-column-table-name16
    sqlite3-column-origin-name		sqlite3-column-origin-name16
    sqlite3-column-decltype		sqlite3-column-decltype16
    sqlite3-data-count
    sqlite3-column-blob
    sqlite3-column-bytes		sqlite3-column-bytes16
    sqlite3-column-double		sqlite3-column-int
    sqlite3-column-int64
    sqlite3-column-text			sqlite3-column-text16
    sqlite3-column-type			sqlite3-column-value

    ;; extensions
    sqlite3-load-extension		sqlite3-enable-load-extension
    sqlite3-auto-extension		sqlite3-reset-auto-extension

    ;; BLOBs for incremental input/output
    sqlite3-blob-open			sqlite3-blob-reopen
    sqlite3-blob-close			sqlite3-blob-bytes
    sqlite3-blob-read			sqlite3-blob-write

    ;; custom SQL functions
    sqlite3-create-function		sqlite3-create-function16
    sqlite3-create-function-v2

    sqlite3-value-blob			sqlite3-value-bytes
    sqlite3-value-bytes16		sqlite3-value-double
    sqlite3-value-int			sqlite3-value-int64
    sqlite3-value-text			sqlite3-value-text16
    sqlite3-value-text16le		sqlite3-value-text16be
    sqlite3-value-type			sqlite3-value-numeric-type

    sqlite3-aggregate-context		sqlite3-user-data
    sqlite3-context-db-handle
    sqlite3-get-auxdata			sqlite3-set-auxdata

    sqlite3-result-blob			sqlite3-result-double
    sqlite3-result-error		sqlite3-result-error16
    sqlite3-result-error-toobig		sqlite3-result-error-nomem
    sqlite3-result-error-code		sqlite3-result-int
    sqlite3-result-int64		sqlite3-result-null
    sqlite3-result-text			sqlite3-result-text16
    sqlite3-result-text16le		sqlite3-result-text16be
    sqlite3-result-value		sqlite3-result-zeroblob

    sqlite-c-array-to-pointers

    ;; backup
    sqlite3-backup-init			sqlite3-backup-finish
    sqlite3-backup-step
    sqlite3-backup-remaining		sqlite3-backup-pagecount

    ;; collation functions
    sqlite3-create-collation-v2
    sqlite3-create-collation		sqlite3-create-collation16
    sqlite3-collation-needed		sqlite3-collation-needed16

    ;; miscellaneous functions
    sqlite3-sleep			sqlite3-log
    sqlite3-randomness
    sqlite3-uri-parameter		sqlite3-uri-boolean
    sqlite3-uri-int64

    ;; interfaced but untested
    sqlite3-key				sqlite3-rekey
    sqlite3-activate-see
    sqlite3-activate-cerod

    sqlite3-wal-hook			sqlite3-wal-autocheckpoint
    sqlite3-wal-checkpoint		sqlite3-wal-checkpoint-v2

;;; --------------------------------------------------------------------
;;; still to be implemented

    sqlite3-create-module
    sqlite3-create-module-v2
    sqlite3-declare-vtab
    sqlite3-overload-function
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
    sqlite3-unlock-notify
    sqlite3-stricmp
    sqlite3-strnicmp
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

;;; --------------------------------------------------------------------

(define-inline (sqlite3-enable-shared-cache bool)
  (foreign-call "ik_sqlite3_enable_shared_cache" bool))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-release-memory number-of-bytes)
  (foreign-call "ik_sqlite3_release_memory" number-of-bytes))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-soft-heap-limit64 limit)
  (foreign-call "ik_sqlite3_soft_heap_limit64" limit))

(define-inline (sqlite3-soft-heap-limit limit)
  (foreign-call "ik_sqlite3_soft_heap_limit" limit))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-status opcode reset?)
  (foreign-call "ik_sqlite3_status" opcode reset?))


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

(define-inline (sqlite3-limit conn limit-identifier limit-value)
  (foreign-call "ik_sqlite3_limit" conn limit-identifier limit-value))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-busy-handler connection callback)
  (foreign-call "ik_sqlite3_busy_handler" connection callback))

(define-inline (sqlite3-busy-timeout connection milliseconds)
  (foreign-call "ik_sqlite3_busy_timeout" connection milliseconds))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-get-autocommit connection)
  (foreign-call "ik_sqlite3_get_autocommit" connection))

(define-inline (sqlite3-db-filename connection database)
  (foreign-call "ik_sqlite3_db_filename" connection database))

(define-inline (sqlite3-db-filename-from-pointer connection-pointer database)
  (foreign-call "ik_sqlite3_db_filename_from_pointer" connection-pointer database))

(define-inline (sqlite3-db-readonly connection database)
  (foreign-call "ik_sqlite3_db_readonly" connection database))

(define-inline (sqlite3-next-stmt connection statement)
  (foreign-call "ik_sqlite3_next_stmt" connection statement))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-commit-hook connection callback)
  (foreign-call "ik_sqlite3_commit_hook" connection callback))

(define-inline (sqlite3-rollback-hook connection callback)
  (foreign-call "ik_sqlite3_rollback_hook" connection callback))

(define-inline (sqlite3-update-hook connection callback)
  (foreign-call "ik_sqlite3_update_hook" connection callback))

(define-inline (sqlite3-trace connection callback)
  (foreign-call "ik_sqlite3_trace" connection callback))

(define-inline (sqlite3-profile connection callback)
  (foreign-call "ik_sqlite3_profile" connection callback))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-db-release-memory connection)
  (foreign-call "ik_sqlite3_db_release_memory" connection))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-table-column-metadata connection database-name table-name column-name)
  (foreign-call "ik_sqlite3_table_column_metadata"
		connection database-name table-name column-name))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-set-authorizer connection callback)
  (foreign-call "ik_sqlite3_set_authorizer" connection callback))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-db-status connection opcode reset?)
  (foreign-call "ik_sqlite3_db_status" connection opcode reset?))


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


;;;; prepared SQL statements: initialisation, finalisation, etc

(define-inline (sqlite3-finalize statement)
  (foreign-call "ik_sqlite3_finalize" statement))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-prepare connection sql-snippet sql-start statement store-sql-text?)
  (foreign-call "ik_sqlite3_prepare" connection sql-snippet sql-start
		statement store-sql-text?))

(define-inline (sqlite3-prepare-v2 connection sql-snippet sql-start statement store-sql-text?)
  (foreign-call "ik_sqlite3_prepare_v2" connection sql-snippet sql-start
		statement store-sql-text?))

(define-inline (sqlite3-prepare16 connection sql-snippet sql-start statement store-sql-text?)
  (foreign-call "ik_sqlite3_prepare16" connection sql-snippet sql-start
		statement store-sql-text?))

(define-inline (sqlite3-prepare16-v2 connection sql-snippet sql-start statement store-sql-text?)
  (foreign-call "ik_sqlite3_prepare16_v2" connection sql-snippet sql-start
		statement store-sql-text?))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-reset statement)
  (foreign-call "ik_sqlite3_reset" statement))

(define-inline (sqlite3-step statement)
  (foreign-call "ik_sqlite3_step" statement))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-sql statement)
  (foreign-call "ik_sqlite3_sql" statement))

(define-inline (sqlite3-stmt-readonly statement)
  (foreign-call "ik_sqlite3_stmt_readonly" statement))

(define-inline (sqlite3-stmt-busy statement)
  (foreign-call "ik_sqlite3_stmt_busy" statement))

;;not interfaced
;;
;;(define-inline (sqlite3-db-handle statement)
;;  (foreign-call "ik_sqlite3_db_handle" statement))

(define-inline (sqlite3-stmt-status statement opcode reset?)
  (foreign-call "ik_sqlite3_stmt_status" statement opcode reset?))


;;;; prepared SQL statements: binding parameters to values

(define-inline (sqlite3-bind-blob statement parameter-index
				  blob.data blob.start blob.length blob.destructor)
  (foreign-call "ik_sqlite3_bind_blob"
		statement parameter-index blob.data blob.start blob.length blob.destructor))

(define-inline (sqlite3-bind-double statement parameter-index value)
  (foreign-call "ik_sqlite3_bind_double" statement parameter-index value))

(define-inline (sqlite3-bind-int statement parameter-index value)
  (foreign-call "ik_sqlite3_bind_int" statement parameter-index value))

(define-inline (sqlite3-bind-int64 statement parameter-index value)
  (foreign-call "ik_sqlite3_bind_int64" statement parameter-index value))

(define-inline (sqlite3-bind-null statement parameter-index)
  (foreign-call "ik_sqlite3_bind_null" statement parameter-index))

(define-inline (sqlite3-bind-text statement parameter-index
				  blob.data blob.start blob.length blob.destructor)
  (foreign-call "ik_sqlite3_bind_text"
		statement parameter-index blob.data blob.start blob.length blob.destructor))

(define-inline (sqlite3-bind-text16 statement parameter-index
				    blob.data blob.start blob.length blob.destructor)
  (foreign-call "ik_sqlite3_bind_text16"
		statement parameter-index blob.data blob.start blob.length blob.destructor))

(define-inline (sqlite3-bind-value statement parameter-index value)
  (foreign-call "ik_sqlite3_bind_value" statement parameter-index value))

(define-inline (sqlite3-bind-zeroblob statement parameter-index blob-length)
  (foreign-call "ik_sqlite3_bind_zeroblob" statement parameter-index blob-length))

(define-inline (sqlite3-bind-parameter-count statement)
  (foreign-call "ik_sqlite3_bind_parameter_count" statement))

(define-inline (sqlite3-bind-parameter-name statement parameter-index)
  (foreign-call "ik_sqlite3_bind_parameter_name" statement parameter-index))

(define-inline (sqlite3-bind-parameter-index statement parameter-name)
  (foreign-call "ik_sqlite3_bind_parameter_index" statement parameter-name))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-clear-bindings statement)
  (foreign-call "ik_sqlite3_clear_bindings" statement))


;;;; prepared SQL statements: inspecting the resulting row

(define-inline (sqlite3-column-count statement)
  (foreign-call "ik_sqlite3_column_count" statement))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-column-name statement column-index)
  (foreign-call "ik_sqlite3_column_name" statement column-index))

(define-inline (sqlite3-column-name16 statement column-index)
  (foreign-call "ik_sqlite3_column_name16" statement column-index))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-column-database-name statement column-index)
  (foreign-call "ik_sqlite3_column_database_name" statement column-index))

(define-inline (sqlite3-column-database-name16 statement column-index)
  (foreign-call "ik_sqlite3_column_database_name16" statement column-index))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-column-table-name statement column-index)
  (foreign-call "ik_sqlite3_column_table_name" statement column-index))

(define-inline (sqlite3-column-table-name16 statement column-index)
  (foreign-call "ik_sqlite3_column_table_name16" statement column-index))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-column-origin-name statement column-index)
  (foreign-call "ik_sqlite3_column_origin_name" statement column-index))

(define-inline (sqlite3-column-origin-name16 statement column-index)
  (foreign-call "ik_sqlite3_column_origin_name16" statement column-index))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-column-decltype statement column-index)
  (foreign-call "ik_sqlite3_column_decltype" statement column-index))

(define-inline (sqlite3-column-decltype16 statement column-index)
  (foreign-call "ik_sqlite3_column_decltype16" statement column-index))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-data-count statement)
  (foreign-call "ik_sqlite3_data_count" statement))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-column-blob statement column-index)
  (foreign-call "ik_sqlite3_column_blob" statement column-index))

(define-inline (sqlite3-column-bytes statement column-index)
  (foreign-call "ik_sqlite3_column_bytes" statement column-index))

(define-inline (sqlite3-column-bytes16 statement column-index)
  (foreign-call "ik_sqlite3_column_bytes16" statement column-index))

(define-inline (sqlite3-column-double statement column-index)
  (foreign-call "ik_sqlite3_column_double" statement column-index))

(define-inline (sqlite3-column-int statement column-index)
  (foreign-call "ik_sqlite3_column_int" statement column-index))

(define-inline (sqlite3-column-int64 statement column-index)
  (foreign-call "ik_sqlite3_column_int64" statement column-index))

(define-inline (sqlite3-column-text statement column-index)
  (foreign-call "ik_sqlite3_column_text" statement column-index))

(define-inline (sqlite3-column-text16 statement column-index)
  (foreign-call "ik_sqlite3_column_text16" statement column-index))

(define-inline (sqlite3-column-type statement column-index)
  (foreign-call "ik_sqlite3_column_type" statement column-index))

(define-inline (sqlite3-column-value statement column-index)
  (foreign-call "ik_sqlite3_column_value" statement column-index))


;;;; SQLite extensions

(define-inline (sqlite3-load-extension connection pathname procname)
  (foreign-call "ik_sqlite3_load_extension" connection pathname procname))

(define-inline (sqlite3-enable-load-extension onoff)
  (foreign-call "ik_sqlite3_enable_load_extension" onoff))

(define-inline (sqlite3-auto-extension entry-point)
  (foreign-call "ik_sqlite3_auto_extension" entry-point))

(define-inline (sqlite3-reset-auto-extension)
  (foreign-call "ik_sqlite3_reset_auto_extension"))


;;;; BLOBs for incremental input/output

(define-inline (sqlite3-blob-open connection database-name table-name column-name
				  rowid write-enabled? blob)
  (foreign-call "ik_sqlite3_blob_open" connection database-name table-name column-name
		rowid write-enabled? blob))

(define-inline (sqlite3-blob-reopen blob rowid)
  (foreign-call "ik_sqlite3_blob_reopen" blob rowid))

(define-inline (sqlite3-blob-close blob)
  (foreign-call "ik_sqlite3_blob_close" blob))

(define-inline (sqlite3-blob-bytes blob)
  (foreign-call "ik_sqlite3_blob_bytes" blob))

(define-inline (sqlite3-blob-read src-blob src-offset dst-buffer dst-offset number-of-bytes)
  (foreign-call "ik_sqlite3_blob_read"
		src-blob   src-offset
		dst-buffer dst-offset
		number-of-bytes))

(define-inline (sqlite3-blob-write dst-blob dst-offset src-buffer src-offset number-of-bytes)
  (foreign-call "ik_sqlite3_blob_write"
		dst-blob   dst-offset
		src-buffer src-offset
		number-of-bytes))


;;;; custom SQL functions

(define-inline (sqlite3-create-function connection function-name
					arity text-encoding custom-data
					func step final)
  (foreign-call "ik_sqlite3_create_function"
		connection function-name arity text-encoding custom-data func step final))

(define-inline (sqlite3-create-function16 connection function-name
					  arity text-encoding custom-data
					  func step final)
  (foreign-call "ik_sqlite3_create_function16"
		connection function-name arity text-encoding custom-data func step final))

(define-inline (sqlite3-create-function-v2 connection function-name
					   arity text-encoding custom-data
					   func step final destroy)
  (foreign-call "ik_sqlite3_create_function_v2"
		connection function-name arity text-encoding custom-data
		func step final destroy))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-value-blob value)
  (foreign-call "ik_sqlite3_value_blob" value))

(define-inline (sqlite3-value-bytes value)
  (foreign-call "ik_sqlite3_value_bytes" value))

(define-inline (sqlite3-value-bytes16 value)
  (foreign-call "ik_sqlite3_value_bytes16" value))

(define-inline (sqlite3-value-double value)
  (foreign-call "ik_sqlite3_value_double" value))

(define-inline (sqlite3-value-int value)
  (foreign-call "ik_sqlite3_value_int" value))

(define-inline (sqlite3-value-int64 value)
  (foreign-call "ik_sqlite3_value_int64" value))

(define-inline (sqlite3-value-text value)
  (foreign-call "ik_sqlite3_value_text" value))

(define-inline (sqlite3-value-text16 value)
  (foreign-call "ik_sqlite3_value_text16" value))

(define-inline (sqlite3-value-text16le value)
  (foreign-call "ik_sqlite3_value_text16le" value))

(define-inline (sqlite3-value-text16be value)
  (foreign-call "ik_sqlite3_value_text16be" value))

(define-inline (sqlite3-value-type value)
  (foreign-call "ik_sqlite3_value_type" value))

(define-inline (sqlite3-value-numeric-type value)
  (foreign-call "ik_sqlite3_value_numeric_type" value))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-result-blob context blob.data blob.start blob.len destructor)
  (foreign-call "ik_sqlite3_result_blob" context blob.data blob.start blob.len destructor))

(define-inline (sqlite3-result-zeroblob context blob.len)
  (foreign-call "ik_sqlite3_result_zeroblob" context blob.len))

;;;

(define-inline (sqlite3-result-double context retval)
  (foreign-call "ik_sqlite3_result_double" context retval))

(define-inline (sqlite3-result-int context retval)
  (foreign-call "ik_sqlite3_result_int" context retval))

(define-inline (sqlite3-result-int64 context retval)
  (foreign-call "ik_sqlite3_result_int64" context retval))

;;;

(define-inline (sqlite3-result-null context)
  (foreign-call "ik_sqlite3_result_null" context))

(define-inline (sqlite3-result-value context retval)
  (foreign-call "ik_sqlite3_result_value" context retval))

;;;

(define-inline (sqlite3-result-text context text.data text.start text.len destructor)
  (foreign-call "ik_sqlite3_result_text" context text.data text.start text.len destructor))

(define-inline (sqlite3-result-text16 context text.data text.start text.len destructor)
  (foreign-call "ik_sqlite3_result_text16" context text.data text.start text.len destructor))

(define-inline (sqlite3-result-text16le context text.data text.start text.len destructor)
  (foreign-call "ik_sqlite3_result_text16le" context text.data text.start text.len destructor))

(define-inline (sqlite3-result-text16be context text.data text.start text.len destructor)
  (foreign-call "ik_sqlite3_result_text16be" context text.data text.start text.len destructor))

;;;

(define-inline (sqlite3-result-error context error-message)
  (foreign-call "ik_sqlite3_result_error" context error-message))

(define-inline (sqlite3-result-error16 context error-message)
  (foreign-call "ik_sqlite3_result_error16" context error-message))

(define-inline (sqlite3-result-error-toobig context)
  (foreign-call "ik_sqlite3_result_error_toobig" context))

(define-inline (sqlite3-result-error-nomem context)
  (foreign-call "ik_sqlite3_result_error_nomem" context))

(define-inline (sqlite3-result-error-code context errcode)
  (foreign-call "ik_sqlite3_result_error_code" context errcode))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-aggregate-context context number-of-bytes)
  (foreign-call "ik_sqlite3_aggregate_context" context number-of-bytes))

(define-inline (sqlite3-user-data context)
  (foreign-call "ik_sqlite3_user_data" context))

(define-inline (sqlite3-context-db-handle context)
  (foreign-call "ik_sqlite3_context_db_handle" context))

(define-inline (sqlite3-get-auxdata context argnum)
  (foreign-call "ik_sqlite3_get_auxdata" context argnum))

(define-inline (sqlite3-set-auxdata context argnum auxdata destructor)
  (foreign-call "ik_sqlite3_set_auxdata" context argnum auxdata destructor))

;;; --------------------------------------------------------------------

(define-inline (sqlite-c-array-to-pointers num-of-pointers c-array)
  (foreign-call "ik_sqlite3_c_array_to_pointers" num-of-pointers c-array))


;;;; backup functions

(define-inline (sqlite3-backup-init dst-connection dst-name src-connection src-name)
  (foreign-call "ik_sqlite3_backup_init" dst-connection dst-name src-connection src-name))

(define-inline (sqlite3-backup-step backup number-of-pages)
  (foreign-call "ik_sqlite3_backup_step" backup number-of-pages))

(define-inline (sqlite3-backup-finish backup)
  (foreign-call "ik_sqlite3_backup_finish" backup))

(define-inline (sqlite3-backup-remaining backup)
  (foreign-call "ik_sqlite3_backup_remaining" backup))

(define-inline (sqlite3-backup-pagecount backup)
  (foreign-call "ik_sqlite3_backup_pagecount" backup))


;;;; collation functions

(define-inline (sqlite3-create-collation connection collation-name encoding
					 custom-data callback)
  (foreign-call "ik_sqlite3_create_collation"
		connection collation-name encoding custom-data callback))

(define-inline (sqlite3-create-collation-v2 connection collation-name encoding
					    custom-data callback destroy)
  (foreign-call "ik_sqlite3_create_collation_v2"
		connection collation-name encoding custom-data callback destroy))

(define-inline (sqlite3-create-collation16 connection collation-name encoding
					   custom-data callback)
  (foreign-call "ik_sqlite3_create_collation16"
		connection collation-name encoding custom-data callback))

(define-inline (sqlite3-collation-needed connection custom-data callback)
  (foreign-call "ik_sqlite3_collation_needed" connection custom-data callback))

(define-inline (sqlite3-collation-needed16 connection custom-data callback)
  (foreign-call "ik_sqlite3_collation_needed16" connection custom-data callback))


;;;; miscellaneous functions

(define-inline (sqlite3-sleep milliseconds)
  (foreign-call "ik_sqlite3_sleep" milliseconds))

(define-inline (sqlite3-log error-code message)
  (foreign-call "ik_sqlite3_log" error-code message))

(define-inline (sqlite3-randomness bytevector)
  (foreign-call "ik_sqlite3_randomness" bytevector))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-uri-parameter filename param-name)
  (foreign-call "ik_sqlite3_uri_parameter" filename param-name))

(define-inline (sqlite3-uri-boolean filename param-name default)
  (foreign-call "ik_sqlite3_uri_boolean" filename param-name default))

(define-inline (sqlite3-uri-int64 filename param-name default)
  (foreign-call "ik_sqlite3_uri_int64" filename param-name default))


;;;; interfaced but untested

(define-inline (sqlite3-key conn key.data key.len)
  (foreign-call "ik_sqlite3_key" conn key.data key.len))

(define-inline (sqlite3-rekey conn key.data key.len)
  (foreign-call "ik_sqlite3_rekey" conn key.data key.len))

(define-inline (sqlite3-activate-see pass-phrase)
  (foreign-call "ik_sqlite3_activate_see" pass-phrase))

(define-inline (sqlite3-activate-cerod pass-phrase)
  (foreign-call "ik_sqlite3_activate_cerod" pass-phrase))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-wal-hook connection callback custom-data)
  (foreign-call "ik_sqlite3_wal_hook" connection callback custom-data))

(define-inline (sqlite3-wal-autocheckpoint connection number-of-frames)
  (foreign-call "ik_sqlite3_wal_autocheckpoint" connection number-of-frames))

(define-inline (sqlite3-wal-checkpoint connection database-name)
  (foreign-call "ik_sqlite3_wal_checkpoint" connection database-name))

(define-inline (sqlite3-wal-checkpoint-v2 connection database-name checkpoint-mode)
  (foreign-call "ik_sqlite3_wal_checkpoint_v2" connection database-name checkpoint-mode))


;;;; still to be implemented

(define-inline (sqlite3-create-module)
  (foreign-call "ik_sqlite3_create_module"))

(define-inline (sqlite3-create-module-v2)
  (foreign-call "ik_sqlite3_create_module_v2"))

(define-inline (sqlite3-declare-vtab)
  (foreign-call "ik_sqlite3_declare_vtab"))

(define-inline (sqlite3-overload-function)
  (foreign-call "ik_sqlite3_overload_function"))

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

(define-inline (sqlite3-unlock-notify)
  (foreign-call "ik_sqlite3_unlock_notify"))

(define-inline (sqlite3-stricmp)
  (foreign-call "ik_sqlite3_stricmp"))

(define-inline (sqlite3-strnicmp)
  (foreign-call "ik_sqlite3_strnicmp"))

(define-inline (sqlite3-vtab-config)
  (foreign-call "ik_sqlite3_vtab_config"))

(define-inline (sqlite3-vtab-on-conflict)
  (foreign-call "ik_sqlite3_vtab_on_conflict"))

(define-inline (sqlite3-rtree-geometry-callback)
  (foreign-call "ik_sqlite3_rtree_geometry_callback"))


;;;; done

)

;;; end of file
