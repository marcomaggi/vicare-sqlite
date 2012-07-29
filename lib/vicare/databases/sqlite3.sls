;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: SQLite binding backend
;;;Date: Thu Feb  2, 2012
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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
#!(load-shared-library "vicare-sqlite3")
(library (vicare databases sqlite3)
  (export

    ;; library initialisation and finalisation
    sqlite3-initialize			sqlite3-shutdown
    sqlite3-os-init			sqlite3-os-end
    sqlite3-config

    ;; version functions
    vicare-sqlite3-version-string
    vicare-sqlite3-version-interface-current
    vicare-sqlite3-version-interface-revision
    vicare-sqlite3-version-interface-age
    sqlite3-libversion
    sqlite3-libversion-number
    sqlite3-sourceid

    ;; compiled options
    sqlite3-compileoption-used		sqlite3-compileoption-get
    sqlite3-threadsafe

    ;; error codes and error messages
    sqlite3-errcode			sqlite3-extended-errcode
    sqlite3-errmsg			sqlite3-errmsg16

    ;; connection handling
    sqlite3?				sqlite3?/open
    sqlite3-close			sqlite3-open
    sqlite3-open16			sqlite3-open-v2
    sqlite3-db-config			sqlite3-extended-result-codes
    sqlite3-busy-handler		make-sqlite3-busy-handler-callback
    sqlite3-busy-timeout

    ;; convenience execution of SQL snippets
    sqlite3-exec			make-sqlite3-exec-callback
    sqlite3-get-table			sqlite3-free-table
    sqlite3-table-to-vector

    ;; SQL execution auxiliary functions
    sqlite3-last-insert-rowid
    sqlite3-changes			sqlite3-total-changes
    sqlite3-interrupt
    sqlite3-complete			sqlite3-complete16

;;; --------------------------------------------------------------------
;;; still to be implemented

    sqlite3-memory-used
    sqlite3-memory-highwater
    sqlite3-randomness
    sqlite3-set-authorizer
    sqlite3-trace
    sqlite3-profile
    sqlite3-progress-handler
    sqlite3-uri-parameter
    sqlite3-uri-boolean
    sqlite3-uri-int64
    sqlite3-limit
    sqlite3-prepare
    sqlite3-prepare-v2
    sqlite3-prepare16
    sqlite3-prepare16-v2
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
    sqlite3-finalize
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
  (import (vicare)
    (vicare databases sqlite3 constants)
    (vicare syntactic-extensions)
    (prefix (vicare ffi) ffi.)
    (prefix (vicare databases sqlite3 unsafe-capi) capi.)
    (prefix (vicare unsafe-operations) unsafe.)
    (prefix (vicare words) words.))


;;;; helpers

(define-syntax with-ascii-bytevectors
  (syntax-rules ()
    ((_ ((?ascii.bv ?ascii) ...) . ?body)
     (let ((?ascii.bv (let ((ascii ?ascii))
			(if (bytevector? ascii)
			    ascii
			  (string->ascii ascii))))
	   ...)
       . ?body))))

(define-syntax with-utf8-bytevectors
  (syntax-rules ()
    ((_ ((?utf8.bv ?utf8) ...) . ?body)
     (let ((?utf8.bv (let ((utf8 ?utf8))
		       (if (bytevector? utf8)
			   utf8
			 (string->utf8 utf8))))
	   ...)
       . ?body))))

(define-syntax with-utf16-bytevectors
  (syntax-rules ()
    ((_ ((?utf16.bv ?utf16) ...) . ?body)
     (let ((?utf16.bv (let ((utf16 ?utf16))
			(if (bytevector? utf16)
			    utf16
			  (string->utf16n utf16))))
	   ...)
       . ?body))))

(define-syntax with-pathnames/utf8
  (syntax-rules ()
    ((_ ((?pathname.bv ?pathname) ...) . ?body)
     (let ((?pathname.bv (let ((pathname ?pathname))
			   (if (bytevector? pathname)
			       pathname
			     (string->utf8 pathname))))
	   ...)
       . ?body))))

(define-syntax with-pathnames/utf16n
  (syntax-rules ()
    ((_ ((?pathname.bv ?pathname) ...) . ?body)
     (let ((?pathname.bv (let ((pathname ?pathname))
			   (if (bytevector? pathname)
			       pathname
			     (string->utf16n pathname))))
	   ...)
       . ?body))))

(define-inline (%pathname? ?obj)
  (let ((obj ?obj))
    (or (bytevector? obj) (string? obj))))


;;;; arguments validation

(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

(define-argument-validation (pointer who obj)
  (ffi.pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (string/false who obj)
  (or (not obj) (string? obj))
  (assertion-violation who "expected false or string as argument" obj))

(define-argument-validation (bytevector/false who obj)
  (or (not obj) (bytevector? obj))
  (assertion-violation who "expected false or bytevector as argument" obj))

(define-argument-validation (pointer/bytevector who obj)
  (or (ffi.pointer? obj) (bytevector? obj))
  (assertion-violation who "expected pointer or bytevector as argument" obj))

(define-argument-validation (signed-int who obj)
  (words.signed-int? obj)
  (assertion-violation who
    "expected exact integer representing C language signed int as argument" obj))

(define-argument-validation (string/bytevector who obj)
  (or (string? obj) (bytevector? obj))
  (assertion-violation who "expected string or bytevector as argument" obj))

(define-argument-validation (callback who obj)
  (ffi.pointer? obj)
  (assertion-violation who "expected callback as argument" obj))

(define-argument-validation (callback/false who obj)
  (or (not obj) (pointer? obj))
  (assertion-violation who "expected false or callback as argument" obj))

(define-argument-validation (number-of-items who obj)
  (and (words.signed-int? obj)
       (<= 0 obj))
  (assertion-violation who "expected non-negative excact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (pathname who obj)
  (%pathname? obj)
  (assertion-violation who "expected string or bytevector as pathname argument" obj))

(define-argument-validation (sqlite3 who obj)
  (sqlite3? obj)
  (assertion-violation who "expected sqlite3 instance as argument" obj))

(define-argument-validation (sqlite3/open who obj)
  (sqlite3?/open obj)
  (assertion-violation who
    "expected sqlite3 instance representing open connection as argument" obj))


;;;; data structures

(define %sqlite3-guardian
  (make-guardian))

(define (%sqlite3-guardian-destructor)
  (do ((P (%sqlite3-guardian) (%sqlite3-guardian)))
      ((not P))
    ;;Try to close and ignore errors.
    (capi.sqlite3-close P)))

;;; --------------------------------------------------------------------

(define-struct sqlite3
  (pointer pathname))

(define (sqlite3?/open obj)
  (and (sqlite3? obj)
       (not (pointer-null? (sqlite3-pointer obj)))))

(define (%struct-sqlite3-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[sqlite3")
  (%display " pointer=")	(%display (sqlite3-pointer  S))
  (%display " pathname=")	(%write   (sqlite3-pathname S))
  (%display "]"))


;;;; library initialisation and finalisation

(define (sqlite3-initialize)
  (capi.sqlite3-initialize))

(define (sqlite3-shutdown)
  (capi.sqlite3-shutdown))

(define (sqlite3-os-init)
  (capi.sqlite3-os-init))

(define (sqlite3-os-end)
  (capi.sqlite3-os-end))

(define (sqlite3-config option-identifier . args)
  (define who 'sqlite3-config)
  (with-arguments-validation (who)
      ((fixnum	option-identifier))
    (capi.sqlite3-config option-identifier (if (null? args)
					       #f
					     (list->vector args)))))


;;;; version functions

(define (vicare-sqlite3-version-string)
  (latin1->string (capi.vicare-sqlite3-version-string)))

(define (vicare-sqlite3-version-interface-current)
  (capi.vicare-sqlite3-version-interface-current))

(define (vicare-sqlite3-version-interface-revision)
  (capi.vicare-sqlite3-version-interface-revision))

(define (vicare-sqlite3-version-interface-age)
  (capi.vicare-sqlite3-version-interface-age))

(define (sqlite3-libversion)
  (latin1->string (capi.sqlite3-libversion)))

(define (sqlite3-libversion-number)
  (capi.sqlite3-libversion-number))

(define (sqlite3-sourceid)
  (latin1->string (capi.sqlite3-sourceid)))


;;;; compiled options

(define (sqlite3-compileoption-used option-name)
  (define who 'sqlite3-compileoption-used)
  (with-arguments-validation (who)
      ((string	option-name))
    (with-ascii-bytevectors ((option-name.bv option-name))
      (capi.sqlite3-compileoption-used option-name.bv))))

(define (sqlite3-compileoption-get option-index)
  (define who 'sqlite3-compileoption-useget)
  (with-arguments-validation (who)
      ((fixnum	option-index))
    (let ((rv (capi.sqlite3-compileoption-get option-index)))
      (and rv (ascii->string rv)))))

(define (sqlite3-threadsafe)
  (capi.sqlite3-threadsafe))


;;;; error codes and error messages

(define (sqlite3-errcode  connection)
  (define who 'sqlite3-errcode)
  (with-arguments-validation (who)
      ((sqlite3/open	connection))
    (capi.sqlite3-errcode connection)))

(define (sqlite3-extended-errcode  connection)
  (define who 'sqlite3-extended-errcode)
  (with-arguments-validation (who)
      ((sqlite3/open	connection))
    (capi.sqlite3-extended-errcode connection)))

(define (sqlite3-errmsg  connection)
  (define who 'sqlite3-errmsg)
  (with-arguments-validation (who)
      ((sqlite3/open	connection))
    (utf8->string (capi.sqlite3-errmsg connection))))

(define (sqlite3-errmsg16  connection)
  (define who 'sqlite3-errmsg16)
  (with-arguments-validation (who)
      ((sqlite3/open	connection))
    (utf16n->string (capi.sqlite3-errmsg16 connection))))


;;;; connection handling

(define (sqlite3-open pathname)
  (define who 'sqlite3-open)
  (with-arguments-validation (who)
      ((pathname	pathname))
    (with-pathnames/utf8 ((pathname.bv pathname))
      (let* ((conn	(make-sqlite3 (null-pointer)
				      (if (string? pathname)
					  pathname
					(utf8->string pathname))))
	     (rv	(capi.sqlite3-open pathname.bv conn)))
	(if (unsafe.fx= rv SQLITE_OK)
	    (%sqlite3-guardian conn)
	  rv)))))

(define (sqlite3-open16 pathname)
  (define who 'sqlite3-open16)
  (with-arguments-validation (who)
      ((pathname	pathname))
    (with-pathnames/utf16n ((pathname.bv pathname))
      (let* ((conn	(make-sqlite3 (null-pointer)
				      (if (string? pathname)
					  pathname
					(utf16n->string pathname))))
	     (rv	(capi.sqlite3-open16 pathname.bv conn)))
	(if (unsafe.fx= rv SQLITE_OK)
	    (%sqlite3-guardian conn)
	  rv)))))

(define sqlite3-open-v2
  (case-lambda
   ((pathname flags)
    (sqlite3-open-v2 pathname flags #f))
   ((pathname flags vfs-module)
    (define who 'sqlite3-open-v2)
    (with-arguments-validation (who)
	((pathname	pathname)
	 (signed-int	flags)
	 (string/false	vfs-module))
      (with-pathnames/utf8 ((pathname.bv pathname))
	(let* ((vfs	(if (string? vfs-module)
			    (string->utf8 vfs-module)
			  vfs-module))
	       (conn	(make-sqlite3 (null-pointer)
				      (if (string? pathname)
					  pathname
					(utf8->string pathname))))
	       (rv	(capi.sqlite3-open-v2 pathname.bv conn flags vfs)))
	  (if (unsafe.fx= rv SQLITE_OK)
	      (%sqlite3-guardian conn)
	    rv)))))))

(define (sqlite3-close connection)
  (define who 'sqlite3-close)
  (with-arguments-validation (who)
      ((sqlite3	connection))
    (capi.sqlite3-close connection)))

;;; --------------------------------------------------------------------

(define (sqlite3-db-config connection option-identifier . args)
  (define who 'sqlite3-db-config)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (fixnum		option-identifier))
    (capi.sqlite3-db-config connection option-identifier
			    (if (null? args)
				#f
			      (list->vector args)))))

(define (sqlite3-extended-result-codes connection boolean)
  (define who 'sqlite3-extended-result-codes)
  (with-arguments-validation (who)
      ((sqlite3/open	connection))
    (capi.sqlite3-extended-result-codes connection boolean)))

;;; --------------------------------------------------------------------

(define (make-sqlite3-busy-handler-callback user-scheme-callback)
  (let ((%sqlite3-busy-handler-callback-maker
	 ;; int (*) (void*,int)
	 (ffi.make-c-callback-maker 'signed-int '(pointer signed-int))))
    (%sqlite3-busy-handler-callback-maker
     (lambda (number-of-invocations)
       (guard (E (else
		  #;(pretty-print E (current-error-port))
		  0))
	 (if (user-scheme-callback number-of-invocations)
	     1
	   0))))))

(define sqlite3-busy-handler
  (case-lambda
   ((connection)
    (sqlite3-busy-handler connection #f))
   ((connection callback)
    (define who 'sqlite3-busy-handler)
    (with-arguments-validation (who)
	((sqlite3/open		connection)
	 (callback/false	callback))
      (capi.sqlite3-busy-handler connection callback)))))

(define (sqlite3-busy-timeout connection milliseconds)
  (define who 'sqlite3-busy-timeout)
  (with-arguments-validation (who)
	((sqlite3/open		connection)
	 (fixnum		milliseconds))
    (capi.sqlite3-busy-timeout connection milliseconds)))


;;;; convenience execution of SQL snippets

(define make-sqlite3-exec-callback
  (let ((%sqlite3-exec-callback-maker
	 ;; int (*callback)(void*,int,char**,char**)
	 (ffi.make-c-callback-maker 'signed-int '(pointer signed-int pointer pointer))))
    (lambda (user-scheme-callback)
      (%sqlite3-exec-callback-maker
       (lambda (dummy number-of-rows c-array-texts c-array-names)
	 (guard (E (else
		    #;(pretty-print E (current-error-port))
		    SQLITE_ABORT))
	   (if (fixnum? number-of-rows)
	       (if (user-scheme-callback
		    number-of-rows
		    (capi.%c-array->bytevectors number-of-rows c-array-texts)
		    (capi.%c-array->bytevectors number-of-rows c-array-names))
		   SQLITE_ABORT
		 SQLITE_OK)
	     ;;FIXME If the number of rows is soo big that the result cannot
	     ;;be stored in a Scheme vector: just break the loop.
	     SQLITE_ABORT)))))))

(define sqlite3-exec
  (case-lambda
   ((connection sql-snippet)
    (sqlite3-exec connection sql-snippet #f))
   ((connection sql-snippet each-row-callback)
    (define who 'sqlite3-exec)
    (with-arguments-validation (who)
	((sqlite3/open		connection)
	 (string/bytevector	sql-snippet)
	 (callback/false	each-row-callback))
      (with-utf8-bytevectors ((sql-snippet.bv sql-snippet))
	(let ((rv (capi.sqlite3-exec connection sql-snippet.bv each-row-callback)))
	  (if (pair? rv)
	      (values (car rv) (utf8->string (cdr rv)))
	    (values rv #f))))))))

;;; --------------------------------------------------------------------

(define (sqlite3-get-table connection sql-snippet)
  (define who 'sqlite3-get-table)
  (with-arguments-validation (who)
      ((sqlite3/open		connection)
       (string/bytevector	sql-snippet))
    (with-utf8-bytevectors ((sql-snippet.bv sql-snippet))
      (let ((rv (capi.sqlite3-get-table connection sql-snippet.bv)))
	(values (unsafe.vector-ref rv 0) ;fixnum representing SQLITE_ code
		(unsafe.vector-ref rv 1) ;false or string representing error message
		(unsafe.vector-ref rv 2) ;number of rows in result, possibly zero
		(unsafe.vector-ref rv 3) ;number of columns in result, possibly zero
		(unsafe.vector-ref rv 4) ;false or pointer object referencing result
		)))))

(define (sqlite3-free-table result-pointer)
  (define who 'sqlite3-free-table)
  (with-arguments-validation (who)
      ((pointer	result-pointer))
    (capi.sqlite3-free-table result-pointer)))

(define (sqlite3-table-to-vector num-of-rows num-of-cols table-pointer)
  (define who 'sqlite3-table-to-scheme)
  (with-arguments-validation (who)
      ((number-of-items	num-of-rows)
       (number-of-items	num-of-cols)
       (pointer		table-pointer))
    (capi.sqlite3-table-to-vector num-of-rows num-of-cols table-pointer)))


;;;; SQL execution auxiliary functions

(define (sqlite3-last-insert-rowid connection)
  (define who 'sqlite3-last-insert-rowid)
  (with-arguments-validation (who)
      ((sqlite3/open	connection))
    (capi.sqlite3-last-insert-rowid connection)))

(define (sqlite3-changes connection)
  (define who 'sqlite3-changes)
  (with-arguments-validation (who)
      ((sqlite3/open	connection))
    (capi.sqlite3-changes connection)))

(define (sqlite3-total-changes connection)
  (define who 'sqlite3-total-changes)
  (with-arguments-validation (who)
      ((sqlite3/open	connection))
    (capi.sqlite3-total-changes connection)))

(define (sqlite3-interrupt connection)
  (define who 'sqlite3-interrupt)
  (with-arguments-validation (who)
      ((sqlite3/open	connection))
    (capi.sqlite3-interrupt connection)))

(define (sqlite3-complete sql-snippet)
  (define who 'sqlite3-complete)
  (with-arguments-validation (who)
      ((string/bytevector	sql-snippet))
    (with-utf8-bytevectors ((sql-snippet.bv sql-snippet))
      (capi.sqlite3-complete sql-snippet.bv))))

(define (sqlite3-complete16 sql-snippet)
  (define who 'sqlite3-complete16)
  (with-arguments-validation (who)
      ((string/bytevector	sql-snippet))
    (with-utf16-bytevectors ((sql-snippet.bv sql-snippet))
      (capi.sqlite3-complete16 sql-snippet.bv))))


;;;; still to be implemented

(define-inline (unimplemented who)
  (assertion-violation who "unimplemented function"))

(define (sqlite3-memory-used . args)
  (define who 'sqlite3-memory-used)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-memory-highwater . args)
  (define who 'sqlite3-memory-highwater)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-randomness . args)
  (define who 'sqlite3-randomness)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-set-authorizer . args)
  (define who 'sqlite3-set-authorizer)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-trace . args)
  (define who 'sqlite3-trace)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-profile . args)
  (define who 'sqlite3-profile)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-progress-handler . args)
  (define who 'sqlite3-progress-handler)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-uri-parameter . args)
  (define who 'sqlite3-uri-parameter)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-uri-boolean . args)
  (define who 'sqlite3-uri-boolean)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-uri-int64 . args)
  (define who 'sqlite3-uri-int64)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-limit . args)
  (define who 'sqlite3-limit)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-prepare . args)
  (define who 'sqlite3-prepare)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-prepare-v2 . args)
  (define who 'sqlite3-prepare-v2)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-prepare16 . args)
  (define who 'sqlite3-prepare16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-prepare16-v2 . args)
  (define who 'sqlite3-prepare16-v2)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-sql . args)
  (define who 'sqlite3-sql)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-stmt-readonly . args)
  (define who 'sqlite3-stmt-readonly)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-stmt-busy . args)
  (define who 'sqlite3-stmt-busy)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-bind-blob . args)
  (define who 'sqlite3-bind-blob)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-bind-double . args)
  (define who 'sqlite3-bind-double)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-bind-int . args)
  (define who 'sqlite3-bind-int)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-bind-int64 . args)
  (define who 'sqlite3-bind-int64)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-bind-null . args)
  (define who 'sqlite3-bind-null)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-bind-text . args)
  (define who 'sqlite3-bind-text)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-bind-text16 . args)
  (define who 'sqlite3-bind-text16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-bind-value . args)
  (define who 'sqlite3-bind-value)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-bind-zeroblob . args)
  (define who 'sqlite3-bind-zeroblob)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-bind-parameter-count . args)
  (define who 'sqlite3-bind-parameter-count)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-bind-parameter-name . args)
  (define who 'sqlite3-bind-parameter-name)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-bind-parameter-index . args)
  (define who 'sqlite3-bind-parameter-index)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-clear-bindings . args)
  (define who 'sqlite3-clear-bindings)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-count . args)
  (define who 'sqlite3-column-count)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-name . args)
  (define who 'sqlite3-column-name)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-name16 . args)
  (define who 'sqlite3-column-name16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-database-name . args)
  (define who 'sqlite3-column-database-name)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-database-name16 . args)
  (define who 'sqlite3-column-database-name16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-table-name . args)
  (define who 'sqlite3-column-table-name)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-table-name16 . args)
  (define who 'sqlite3-column-table-name16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-origin-name . args)
  (define who 'sqlite3-column-origin-name)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-origin-name16 . args)
  (define who 'sqlite3-column-origin-name16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-decltype . args)
  (define who 'sqlite3-column-decltype)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-decltype16 . args)
  (define who 'sqlite3-column-decltype16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-step . args)
  (define who 'sqlite3-step)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-data-count . args)
  (define who 'sqlite3-data-count)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-blob . args)
  (define who 'sqlite3-column-blob)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-bytes . args)
  (define who 'sqlite3-column-bytes)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-bytes16 . args)
  (define who 'sqlite3-column-bytes16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-double . args)
  (define who 'sqlite3-column-double)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-int . args)
  (define who 'sqlite3-column-int)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-int64 . args)
  (define who 'sqlite3-column-int64)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-text . args)
  (define who 'sqlite3-column-text)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-text16 . args)
  (define who 'sqlite3-column-text16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-type . args)
  (define who 'sqlite3-column-type)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-column-value . args)
  (define who 'sqlite3-column-value)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-finalize . args)
  (define who 'sqlite3-finalize)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-reset . args)
  (define who 'sqlite3-reset)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-create-function . args)
  (define who 'sqlite3-create-function)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-create-function16 . args)
  (define who 'sqlite3-create-function16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-create-function-v2 . args)
  (define who 'sqlite3-create-function-v2)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-value-blob . args)
  (define who 'sqlite3-value-blob)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-value-bytes . args)
  (define who 'sqlite3-value-bytes)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-value-bytes16 . args)
  (define who 'sqlite3-value-bytes16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-value-double . args)
  (define who 'sqlite3-value-double)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-value-int . args)
  (define who 'sqlite3-value-int)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-value-int64 . args)
  (define who 'sqlite3-value-int64)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-value-text . args)
  (define who 'sqlite3-value-text)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-value-text16 . args)
  (define who 'sqlite3-value-text16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-value-text16le . args)
  (define who 'sqlite3-value-text16le)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-value-text16be . args)
  (define who 'sqlite3-value-text16be)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-value-type . args)
  (define who 'sqlite3-value-type)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-value-numeric-type . args)
  (define who 'sqlite3-value-numeric-type)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-aggregate-context . args)
  (define who 'sqlite3-aggregate-context)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-user-data . args)
  (define who 'sqlite3-user-data)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-context-db-handle . args)
  (define who 'sqlite3-context-db-handle)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-get-auxdata . args)
  (define who 'sqlite3-get-auxdata)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-set-auxdata . args)
  (define who 'sqlite3-set-auxdata)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-blob . args)
  (define who 'sqlite3-result-blob)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-double . args)
  (define who 'sqlite3-result-double)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-error . args)
  (define who 'sqlite3-result-error)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-error16 . args)
  (define who 'sqlite3-result-error16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-error-toobig . args)
  (define who 'sqlite3-result-error-toobig)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-error-nomem . args)
  (define who 'sqlite3-result-error-nomem)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-error-code . args)
  (define who 'sqlite3-result-error-code)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-int . args)
  (define who 'sqlite3-result-int)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-int64 . args)
  (define who 'sqlite3-result-int64)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-null . args)
  (define who 'sqlite3-result-null)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-text . args)
  (define who 'sqlite3-result-text)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-text16 . args)
  (define who 'sqlite3-result-text16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-text16le . args)
  (define who 'sqlite3-result-text16le)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-text16be . args)
  (define who 'sqlite3-result-text16be)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-value . args)
  (define who 'sqlite3-result-value)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-result-zeroblob . args)
  (define who 'sqlite3-result-zeroblob)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-create-collation . args)
  (define who 'sqlite3-create-collation)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-create-collation-v2 . args)
  (define who 'sqlite3-create-collation-v2)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-create-collation16 . args)
  (define who 'sqlite3-create-collation16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-collation-needed . args)
  (define who 'sqlite3-collation-needed)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-collation-needed16 . args)
  (define who 'sqlite3-collation-needed16)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-key . args)
  (define who 'sqlite3-key)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-rekey . args)
  (define who 'sqlite3-rekey)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-activate-see . args)
  (define who 'sqlite3-activate-see)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3_activate_cerod . args)
  (define who 'sqlite3_activate_cerod)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-sleep . args)
  (define who 'sqlite3-sleep)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-get-autocommit . args)
  (define who 'sqlite3-get-autocommit)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-db-handle . args)
  (define who 'sqlite3-db-handle)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-db-filename . args)
  (define who 'sqlite3-db-filename)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-db-readonly . args)
  (define who 'sqlite3-db-readonly)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-next-stmt . args)
  (define who 'sqlite3-next-stmt)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-commit-hook . args)
  (define who 'sqlite3-commit-hook)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-rollback-hook . args)
  (define who 'sqlite3-rollback-hook)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-update-hook . args)
  (define who 'sqlite3-update-hook)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-enable-shared-cache . args)
  (define who 'sqlite3-enable-shared-cache)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-release-memory . args)
  (define who 'sqlite3-release-memory)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-db-release-memory . args)
  (define who 'sqlite3-db-release-memory)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-soft-heap-limit64 . args)
  (define who 'sqlite3-soft-heap-limit64)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-soft-heap-limit . args)
  (define who 'sqlite3-soft-heap-limit)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-table-column-metadata . args)
  (define who 'sqlite3-table-column-metadata)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-load-extension . args)
  (define who 'sqlite3-load-extension)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-enable-load-extension . args)
  (define who 'sqlite3-enable-load-extension)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-auto-extension . args)
  (define who 'sqlite3-auto-extension)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-reset-auto-extension . args)
  (define who 'sqlite3-reset-auto-extension)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-create-module . args)
  (define who 'sqlite3-create-module)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-create-module-v2 . args)
  (define who 'sqlite3-create-module-v2)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-declare-vtab . args)
  (define who 'sqlite3-declare-vtab)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-overload-function . args)
  (define who 'sqlite3-overload-function)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-blob-open . args)
  (define who 'sqlite3-blob-open)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-blob-reopen . args)
  (define who 'sqlite3-blob-reopen)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-blob-close . args)
  (define who 'sqlite3-blob-close)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-blob-bytes . args)
  (define who 'sqlite3-blob-bytes)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-blob-read . args)
  (define who 'sqlite3-blob-read)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-blob-write . args)
  (define who 'sqlite3-blob-write)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-vfs-find . args)
  (define who 'sqlite3-vfs-find)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-vfs-register . args)
  (define who 'sqlite3-vfs-register)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-vfs-unregister . args)
  (define who 'sqlite3-vfs-unregister)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-mutex-alloc . args)
  (define who 'sqlite3-mutex-alloc)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-mutex-free . args)
  (define who 'sqlite3-mutex-free)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-mutex-enter . args)
  (define who 'sqlite3-mutex-enter)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-mutex-try . args)
  (define who 'sqlite3-mutex-try)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-mutex-leave . args)
  (define who 'sqlite3-mutex-leave)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-mutex-held . args)
  (define who 'sqlite3-mutex-held)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-mutex-notheld . args)
  (define who 'sqlite3-mutex-notheld)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-db-mutex . args)
  (define who 'sqlite3-db-mutex)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-file-control . args)
  (define who 'sqlite3-file-control)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-test-control . args)
  (define who 'sqlite3-test-control)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-status . args)
  (define who 'sqlite3-status)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-db-status . args)
  (define who 'sqlite3-db-status)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-stmt-status . args)
  (define who 'sqlite3-stmt-status)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-backup-init . args)
  (define who 'sqlite3-backup-init)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-backup-step . args)
  (define who 'sqlite3-backup-step)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-backup-finish . args)
  (define who 'sqlite3-backup-finish)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-backup-remaining . args)
  (define who 'sqlite3-backup-remaining)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-backup-pagecount . args)
  (define who 'sqlite3-backup-pagecount)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-unlock-notify . args)
  (define who 'sqlite3-unlock-notify)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-stricmp . args)
  (define who 'sqlite3-stricmp)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-strnicmp . args)
  (define who 'sqlite3-strnicmp)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-log . args)
  (define who 'sqlite3-log)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-wal-hook . args)
  (define who 'sqlite3-wal-hook)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-wal-autocheckpoint . args)
  (define who 'sqlite3-wal-autocheckpoint)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-wal-checkpoint . args)
  (define who 'sqlite3-wal-checkpoint)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-wal-checkpoint-v2 . args)
  (define who 'sqlite3-wal-checkpoint-v2)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-vtab-config . args)
  (define who 'sqlite3-vtab-config)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-vtab-on-conflict . args)
  (define who 'sqlite3-vtab-on-conflict)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (sqlite3-rtree-geometry-callback . args)
  (define who 'sqlite3-rtree-geometry-callback)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))


;;;; done

(set-rtd-printer! (type-descriptor sqlite3)       %struct-sqlite3-printer)

(post-gc-hooks (cons* %sqlite3-guardian-destructor
		      (post-gc-hooks)))

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-pathnames 'scheme-indent-function 1)
;; eval: (put 'with-pathnames/utf8 'scheme-indent-function 1)
;; eval: (put 'with-pathnames/utf16n 'scheme-indent-function 1)
;; eval: (put 'with-bytevectors 'scheme-indent-function 1)
;; eval: (put 'with-bytevectors/or-false 'scheme-indent-function 1)
;; eval: (put 'with-ascii-bytevectors 'scheme-indent-function 1)
;; eval: (put 'with-utf8-bytevectors 'scheme-indent-function 1)
;; eval: (put 'with-utf16-bytevectors 'scheme-indent-function 1)
;; End:
