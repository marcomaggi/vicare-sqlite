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

    ;; library initialisation, finalisation, configuration and auxiliary
    sqlite3-initialize			sqlite3-shutdown
    sqlite3-os-init			sqlite3-os-end
    sqlite3-config
    sqlite3-memory-used			sqlite3-memory-highwater
    sqlite3-enable-shared-cache
    sqlite3-release-memory		sqlite3-db-release-memory
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

    ;; compiled options
    sqlite3-compileoption-used		sqlite3-compileoption-get
    sqlite3-threadsafe

    ;; error codes and error messages
    sqlite3-errcode			sqlite3-extended-errcode
    sqlite3-errmsg			sqlite3-errmsg16

    ;; connection handling
    sqlite3?				sqlite3?/open
    sqlite3-destructor			set-sqlite3-destructor!
    sqlite3-close			sqlite3-open
    sqlite3-open16			sqlite3-open-v2
    sqlite3-db-config			sqlite3-extended-result-codes
    sqlite3-busy-handler		make-sqlite3-busy-handler-callback
    sqlite3-busy-timeout
    sqlite3-limit			sqlite3-get-autocommit
    sqlite3-db-filename			sqlite3-db-filename/string
    sqlite3-db-readonly			sqlite3-next-stmt
    sqlite3-commit-hook			make-sqlite3-commit-hook-callback
    sqlite3-rollback-hook		make-sqlite3-rollback-hook-callback
    sqlite3-update-hook			make-sqlite3-update-hook-callback
    sqlite3-trace			make-sqlite3-trace-callback
    sqlite3-profile			make-sqlite3-profile-callback
    sqlite3-table-column-metadata	sqlite3-db-status

    sqlite3-set-authorizer		make-sqlite3-authorizer-callback
    sqlite3-authorizer-return-code->symbol
    sqlite3-authorizer-action-code->symbol

    (rename (sqlite3-db-readonly	sqlite3-db-readonly?))

    ;; convenience execution of SQL snippets
    sqlite3-exec			sqlite3-exec*
    make-sqlite3-exec-callback
    sqlite3-get-table			sqlite3-free-table
    sqlite3-table-to-vector

    ;; SQL execution auxiliary functions
    sqlite3-last-insert-rowid
    sqlite3-changes			sqlite3-total-changes
    sqlite3-interrupt
    sqlite3-complete			sqlite3-complete16
    sqlite3-progress-handler		make-sqlite3-progress-handler-callback

    ;; prepared SQL statements
    sqlite3-stmt?			sqlite3-stmt?/valid
    sqlite3-stmt-destructor		set-sqlite3-stmt-destructor!
    sqlite3-finalize
    sqlite3-prepare			sqlite3-prepare-v2
    sqlite3-prepare16			sqlite3-prepare16-v2
    sqlite3-sql				sqlite3-sql/string
    sqlite3-stmt-readonly		sqlite3-stmt-busy
    sqlite3-step			sqlite3-reset
    (rename (sqlite3-stmt-readonly	sqlite3-stmt-readonly?)
	    (sqlite3-stmt-busy		sqlite3-stmt-busy?)
	    (sqlite3-stmt-connection	sqlite3-db-handle))
    sqlite3-stmt-status

    ;; prepared-SQL statements: binding parameters to values
    sqlite3-bind-blob			sqlite3-bind-double
    sqlite3-bind-int			sqlite3-bind-int64
    sqlite3-bind-null			sqlite3-bind-text
    sqlite3-bind-text16			sqlite3-bind-value
    sqlite3-bind-zeroblob
    sqlite3-bind-parameter-count	sqlite3-bind-parameter-name
    sqlite3-bind-parameter-index
    sqlite3-clear-bindings

    ;; prepared SQL statement: inspecting the resulting row
    sqlite3-column-count
    sqlite3-column-name			sqlite3-column-name16
    sqlite3-column-name/string		sqlite3-column-name16/string
    sqlite3-column-database-name	sqlite3-column-database-name16
    sqlite3-column-database-name/string	sqlite3-column-database-name16/string
    sqlite3-column-table-name		sqlite3-column-table-name16
    sqlite3-column-table-name/string	sqlite3-column-table-name16/string
    sqlite3-column-origin-name		sqlite3-column-origin-name16
    sqlite3-column-origin-name/string	sqlite3-column-origin-name16/string
    sqlite3-column-decltype		sqlite3-column-decltype16
    sqlite3-column-decltype/string	sqlite3-column-decltype16/string
    sqlite3-data-count
    sqlite3-column-blob
    sqlite3-column-bytes		sqlite3-column-bytes16
    sqlite3-column-double
    sqlite3-column-int			sqlite3-column-int64
    sqlite3-column-text			sqlite3-column-text16
    sqlite3-column-text/string		sqlite3-column-text16/string
    sqlite3-column-type			sqlite3-column-value

    ;; SQLite extensions
    sqlite3-load-extension		sqlite3-enable-load-extension
    sqlite3-auto-extension		sqlite3-reset-auto-extension

    ;; BLOBs for incremental input/output
    sqlite3-blob
    sqlite3-blob?			sqlite3-blob?/open
    sqlite3-blob-destructor		set-sqlite3-blob-destructor!
    sqlite3-blob-open			sqlite3-blob-reopen
    sqlite3-blob-close			sqlite3-blob-bytes
    sqlite3-blob-read			sqlite3-blob-write

    sqlite3-blob-connection		sqlite3-blob-database
    sqlite3-blob-table			sqlite3-blob-column
    sqlite3-blob-rowid			sqlite3-blob-write-enabled?

    ;; custom SQL functions
    sqlite3-create-function		sqlite3-create-function16
    sqlite3-create-function-v2
    sqlite3-delete-function		sqlite3-delete-function16

    make-sqlite3-function
    make-sqlite3-aggregate-step
    make-sqlite3-aggregate-final
    make-sqlite3-function-destructor

    sqlite3-value-blob			sqlite3-value-bytes
    sqlite3-value-bytes16		sqlite3-value-double
    sqlite3-value-int			sqlite3-value-int64
    sqlite3-value-text			sqlite3-value-text/string
    sqlite3-value-text16		sqlite3-value-text16/string
    sqlite3-value-text16le		sqlite3-value-text16le/string
    sqlite3-value-text16be		sqlite3-value-text16be/string
    sqlite3-value-type			sqlite3-value-numeric-type

    sqlite3-aggregate-context		sqlite3-user-data
    sqlite3-context-db-handle
    sqlite3-set-auxdata			sqlite3-get-auxdata
    make-sqlite3-auxdata-destructor

    sqlite3-result-blob			sqlite3-result-double
    sqlite3-result-error		sqlite3-result-error16
    sqlite3-result-error-toobig		sqlite3-result-error-nomem
    sqlite3-result-error-code		sqlite3-result-int
    sqlite3-result-int64		sqlite3-result-null
    sqlite3-result-text			sqlite3-result-text16
    sqlite3-result-text16le		sqlite3-result-text16be
    sqlite3-result-value		sqlite3-result-zeroblob

    sqlite3-value			sqlite3-value?
    sqlite3-context			sqlite3-context?

    ;; backup functions
    sqlite3-backup
    sqlite3-backup?			sqlite3-backup?/running
    sqlite3-backup-destructor		set-sqlite3-backup-destructor!
    sqlite3-backup-init			sqlite3-backup-finish
    sqlite3-backup-step
    sqlite3-backup-remaining		sqlite3-backup-pagecount

    ;; collation functions
    sqlite3-create-collation		sqlite3-create-collation16
    sqlite3-create-collation-v2
    sqlite3-collation-needed		sqlite3-collation-needed16
    make-sqlite3-collation-callback	make-sqlite3-collation-destructor
    make-sqlite3-collation-needed-callback
    make-sqlite3-collation-needed16-callback

    ;; miscellaneous functions
    sqlite3-sleep
    sqlite3-log				make-sqlite3-log-callback
    sqlite3-randomness			sqlite3-randomness!
    sqlite3-uri-parameter		sqlite3-uri-parameter/string
    sqlite3-uri-boolean			sqlite3-uri-int64

    ;; error code conversion
    sqlite3-error-code->symbol		sqlite3-extended-error-code->symbol

    ;; Interfaced but untested
    sqlite3-key				sqlite3-rekey
    sqlite3-activate-see		sqlite3-activate-cerod

    sqlite3-wal-hook			sqlite3-wal-autocheckpoint
    sqlite3-wal-checkpoint		sqlite3-wal-checkpoint-v2
    make-sqlite3-wal-hook

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
  (import (vicare)
    (vicare databases sqlite3 constants)
    (vicare syntactic-extensions)
    (prefix (vicare ffi) ffi.)
    (prefix (vicare databases sqlite3 unsafe-capi) capi.)
    (prefix (vicare unsafe-operations) unsafe.)
    (prefix (vicare words) words.))


;;;; helpers

(define (%any->string who obj)
  (cond ((string? obj)
	 obj)
	((bytevector? obj)
	 (utf8->string obj))
	((pointer? obj)
	 (cstring->string obj))
	((memory-block? obj)
	 (cstring->string (memory-block-pointer obj)
			  (memory-block-size    obj)))
	(else
	 (assertion-violation who
	   "expected string, UTF-8 bytevector or pointer" obj))))

(define-inline (%string->terminated-utf16n S)
  ;;It appears that SQLite's idea  of zero-terminated UTF-16 array means
  ;;that the  array must end  with 2  zero bytes; if  I do not  do this,
  ;;strange things happen.  (Marco Maggi; Mon Jul 30, 2012)
  (bytevector-append (string->utf16n S) '#vu8(0 0)))

(define-inline (%string->terminated-utf16le S)
  (bytevector-append (string->utf16le S) '#vu8(0 0)))

(define-inline (%string->terminated-utf16be S)
  (bytevector-append (string->utf16be S) '#vu8(0 0)))

(define-inline (%pathname? ?obj)
  (let ((obj ?obj))
    (or (pointer? obj )(bytevector? obj) (string? obj))))

(define-syntax %struct-destructor-application
  ;;Data structures might have a field called DESTRUCTOR holding #f or a
  ;;function  to be  applied to  the struct  instance upon  finalisation
  ;;(either when the finaliser is  explicitly called by the application,
  ;;or when  the garbage collector  performs the finalisation  through a
  ;;guardian).
  ;;
  ;;This macro should  be used in the finalisation  function to properly
  ;;apply the destructor to the structure.
  ;;
  ;;For example, given the definition:
  ;;
  ;;  (define-struct the-type (the-field destructor))
  ;;
  ;;the code:
  ;;
  ;;  (define (%unsafe.the-type-final struct)
  ;;    (%struct-destructor-application struct
  ;;      the-type-destructor set-the-type-destructor!))
  ;;
  ;;expands to:
  ;;
  ;;  (define (%unsafe.the-type-final struct)
  ;;    (let ((destructor (the-type-destructor struct)))
  ;;      (when destructor
  ;;        (guard (E (else (void)))
  ;;          (destructor struct))
  ;;        (?mutator ?struct #f))))
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?struct ?accessor ?mutator)
       (and (identifier? #'?struct)
	    (identifier? #'?accessor))
       #'(let ((destructor (?accessor ?struct)))
	   (when destructor
	     (guard (E (else (void)))
	       (destructor ?struct))
	     (?mutator ?struct #f)))))))

;;; --------------------------------------------------------------------

(define-syntax with-ascii-bytevectors
  (syntax-rules ()
    ((_ ((?ascii.bv ?ascii) ...) . ?body)
     (let ((?ascii.bv (let ((ascii ?ascii))
			(if (bytevector? ascii)
			    ascii
			  (string->ascii ascii))))
	   ...)
       . ?body))))

;;; --------------------------------------------------------------------

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
			  (%string->terminated-utf16n utf16))))
	   ...)
       . ?body))))

;;; --------------------------------------------------------------------

(define-syntax with-utf8-bytevectors/false
  (syntax-rules ()
    ((_ ((?utf8^ ?utf8) ...) . ?body)
     (let ((?utf8^ (let ((utf8 ?utf8))
		     ;;UTF8 must be a string or bytevector or pointer or
		     ;;memory-block or false.
		     (cond ((string? utf8)
			    (string->utf8 utf8))
			   ((not utf8)
			    #f)
			   (else
			    utf8))))
	   ...)
       . ?body))))

;;; --------------------------------------------------------------------

(define-syntax with-general-strings
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((?str^ ?str) ...) ?string->bytevector ?body0 . ?body)
       (identifier? #'?string->bytevector)
       #'(let ((?str^ (let ((str ?str))
			(cond ((string? str)
			       (?string->bytevector str))
			      ((or (bytevector?   str)
				   (pointer?      str)
				   (memory-block? str))
			       str)
			      (else
			       (assertion-violation #f "invalid general string" str)))))
	       ...)
	   ?body0 . ?body)))))

(define-syntax with-general-strings/false
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((?str^ ?str) ...) ?string->bytevector ?body0 . ?body)
       (identifier? #'?string->bytevector)
       #'(let ((?str^ (let ((str ?str))
			(cond ((string? str)
			       (?string->bytevector str))
			      ((or (bytevector?   str)
				   (pointer?      str)
				   (memory-block? str))
			       str)
			      ((not str)
			       str)
			      (else
			       (assertion-violation #f "invalid general string" str)))))
	       ...)
	   ?body0 . ?body)))))

;;; --------------------------------------------------------------------

(define-syntax with-utf8-bytevectors/pointers
  (syntax-rules ()
    ((_ ((?utf8^ ?utf8) ...) . ?body)
     (let ((?utf8^ (let ((utf8 ?utf8))
		     (if (string? utf8)
			 (string->utf8 utf8)
		       utf8)))
	   ...)
       . ?body))))

(define-syntax with-utf16-bytevectors/pointers
  (syntax-rules ()
    ((_ ((?utf16^ ?utf16) ...) . ?body)
     (let ((?utf16^ (let ((utf16 ?utf16))
		      (if (string? utf16)
			  (%string->terminated-utf16n utf16)
			utf16)))
	   ...)
       . ?body))))

(define-syntax with-utf16le-bytevectors/pointers
  (syntax-rules ()
    ((_ ((?utf16^ ?utf16) ...) . ?body)
     (let ((?utf16^ (let ((utf16 ?utf16))
		      (if (string? utf16)
			  (%string->terminated-utf16le utf16)
			utf16)))
	   ...)
       . ?body))))

(define-syntax with-utf16be-bytevectors/pointers
  (syntax-rules ()
    ((_ ((?utf16^ ?utf16) ...) . ?body)
     (let ((?utf16^ (let ((utf16 ?utf16))
		      (if (string? utf16)
			  (%string->terminated-utf16be utf16)
			utf16)))
	   ...)
       . ?body))))

;;; --------------------------------------------------------------------

(define-syntax with-pathnames/utf8
  (syntax-rules ()
    ((_ ((?pathname.bv ?pathname) ...) . ?body)
     (let ((?pathname.bv (let ((pathname ?pathname))
			   (cond ((string? pathname)
				  (string->utf8 pathname))
				 ((or (bytevector? pathname)
				      (pointer? pathname))
				  pathname)
				 (else
				  (assertion-violation #f
				    "expected string or bytevector or pointer"
				    pathname)))))
	   ...)
       . ?body))))

(define-syntax with-pathnames/utf16n
  (syntax-rules ()
    ((_ ((?pathname.bv ?pathname) ...) . ?body)
     (let ((?pathname.bv (let ((pathname ?pathname))
			   (cond ((string? pathname)
				  (%string->terminated-utf16n pathname))
				 ((or (bytevector? pathname)
				      (pointer? pathname))
				  pathname)
				 (else
				  (assertion-violation #f
				    "expected string or bytevector or pointer"
				    pathname)))))
	   ...)
       . ?body))))


;;;; arguments validation

(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

(define-argument-validation (pointer who obj)
  (pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))

(define-argument-validation (flonum who obj)
  (flonum? obj)
  (assertion-violation who "expected flonum as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (fixnum/false who obj)
  (or (not obj) (fixnum? obj))
  (assertion-violation who "expected false or fixnum as argument" obj))

(define-argument-validation (string/false who obj)
  (or (not obj) (string? obj))
  (assertion-violation who "expected false or string as argument" obj))

(define-argument-validation (bytevector/false who obj)
  (or (not obj) (bytevector? obj))
  (assertion-violation who "expected false or bytevector as argument" obj))

(define-argument-validation (pointer/false who obj)
  (or (not obj) (pointer? obj))
  (assertion-violation who "expected false or pointer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-int who obj)
  (words.signed-int? obj)
  (assertion-violation who
    "expected exact integer representing C language signed int as argument" obj))

(define-argument-validation (signed-int/false who obj)
  (or (not obj) (words.signed-int? obj))
  (assertion-violation who
    "expected false or exact integer representing C language signed int as argument" obj))

(define-argument-validation (non-negative-signed-int who obj)
  (and (words.signed-int? obj)
       (<= 0 obj))
  (assertion-violation who
    "expected exact integer representing C language signed int as argument" obj))

(define-argument-validation (signed-int64 who obj)
  (words.word-s64? obj)
  (assertion-violation who
    "expected exact integer representing C language signed int as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (general-string who obj)
  (or (string? obj) (bytevector? obj) (pointer? obj) (memory-block? obj))
  (assertion-violation who
    "expected string, bytevector, memory-block or pointer as argument" obj))

(define-argument-validation (general-string/false who obj)
  (or (not obj) (bytevector? obj) (string? obj) (pointer? obj) (memory-block? obj))
  (assertion-violation who
    "expected false, string, bytevector, memory-block or pointer as argument" obj))

(define-argument-validation (index-of-general-string who idx str)
  ;;When the general string STR is an actual Scheme string: we expect it
  ;;to  have  been already  converted  to  a bytevector  of  appropriate
  ;;encoding.
  (cond ((bytevector? str)
	 (and (fixnum? idx)
	      (unsafe.fx>= idx 0)
	      (unsafe.fx<  idx (unsafe.bytevector-length str))))
	((memory-block? str)
	 (and (>= idx 0)
	      (<  idx (memory-block-size str))))
	(else #t))
  (assertion-violation who "index out of range for general string" str idx))

;;; --------------------------------------------------------------------

(define-argument-validation (general-buffer who obj)
  (or (bytevector? obj) (pointer? obj) (memory-block? obj))
  (assertion-violation who
    "expected bytevector, memory-block or pointer as general buffer argument" obj))

(define-argument-validation (general-buffer/false who obj)
  (or (not obj) (bytevector? obj) (pointer? obj) (memory-block? obj))
  (assertion-violation who
    "expected false, bytevector, memory-block or pointer as general buffer argument" obj))

(define-argument-validation (index-of-general-buffer who idx buf)
  ;;We assume that BUF has already been validated as general buffer.
  (cond ((bytevector? buf)
	 (and (fixnum? idx)
	      (unsafe.fx>= idx 0)
	      (unsafe.fx<  idx (unsafe.bytevector-length buf))))
	((memory-block? buf)
	 ;;Notice that SQLite  requires the index to be in  the range of
	 ;;"int".
	 (and (words.signed-int? idx)
	      (>= idx 0)
	      (<  idx (memory-block-size buf))))
	(else #t))
  (assertion-violation who
    "index out of range for general buffer" buf idx))

(define-argument-validation (index-and-count-of-general-buffer who idx count buf)
  ;;We assume that BUF has already  been validated as general buffer and
  ;;IDX has already been validated as index for BUF.
  (cond ((bytevector? buf)
	 (and (fixnum? count)
	      (unsafe.fx<= 0 count)
	      (let ((past (+ idx count)))
		(and (fixnum? past)
		     (unsafe.fx>= past 0)
		     (unsafe.fx<= past (unsafe.bytevector-length buf))))))
	((memory-block? buf)
	 ;;Notice that SQLite requires the index  and count to be in the
	 ;;range of "int".
	 (and (words.signed-int? count)
	      (<= 0 count)
	      (let ((past (+ idx count)))
		(and (words.size_t? past)
		     (>= past 0)
		     (<= past (memory-block-size buf))))))
	(else #t))
  (assertion-violation who
    "index and count out of range for general buffer" idx count buf))

;;; --------------------------------------------------------------------

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

(define-argument-validation (offset who obj)
  (and (fixnum? obj)
       (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as argument" obj))

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

(define-argument-validation (sqlite3-stmt who obj)
  (sqlite3-stmt? obj)
  (assertion-violation who "expected sqlite3-stmt instance as argument" obj))

(define-argument-validation (sqlite3-stmt/valid who obj)
  (sqlite3-stmt?/valid obj)
  (assertion-violation who
    "expected sqlite3-stmt instance representing valid statement as argument" obj))

(define-argument-validation (sqlite3-stmt/false who obj)
  (or (not obj) (sqlite3-stmt? obj))
  (assertion-violation who "expected false or sqlite3-stmt instance as argument" obj))

(define-argument-validation (sqlite3-blob who obj)
  (sqlite3-blob? obj)
  (assertion-violation who "expected sqlite3-blob instance as argument" obj))

(define-argument-validation (sqlite3-blob/open who obj)
  (sqlite3-blob?/open obj)
  (assertion-violation who
    "expected sqlite3-blob instance representing open connection as argument" obj))

(define-argument-validation (parameter-index who obj)
  (and (fixnum? obj)
       (unsafe.fx< 0 obj))
  (assertion-violation who "expected fixnum higher than zero as parameter index" obj))

(define-argument-validation (function-arity who obj)
  (and (fixnum? obj)
       (unsafe.fx<= -1 obj))
  (assertion-violation who "expected fixnum greater than -2 as function arity argument" obj))

(define-argument-validation (sqlite3-context who obj)
  (sqlite3-context? obj)
  (assertion-violation who "expected instance of sqlite3-context as argument" obj))

(define-argument-validation (sqlite3-value who obj)
  (sqlite3-value? obj)
  (assertion-violation who "expected instance of sqlite3-value as argument" obj))

(define-argument-validation (sqlite3-backup who obj)
  (sqlite3-backup? obj)
  (assertion-violation who "expected instance of sqlite3-backup as argument" obj))

(define-argument-validation (sqlite3-backup/running who obj)
  (sqlite3-backup?/running obj)
  (assertion-violation who "expected running instance of sqlite3-backup as argument" obj))


;;;; data structures: sqlite3 database connection

(define-struct sqlite3
  (pointer
		;Pointer object  to an instance  of the C  language type
		;"sqlite3".
   pathname
		;String representing the pathname of the database.
   statements
		;Hashtable holding  the SQL statements created  for this
		;connection.   When  this   connection  is  closed:  the
		;statements are finalised.
   blobs
		;Hahstable   holding   the   BLOBs  created   for   this
		;connection.  When this connection  is closed: the BLOBs
		;are finalised.
   owner?
		;Boolean,  when true:  finalising this  structure causes
		;the finalisation of  the database connection referenced
		;by the  POINTER field;  else finalising  this strucutre
		;leaves the connection open.
		;
		;This fiels allows SQLITE3-CONTEXT-DB-HANDLE to return a
		;SQLITE3  structure in  the  implementation function  of
		;an application defined SQL function.
   destructor
		;False or a user-supplied function to be called whenever
		;this instance  is closed.  The function  must accept at
		;least one argument being the data structure itself.
   ))

(define-inline (%make-sqlite3 pointer pathname)
  (make-sqlite3 pointer pathname
		(make-hashtable values =)
		(make-hashtable values =)
		#t #f))

(define-inline (%make-sqlite3/disown pointer pathname)
  (make-sqlite3 pointer pathname
		(make-hashtable values =)
		(make-hashtable values =)
		#f #f))

;;; --------------------------------------------------------------------

(define (%unsafe.sqlite3-close connection)
  (%struct-destructor-application connection
    $sqlite3-destructor $set-sqlite3-destructor!)
  (let-values (((dummy stmts)
		(hashtable-entries (sqlite3-statements connection))))
;;;(pretty-print stmts (current-error-port))
    (let ((len (unsafe.vector-length stmts)))
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   (hashtable-clear! (sqlite3-statements connection)))
;;;(pretty-print (unsafe.vector-ref stmts i) (current-error-port))
	(capi.sqlite3-finalize (unsafe.vector-ref stmts i)))))
  (let-values (((dummy blobs)
		(hashtable-entries (sqlite3-blobs connection))))
;;;(pretty-print blobs (current-error-port))
    (let ((len (unsafe.vector-length blobs)))
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   (hashtable-clear! (sqlite3-blobs connection)))
;;;(pretty-print (unsafe.vector-ref blobs i) (current-error-port))
	(capi.sqlite3-blob-close (unsafe.vector-ref blobs i)))))
  (when (sqlite3-owner? connection)
;;;(pretty-print (list 'close connection) (current-error-port))
    (capi.sqlite3-close connection)))

;;; --------------------------------------------------------------------

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
  (%display " owner?=")		(%write   (sqlite3-owner? S))
  (%display "]"))


;;;; data structures: sqlite3 prepared statement

(define-struct sqlite3-stmt
  (connection
		;Instance of  Scheme data  structure "sqlite3"  to which
		;this statement belongs.
   pointer
		;Pointer  object referencing  a C  language instance  of
		;"sqlite3_stmt".
   sql-code
		;Bytevector  holding  the  SQL  statement  code  in  the
		;encoding specified by the ENCODING field.
   encoding
		;Scheme symbol representing the encoding of the SQL code
		;in the SQL-CODE field.
   destructor
		;False or a user-supplied function to be called whenever
		;this instance  is closed.  The function  must accept at
		;least one argument being the data structure itself.
   ))

(define-inline (%make-sqlite3-stmt connection pointer sql-code encoding)
  (make-sqlite3-stmt connection pointer sql-code encoding
		     #f #;destructor))

(define-inline (%sqlite3-stmt-register! connection statement)
  (hashtable-set! (sqlite3-statements connection)
		  (pointer->integer (sqlite3-stmt-pointer statement))
		  statement))

;;; --------------------------------------------------------------------

(define (%unsafe.sqlite3-finalize statement)
  (%struct-destructor-application statement
    $sqlite3-stmt-destructor $set-sqlite3-stmt-destructor!)
  (let ((connection	(sqlite3-stmt-connection statement))
	(key		(pointer->integer (sqlite3-stmt-pointer statement))))
    (when connection
      (hashtable-delete! (sqlite3-statements connection) key)))
  (capi.sqlite3-finalize statement))

;;; --------------------------------------------------------------------

(define (sqlite3-stmt?/valid obj)
  (and (sqlite3-stmt? obj)
       (not (pointer-null? (sqlite3-stmt-pointer obj)))))

(define (%struct-sqlite3-stmt-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[sqlite3-stmt")
  (%display " connection=")	(%display (sqlite3-stmt-connection S))
  (%display " pointer=")	(%display (sqlite3-stmt-pointer    S))
  (%display " sql-code=")	(%write (let ((bv       (sqlite3-stmt-sql-code S))
					      (encoding (sqlite3-stmt-encoding S)))
					  (and bv
					       (case encoding
						 ((utf8)
						  (utf8->string bv))
						 ((utf16n)
						  (utf16n->string bv))
						 (else
						  (assertion-violation 'sqlite3-stmt
						    "invalid SQL code encoding"
						    encoding))))))
  (%display "]"))


;;;; data structures: sqlite3 BLOB

(define-struct sqlite3-blob
  (pointer
		;Pointer  object  referencing  an   instance  of  the  C
		;language type "sqlite3_blob".
   connection
		;Instance of  Scheme data  structure "sqlite3"  to which
		;this BLOB belongs.
   database
		;String representing  the name of the  database to which
		;this BLOB belongs.
   table
		;String representing the name of the table to which this
		;BLOB belongs.
   column
		;String  representing the  name of  the column  to which
		;this BLOB belongs.
   rowid
		;Exact integer representing the identifier of the row to
		;which this BLOB belongs.
   write-enabled?
		;Boolean, true if this BLOB is write enabled.
   destructor
		;False or a user-supplied function to be called whenever
		;this instance  is closed.  The function  must accept at
		;least one argument being the data structure itself.
   ))

(define-inline (%make-sqlite3-blob pointer connection
				   database table column rowid
				   write-enabled?)
  (make-sqlite3-blob pointer connection
		     database table column rowid
		     write-enabled? #f))

(define-inline (%sqlite3-blob-register! connection blob)
  (hashtable-set! (sqlite3-blobs connection)
		  (pointer->integer (sqlite3-blob-pointer blob))
		  blob))

;;; --------------------------------------------------------------------

(define (%unsafe.sqlite3-blob-close blob)
  (%struct-destructor-application blob
    $sqlite3-blob-destructor $set-sqlite3-blob-destructor!)
  (let ((connection (sqlite3-blob-connection blob)))
    (when connection
      (let ((T (sqlite3-blobs connection))
	    (K (pointer->integer (sqlite3-blob-pointer blob))))
	(when (hashtable? T)
	  (hashtable-delete! T K)))))
  (capi.sqlite3-blob-close blob))

;;; --------------------------------------------------------------------

(define (sqlite3-blob?/open obj)
  (and (sqlite3-blob? obj)
       (not (pointer-null? (sqlite3-blob-pointer obj)))))

(define (%struct-sqlite3-blob-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[sqlite3-blob")
  (%display " pointer=")	(%display (sqlite3-blob-pointer		S))
  (%display " connection=")	(%write   (sqlite3-blob-connection	S))
  (%display " database=")	(%write   (sqlite3-blob-database	S))
  (%display " table=")		(%write   (sqlite3-blob-table		S))
  (%display " column=")		(%write   (sqlite3-blob-column		S))
  (%display " rowid=")		(%display (sqlite3-blob-rowid		S))
  (%display " write-enabled?=")	(%display (sqlite3-blob-write-enabled?	S))
  (%display "]"))


;;;; data structures: sqlite3 value

(define-struct sqlite3-value
  (pointer))

(define (%struct-sqlite3-value-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (let ((P (sqlite3-value-pointer   S))
	(T (capi.sqlite3-value-type S)))
    (%display "#[sqlite3-value")
    (%display " pointer=")	(%display P)
    (%display " type=")		(%display (cond ((= T SQLITE_INTEGER)
						 "SQLITE_INTEGER")
						((= T SQLITE_FLOAT)
						 "SQLITE_FLOAT")
						((= T SQLITE_BLOB)
						 "SQLITE_BLOB")
						((= T SQLITE_TEXT)
						 "SQLITE_TEXT")
						((= T SQLITE_NULL)
						 "SQLITE_NULL")
						(else T)))
    (%display "]")))


;;;; data structures: sqlite3 context

(define-struct sqlite3-context
  (pointer))

(define (%struct-sqlite3-context-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (let ((P (sqlite3-context-pointer  S)))
    (%display "#[sqlite3-context")
    (%display " pointer=")	(%display P)
    (%display "]")))

;;These  two  are  useless  because:  in  the  course  of  an  aggregate
;;computation, we  cannot rely on the  context being the same  for every
;;call to the aggregate implementation functions.  (Marco Maggi; Aug 16,
;;2012)
;;
;; (define (sqlite3-context-hash context)
;;   (define who 'sqlite3-context-hash)
;;   (with-arguments-validation (who)
;;       ((sqlite3-context	context))
;;     (pointer->integer (sqlite3-context-pointer context))))
;;
;; (define (sqlite3-context=? context1 context2)
;;   (or (eq? context1 context2)
;;       (and (sqlite3-context? context1)
;; 	   (sqlite3-context? context2)
;; 	   (pointer=? (sqlite3-context-pointer context1)
;; 		      (sqlite3-context-pointer context2)))))


;;;; data structures: sqlite3 backup

(define-struct sqlite3-backup
  (pointer
		;Pointer  object  referencing  an   instance  of  the  C
		;language type "sqlite3_backup".   After an instance has
		;been finalised: this pointer is reset to NULL.
   dst-connection
		;Instance  of  "sqlite3"  representing  the  destination
		;connection.
   dst-name
		;String representing the destination database name.
   src-connection
		;Instance   of   "sqlite3"   representing   the   source
		;connection.
   src-name
		;String representing the source database name.
   destructor
		;False or a user-supplied function to be called whenever
		;this instance  is closed.  The function  must accept at
		;least one argument being the data structure itself.
   ))

(define-inline (%make-sqlite3-backup pointer dst-conn dst-name src-conn src-name)
  (make-sqlite3-backup pointer dst-conn dst-name src-conn src-name #f))

;;; --------------------------------------------------------------------

(define (%unsafe.sqlite3-backup-finish backup)
  (%struct-destructor-application backup
    $sqlite3-backup-destructor $set-sqlite3-backup-destructor!)
  (capi.sqlite3-backup-finish backup))

;;; --------------------------------------------------------------------

(define (sqlite3-backup?/running obj)
  (and (sqlite3-backup? obj)
       (not (pointer-null? (sqlite3-backup-pointer obj)))))

(define (%struct-sqlite3-backup-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (let ((P (sqlite3-backup-pointer  S)))
    (%display "#[sqlite3-backup")
    (%display " pointer=")	(%display P)
    (%display " dst-conn=")	(%display (sqlite3-backup-dst-connection S))
    (%display " dst-name=")	(%write   (sqlite3-backup-dst-name S))
    (%display " src-conn=")	(%display (sqlite3-backup-src-connection S))
    (%display " src-name=")	(%write   (sqlite3-backup-dst-name S))
    (%display "]")))


;;;; library initialisation, finalisation, configuration and auxiliary functions

(define (sqlite3-initialize)
  (capi.sqlite3-initialize))

(define (sqlite3-shutdown)
  (capi.sqlite3-shutdown))

(define (sqlite3-os-init)
  (capi.sqlite3-os-init))

(define (sqlite3-os-end)
  (capi.sqlite3-os-end))

;;; --------------------------------------------------------------------

(define (sqlite3-config option-identifier . args)
  (define who 'sqlite3-config)
  (with-arguments-validation (who)
      ((fixnum	option-identifier))
    (capi.sqlite3-config option-identifier (if (null? args)
					       #f
					     (list->vector args)))))

;;; --------------------------------------------------------------------

(define (sqlite3-memory-used)
  (capi.sqlite3-memory-used))

(define (sqlite3-memory-highwater reset)
  (capi.sqlite3-memory-highwater reset))

;;; --------------------------------------------------------------------

(define (sqlite3-enable-shared-cache bool)
  (capi.sqlite3-enable-shared-cache bool))

(define (sqlite3-release-memory number-of-bytes)
  (define who 'sqlite3-release-memory)
  (with-arguments-validation (who)
      ((signed-int	number-of-bytes))
    (capi.sqlite3-release-memory number-of-bytes)))

(define (sqlite3-soft-heap-limit64 limit)
  (define who 'sqlite3-soft-heap-limit64)
  (with-arguments-validation (who)
      ((signed-int64	limit))
    (capi.sqlite3-soft-heap-limit64 limit)))

(define (sqlite3-soft-heap-limit limit)
  (define who 'sqlite3-soft-heap-limit)
  (with-arguments-validation (who)
      ((signed-int	limit))
    (capi.sqlite3-soft-heap-limit limit)))

;;; --------------------------------------------------------------------

(define sqlite3-status
  (case-lambda
   ((opcode)
    (sqlite3-status opcode #f))
   ((opcode reset?)
    (define who 'sqlite3-status)
    (with-arguments-validation (who)
	((signed-int	opcode))
      (let ((rv (capi.sqlite3-status opcode reset?)))
	(if (vector? rv)
	    (values (unsafe.vector-ref rv 0)
		    (unsafe.vector-ref rv 1)
		    (unsafe.vector-ref rv 2))
	  (values rv #f #f)))))))


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
      ((general-string	pathname))
    (with-general-strings ((pathname^ pathname))
	string->utf8
      (let* ((conn	(%make-sqlite3 (null-pointer) (%any->string who pathname)))
	     (rv	(capi.sqlite3-open pathname^ conn)))
	(if (unsafe.fx= rv SQLITE_OK)
	    conn
	  rv)))))

(define (sqlite3-open16 pathname)
  (define who 'sqlite3-open16)
  (with-arguments-validation (who)
      ((general-string	pathname))
    (with-general-strings ((pathname^ pathname))
	%string->terminated-utf16n
      (let* ((conn	(%make-sqlite3 (null-pointer) (%any->string who pathname)))
	     (rv	(capi.sqlite3-open16 pathname^ conn)))
	(if (unsafe.fx= rv SQLITE_OK)
	    conn
	  rv)))))

(define sqlite3-open-v2
  (case-lambda
   ((pathname flags)
    (sqlite3-open-v2 pathname flags #f))
   ((pathname flags vfs-module)
    (define who 'sqlite3-open-v2)
    (with-arguments-validation (who)
	((general-string	pathname)
	 (signed-int		flags)
	 (general-string/false	vfs-module))
      (with-general-strings ((pathname^ pathname))
	  string->utf8
	(with-general-strings/false ((vfs-module^ vfs-module))
	    string->utf8
	  (let* ((conn	(%make-sqlite3 (null-pointer) (%any->string who pathname)))
		 (rv	(capi.sqlite3-open-v2 pathname^ conn flags vfs-module^)))
	    (if (unsafe.fx= rv SQLITE_OK)
		conn
	      rv))))))))

(define (sqlite3-close connection)
  (define who 'sqlite3-close)
  (with-arguments-validation (who)
      ((sqlite3	connection))
    (%unsafe.sqlite3-close connection)))

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

(define (sqlite3-limit connection limit-identifier limit-value)
  (define who 'sqlite3-limit)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (signed-int	limit-identifier)
       (signed-int	limit-value))
    (capi.sqlite3-limit connection limit-identifier limit-value)))

;;; --------------------------------------------------------------------

(define make-sqlite3-busy-handler-callback
  (let ((%sqlite3-busy-handler-callback-maker
	 ;; int (*) (void*,int)
	 (ffi.make-c-callback-maker 'signed-int '(pointer signed-int))))
    (lambda (user-scheme-callback)
      (%sqlite3-busy-handler-callback-maker
       (lambda (number-of-invocations)
	 (guard (E (else
		    #;(pretty-print E (current-error-port))
		    0))
	   (if (user-scheme-callback number-of-invocations)
	       1
	     0)))))))

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
      ((sqlite3/open	connection)
       (fixnum		milliseconds))
    (capi.sqlite3-busy-timeout connection milliseconds)))

;;; --------------------------------------------------------------------

(define (sqlite3-get-autocommit connection)
  (define who 'sqlite3-get-autocommit)
  (with-arguments-validation (who)
      ((sqlite3/open	connection))
    (capi.sqlite3-get-autocommit connection)))

(define (sqlite3-db-filename connection database)
  (define who 'sqlite3-db-filename)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (general-string	database))
    (with-general-strings ((database^ database))
	string->utf8
      (capi.sqlite3-db-filename connection database^))))

(define (sqlite3-db-filename/string connection database)
  (utf8->string (sqlite3-db-filename connection database)))

(define (sqlite3-db-readonly connection database)
  (define who 'sqlite3-db-readonly)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (general-string	database))
    (with-general-strings ((database^ database))
	string->utf8
      (capi.sqlite3-db-readonly connection database^))))

(define sqlite3-next-stmt
  (case-lambda
   ((connection)
    (sqlite3-next-stmt connection #f))
   ((connection statement)
    (define who 'sqlite3-next-stmt)
    (with-arguments-validation (who)
	((sqlite3/open		connection)
	 (sqlite3-stmt/false	statement))
      (let ((rv (capi.sqlite3-next-stmt connection statement)))
	(and rv (hashtable-ref (sqlite3-statements connection) rv #f)))))))

;;; --------------------------------------------------------------------

(define (sqlite3-commit-hook connection callback)
  (define who 'sqlite3-commit-hook)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (callback/false	callback))
    (capi.sqlite3-commit-hook connection callback)
    (values)))

(define make-sqlite3-commit-hook-callback
  ;; int (*) (void*)
  (let ((maker (ffi.make-c-callback-maker 'signed-int '(pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (dummy)
	       (guard (E (else 0))
		 (if (user-scheme-callback) 1 0)))))))

;;; --------------------------------------------------------------------

(define (sqlite3-rollback-hook connection callback)
  (define who 'sqlite3-rollback-hook)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (callback/false	callback))
    (capi.sqlite3-rollback-hook connection callback)
    (values)))

(define make-sqlite3-rollback-hook-callback
  ;; void (*) (void*)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (dummy)
	       (guard (E (else (void)))
		 (user-scheme-callback)
		 (void)))))))

;;; --------------------------------------------------------------------

(define (sqlite3-update-hook connection callback)
  (define who 'sqlite3-update-hook)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (callback/false	callback))
    (capi.sqlite3-update-hook connection callback)))

(define make-sqlite3-update-hook-callback
  ;; void (*) (void *, int, char const *, char const *, sqlite3_int64)
  (let ((maker (ffi.make-c-callback-maker 'void
					  '(pointer signed-int pointer pointer int64_t))))
    (lambda (user-scheme-callback)
      (maker (lambda (dummy operation database-name.ptr table-name.ptr rowid)
	       (guard (E (else 0))
		 (user-scheme-callback operation database-name.ptr table-name.ptr rowid)
		 (void)))))))

;;; --------------------------------------------------------------------

(define (sqlite3-trace connection callback)
  (define who 'sqlite3-trace)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (callback	callback))
    (capi.sqlite3-trace connection callback)))

(define make-sqlite3-trace-callback
  ;; void (*) (void*, const char*)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (dummy sql-code)
	       (guard (E (else (void)))
		 (user-scheme-callback sql-code)
		 (void)))))))

;;; --------------------------------------------------------------------

(define (sqlite3-profile connection callback)
  (define who 'sqlite3-profile)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (callback	callback))
    (capi.sqlite3-profile connection callback)))

(define make-sqlite3-profile-callback
  ;; void (*) (void*, const char*, sqlite3_uint64)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer pointer uint64_t))))
    (lambda (user-scheme-callback)
      (maker (lambda (dummy sql-code nanoseconds)
	       (guard (E (else (void)))
		 (user-scheme-callback sql-code nanoseconds)
		 (void)))))))

;;; --------------------------------------------------------------------

(define (sqlite3-db-release-memory connection)
  (define who 'sqlite3-db-release-memory)
  (with-arguments-validation (who)
      ((sqlite3/open	connection))
    (capi.sqlite3-db-release-memory connection)))

;;; --------------------------------------------------------------------

(define (sqlite3-table-column-metadata connection database-name table-name column-name)
  (define who 'sqlite3-table-column-metadata)
  (with-arguments-validation (who)
      ((sqlite3/open		connection)
       (general-string/false	database-name)
       (general-string		table-name)
       (general-string		column-name))
    (with-general-strings/false ((database-name^ database-name))
	string->utf8
      (with-general-strings ((table-name^	table-name)
			     (column-name^	column-name))
	  string->utf8
	(let ((rv (capi.sqlite3-table-column-metadata connection database-name^
						      table-name^ column-name^)))
	  (if (vector? rv)
	      (values SQLITE_OK
		      (unsafe.vector-ref rv 0)
		      (unsafe.vector-ref rv 1)
		      (unsafe.vector-ref rv 2)
		      (unsafe.vector-ref rv 3)
		      (unsafe.vector-ref rv 4))
	    (values rv #f #f #f #f #f)))))))

;;; --------------------------------------------------------------------

(define (sqlite3-set-authorizer connection callback)
  (define who 'sqlite3-set-authorizer)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (callback/false	callback))
    (capi.sqlite3-set-authorizer connection callback)))

(define make-sqlite3-authorizer-callback
  ;;int (*xAuth) (void*, int, const char*, const char*, const char*, const char*)
  (let ((make (ffi.make-c-callback-maker 'signed-int
					 '(pointer signed-int
						   pointer pointer pointer pointer))))
    (lambda (user-scheme-callback)
      (make (lambda (dummy action-integer cstr1 cstr2 cstr3 cstr4)
	      (guard (E (else
			 #;(pretty-print E (current-error-port))
			 SQLITE_DENY))
		(user-scheme-callback action-integer cstr1 cstr2 cstr3 cstr4)))))))

;;; --------------------------------------------------------------------

(define sqlite3-db-status
  (case-lambda
   ((connection opcode)
    (sqlite3-db-status connection opcode #f))
   ((connection opcode reset?)
    (define who 'sqlite3-db-status)
    (with-arguments-validation (who)
	((sqlite3/open	connection)
	 (signed-int	opcode))
      (let ((rv (capi.sqlite3-db-status connection opcode reset?)))
	(if (vector? rv)
	    (values (unsafe.vector-ref rv 0)
		    (unsafe.vector-ref rv 1)
		    (unsafe.vector-ref rv 2))
	  (values rv #f #f)))))))


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
	 (general-string	sql-snippet)
	 (callback/false	each-row-callback))
      (with-general-strings ((sql-snippet^ sql-snippet))
	  string->utf8
	(let ((rv (capi.sqlite3-exec connection sql-snippet^ each-row-callback)))
	  (if (pair? rv)
	      (values (unsafe.car rv) (utf8->string (unsafe.cdr rv)))
	    (values rv #f))))))))

(define (sqlite3-exec* . args)
  (let-values (((code errmsg)
		(apply sqlite3-exec args)))
    code))

;;; --------------------------------------------------------------------

(define (sqlite3-get-table connection sql-snippet)
  (define who 'sqlite3-get-table)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (general-string	sql-snippet))
    (with-general-strings ((sql-snippet^ sql-snippet))
	string->utf8
      (let ((rv (capi.sqlite3-get-table connection sql-snippet^)))
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
      ((general-string	sql-snippet))
    (with-general-strings ((sql-snippet^ sql-snippet))
	string->utf8
      (capi.sqlite3-complete sql-snippet^))))

(define (sqlite3-complete16 sql-snippet)
  (define who 'sqlite3-complete16)
  (with-arguments-validation (who)
      ((general-string	sql-snippet))
    (with-general-strings ((sql-snippet^ sql-snippet))
	%string->terminated-utf16n
      (capi.sqlite3-complete16 sql-snippet^))))

;;; --------------------------------------------------------------------

(define make-sqlite3-progress-handler-callback
  (let ((%sqlite3-progress-handler-callback-maker
	 ;; int(*)(void*)
	 (ffi.make-c-callback-maker 'signed-int '(pointer))))
    (lambda (user-scheme-callback)
      (%sqlite3-progress-handler-callback-maker
       (lambda ()
	 (guard (E (else
		    #;(pretty-print E (current-error-port))
		    0))
	   (if (user-scheme-callback)
	       1
	     0)))))))

(define sqlite3-progress-handler
  (case-lambda
   ((connection)
    (sqlite3-progress-handler connection 0 #f))
   ((connection instruction-count callback)
    (define who 'sqlite3-progress-handler)
    (with-arguments-validation (who)
	((sqlite3/open			connection)
	 (non-negative-signed-int	instruction-count)
	 (callback/false		callback))
      (capi.sqlite3-progress-handler connection instruction-count callback)))))


;;;; prepared SQL statements: initialisation, finalisation, etc

(define (sqlite3-finalize statement)
  (define who 'sqlite3-finalize)
  (with-arguments-validation (who)
      ((sqlite3-stmt	statement))
    (%unsafe.sqlite3-finalize statement)))

;;; --------------------------------------------------------------------

(define sqlite3-prepare
  (case-lambda
   ((connection sql-snippet)
    (sqlite3-prepare connection sql-snippet 0 #t))
   ((connection sql-snippet sql-offset)
    (sqlite3-prepare connection sql-snippet sql-offset #t))
   ((connection sql-snippet sql-offset store-sql-text?)
    (define who 'sqlite3-prepare)
    (with-arguments-validation (who)
	((sqlite3/open		connection)
	 (general-string	sql-snippet)
	 (offset		sql-offset))
      (with-general-strings ((sql-snippet^ sql-snippet))
	  string->utf8
	(with-arguments-validation (who)
	    ((index-of-general-string sql-offset sql-snippet^))
	  (let* ((stmt (%make-sqlite3-stmt connection (null-pointer) #;pointer
					   #f #;sql-code 'utf8))
		 (rv   (capi.sqlite3-prepare connection sql-snippet^ sql-offset
					     stmt store-sql-text?)))
	    (if (pair? rv)
		(begin
		  (%sqlite3-stmt-register! connection stmt)
		  (values (unsafe.car rv)
			  stmt
			  (unsafe.cdr rv)))
	      (values rv #f sql-offset)))))))))

(define sqlite3-prepare-v2
  (case-lambda
   ((connection sql-snippet)
    (sqlite3-prepare-v2 connection sql-snippet 0 #t))
   ((connection sql-snippet sql-offset)
    (sqlite3-prepare-v2 connection sql-snippet sql-offset #t))
   ((connection sql-snippet sql-offset store-sql-text?)
    (define who 'sqlite3-prepare-v2)
    (with-arguments-validation (who)
	((sqlite3/open		connection)
	 (general-string	sql-snippet)
	 (offset		sql-offset))
      (with-general-strings ((sql-snippet^ sql-snippet))
	  string->utf8
	(with-arguments-validation (who)
	    ((index-of-general-string sql-offset sql-snippet^))
	  (let* ((stmt (%make-sqlite3-stmt connection
					   (null-pointer) #;pointer
					   #f #;sql-code 'utf8))
		 (rv   (capi.sqlite3-prepare-v2 connection sql-snippet^ sql-offset
						stmt store-sql-text?)))
	    (if (pair? rv)
		(begin
		  (%sqlite3-stmt-register! connection stmt)
		  (values (unsafe.car rv)
			  stmt
			  (unsafe.cdr rv)))
	      (values rv #f sql-offset)))))))))

(define sqlite3-prepare16
  (case-lambda
   ((connection sql-snippet)
    (sqlite3-prepare16 connection sql-snippet 0 #t))
   ((connection sql-snippet sql-offset)
    (sqlite3-prepare16 connection sql-snippet sql-offset #t))
   ((connection sql-snippet sql-offset store-sql-text?)
    (define who 'sqlite3-prepare16)
    (with-arguments-validation (who)
	((sqlite3/open		connection)
	 (general-string	sql-snippet)
	 (offset		sql-offset))
      (with-general-strings ((sql-snippet^ sql-snippet))
	  %string->terminated-utf16n
	(with-arguments-validation (who)
	    ((index-of-general-string sql-offset sql-snippet^))
	  (let* ((stmt (%make-sqlite3-stmt connection
					   (null-pointer) #;pointer
					   #f #;sql-code 'utf16n))
		 (rv   (capi.sqlite3-prepare16 connection sql-snippet^ sql-offset
					       stmt store-sql-text?)))
	    (if (pair? rv)
		(begin
		  (%sqlite3-stmt-register! connection stmt)
		  (values (unsafe.car rv)
			  stmt
			  (unsafe.cdr rv)))
	      (values rv #f sql-offset)))))))))

(define sqlite3-prepare16-v2
  (case-lambda
   ((connection sql-snippet)
    (sqlite3-prepare16-v2 connection sql-snippet 0 #t))
   ((connection sql-snippet sql-offset)
    (sqlite3-prepare16-v2 connection sql-snippet sql-offset #t))
   ((connection sql-snippet sql-offset store-sql-text?)
    (define who 'sqlite3-prepare16-v2)
    (with-arguments-validation (who)
	((sqlite3/open		connection)
	 (general-string	sql-snippet)
	 (offset		sql-offset))
      (with-general-strings ((sql-snippet^ sql-snippet))
	  %string->terminated-utf16n
	(with-arguments-validation (who)
	    ((index-of-general-string sql-offset sql-snippet^))
	  (let* ((stmt (%make-sqlite3-stmt connection
					   (null-pointer) #;pointer
					   #f #;sql-code 'utf16n))
		 (rv   (capi.sqlite3-prepare16-v2 connection sql-snippet^ sql-offset
						  stmt store-sql-text?)))
	    (if (pair? rv)
		(begin
		  (%sqlite3-stmt-register! connection stmt)
		  (values (unsafe.car rv)
			  stmt
			  (unsafe.cdr rv)))
	      (values rv #f sql-offset)))))))))

;;; --------------------------------------------------------------------

(define (sqlite3-step statement)
  (define who 'sqlite3-step)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement))
    (capi.sqlite3-step statement)))

(define (sqlite3-reset statement)
  (define who 'sqlite3-reset)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement))
    (capi.sqlite3-reset statement)))

;;; --------------------------------------------------------------------

(define (sqlite3-sql statement)
  (define who 'sqlite3-sql)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid statement))
    (capi.sqlite3-sql statement)))

(define (sqlite3-sql/string statement)
  (define who 'sqlite3-sql/string)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid statement))
    (utf8->string (capi.sqlite3-sql statement))))

(define (sqlite3-stmt-readonly statement)
  (define who 'sqlite3-stmt-readonly)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid statement))
    (capi.sqlite3-stmt-readonly statement)))

(define (sqlite3-stmt-busy statement)
  (define who 'sqlite3-stmt-busy)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid statement))
    (capi.sqlite3-stmt-busy statement)))

;;Not interfaced.  SQLITE3-STMT-CONNECTION is used instead.
;;
;; (define (sqlite3-db-handle statement)
;;   (define who 'sqlite3-db-handle)
;;   (with-arguments-validation (who)
;;       ((sqlite3-stmt/valid statement))
;;     (capi.sqlite3-db-handle statement)))

(define sqlite3-stmt-status
  (case-lambda
   ((statement opcode)
    (sqlite3-stmt-status statement opcode #f))
   ((statement opcode reset?)
    (define who 'sqlite3-stmt-status)
    (with-arguments-validation (who)
	((sqlite3-stmt/valid	statement)
	 (signed-int		opcode))
      (capi.sqlite3-stmt-status statement opcode reset?)))))


;;;; prepared SQL statements: binding parameters to values

(define (sqlite3-bind-blob statement parameter-index
			   blob.data blob.start blob.length blob.destructor)
  (define who 'sqlite3-bind-blob)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (fixnum			parameter-index)
       (general-string		blob.data)
       (fixnum			blob.start)
       (fixnum			blob.length)
       (pointer			blob.destructor))
    (with-general-strings ((blob.data^ blob.data))
	string->utf8
      (capi.sqlite3-bind-blob statement parameter-index
			      blob.data^ blob.start blob.length blob.destructor))))

(define (sqlite3-bind-double statement parameter-index value)
  (define who 'sqlite3-bind-double)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (fixnum			parameter-index)
       (flonum			value))
    (capi.sqlite3-bind-double statement parameter-index value)))

(define (sqlite3-bind-int statement parameter-index value)
  (define who 'sqlite3-bind-int)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (fixnum			parameter-index)
       (signed-int		value))
    (capi.sqlite3-bind-int statement parameter-index value)))

(define (sqlite3-bind-int64 statement parameter-index value)
  (define who 'sqlite3-bind-int64)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (fixnum			parameter-index)
       (signed-int64		value))
    (capi.sqlite3-bind-int64 statement parameter-index value)))

(define (sqlite3-bind-null statement parameter-index)
  (define who 'sqlite3-bind-null)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (fixnum			parameter-index))
    (capi.sqlite3-bind-null statement parameter-index)))

(define (sqlite3-bind-text statement parameter-index
			   blob.data blob.start blob.length blob.destructor)
  (define who 'sqlite3-bind-text)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (parameter-index		parameter-index)
       (general-string		blob.data)
       (fixnum/false		blob.start)
       (fixnum/false		blob.length)
       (pointer			blob.destructor))
    (with-general-strings ((blob.data^ blob.data))
	string->utf8
      (when (or (string?     blob.data)
		(bytevector? blob.data))
	(unless blob.start
	  (set! blob.start 0))
	(unless blob.length
	  (set! blob.length (bytevector-length blob.data^))))
      (capi.sqlite3-bind-text statement parameter-index
			      blob.data^ blob.start blob.length blob.destructor))))

(define (sqlite3-bind-text16 statement parameter-index
			     blob.data blob.start blob.length blob.destructor)
  (define who 'sqlite3-bind-text16)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (parameter-index		parameter-index)
       (general-string		blob.data)
       (fixnum/false		blob.start)
       (fixnum/false		blob.length)
       (pointer			blob.destructor))
    (with-general-strings ((blob.data^ blob.data))
	%string->terminated-utf16n
      (cond ((bytevector? blob.data^)
	     (unless blob.start
	       (set! blob.start 0))
	     (unless blob.length
	       (set! blob.length (bytevector-length blob.data^))))
	    ((memory-block? blob.data^)
	     (unless blob.start
	       (set! blob.start 0))
	     (unless blob.length
	       (set! blob.length (memory-block-size blob.data^)))))
      (capi.sqlite3-bind-text16 statement parameter-index
				blob.data^ blob.start blob.length blob.destructor))))

(define (sqlite3-bind-value statement parameter-index value)
  (define who 'sqlite3-bind-value)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (fixnum			parameter-index)
       (sqlite3-value		value))
    (capi.sqlite3-bind-value statement parameter-index value)))

(define (sqlite3-bind-zeroblob statement parameter-index blob-length)
  (define who 'sqlite3-bind-zeroblob)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (fixnum			parameter-index)
       (signed-int		blob-length))
    (capi.sqlite3-bind-zeroblob statement parameter-index blob-length)))

(define (sqlite3-bind-parameter-count statement)
  (define who 'sqlite3-bind-parameter-count)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement))
    (capi.sqlite3-bind-parameter-count statement)))

(define (sqlite3-bind-parameter-name statement parameter-index)
  (define who 'sqlite3-bind-parameter-name)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (fixnum			parameter-index))
    (let ((rv (capi.sqlite3-bind-parameter-name statement parameter-index)))
      (and rv (utf8->string rv)))))

(define (sqlite3-bind-parameter-index statement parameter-name)
  (define who 'sqlite3-bind-parameter-index)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (general-string		parameter-name))
    (with-general-strings ((parameter-name^ parameter-name))
	string->utf8
      (capi.sqlite3-bind-parameter-index statement parameter-name^))))

(define (sqlite3-clear-bindings statement)
  (define who 'sqlite3-clear-bindings)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement))
    (capi.sqlite3-clear-bindings statement)))


;;;; prepared SQL statements: inspecting the resulting row

(define (sqlite3-column-count statement)
  (define who 'sqlite3-column-count)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement))
    (capi.sqlite3-column-count statement)))

;;; --------------------------------------------------------------------

(define (sqlite3-column-name statement column-index)
  (define who 'sqlite3-column-name)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-name statement column-index)))

(define (sqlite3-column-name/string statement column-index)
  (let ((rv (sqlite3-column-name statement column-index)))
    (and rv (utf8->string rv))))

(define (sqlite3-column-name16 statement column-index)
  (define who 'sqlite3-column-name16)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-name16 statement column-index)))

(define (sqlite3-column-name16/string statement column-index)
  (let ((rv (sqlite3-column-name16 statement column-index)))
    (and rv (utf16n->string rv))))

;;; --------------------------------------------------------------------

(define (sqlite3-column-database-name statement column-index)
  (define who 'sqlite3-column-database-name)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-database-name statement column-index)))

(define (sqlite3-column-database-name/string statement column-index)
  (let ((rv (sqlite3-column-database-name statement column-index)))
    (and rv (utf8->string rv))))

(define (sqlite3-column-database-name16 statement column-index)
  (define who 'sqlite3-column-database-name16)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-database-name16 statement column-index)))

(define (sqlite3-column-database-name16/string statement column-index)
  (let ((rv (sqlite3-column-database-name16 statement column-index)))
    (and rv (utf16n->string rv))))

;;; --------------------------------------------------------------------

(define (sqlite3-column-table-name statement column-index)
  (define who 'sqlite3-column-table-name)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-table-name statement column-index)))

(define (sqlite3-column-table-name/string statement column-index)
  (let ((rv (sqlite3-column-table-name statement column-index)))
    (and rv (utf8->string rv))))

(define (sqlite3-column-table-name16 statement column-index)
  (define who 'sqlite3-column-table-name16)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-table-name16 statement column-index)))

(define (sqlite3-column-table-name16/string statement column-index)
  (let ((rv (sqlite3-column-table-name16 statement column-index)))
    (and rv (utf16n->string rv))))

;;; --------------------------------------------------------------------

(define (sqlite3-column-origin-name statement column-index)
  (define who 'sqlite3-column-origin-name)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-origin-name statement column-index)))

(define (sqlite3-column-origin-name/string statement column-index)
  (let ((rv (sqlite3-column-origin-name statement column-index)))
    (and rv (utf8->string rv))))

(define (sqlite3-column-origin-name16 statement column-index)
  (define who 'sqlite3-column-origin-name16)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-origin-name16 statement column-index)))

(define (sqlite3-column-origin-name16/string statement column-index)
  (let ((rv (sqlite3-column-origin-name16 statement column-index)))
    (and rv (utf16n->string rv))))

;;; --------------------------------------------------------------------

(define (sqlite3-column-decltype statement column-index)
  (define who 'sqlite3-column-decltype)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-decltype statement column-index)))

(define (sqlite3-column-decltype/string statement column-index)
  (let ((rv (sqlite3-column-decltype statement column-index)))
    (and rv (utf8->string rv))))

(define (sqlite3-column-decltype16 statement column-index)
  (define who 'sqlite3-column-decltype16)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-decltype16 statement column-index)))

(define (sqlite3-column-decltype16/string statement column-index)
  (let ((rv (sqlite3-column-decltype16 statement column-index)))
    (and rv (utf16n->string rv))))

;;; --------------------------------------------------------------------

(define (sqlite3-data-count statement)
  (define who 'sqlite3-data-count)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement))
    (capi.sqlite3-data-count statement)))

(define (sqlite3-column-type statement column-index)
  (define who 'sqlite3-column-type)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-type statement column-index)))

;;; --------------------------------------------------------------------

(define (sqlite3-column-blob statement column-index)
  (define who 'sqlite3-column-blob)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-blob statement column-index)))

(define (sqlite3-column-bytes statement column-index)
  (define who 'sqlite3-column-bytes)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-bytes statement column-index)))

(define (sqlite3-column-bytes16 statement column-index)
  (define who 'sqlite3-column-bytes16)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-bytes16 statement column-index)))

(define (sqlite3-column-double statement column-index)
  (define who 'sqlite3-column-double)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-double statement column-index)))

(define (sqlite3-column-int statement column-index)
  (define who 'sqlite3-column-int)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-int statement column-index)))

(define (sqlite3-column-int64 statement column-index)
  (define who 'sqlite3-column-int64)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-int64 statement column-index)))

(define (sqlite3-column-text statement column-index)
  (define who 'sqlite3-column-text)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-text statement column-index)))

(define (sqlite3-column-text/string statement column-index)
  (let ((rv (sqlite3-column-text statement column-index)))
    (or (not rv) (utf8->string rv))))

(define (sqlite3-column-text16 statement column-index)
  (define who 'sqlite3-column-text16)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (capi.sqlite3-column-text16 statement column-index)))

(define (sqlite3-column-text16/string statement column-index)
  (let ((rv (sqlite3-column-text16 statement column-index)))
    (or (not rv) (utf16n->string rv))))

(define (sqlite3-column-value statement column-index)
  (define who 'sqlite3-column-value)
  (with-arguments-validation (who)
      ((sqlite3-stmt/valid	statement)
       (signed-int		column-index))
    (make-sqlite3-value (capi.sqlite3-column-value statement column-index))))


;;;; SQLite extensions

(define sqlite3-load-extension
  (case-lambda
   ((sqlite3-load-extension connection pathname)
    (sqlite3-load-extension connection pathname #f))
   ((connection pathname procname)
    (define who 'sqlite3-load-extension)
    (with-arguments-validation (who)
	((sqlite3/open		connection)
	 (general-string	pathname)
	 (general-string/false	procname))
      (with-general-strings ((pathname^ pathname))
	  string->utf8
	(with-general-strings/false ((procname^ procname))
	    string->utf8
	  (let ((rv (capi.sqlite3-load-extension connection pathname^ procname^)))
	    (if (pair? rv)
		(values (unsafe.car rv) (utf8->string (unsafe.cdr rv)))
	      (values rv #f)))))))))

(define (sqlite3-enable-load-extension onoff)
  (capi.sqlite3-enable-load-extension onoff))

(define (sqlite3-auto-extension entry-point)
  (define who 'sqlite3-auto-extension)
  (with-arguments-validation (who)
      ((callback	entry-point))
    (capi.sqlite3-auto-extension entry-point)))

(define (sqlite3-reset-auto-extension)
  (capi.sqlite3-reset-auto-extension))


;;;; BLOBs for incremental input/output

(define (sqlite3-blob-open connection database-name table-name column-name
			   rowid write-enabled?)
  (define who 'sqlite3-blob-open)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (general-string	database-name)
       (general-string	table-name)
       (general-string	column-name)
       (signed-int64	rowid))
    (with-general-strings ((database-name^	database-name)
			   (table-name^		table-name)
			   (column-name^	column-name))
	string->utf8
      (let* ((blob (%make-sqlite3-blob (null-pointer)
				       connection
				       (%any->string who database-name)
				       (%any->string who table-name)
				       (%any->string who column-name)
				       rowid (if write-enabled? #t #f)))
	     (rv   (capi.sqlite3-blob-open connection
					   database-name^ table-name^ column-name^
					   rowid write-enabled? blob)))
	(if (= SQLITE_OK rv)
	    (begin
	      (%sqlite3-blob-register! connection blob)
	      (values rv blob))
	  (values rv #f))))))

(define (sqlite3-blob-reopen blob rowid)
  (define who 'sqlite3-blob-reopen)
  (with-arguments-validation (who)
      ((sqlite3-blob/open	blob)
       (signed-int64		rowid))
    (capi.sqlite3-blob-reopen blob rowid)))

(define (sqlite3-blob-close blob)
  (define who 'sqlite3-blob-close)
  (with-arguments-validation (who)
      ((sqlite3-blob	blob))
    (%unsafe.sqlite3-blob-close blob)))

(define (sqlite3-blob-bytes blob)
  (define who 'sqlite3-blob-bytes)
  (with-arguments-validation (who)
      ((sqlite3-blob/open	blob))
    (capi.sqlite3-blob-bytes blob)))

(define (sqlite3-blob-read src.blob   src.offset
			   dst.buffer dst.offset
			   number-of-bytes)
  (define who 'sqlite3-blob-read)
  (with-arguments-validation (who)
      ((sqlite3-blob/open			src.blob)
       (non-negative-signed-int			src.offset)
       (general-buffer				dst.buffer)
       (index-of-general-buffer			dst.offset dst.buffer)
       (index-and-count-of-general-buffer	dst.offset number-of-bytes dst.buffer))
    (capi.sqlite3-blob-read src.blob   src.offset
			    dst.buffer dst.offset
			    number-of-bytes)))

(define (sqlite3-blob-write dst.blob   dst.offset
			    src.buffer src.offset
			    number-of-bytes)
  (define who 'sqlite3-blob-write)
  (with-arguments-validation (who)
      ((sqlite3-blob/open			dst.blob)
       (non-negative-signed-int			dst.offset)
       (general-buffer				src.buffer)
       (index-of-general-buffer			src.offset src.buffer)
       (index-and-count-of-general-buffer	src.offset number-of-bytes src.buffer))
    (capi.sqlite3-blob-write dst.blob   dst.offset
			     src.buffer src.offset
			     number-of-bytes)))


;;;; custom SQL functions: creation

(define (sqlite3-create-function connection function-name arity text-encoding
				 custom-data func step final)
  (define who 'sqlite3-create-function)
  (with-arguments-validation (who)
      ((sqlite3/open		connection)
       (general-string	function-name)
       (function-arity		arity)
       (fixnum			text-encoding)
       (pointer/false		custom-data)
       (callback/false		func)
       (callback/false		step)
       (callback/false		final))
    (with-general-strings ((function-name^ function-name))
	string->utf8
      (capi.sqlite3-create-function connection function-name^ arity text-encoding
				    custom-data func step final))))

(define (sqlite3-create-function16 connection function-name arity text-encoding
				   custom-data func step final)
  (define who 'sqlite3-create-function16)
  (with-arguments-validation (who)
      ((sqlite3/open		connection)
       (general-string		function-name)
       (function-arity		arity)
       (fixnum			text-encoding)
       (pointer/false		custom-data)
       (callback/false		func)
       (callback/false		step)
       (callback/false		final))
    (with-general-strings ((function-name^ function-name))
	%string->terminated-utf16n
      (capi.sqlite3-create-function16 connection function-name^ arity text-encoding
				      custom-data func step final))))

(define (sqlite3-create-function-v2 connection function-name arity text-encoding
				    custom-data func step final destroy)
  (define who 'sqlite3-create-function-v2)
  (with-arguments-validation (who)
      ((sqlite3/open		connection)
       (general-string	function-name)
       (function-arity		arity)
       (fixnum			text-encoding)
       (pointer/false		custom-data)
       (callback/false		func)
       (callback/false		step)
       (callback/false		final)
       (callback/false		destroy))
    (with-general-strings ((function-name^ function-name))
	string->utf8
      (capi.sqlite3-create-function-v2 connection function-name^ arity text-encoding
				       custom-data func step final destroy))))

;;; --------------------------------------------------------------------

(define (%sql-function-error-from-condition-object context condition)
  (sqlite3-result-error-code context SQLITE_ERROR)
  (sqlite3-result-error context
			(if (message-condition? condition)
			    (condition-message condition)
			  "unspecified error while executing custom SQL function")))

(define make-sqlite3-function
  ;; void (*xFunc)(sqlite3_context * context, int arity, sqlite3_value** arguments)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker
       (lambda (context-pointer arity args)
	 (let ((context (make-sqlite3-context context-pointer)))
	   (guard (E (else
		      ;;(pretty-print E (current-error-port))
		      (%sql-function-error-from-condition-object context E)
		      (void)))
	     (user-scheme-callback context (vector-map make-sqlite3-value
					     (capi.sqlite-c-array-to-pointers arity args)))
	     (void))))))))

(define make-sqlite3-aggregate-step
  ;; void (*xStep) (sqlite3_context* context, int arity, sqlite3_value** arguments)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker
       (lambda (context-pointer arity args)
	 (let ((context (make-sqlite3-context context-pointer)))
	   (guard (E (else
		      #;(pretty-print E (current-error-port))
		      (%sql-function-error-from-condition-object context E)
		      (void)))
	     (user-scheme-callback context (vector-map make-sqlite3-value
					     (capi.sqlite-c-array-to-pointers arity args)))
	     (void))))))))

(define make-sqlite3-aggregate-final
  ;; void (*xFinal) (sqlite3_context* context)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer))))
    (lambda (user-scheme-callback)
      (maker
       (lambda (context-pointer)
	 (let ((context (make-sqlite3-context context-pointer)))
	   (guard (E (else
		      #;(pretty-print E (current-error-port))
		      (%sql-function-error-from-condition-object context E)
		      (void)))
	     (user-scheme-callback context)
	     (void))))))))

(define make-sqlite3-function-destructor
  ;; void (*xDestroy)(void *)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer))))
    (lambda (user-scheme-callback)
      (maker
       (lambda (custom-data)
	 (guard (E (else (void)))
	   (user-scheme-callback (if (pointer-null? custom-data)
				     #f
				   custom-data))
	   (void)))))))

;;; --------------------------------------------------------------------

(define (sqlite3-delete-function connection function-name)
  (sqlite3-create-function connection function-name 0 SQLITE_ANY #f #f #f #f))

(define (sqlite3-delete-function16 connection function-name)
  (sqlite3-create-function16 connection function-name 0 SQLITE_ANY #f #f #f #f))


;;;; custom SQL functions: SQL arguments to Scheme arguments

(define (sqlite3-value-blob value)
  (define who 'sqlite3-value-blob)
  (with-arguments-validation (who)
      ((sqlite3-value	value))
    (capi.sqlite3-value-blob value)))

(define (sqlite3-value-bytes value)
  (define who 'sqlite3-value-bytes)
  (with-arguments-validation (who)
      ((sqlite3-value	value))
    (capi.sqlite3-value-bytes value)))

(define (sqlite3-value-bytes16 value)
  (define who 'sqlite3-value-bytes16)
  (with-arguments-validation (who)
      ((sqlite3-value	value))
    (capi.sqlite3-value-bytes16 value)))

(define (sqlite3-value-double value)
  (define who 'sqlite3-value-double)
  (with-arguments-validation (who)
      ((sqlite3-value	value))
    (capi.sqlite3-value-double value)))

(define (sqlite3-value-int value)
  (define who 'sqlite3-value-int)
  (with-arguments-validation (who)
      ((sqlite3-value	value))
    (capi.sqlite3-value-int value)))

(define (sqlite3-value-int64 value)
  (define who 'sqlite3-value-int64)
  (with-arguments-validation (who)
      ((sqlite3-value	value))
    (capi.sqlite3-value-int64 value)))

;;; --------------------------------------------------------------------

(define (sqlite3-value-text value)
  (define who 'sqlite3-value-text)
  (with-arguments-validation (who)
      ((sqlite3-value	value))
    (capi.sqlite3-value-text value)))

(define (sqlite3-value-text/string value)
  (let ((rv (sqlite3-value-text value)))
    (if (bytevector? rv)
	(utf8->string rv)
      rv)))

;;; --------------------------------------------------------------------

(define (sqlite3-value-text16 value)
  (define who 'sqlite3-value-text16)
  (with-arguments-validation (who)
      ((sqlite3-value	value))
    (capi.sqlite3-value-text16 value)))

(define (sqlite3-value-text16/string value)
  (let ((rv (sqlite3-value-text16 value)))
    (if (bytevector? rv)
	(utf16n->string rv)
      rv)))

;;; --------------------------------------------------------------------

(define (sqlite3-value-text16le value)
  (define who 'sqlite3-value-text16le)
  (with-arguments-validation (who)
      ((sqlite3-value	value))
    (capi.sqlite3-value-text16le value)))

(define (sqlite3-value-text16le/string value)
  (let ((rv (sqlite3-value-text16le value)))
    (if (bytevector? rv)
	(utf16le->string rv)
      rv)))

;;; --------------------------------------------------------------------

(define (sqlite3-value-text16be value)
  (define who 'sqlite3-value-text16be)
  (with-arguments-validation (who)
      ((sqlite3-value	value))
    (capi.sqlite3-value-text16be value)))

(define (sqlite3-value-text16be/string value)
  (let ((rv (sqlite3-value-text16be value)))
    (if (bytevector? rv)
	(utf16be->string rv)
      rv)))

;;; --------------------------------------------------------------------

(define (sqlite3-value-type value)
  (define who 'sqlite3-value-type)
  (with-arguments-validation (who)
      ((sqlite3-value	value))
    (capi.sqlite3-value-type value)))

(define (sqlite3-value-numeric-type value)
  (define who 'sqlite3-value-numeric-type)
  (with-arguments-validation (who)
      ((sqlite3-value	value))
    (capi.sqlite3-value-numeric-type value)))


;;;; custom SQL functions: auxiliary functions

(define sqlite3-aggregate-context
  (case-lambda
   ((context)
    (sqlite3-aggregate-context context 0))
   ((context number-of-bytes)
    (define who 'sqlite3-aggregate-context)
    (with-arguments-validation (who)
	((sqlite3-context	context)
	 (signed-int		number-of-bytes))
      (capi.sqlite3-aggregate-context context number-of-bytes)))))

(define (sqlite3-user-data context)
  (define who 'sqlite3-user-data)
  (with-arguments-validation (who)
      ((sqlite3-context	context))
    (capi.sqlite3-user-data context)))

(define (sqlite3-context-db-handle context)
  (define who 'sqlite3-context-db-handle)
  (with-arguments-validation (who)
      ((sqlite3-context	context))
    (let ((P (capi.sqlite3-context-db-handle context)))
      (if P
	  (let ((F (capi.sqlite3-db-filename-from-pointer P #ve(utf8 "main"))))
	    (%make-sqlite3/disown P (and F (utf8->string F))))
	#f))))

(define (sqlite3-get-auxdata context argnum)
  (define who 'sqlite3-get-auxdata)
  (with-arguments-validation (who)
      ((sqlite3-context		context)
       (non-negative-signed-int	argnum))
    (capi.sqlite3-get-auxdata context argnum)))

(define (sqlite3-set-auxdata context argnum auxdata destructor)
  (define who 'sqlite3-set-auxdata)
  (with-arguments-validation (who)
      ((sqlite3-context		context)
       (non-negative-signed-int	argnum)
       (pointer/false		auxdata)
       (callback/false		destructor))
    (capi.sqlite3-set-auxdata context argnum auxdata destructor)))

(define make-sqlite3-auxdata-destructor
  ;; void (*) (void* aux_data)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (pointer)
	       (guard (E (else
			  ;;(pretty-print E (current-error-port))
			  (void)))
		 (user-scheme-callback pointer)
		 (void)))))))


;;;; custom SQL functions: Scheme return values to SQL return values

(define (sqlite3-result-blob context blob.data blob.start blob.len destructor)
  (define who 'sqlite3-result-blob)
  (with-arguments-validation (who)
      ((sqlite3-context		context)
       (general-string		blob.data)
       (non-negative-signed-int	blob.start)
       (signed-int/false	blob.len))
    (with-general-strings ((blob.data^ blob.data))
	string->utf8
      (capi.sqlite3-result-blob context blob.data^ blob.start blob.len destructor))))

(define (sqlite3-result-zeroblob context blob.len)
  (define who 'sqlite3-result-zeroblob)
  (with-arguments-validation (who)
      ((sqlite3-context		context)
       (non-negative-signed-int	blob.len))
    (capi.sqlite3-result-zeroblob context blob.len)))

;;; --------------------------------------------------------------------

(define (sqlite3-result-double context retval)
  (define who 'sqlite3-result-double)
  (with-arguments-validation (who)
      ((sqlite3-context	context)
       (flonum		retval))
    (capi.sqlite3-result-double context retval)))

(define (sqlite3-result-int context retval)
  (define who 'sqlite3-result-int)
  (with-arguments-validation (who)
      ((sqlite3-context	context)
       (signed-int	retval))
    (capi.sqlite3-result-int context retval)))

(define (sqlite3-result-int64 context retval)
  (define who 'sqlite3-result-int64)
  (with-arguments-validation (who)
      ((sqlite3-context	context)
       (signed-int64	retval))
    (capi.sqlite3-result-int64 context retval)))

(define (sqlite3-result-null context)
  (define who 'sqlite3-result-null)
  (with-arguments-validation (who)
      ((sqlite3-context	context))
    (capi.sqlite3-result-null context)))

(define (sqlite3-result-value context retval)
  (define who 'sqlite3-result-value)
  (with-arguments-validation (who)
      ((sqlite3-context	context)
       (sqlite3-value	retval))
    (capi.sqlite3-result-value context retval)))

;;; --------------------------------------------------------------------

(define (sqlite3-result-text context text.data text.start text.len destructor)
  (define who 'sqlite3-result-text)
  (with-arguments-validation (who)
      ((sqlite3-context		context)
       (general-string		text.data)
       (non-negative-signed-int	text.start)
       (signed-int/false	text.len))
    (with-general-strings ((text.data^	text.data))
	string->utf8
      (capi.sqlite3-result-text context text.data^ text.start text.len destructor))))

(define (sqlite3-result-text16 context text.data text.start text.len destructor)
  (define who 'sqlite3-result-text16)
  (with-arguments-validation (who)
      ((sqlite3-context		context)
       (general-string		text.data)
       (non-negative-signed-int	text.start)
       (signed-int/false	text.len))
    (with-general-strings ((text.data^	text.data))
	string->utf16n
      (capi.sqlite3-result-text16 context text.data^ text.start text.len destructor))))

(define (sqlite3-result-text16le context text.data text.start text.len destructor)
  (define who 'sqlite3-result-text16le)
  (with-arguments-validation (who)
      ((sqlite3-context		context)
       (general-string		text.data)
       (non-negative-signed-int	text.start)
       (signed-int/false	text.len))
    (with-general-strings ((text.data^	text.data))
	string->utf16le
      (capi.sqlite3-result-text16le context text.data^ text.start text.len destructor))))

(define (sqlite3-result-text16be context text.data text.start text.len destructor)
  (define who 'sqlite3-result-text16be)
  (with-arguments-validation (who)
      ((sqlite3-context		context)
       (general-string		text.data)
       (non-negative-signed-int	text.start)
       (signed-int/false	text.len))
    (with-general-strings ((text.data^	text.data))
	string->utf16be
      (capi.sqlite3-result-text16be context text.data^ text.start text.len destructor))))

;;; --------------------------------------------------------------------

(define (sqlite3-result-error context error-message)
  (define who 'sqlite3-result-error)
  (with-arguments-validation (who)
      ((sqlite3-context		context)
       (general-string		error-message))
    (with-general-strings ((error-message^ error-message))
	string->utf8
      (capi.sqlite3-result-error context error-message^))))

(define (sqlite3-result-error16 context error-message)
  (define who 'sqlite3-result-error16)
  (with-arguments-validation (who)
      ((sqlite3-context		context)
       (general-string		error-message))
    (with-general-strings ((error-message^ error-message))
	%string->terminated-utf16n
      (capi.sqlite3-result-error16 context error-message^))))

(define (sqlite3-result-error-toobig context)
  (define who 'sqlite3-result-error-toobig)
  (with-arguments-validation (who)
      ((sqlite3-context	context))
    (capi.sqlite3-result-error-toobig context)))

(define (sqlite3-result-error-nomem context)
  (define who 'sqlite3-result-error-nomem)
  (with-arguments-validation (who)
      ((sqlite3-context	context))
    (capi.sqlite3-result-error-nomem context)))

(define (sqlite3-result-error-code context errcode)
  (define who 'sqlite3-result-error-code)
  (with-arguments-validation (who)
      ((sqlite3-context	context)
       (signed-int	errcode))
    (capi.sqlite3-result-error-code context errcode)))


;;;; backup functions

(define (sqlite3-backup-init dst-connection dst-name src-connection src-name)
  (define who 'sqlite3-backup-init)
  (with-arguments-validation (who)
      ((sqlite3/open	dst-connection)
       (general-string	dst-name)
       (sqlite3/open	src-connection)
       (general-string	src-name))
    (with-general-strings ((dst-name^	dst-name)
			   (src-name^	src-name))
	string->utf8
      (%make-sqlite3-backup (capi.sqlite3-backup-init dst-connection dst-name^
						      src-connection src-name^)
			    dst-connection (%any->string who dst-name)
			    src-connection (%any->string who src-name)))))

(define (sqlite3-backup-step backup number-of-pages)
  (define who 'sqlite3-backup-step)
  (with-arguments-validation (who)
      ((sqlite3-backup/running	backup)
       (signed-int		number-of-pages))
    (capi.sqlite3-backup-step backup number-of-pages)))

(define (sqlite3-backup-finish backup)
  (define who 'sqlite3-backup-finish)
  (with-arguments-validation (who)
      ((sqlite3-backup	backup))
    (%unsafe.sqlite3-backup-finish backup)))

(define (sqlite3-backup-remaining backup)
  (define who 'sqlite3-backup-remaining)
  (with-arguments-validation (who)
      ((sqlite3-backup/running	backup))
    (capi.sqlite3-backup-remaining backup)))

(define (sqlite3-backup-pagecount backup)
  (define who 'sqlite3-backup-pagecount)
  (with-arguments-validation (who)
      ((sqlite3-backup/running	backup))
    (capi.sqlite3-backup-pagecount backup)))


;;;; collation functions

(define (sqlite3-create-collation connection collation-name encoding custom-data callback)
  (define who 'sqlite3-create-collation)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (general-string	collation-name)
       (signed-int	encoding)
       (pointer/false	custom-data)
       (callback/false	callback))
    (with-general-strings ((collation-name^ collation-name))
	string->utf8
      (capi.sqlite3-create-collation connection collation-name^ encoding
				     custom-data callback))))

(define (sqlite3-create-collation-v2 connection collation-name encoding
				     custom-data callback destroy)
  (define who 'sqlite3-create-collation-v2)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (general-string	collation-name)
       (signed-int	encoding)
       (pointer/false	custom-data)
       (callback/false	callback)
       (callback/false	destroy))
    (with-general-strings ((collation-name^ collation-name))
	string->utf8
      (capi.sqlite3-create-collation-v2 connection collation-name^ encoding
					custom-data callback destroy))))

(define (sqlite3-create-collation16 connection collation-name encoding custom-data callback)
  (define who 'sqlite3-create-collation16)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (general-string	collation-name)
       (signed-int	encoding)
       (pointer/false	custom-data)
       (callback/false	callback))
    (with-general-strings ((collation-name^ collation-name))
	%string->terminated-utf16n
      (capi.sqlite3-create-collation16 connection collation-name^ encoding
				       custom-data callback))))

(define (sqlite3-collation-needed connection custom-data callback)
  (define who 'sqlite3-collation-needed)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (pointer/false	custom-data)
       (callback/false	callback))
    (capi.sqlite3-collation-needed connection custom-data callback)))

(define (sqlite3-collation-needed16 connection custom-data callback)
  (define who 'sqlite3-collation-needed16)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (pointer/false	custom-data)
       (callback/false	callback))
    (capi.sqlite3-collation-needed16 connection custom-data callback)))

;;; --------------------------------------------------------------------

(define make-sqlite3-collation-callback
  ;; int (*xCompare) (void* data, int, const void*, int, const void*)
  (let ((maker (ffi.make-c-callback-maker 'signed-int
					  '(pointer signed-int pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data len1 ptr1 len2 ptr2)
	       (guard (E (else
			  (pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback (if (pointer-null? custom-data)
					   #f
					 custom-data)
				       len1 ptr1 len2 ptr2)))))))

(define make-sqlite3-collation-destructor
  ;; void (*) (void*)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (pointer)
	       (guard (E (else
			  ;;(pretty-print E (current-error-port))
			  (void)))
		 (user-scheme-callback pointer)
		 (void)))))))

(define make-sqlite3-collation-needed-callback
  ;; void (*) (void*, sqlite3*, int eTextRep, const char*)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker
       (lambda (custom-data connection-pointer encoding collation-name-pointer)
	 (guard (E (else
		    ;;(pretty-print E (current-error-port))
		    (void)))
	   (let* ((pathname (capi.sqlite3-db-filename-from-pointer connection-pointer
								   #ve(utf8 "main")))
		  (conn     (%make-sqlite3/disown connection-pointer
						  (and pathname (utf8->string pathname)))))
	     (user-scheme-callback (if (pointer-null? custom-data)
				       #f
				     custom-data)
				   conn encoding
				   (cstring->string collation-name-pointer))
	     (void))))))))

(define make-sqlite3-collation-needed16-callback
  ;; void (*) (void*, sqlite3*, int eTextRep, const void*)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker
       (lambda (custom-data connection-pointer encoding collation-name-pointer)
	 (guard (E (else
		    ;;(pretty-print E (current-error-port))
		    (void)))
	   (let* ((pathname (capi.sqlite3-db-filename-from-pointer connection-pointer
								   #ve(utf8 "main")))
		  (conn     (%make-sqlite3/disown connection-pointer
						  (and pathname (utf8->string pathname)))))
	     (user-scheme-callback (if (pointer-null? custom-data)
				       #f
				     custom-data)
				   conn encoding
				   (cstring16n->string collation-name-pointer))
	     (void))))))))


;;;; miscellaneous functions

(define (sqlite3-sleep milliseconds)
  (define who 'sqlite3-sleep)
  (with-arguments-validation (who)
      ((signed-int	milliseconds))
    (capi.sqlite3-sleep milliseconds)))

;;; --------------------------------------------------------------------

(define (sqlite3-log error-code message)
  (define who 'sqlite3-log)
  (with-arguments-validation (who)
      ((signed-int	error-code)
       (general-string	message))
    (with-general-strings ((message^ message))
	string->utf8
      (capi.sqlite3-log error-code message^))))

(define make-sqlite3-log-callback
  ;; void(*)(void*,int,const char*)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (dummy error-code message)
	       (guard (E (else (void)))
		 (user-scheme-callback error-code message)
		 (void)))))))

;;; --------------------------------------------------------------------

(define (sqlite3-randomness number-of-bytes)
  (define who 'sqlite3-randomness)
  (with-arguments-validation (who)
      ((non-negative-signed-int	number-of-bytes))
    (capi.sqlite3-randomness (unsafe.make-bytevector number-of-bytes))))

(define (sqlite3-randomness! bytevector)
  (define who 'sqlite3-randomness!)
  (with-arguments-validation (who)
      ((bytevector bytevector))
    (capi.sqlite3-randomness bytevector)))

;;; --------------------------------------------------------------------

(define (sqlite3-uri-parameter filename param-name)
  (define who 'sqlite3-uri-parameter)
  (with-arguments-validation (who)
      ((general-string	filename)
       (general-string	param-name))
    (with-general-strings ((filename^	filename)
			   (param-name^	param-name))
	string->utf8
      (capi.sqlite3-uri-parameter filename^ param-name^))))

(define (sqlite3-uri-parameter/string filename param-name)
  (let ((rv (sqlite3-uri-parameter filename param-name)))
    (and rv (utf8->string rv))))

(define (sqlite3-uri-boolean filename param-name default)
  (define who 'sqlite3-uri-boolean)
  (with-arguments-validation (who)
      ((general-string	filename)
       (general-string	param-name))
    (with-general-strings ((filename^	filename)
			   (param-name^	param-name))
	string->utf8
      (capi.sqlite3-uri-boolean filename^ param-name^ default))))

(define (sqlite3-uri-int64 filename param-name default)
  (define who 'sqlite3-uri-int64)
  (with-arguments-validation (who)
      ((general-string	filename)
       (general-string	param-name)
       (signed-int64	default))
    (with-general-strings ((filename^	filename)
			   (param-name^	param-name))
	string->utf8
      (capi.sqlite3-uri-int64 filename^ param-name^ default))))


;;;; interfaced but untested

(define sqlite3-key
  (case-lambda
   ((conn key.data key.len)
    (sqlite3-key conn key.data #f))
   ((conn key.data key.len)
    (define who 'sqlite3-key)
    (with-arguments-validation (who)
	((sqlite3/open		conn)
	 (general-string/false	key.data)
	 (signed-int/false	key.len))
      (with-general-strings/false ((key.data^ key.data))
	  string->utf8
	(capi.sqlite3-key conn key.data^ key.len))))))

(define sqlite3-rekey
  (case-lambda
   ((conn key.data)
    (sqlite3-rekey conn key.data #f))
   ((conn key.data key.len)
    (define who 'sqlite3-rekey)
    (with-arguments-validation (who)
	((sqlite3/open		conn)
	 (general-string/false	key.data)
	 (signed-int/false	key.len))
      (with-general-strings/false ((key.data^ key.data))
	  string->utf8
	(capi.sqlite3-rekey conn key.data^ key.len))))))

(define (sqlite3-activate-see pass-phrase)
  (define who 'sqlite3-activate-see)
  (with-arguments-validation (who)
      ((general-string	pass-phrase))
    (with-general-strings ((pass-phrase^ pass-phrase))
	string->utf8
      (capi.sqlite3-activate-see pass-phrase^))))

(define (sqlite3-activate-cerod pass-phrase)
  (define who 'sqlite3-activate-cerod)
  (with-arguments-validation (who)
      ((general-string	pass-phrase))
    (with-general-strings ((pass-phrase^ pass-phrase))
	string->utf8
      (capi.sqlite3-activate-cerod pass-phrase^))))

;;; --------------------------------------------------------------------

(define sqlite3-wal-hook
  (case-lambda
   ((connection callback)
    (sqlite3-wal-hook connection callback #f))
   ((connection callback custom-data)
    (define who 'sqlite3-wal-hook)
    (with-arguments-validation (who)
	((sqlite3/open	connection)
	 (callback	callback)
	 (pointer/false	custom-data))
      (capi.sqlite3-wal-hook connection callback custom-data)))))

(define (sqlite3-wal-autocheckpoint connection number-of-frames)
  (define who 'sqlite3-wal-autocheckpoint)
  (with-arguments-validation (who)
      ((sqlite3/open	connection)
       (signed-int	number-of-frames))
    (capi.sqlite3-wal-autocheckpoint connection number-of-frames)))

(define sqlite3-wal-checkpoint
  (case-lambda
   ((connection)
    (sqlite3-wal-checkpoint connection #f))
   ((connection database-name)
    (define who 'sqlite3-wal-checkpoint)
    (with-arguments-validation (who)
	((sqlite3/open		connection)
	 (general-string/false	database-name))
      (with-general-strings/false ((database-name^ database-name))
	  string->utf8
	(capi.sqlite3-wal-checkpoint connection database-name^))))))

(define (sqlite3-wal-checkpoint-v2 connection database-name checkpoint-mode)
  (define who 'sqlite3-wal-checkpoint-v2)
  (with-arguments-validation (who)
      ((sqlite3/open		connection)
       (general-string/false	database-name)
       (signed-int		checkpoint-mode))
    (with-general-strings/false ((database-name^ database-name))
	string->utf8
      (let ((rv (capi.sqlite3-wal-checkpoint-v2 connection database-name^ checkpoint-mode)))
	(if (vector? rv)
	    (values (unsafe.vector-ref rv 0)
		    (unsafe.vector-ref rv 1)
		    (unsafe.vector-ref rv 2))
	  (values rv #f #f))))))

(define make-sqlite3-wal-hook
  ;; int callback (void *, sqlite3*, const char*, int)
  (let ((maker (ffi.make-c-callback-maker 'signed-int
					  '(pointer pointer pointer signed-int))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data handle database-name number-of-pages)
	       (guard (E (else SQLITE_ERROR))
		 (let ((database-name (cstring->string database-name)))
		   (user-scheme-callback (if (pointer-null? custom-data)
					     #f
					   custom-data)
					 (%make-sqlite3/disown handle database-name)
					 database-name
					 number-of-pages))))))))


;;;; error codes conversion

(define-exact-integer->symbol-function sqlite3-error-code->symbol
  (SQLITE_OK
   SQLITE_ERROR
   SQLITE_INTERNAL
   SQLITE_PERM
   SQLITE_ABORT
   SQLITE_BUSY
   SQLITE_LOCKED
   SQLITE_NOMEM
   SQLITE_READONLY
   SQLITE_INTERRUPT
   SQLITE_IOERR
   SQLITE_CORRUPT
   SQLITE_NOTFOUND
   SQLITE_FULL
   SQLITE_CANTOPEN
   SQLITE_PROTOCOL
   SQLITE_EMPTY
   SQLITE_SCHEMA
   SQLITE_TOOBIG
   SQLITE_CONSTRAINT
   SQLITE_MISMATCH
   SQLITE_MISUSE
   SQLITE_NOLFS
   SQLITE_AUTH
   SQLITE_FORMAT
   SQLITE_RANGE
   SQLITE_NOTADB
   SQLITE_ROW
   SQLITE_DONE))

(define-exact-integer->symbol-function sqlite3-extended-error-code->symbol
  (SQLITE_OK
   SQLITE_ERROR
   SQLITE_INTERNAL
   SQLITE_PERM
   SQLITE_ABORT
   SQLITE_BUSY
   SQLITE_LOCKED
   SQLITE_NOMEM
   SQLITE_READONLY
   SQLITE_INTERRUPT
   SQLITE_IOERR
   SQLITE_CORRUPT
   SQLITE_NOTFOUND
   SQLITE_FULL
   SQLITE_CANTOPEN
   SQLITE_PROTOCOL
   SQLITE_EMPTY
   SQLITE_SCHEMA
   SQLITE_TOOBIG
   SQLITE_CONSTRAINT
   SQLITE_MISMATCH
   SQLITE_MISUSE
   SQLITE_NOLFS
   SQLITE_AUTH
   SQLITE_FORMAT
   SQLITE_RANGE
   SQLITE_NOTADB
   SQLITE_ROW
   SQLITE_DONE
   SQLITE_IOERR_READ
   SQLITE_IOERR_SHORT_READ
   SQLITE_IOERR_WRITE
   SQLITE_IOERR_FSYNC
   SQLITE_IOERR_DIR_FSYNC
   SQLITE_IOERR_TRUNCATE
   SQLITE_IOERR_FSTAT
   SQLITE_IOERR_UNLOCK
   SQLITE_IOERR_RDLOCK
   SQLITE_IOERR_DELETE
   SQLITE_IOERR_BLOCKED
   SQLITE_IOERR_NOMEM
   SQLITE_IOERR_ACCESS
   SQLITE_IOERR_CHECKRESERVEDLOCK
   SQLITE_IOERR_LOCK
   SQLITE_IOERR_CLOSE
   SQLITE_IOERR_DIR_CLOSE
   SQLITE_IOERR_SHMOPEN
   SQLITE_IOERR_SHMSIZE
   SQLITE_IOERR_SHMLOCK
   SQLITE_IOERR_SHMMAP
   SQLITE_IOERR_SEEK
   SQLITE_LOCKED_SHAREDCACHE
   SQLITE_BUSY_RECOVERY
   SQLITE_CANTOPEN_NOTEMPDIR
   SQLITE_CANTOPEN_ISDIR
   SQLITE_CORRUPT_VTAB
   SQLITE_READONLY_RECOVERY
   SQLITE_READONLY_CANTLOCK
   SQLITE_ABORT_ROLLBACK))

(define-exact-integer->symbol-function sqlite3-authorizer-return-code->symbol
  (SQLITE_OK SQLITE_DENY SQLITE_IGNORE))

(define-exact-integer->symbol-function sqlite3-authorizer-action-code->symbol
  (SQLITE_CREATE_INDEX
   SQLITE_CREATE_TABLE
   SQLITE_CREATE_TEMP_INDEX
   SQLITE_CREATE_TEMP_TABLE
   SQLITE_CREATE_TEMP_TRIGGER
   SQLITE_CREATE_TEMP_VIEW
   SQLITE_CREATE_TRIGGER
   SQLITE_CREATE_VIEW
   SQLITE_DELETE
   SQLITE_DROP_INDEX
   SQLITE_DROP_TABLE
   SQLITE_DROP_TEMP_INDEX
   SQLITE_DROP_TEMP_TABLE
   SQLITE_DROP_TEMP_TRIGGER
   SQLITE_DROP_TEMP_VIEW
   SQLITE_DROP_TRIGGER
   SQLITE_DROP_VIEW
   SQLITE_INSERT
   SQLITE_PRAGMA
   SQLITE_READ
   SQLITE_SELECT
   SQLITE_TRANSACTION
   SQLITE_UPDATE
   SQLITE_ATTACH
   SQLITE_DETACH
   SQLITE_ALTER_TABLE
   SQLITE_REINDEX
   SQLITE_ANALYZE
   SQLITE_CREATE_VTABLE
   SQLITE_DROP_VTABLE
   SQLITE_FUNCTION
   SQLITE_SAVEPOINT
   SQLITE_COPY))


;;;; still to be implemented

(define-inline (unimplemented who)
  (assertion-violation who "unimplemented function"))

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

(set-rtd-printer! (type-descriptor sqlite3)		%struct-sqlite3-printer)
(set-rtd-printer! (type-descriptor sqlite3-stmt)	%struct-sqlite3-stmt-printer)
(set-rtd-printer! (type-descriptor sqlite3-blob)	%struct-sqlite3-blob-printer)
(set-rtd-printer! (type-descriptor sqlite3-value)	%struct-sqlite3-value-printer)
(set-rtd-printer! (type-descriptor sqlite3-context)	%struct-sqlite3-context-printer)
(set-rtd-printer! (type-descriptor sqlite3-backup)	%struct-sqlite3-backup-printer)

(set-rtd-destructor! (type-descriptor sqlite3)		%unsafe.sqlite3-close)
(set-rtd-destructor! (type-descriptor sqlite3-stmt)	%unsafe.sqlite3-finalize)
(set-rtd-destructor! (type-descriptor sqlite3-blob)	%unsafe.sqlite3-blob-close)
(set-rtd-destructor! (type-descriptor sqlite3-backup)	%unsafe.sqlite3-backup-finish)

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
;; eval: (put 'with-utf8-bytevectors/false 'scheme-indent-function 1)
;; eval: (put 'with-utf8-bytevectors/pointers 'scheme-indent-function 1)
;; eval: (put 'with-utf16-bytevectors/pointers 'scheme-indent-function 1)
;; eval: (put 'with-utf16le-bytevectors/pointers 'scheme-indent-function 1)
;; eval: (put 'with-utf16be-bytevectors/pointers 'scheme-indent-function 1)
;; eval: (put 'with-general-strings 'scheme-indent-function 2)
;; eval: (put 'with-general-strings/false 'scheme-indent-function 2)
;; eval: (put '%struct-destructor-application 'scheme-indent-function 1)
;; End:
