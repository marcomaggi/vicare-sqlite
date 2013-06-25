;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: tests for SQLite connection functions
;;;Date: Fri Aug 10, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (vicare)
  (vicare databases sqlite3)
  (vicare databases sqlite3 constants)
  (vicare databases sqlite3 features)
  (prefix (vicare ffi) ffi.)
  (vicare language-extensions syntaxes)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare SQLite bindings, database connections\n")

(set-port-buffer-mode! (current-error-port) (buffer-mode line))
(set-port-buffer-mode! (current-output-port) (buffer-mode none))
#;(struct-guardian-logger #t)


;;;; helpers

(define-syntax with-connection
  (syntax-rules ()
    ((_ (?connect-var) . ?body)
     (let ((pathname "sqlite.test.db"))
       (unwind-protect
	   (let ((?connect-var (sqlite3-open pathname)))
	     (unwind-protect
		 (begin . ?body)
	       (when (sqlite3? ?connect-var)
		 (sqlite3-close ?connect-var))))
	 (when (file-exists? pathname)
	   (delete-file pathname)))))))

(define-syntax with-statement
  (syntax-rules ()
    ((_ (?statement-var) . ?body)
     (with-connection (conn)
       (sqlite3-exec conn "create table accounts \
                             (id       INTEGER PRIMARY KEY, \
                              nickname TEXT, \
                              password TEXT);")
       (let ((snippet "insert into accounts (nickname, password) \
                         values (?1, ?2);"))
	 (let-values (((code ?statement-var end-offset)
		       (sqlite3-prepare-v2 conn snippet)))
	   (unwind-protect
	       (let ((rv (begin . ?body)))
;;;		 (check-pretty-print (sqlite3-errmsg conn))
		 rv)
	     (when (sqlite3-stmt?/valid ?statement-var)
	       (sqlite3-finalize ?statement-var)))))))))


(parametrise ((check-test-name	'open-close))

  (check	;sqlite3-open
      (with-connection (conn)
;;;(check-pretty-print conn)
	(sqlite3?/open conn))
    => #t)

  (check 	;sqlite3-open16
      (let ((pathname "sqlite.test.db"))
	(unwind-protect
	    (let ((conn (sqlite3-open16 pathname)))
	      (unwind-protect
		  (sqlite3?/open conn)
		(when (sqlite3? conn)
		  (sqlite3-close conn))))
	  (when (file-exists? pathname)
	    (delete-file pathname))))
    => #t)

  (check	;sqlite3-open-v2
      (let ((pathname "sqlite.test.db"))
	(unwind-protect
	    (let ((conn (sqlite3-open-v2 pathname
					 (fxior SQLITE_OPEN_READWRITE
						SQLITE_OPEN_CREATE))))
	      (unwind-protect
		  (sqlite3?/open conn)
		(when (sqlite3? conn)
		  (sqlite3-close conn))))
	  (when (file-exists? pathname)
	    (delete-file pathname))))
    => #t)

  (check	;sqlite3-open-v2
      (let ((conn (sqlite3-open-v2 ":memory:"
				   (fxior SQLITE_OPEN_READWRITE
					  SQLITE_OPEN_CREATE))))
	(unwind-protect
	    (sqlite3?/open conn)
	  (when (sqlite3? conn)
	    (sqlite3-close conn))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-connection (conn)
	(eqv? conn conn))
    => #t)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
       (with-connection (conn)
	 (set-sqlite3-destructor! conn
				  (lambda (conn)
				    (add-result 123)))
	 #t))
    => '(#t (123)))

  #t)


(parametrise ((check-test-name	'destructor))

  (check	;sqlite3-open
      (with-result
       (with-connection (conn)
	 (set-sqlite3-destructor! conn (lambda (conn)
					 (add-result #t)))
	 #t))
    => '(#t (#t)))

  #t)


(parametrise ((check-test-name	'misc))

  (check	;sqlite3-db-config
      (with-connection (conn)
	(sqlite3-db-config conn SQLITE_DBCONFIG_ENABLE_FKEY 0))
    => #f)

  (check	;sqlite3-extended-result-codes
      (with-connection (conn)
	(sqlite3-extended-result-codes conn #t))
    => SQLITE_OK)

  (check	;sqlite3-limit
      (with-connection (conn)
	(sqlite3-limit conn SQLITE_LIMIT_LENGTH 1000)
	(sqlite3-limit conn SQLITE_LIMIT_LENGTH 1000))
    => 1000)

;;; --------------------------------------------------------------------

  (check	;sqlite3-busy-handler
      (with-connection (conn)
	(sqlite3-busy-handler conn))
    => SQLITE_OK)

  (check	;sqlite3-busy-handler
      (with-connection (conn)
	(sqlite3-busy-handler conn #f))
    => SQLITE_OK)

  (check	;sqlite3-busy-handler
      (with-connection (conn)
	(sqlite3-busy-handler conn (make-sqlite3-busy-handler-callback
				    (lambda (num) #f))))
    => SQLITE_OK)

  (check	;sqlite3-busy-timeout
      (with-connection (conn)
	(sqlite3-busy-timeout conn 0))
    => SQLITE_OK)

;;; --------------------------------------------------------------------

  (check	;sqlite3-get-autocommit
      (with-connection (conn)
	(sqlite3-get-autocommit conn))
    => #t)

  (check	;sqlite3-db-filename
      (with-connection (conn)
	(let* ((rv  (sqlite3-db-filename/string conn "main"))
	       (len (string-length rv)))
	  (substring rv (- len (string-length "sqlite.test.db")) len)))
    => "sqlite.test.db")

  (check	;sqlite3-db-readonly?
      (with-connection (conn)
	(sqlite3-db-readonly? conn "main"))
    => 0)

;;; --------------------------------------------------------------------

  (check	;sqlite3-trace
      (with-result
       (with-connection (conn)
	 (let ((cb (make-sqlite3-trace-callback
		    (lambda (sql.ptr)
		      (add-result (cstring->string sql.ptr))))))
	   (unwind-protect
	       (begin
		 (sqlite3-trace conn cb)
		 (let-values (((code offset)
			       (sqlite3-exec conn "create table accounts \
                      (id INTEGER PRIMARY KEY, nickname TEXT, password TEXT);")))
		   code))
	     (ffi.free-c-callback cb)))))
    => `(,SQLITE_OK ("create table accounts \
                      (id INTEGER PRIMARY KEY, nickname TEXT, password TEXT);")))

;;; --------------------------------------------------------------------

  (check	;sqlite3-profile
      (with-result
       (with-connection (conn)
	 (let ((cb (make-sqlite3-profile-callback
		    (lambda (sql.ptr nanoseconds)
		      (add-result (cstring->string sql.ptr))
		      (add-result (integer? nanoseconds))))))
	   (unwind-protect
	       (begin
		 (sqlite3-profile conn cb)
		 (let-values (((code offset)
			       (sqlite3-exec conn "create table accounts \
                      (id INTEGER PRIMARY KEY, nickname TEXT, password TEXT);")))
		   code))
	     (ffi.free-c-callback cb)))))
    => `(,SQLITE_OK ("create table accounts \
                      (id INTEGER PRIMARY KEY, nickname TEXT, password TEXT);" #t)))

;;; --------------------------------------------------------------------

  (check
      (with-connection (conn)
	(sqlite3-db-release-memory conn))
    => 0)

;;; --------------------------------------------------------------------

  (when HAVE_SQLITE3_TABLE_COLUMN_METADATA
    (check
	(with-connection (conn)
	  (let*-values
	      (((code offset)
		(sqlite3-exec conn "create table accounts \
                          (id INTEGER PRIMARY KEY, nickname TEXT, password TEXT);"))
	       ((code
		 declared-data-type
		 collation-sequence-name
		 not-null-constraint-exists?
		 column-is-part-of-primary-key?
		 column-is-auto-increment?)
		(sqlite3-table-column-metadata conn "main" "accounts" "nickname")))
	    (list code
		  (utf8->string declared-data-type)
		  (utf8->string collation-sequence-name)
		  not-null-constraint-exists?
		  column-is-part-of-primary-key?
		  column-is-auto-increment?)))
      => `(,SQLITE_OK "TEXT" "BINARY" #f #f #f)))

;;; --------------------------------------------------------------------

  (check
      (with-connection (conn)
	(let-values (((code current highwater)
		      (sqlite3-db-status conn SQLITE_DBSTATUS_STMT_USED)))
;;;(check-pretty-print (list code current highwater))
	  code))
    => SQLITE_OK)

  #t)


(parametrise ((check-test-name	'sql-aux))

  (check	;sqlite3-last-insert-rowid
      (with-connection (conn)
	(sqlite3-last-insert-rowid conn))
    => 0)

  (check	;sqlite3-changes
      (with-connection (conn)
	(sqlite3-changes conn))
    => 0)

  (check	;sqlite3-total-changes
      (with-connection (conn)
	(sqlite3-total-changes conn))
    => 0)

  (check	;sqlite3-interrupt
      (with-connection (conn)
	(sqlite3-interrupt conn))
    => (void))

;;; --------------------------------------------------------------------

  (check	;sqlite3-complete
      (with-connection (conn)
	(sqlite3-complete "insert into the_table"))
    => #f)

  (check	;sqlite3-complete
      (with-connection (conn)
	(sqlite3-complete "insert into the_table (a b) values ('c', 'd');"))
    => #t)

  (check	;sqlite3-complete16
      (with-connection (conn)
	(sqlite3-complete16 "insert into the_table"))
    => #f)

  (check	;sqlite3-complete16
      (with-connection (conn)
	(sqlite3-complete16 "insert into the_table (a b) values ('c', 'd');"))
    => #t)

;;; --------------------------------------------------------------------

  (check	;sqlite3-progress-handler
      (with-connection (conn)
	(sqlite3-progress-handler conn))
    => (void))

  (check	;sqlite3-progress-handler
      (with-connection (conn)
	(sqlite3-progress-handler conn 1
				  (make-sqlite3-progress-handler-callback
				   (lambda () 0))))
    => (void))

  #t)


(parametrise ((check-test-name	'hooks))

  (check	;sqlite3-commit-hook
      (with-connection (conn)
	(sqlite3-commit-hook conn #f)
	#t)
    => #t)

  (check	;sqlite3-commit-hook
      (with-connection (conn)
	(sqlite3-commit-hook conn (make-sqlite3-commit-hook-callback
				   (lambda () #f)))
	#t)
    => #t)

  (check 	;make-sqlite3-commit-hook-callback, return true
      (let* ((maker	(ffi.make-c-callout-maker 'signed-int '(pointer)))
	     (callback	(make-sqlite3-commit-hook-callback (lambda () #t)))
	     (func	(maker callback)))
	(func (null-pointer)))
    => 1)

  (check	;make-sqlite3-commit-hook-callback, return false
      (let* ((maker	(ffi.make-c-callout-maker 'signed-int '(pointer)))
	     (callback	(make-sqlite3-commit-hook-callback (lambda () #f)))
	     (func	(maker callback)))
	(func (null-pointer)))
    => 0)

  (check	;make-sqlite3-commit-hook-callback, raise exception
      (let* ((maker	(ffi.make-c-callout-maker 'signed-int '(pointer)))
	     (callback	(make-sqlite3-commit-hook-callback (lambda () (error 'cb "error"))))
	     (func	(maker callback)))
	(func (null-pointer)))
    => 0)

;;; --------------------------------------------------------------------

  (check	;sqlite3-rollback-hook
      (with-connection (conn)
	(sqlite3-rollback-hook conn #f)
	#t)
    => #t)

  (check	;sqlite3-rollback-hook
      (with-connection (conn)
	(sqlite3-rollback-hook conn (make-sqlite3-rollback-hook-callback
				     (lambda () #f)))
	#t)
    => #t)

  (check	;make-sqlite3-rollback-hook-callback, return nothing
      (let* ((maker	(ffi.make-c-callout-maker 'void '(pointer)))
	     (callback	(make-sqlite3-rollback-hook-callback (lambda () (values))))
	     (func	(maker callback)))
	(func (null-pointer)))
    => (void))

  (check	;make-sqlite3-rollback-hook-callback, raise exception
      (let* ((maker	(ffi.make-c-callout-maker 'void '(pointer)))
	     (callback	(make-sqlite3-rollback-hook-callback (lambda () (error 'cb "error"))))
	     (func	(maker callback)))
	(func (null-pointer)))
    => (void))

;;; --------------------------------------------------------------------

  (check	;sqlite3-update-hook
      (with-connection (conn)
	(sqlite3-update-hook conn #f)
	#t)
    => #t)

  (check	;sqlite3-update-hook
      (with-connection (conn)
	(sqlite3-update-hook conn (make-sqlite3-update-hook-callback
				   (lambda (operation database-name table-name rowid)
				     (values))))
	#t)
    => #t)

  (check	;make-sqlite3-update-hook-callback, return nothing
      (with-result
       (let* ((maker	(ffi.make-c-callout-maker
			 'void '(pointer signed-int pointer pointer int64_t)))
	      (callback	(make-sqlite3-update-hook-callback
			 (lambda (operation database-name table-name rowid)
			   (add-result (vector operation
					       (cstring->string database-name)
					       (cstring->string table-name)
					       rowid))
			   (values))))
	      (func	(maker callback)))
	 (func (null-pointer)
	       SQLITE_INSERT
	       (string->guarded-cstring "database")
	       (string->guarded-cstring "table")
	       123)))
    => `(,(void) (#(,SQLITE_INSERT "database" "table" 123))))

  (check	;make-sqlite3-update-hook-callback, raise exception
      (let* ((maker	(ffi.make-c-callout-maker
			 'void '(pointer signed-int pointer pointer int64_t)))
	     (callback	(make-sqlite3-update-hook-callback
			 (lambda (operation database-name table-name rowid)
			   (error 'cb "error"))))
	     (func	(maker callback)))
	(func (null-pointer)
	      SQLITE_INSERT
	      (string->guarded-cstring "database")
	      (string->guarded-cstring "table")
	      123))
    => (void))

  #t)


;;;; done

(collect)
(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; End:
