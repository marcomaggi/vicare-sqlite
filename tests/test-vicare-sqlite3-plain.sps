;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: tests for Expat bindings
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


#!r6rs
(import (vicare)
  (vicare databases sqlite3)
  (vicare databases sqlite3 constants)
  (vicare databases sqlite3 features)
  (prefix (vicare ffi) ffi.)
  (vicare syntactic-extensions)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare SQLite bindings\n")


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


(parametrise ((check-test-name	'library))

  (check
      (sqlite3-initialize)
    => SQLITE_OK)

  (check
      (sqlite3-shutdown)
    => SQLITE_OK)

;;; --------------------------------------------------------------------

  (check
      (sqlite3-os-init)
    => SQLITE_OK)

  (check
      (sqlite3-os-end)
    => SQLITE_OK)

;;; --------------------------------------------------------------------

  (check
      (begin
	(sqlite3-shutdown)
	(sqlite3-config SQLITE_CONFIG_SINGLETHREAD))
    => SQLITE_OK)

;;; --------------------------------------------------------------------

  (when #f
    (printf "memory used: ~a\n" (sqlite3-memory-used))
    (printf "memory highwater: ~a\n" (sqlite3-memory-highwater #f))
    (flush-output-port (current-output-port)))

  (check
      (integer? (sqlite3-memory-used))
    => #t)

  (check
      (integer? (sqlite3-memory-highwater #f))
    => #t)

  #t)


(parametrise ((check-test-name	'version))

  (when #f
    (check-pretty-print
     `((Vicare/SQLite.version		,(vicare-sqlite3-version-string))
       (Vicare/SQLite.current		,(vicare-sqlite3-version-interface-current))
       (Vicare/SQLite.revision		,(vicare-sqlite3-version-interface-revision))
       (Vicare/SQLite.age		,(vicare-sqlite3-version-interface-age))
       (SQLite.libversion		,(sqlite3-libversion))
       (SQLite.libversion-number	,(sqlite3-libversion-number))
       (SQLite.sourceid			,(sqlite3-sourceid)))))

;;; --------------------------------------------------------------------

  (check
      (string? (vicare-sqlite3-version-string))
    => #t)

  (check
      (fixnum? (vicare-sqlite3-version-interface-current))
    => #t)

  (check
      (fixnum? (vicare-sqlite3-version-interface-revision))
    => #t)

  (check
      (fixnum? (vicare-sqlite3-version-interface-age))
    => #t)

  (check
      (string? (sqlite3-libversion))
    => #t)

  (check
      (integer? (sqlite3-libversion-number))
    => #t)

  (check
      (string? (sqlite3-sourceid))
    => #t)

  #t)


(parametrise ((check-test-name	'compiled-options))

  (check
      (sqlite3-compileoption-used "ciao")
    => #f)

  (check
      (string? (sqlite3-compileoption-get 0))
    => #t)

  (when #f
    (let loop ((i  0)
	       (op (sqlite3-compileoption-get 0)))
      (and op
	   (begin
	     (check-pretty-print (list 'compileoption i op))
	     (let ((i (+ 1 i)))
	       (loop i (sqlite3-compileoption-get i)))))))

;;; --------------------------------------------------------------------

  (check
      (boolean? (sqlite3-threadsafe))
    => #t)

  #t)


(parametrise ((check-test-name	'connection))

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

  #;(check	;sqlite3-db-readonly?
      (with-connection (conn)
	(sqlite3-db-readonly? conn "main"))
    => 0)

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


(parametrise ((check-test-name	'errors))

  (check	;sqlite3-errcode
      (with-connection (conn)
	(sqlite3-errcode conn))
    => SQLITE_OK)

;;; --------------------------------------------------------------------

  (check	;sqlite3-extended-errcode
      (with-connection (conn)
	(sqlite3-extended-errcode conn))
    => SQLITE_OK)

;;; --------------------------------------------------------------------

  (check	;sqlite3-errmsg
      (with-connection (conn)
	(sqlite3-errmsg conn))
    => "not an error")

;;; --------------------------------------------------------------------

  (check	;sqlite3-errmsg16
      (with-connection (conn)
	(sqlite3-errmsg16 conn))
    => "\xFFFD;")

  #t)


(parametrise ((check-test-name	'exec))

  (check	;without callback
      (with-connection (conn)
	(let ((sql "create table accounts \
                      (id       INTEGER PRIMARY KEY, \
                       nickname TEXT, \
                       password TEXT); \
                    insert into accounts (nickname, password) \
                      values ('ichigo', 'abcde'); \
                    insert into accounts (nickname, password) \
                      values ('rukia', '12345'); \
                    insert into accounts (nickname, password) \
                      values ('chad', 'fist'); \
                    select * from accounts;"))
	  (let-values (((rv errmsg)
			(sqlite3-exec conn sql)))
	    (list rv errmsg))))
    => `(,SQLITE_OK #f))

  (check	;with callback
      (with-connection (conn)
	(let ((sql "create table accounts \
                      (id       INTEGER PRIMARY KEY, \
                       nickname TEXT, \
                       password TEXT); \
                    insert into accounts (nickname, password) \
                      values ('ichigo', 'abcde'); \
                    insert into accounts (nickname, password) \
                      values ('rukia', '12345'); \
                    insert into accounts (nickname, password) \
                      values ('chad', 'fist'); \
                    select * from accounts;")
	      (cb (make-sqlite3-exec-callback
		   (lambda (number-of-rows texts names)
		     (when #f
		       (check-pretty-print (map utf8->string (vector->list names)))
		       (check-pretty-print (map utf8->string (vector->list texts))))
		     #f))))
	  (unwind-protect
	      (let-values (((rv errmsg)
			    (sqlite3-exec conn sql cb)))
		(list rv errmsg))
	    (ffi.free-c-callback cb))))
    => `(,SQLITE_OK #f))

  #t)


(parametrise ((check-test-name	'get-table))

  (check
      (with-connection (conn)
	(let ((sql "create table accounts \
                      (id       INTEGER PRIMARY KEY, \
                       nickname TEXT, \
                       password TEXT); \
                    insert into accounts (nickname, password) \
                      values ('ichigo', 'abcde'); \
                    insert into accounts (nickname, password) \
                      values ('rukia', '12345'); \
                    insert into accounts (nickname, password) \
                      values ('chad', 'fist'); \
                    select * from accounts;"))
	  (let-values (((rv errmsg num-of-rows num-of-cols result)
			(sqlite3-get-table conn sql)))
	    (unwind-protect
		(list rv errmsg num-of-rows num-of-cols (pointer? result))
	      (when (pointer? result)
		(sqlite3-free-table result))))))
    => `(,SQLITE_OK #f 3 3 #t))

  (check
      (with-connection (conn)
	(let ((sql "create table accounts \
                      (id       INTEGER PRIMARY KEY, \
                       nickname TEXT, \
                       password TEXT); \
                    insert into accounts (nickname, password) \
                      values ('ichigo', 'abcde'); \
                    insert into accounts (nickname, password) \
                      values ('rukia', '12345'); \
                    insert into accounts (nickname, password) \
                      values ('chad', 'fist'); \
                    select * from accounts;"))
	  (let-values (((rv errmsg num-of-rows num-of-cols result)
			(sqlite3-get-table conn sql)))
	    (unwind-protect
		(map (lambda (row)
		       (if (vector? row)
			   (map utf8->string (vector->list row))
			 row))
		  (vector->list (sqlite3-table-to-vector num-of-rows num-of-cols result)))
	      (when (pointer? result)
		(sqlite3-free-table result))))))
    => '(("id" "nickname" "password")
	 ("1" "ichigo" "abcde")
	 ("2" "rukia" "12345")
	 ("3" "chad" "fist")))

  #t)


(parametrise ((check-test-name	'statements))

  ;; run the garbage collection
  #;(collect)

  (check	;sqlite3-prepare-v2
      (with-connection (conn)
	(sqlite3-exec conn "create table accounts \
                             (id       INTEGER PRIMARY KEY, \
                              nickname TEXT, \
                              password TEXT);")
	(let ((snippet "insert into accounts (nickname, password) \
                          values ('?001', '?002');"))
	  (let-values (((code stmt end-offset)
			(sqlite3-prepare-v2 conn snippet)))
;;;	    (check-pretty-print stmt)
	    (unwind-protect
		(list code
		      (= end-offset (bytevector-length (string->utf8 snippet)))
		      (sqlite3-stmt?/valid stmt))
	      (when (sqlite3-stmt?/valid stmt)
		(sqlite3-finalize stmt))))))
    => `(,SQLITE_OK #t #t))

  (check	;sqlite3-prepare
      (with-connection (conn)
	(sqlite3-exec conn "create table accounts \
                             (id       INTEGER PRIMARY KEY, \
                              nickname TEXT, \
                              password TEXT);")
	(let ((snippet "insert into accounts (nickname, password) \
                          values ('?001', '?002');"))
	  (let-values (((code stmt end-offset)
			(sqlite3-prepare conn snippet)))
;;;	    (check-pretty-print stmt)
	    (unwind-protect
		(list code
		      (= end-offset (bytevector-length (string->utf8 snippet)))
		      (sqlite3-stmt?/valid stmt))
	      (when (sqlite3-stmt?/valid stmt)
		(sqlite3-finalize stmt))))))
    => `(,SQLITE_OK #t #t))

;;; --------------------------------------------------------------------

  (check	;sqlite3-prepare16-v2
      (with-connection (conn)
	(sqlite3-exec conn "create table accounts \
                             (id       INTEGER PRIMARY KEY, \
                              nickname TEXT, \
                              password TEXT);")
	(let ((snippet "insert into accounts (nickname, password) \
                          values ('?001', '?002');"))
	  (let-values (((code stmt end-offset)
			(sqlite3-prepare16-v2 conn snippet)))
;;;	    (check-pretty-print stmt)
	    (unwind-protect
		(list code
		      (= end-offset (bytevector-length (string->utf16n snippet)))
		      (sqlite3-stmt?/valid stmt))
	      (when (sqlite3-stmt?/valid stmt)
		(sqlite3-finalize stmt))))))
    => `(,SQLITE_OK #t #t))

  (check	;sqlite3-prepare16
      (with-connection (conn)
	(sqlite3-exec conn "create table accounts \
                             (id       INTEGER PRIMARY KEY, \
                              nickname TEXT, \
                              password TEXT);")
	(let ((snippet "insert into accounts (nickname, password) \
                          values ('?001', '?002');"))
	  (let-values (((code stmt end-offset)
			(sqlite3-prepare16 conn snippet)))
;;;	    (check-pretty-print stmt)
	    (unwind-protect
		(list code
		      (= end-offset (bytevector-length (string->utf16n snippet)))
		      (sqlite3-stmt?/valid stmt))
	      (when (sqlite3-stmt?/valid stmt)
		(sqlite3-finalize stmt))))))
    => `(,SQLITE_OK #t #t))

;;; --------------------------------------------------------------------

  (check
      (with-statement (stmt)
	(eqv? stmt stmt))
    => #t)

;;; --------------------------------------------------------------------

  (check	;sqlite3-sql/string
      (with-statement (stmt)
	(sqlite3-sql/string stmt))
    => "insert into accounts (nickname, password) values (?1, ?2);")

  (check	;sqlite3-stmt-readonly?
      (with-statement (stmt)
	(sqlite3-stmt-readonly? stmt))
    => #f)

  (check	;sqlite3-stmt-busy?
      (with-statement (stmt)
	(sqlite3-stmt-busy? stmt))
    => #f)

;;; --------------------------------------------------------------------

  (check	;sqlite3-bind-blob
      (with-statement (stmt)
	(sqlite3-bind-blob stmt 1 '#vu8(1 2 3) 0 2 SQLITE_TRANSIENT))
    => SQLITE_OK)

  (check	;sqlite3-bind-double
      (with-statement (stmt)
	(sqlite3-bind-double stmt 1 1.2))
    => SQLITE_OK)

  (check	;sqlite3-bind-int
      (with-statement (stmt)
  	(sqlite3-bind-int stmt 1 12))
    => SQLITE_OK)

  (check	;sqlite3-bind-int64
      (with-statement (stmt)
  	(sqlite3-bind-int64 stmt 1 12))
    => SQLITE_OK)

  (check	;sqlite3-bind-null
      (with-statement (stmt)
  	(sqlite3-bind-null stmt 1))
    => SQLITE_OK)

  (check	;sqlite3-bind-text
      (with-statement (stmt)
  	(sqlite3-bind-text stmt 1 "ciao" 0 2 SQLITE_TRANSIENT))
    => SQLITE_OK)

  (check	;sqlite3-bind-text16
      (with-statement (stmt)
  	(sqlite3-bind-text16 stmt 1 "ciao" 0 2 SQLITE_TRANSIENT))
    => SQLITE_OK)

  (check	;sqlite3-bind-zeroblob
      (with-statement (stmt)
  	(sqlite3-bind-zeroblob stmt 1 100))
    => SQLITE_OK)

;;; --------------------------------------------------------------------

  (check	;sqlite3-bind-parameter-count
      (with-statement (stmt)
  	(sqlite3-bind-parameter-count stmt))
    => 2)

  (check	;sqlite3-bind-parameter-name
      (with-statement (stmt)
  	(sqlite3-bind-parameter-name stmt 1))
    => "?1")

  (check	;sqlite3-bind-parameter-name
      (with-statement (stmt)
  	(sqlite3-bind-parameter-name stmt 100))
    => #f)

  (check	;sqlite3-bind-parameter-index
      (with-statement (stmt)
  	(sqlite3-bind-parameter-index stmt "?2"))
    => 2)

  (check	;sqlite3-clear-bindings
      (with-statement (stmt)
  	(sqlite3-bind-int stmt 1 12)
	(sqlite3-clear-bindings stmt))
    => SQLITE_OK)

;;; --------------------------------------------------------------------

  (check	;sqlite3-step
      (with-statement (stmt)
  	(sqlite3-bind-text stmt 1 "rukia" #f #f SQLITE_TRANSIENT)
  	(sqlite3-bind-text stmt 2 "123"   #f #f SQLITE_TRANSIENT)
	(sqlite3-step stmt))
    => SQLITE_DONE)

  (check	;sqlite3-reset
      (with-statement (stmt)
	(sqlite3-reset stmt))
    => SQLITE_OK)

;;; --------------------------------------------------------------------

  (check	;sqlite3-db-handle
      (with-statement (stmt)
	(sqlite3?/open (sqlite3-db-handle stmt)))
    => #t)

;;; --------------------------------------------------------------------

  (check	;sqlite3-next-stmt
      (with-statement (stmt)
	(let* ((conn	(sqlite3-db-handle stmt))
	       (a	(sqlite3-next-stmt conn))
	       (b	(sqlite3-next-stmt conn a)))
	  (list (eqv? a stmt) b)))
    => '(#t #f))

  #f)


(parametrise ((check-test-name	'result-rows))

  (define setup-code
    "create table accounts (id INTEGER PRIMARY KEY, nickname TEXT, password TEXT); \
     insert into accounts (nickname, password) values ('ichigo', 'abcde');	   \
     insert into accounts (nickname, password) values ('rukia', '12345');	   \
     insert into accounts (nickname, password) values ('chad', 'fist');")

  (define-syntax with-statement
    (syntax-rules ()
      ((_ (?statement-var) . ?body)
       (with-connection (conn)
	 (sqlite3-exec conn setup-code)
	 (let ((snippet "select * from accounts;"))
	   (let-values (((code ?statement-var end-offset)
			 (sqlite3-prepare-v2 conn snippet)))
	     (unwind-protect
		 (let ((rv (begin . ?body)))
;;;		 (check-pretty-print (sqlite3-errmsg conn))
		   rv)
	       (when (sqlite3-stmt?/valid ?statement-var)
		 (sqlite3-finalize ?statement-var)))))))))

;;; --------------------------------------------------------------------

  (check	;sqlite3-step
      (with-statement (stmt)
	(let ((a (sqlite3-step stmt))
	      (b (sqlite3-step stmt))
	      (c (sqlite3-step stmt))
	      (d (sqlite3-step stmt)))
	  (list a b c d)))
    => `(,SQLITE_ROW ,SQLITE_ROW ,SQLITE_ROW ,SQLITE_DONE))

;;; --------------------------------------------------------------------

  (check	;sqlite3-column-count
      (with-statement (stmt)
	(sqlite3-column-count stmt))
    => 3)

;;; --------------------------------------------------------------------

  (check	;sqlite3-column-name/string
      (with-statement (stmt)
	(list (sqlite3-column-name/string stmt 1)
	      (sqlite3-column-name/string stmt 2)))
    => '("nickname" "password"))

  (check	;sqlite3-column-name16/string
      (with-statement (stmt)
	(list (sqlite3-column-name16/string stmt 1)
	      (sqlite3-column-name16/string stmt 2)))
    => '("nickname" "password"))

;;; --------------------------------------------------------------------

  (when HAVE_SQLITE3_COLUMN_DATABASE_NAME
    (check	;sqlite3-column-database-name/string
	(with-statement (stmt)
	  (list (sqlite3-column-database-name/string stmt 1)
		(sqlite3-column-database-name/string stmt 2)))
      => '("nickname" "password")))

  (when HAVE_SQLITE3_COLUMN_DATABASE_NAME16
    (check	;sqlite3-column-database-name16/string
	(with-statement (stmt)
	  (list (sqlite3-column-database-name16/string stmt 1)
		(sqlite3-column-database-name16/string stmt 2)))
      => '("nickname" "password")))

;;; --------------------------------------------------------------------

  (when HAVE_SQLITE3_COLUMN_TABLE_NAME
    (check	;sqlite3-column-table-name/string
	(with-statement (stmt)
	  (list (sqlite3-column-table-name/string stmt 1)
		(sqlite3-column-table-name/string stmt 2)))
      => '("nickname" "password")))

  (when HAVE_SQLITE3_COLUMN_TABLE_NAME16
    (check	;sqlite3-column-table-name16/string
	(with-statement (stmt)
	  (list (sqlite3-column-table-name16/string stmt 1)
		(sqlite3-column-table-name16/string stmt 2)))
      => '("nickname" "password")))

;;; --------------------------------------------------------------------

  (when HAVE_SQLITE3_COLUMN_ORIGIN_NAME
    (check	;sqlite3-column-origin-name/string
	(with-statement (stmt)
	  (list (sqlite3-column-origin-name/string stmt 1)
		(sqlite3-column-origin-name/string stmt 2)))
      => '("nickname" "password")))

  (when HAVE_SQLITE3_COLUMN_ORIGIN_NAME16
    (check	;sqlite3-column-origin-name16/string
	(with-statement (stmt)
	  (list (sqlite3-column-origin-name16/string stmt 1)
		(sqlite3-column-origin-name16/string stmt 2)))
      => '("nickname" "password")))

;;; --------------------------------------------------------------------

  (check	;sqlite3-column-decltype/string
      (with-statement (stmt)
	(list (sqlite3-column-decltype/string stmt 1)
	      (sqlite3-column-decltype/string stmt 2)))
    => '("TEXT" "TEXT"))

  (check	;sqlite3-column-decltype16/string
      (with-statement (stmt)
	(list (sqlite3-column-decltype16/string stmt 1)
	      (sqlite3-column-decltype16/string stmt 2)))
    => '("TEXT" "TEXT"))

;;; --------------------------------------------------------------------

  (check	;sqlite3-data-count
      (with-statement (stmt)
	;; no step
	(sqlite3-data-count stmt))
    => 0)

  (check	;sqlite3-data-count
      (with-statement (stmt)
	(sqlite3-step stmt)
	(sqlite3-data-count stmt))
    => 3)

;;; --------------------------------------------------------------------

  (check	;sqlite3-column-type
      (with-statement (stmt)
	(sqlite3-step stmt)
	(sqlite3-column-type stmt 1))
    => SQLITE_TEXT)

;;; --------------------------------------------------------------------

  (check	;sqlite3-column-bytes
      (with-statement (stmt)
	(sqlite3-step stmt)
	(sqlite3-column-bytes stmt 1))
    => (string-length "ichigo"))

  (check	;sqlite3-column-bytes16
      (with-statement (stmt)
	(sqlite3-step stmt)
	(sqlite3-column-bytes16 stmt 1))
    => (* 2 (string-length "ichigo")))

;;; --------------------------------------------------------------------

  (check	;sqlite3-column-double
      (with-statement (stmt)
	(sqlite3-step stmt)
	(sqlite3-step stmt)
	(sqlite3-column-double stmt 2))
    => 12345.0)

;;; --------------------------------------------------------------------

  (check	;sqlite3-column-int
      (with-statement (stmt)
	(sqlite3-step stmt)
	(sqlite3-step stmt)
	(sqlite3-column-int stmt 2))
    => 12345)

  (check	;sqlite3-column-int64
      (with-statement (stmt)
	(sqlite3-step stmt)
	(sqlite3-step stmt)
	(sqlite3-column-int64 stmt 2))
    => 12345)

;;; --------------------------------------------------------------------

  (check	;sqlite3-column-text/string
      (with-statement (stmt)
	(sqlite3-step stmt)
	(sqlite3-column-text/string stmt 1))
    => "ichigo")

  (check	;sqlite3-column-text16/string
      (with-statement (stmt)
	(sqlite3-step stmt)
	(sqlite3-column-text16/string stmt 1))
    => "ichigo")

;;; --------------------------------------------------------------------

  (check	;sqlite3-column-value
      (with-statement (stmt)
	(sqlite3-step stmt)
	(pointer? (sqlite3-column-value stmt 1)))
    => #t)

  #t)


(parametrise ((check-test-name	'misc))

  (check
      (sqlite3-sleep 1)
    => 1)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; End:
