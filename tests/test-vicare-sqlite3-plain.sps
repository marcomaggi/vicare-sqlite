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

  (check	;sqlite3-db-config
      (with-connection (conn)
	(sqlite3-db-config conn SQLITE_DBCONFIG_ENABLE_FKEY 0))
    => #f)

  (check	;sqlite3-extended-result-codes
      (with-connection (conn)
	(sqlite3-extended-result-codes conn #t))
    => SQLITE_OK)

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

  (check	;sqlite3-reset
      (with-statement (stmt)
	(sqlite3-reset stmt))
    => SQLITE_OK)

  ;; run the garbage collection
  #;(collect)
  #f)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; End:
