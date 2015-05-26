;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: tests for SQLite SQL statements functions
;;;Date: Fri Aug 10, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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

(set-port-buffer-mode! (current-error-port) (buffer-mode line))
(set-port-buffer-mode! (current-output-port) (buffer-mode none))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare SQLite bindings, statements\n")

#;(struct-guardian-logger #t)


;;;; helpers

(define-syntax with-connection
  (syntax-rules ()
    ((_ (?conn) . ?body)
     (let* ((pathname ":memory:")
	    (?conn (sqlite3-open pathname)))
       (unwind-protect
	   (let () . ?body)
	 (when (sqlite3? ?conn)
	   (sqlite3-close ?conn)))))))

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
	    (list code
		  (= end-offset (bytevector-length (string->utf8 snippet)))
		  (sqlite3-stmt?/valid stmt)))))
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
	    (list code
		  (= end-offset (bytevector-length (string->utf8 snippet)))
		  (sqlite3-stmt?/valid stmt)))))
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
	    (list code
		  (= end-offset (bytevector-length (string->utf16n snippet)))
		  (sqlite3-stmt?/valid stmt)))))
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
	    (list code
		  (= end-offset (bytevector-length (string->utf16n snippet)))
		  (sqlite3-stmt?/valid stmt)))))
    => `(,SQLITE_OK #t #t))

;;; --------------------------------------------------------------------

  (check
      (with-statement (stmt)
	(eqv? stmt stmt))
    => #t)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
       (with-statement (stmt)
	 (set-sqlite3-stmt-destructor! stmt
				       (lambda (stmt)
					 (add-result 123)))
	 #t))
    => '(#t (123)))

  #t)


(parametrise ((check-test-name	'auxiliary))

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

;;; --------------------------------------------------------------------

  (check	;sqlite3-stmt-status
      (with-statement (stmt)
	(sqlite3-stmt-status stmt SQLITE_STMTSTATUS_AUTOINDEX))
    => 0)

  #t)


(parametrise ((check-test-name	'binding))

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

  #t)


(parametrise ((check-test-name	'stepping))

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
      => '("main" "main")))

  (when HAVE_SQLITE3_COLUMN_DATABASE_NAME16
    (check	;sqlite3-column-database-name16/string
	(with-statement (stmt)
	  (list (sqlite3-column-database-name16/string stmt 1)
		(sqlite3-column-database-name16/string stmt 2)))
      => '("main" "main")))

;;; --------------------------------------------------------------------

  (when HAVE_SQLITE3_COLUMN_TABLE_NAME
    (check	;sqlite3-column-table-name/string
	(with-statement (stmt)
	  (list (sqlite3-column-table-name/string stmt 1)
		(sqlite3-column-table-name/string stmt 2)))
      => '("accounts" "accounts")))

  (when HAVE_SQLITE3_COLUMN_TABLE_NAME16
    (check	;sqlite3-column-table-name16/string
	(with-statement (stmt)
	  (list (sqlite3-column-table-name16/string stmt 1)
		(sqlite3-column-table-name16/string stmt 2)))
      => '("accounts" "accounts")))

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
	(sqlite3-value? (sqlite3-column-value stmt 1)))
    => #t)

  #t)


;;;; done

(collect 4)
(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; End:
