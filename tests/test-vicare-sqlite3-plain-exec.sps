;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: tests for SQLite core functions
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
(check-display "*** testing Vicare SQLite bindings, simple SQL execution\n")

(set-port-buffer-mode! (current-error-port) (buffer-mode line))
(set-port-buffer-mode! (current-output-port) (buffer-mode none))


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


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; End:
