;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: tests for SQLite BLOB functions
;;;Date: Fri Aug 10, 2012
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
(import (vicare)
  (vicare databases sqlite3)
  (vicare databases sqlite3 constants)
  (vicare databases sqlite3 features)
  (prefix (vicare ffi) ffi.)
  (vicare syntactic-extensions)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare SQLite bindings, BLOB functions\n")

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

(define-syntax with-blob
  (syntax-rules ()
    ((_ (?blob-var) . ?body)
     (with-connection (conn)
       (sqlite3-exec conn "create table stuff (id INTEGER PRIMARY KEY, data TEXT);")
       (let-values (((code stmt end-offset1)
		     (sqlite3-prepare-v2 conn "insert into stuff (id, data) \
                                                  values (?1, ?2);")))
	 (when (= SQLITE_OK code)
	   (unwind-protect
	       (begin
		 (sqlite3-bind-int64    stmt 1 1)
		 (sqlite3-bind-zeroblob stmt 2 4)
		 (let ((rv (sqlite3-step stmt)))
		   (when (= SQLITE_DONE rv)
;;;		     (check-pretty-print "done step")
		     (let-values (((rv ?blob-var)
				   (sqlite3-blob-open conn "main" "stuff" "data" 1 #t)))
		       (when (= SQLITE_OK rv)
;;;			 (check-pretty-print "done opening")
;;;			 (check-pretty-print ?blob-var)
			 (unwind-protect
			     (begin . ?body)
			   (sqlite3-blob-close ?blob-var)))))))
	     (when (sqlite3-stmt?/valid stmt)
	       (sqlite3-finalize stmt)))))))))


(parametrise ((check-test-name	'open-close))

  (check
      (with-blob (blob)
	(sqlite3-blob-bytes blob))
    => 4)

  (check
      (with-blob (blob)
	(sqlite3-blob? blob))
    => #t)

  (check
      (with-blob (blob)
	(sqlite3-blob?/open blob))
    => #t)

  #t)


(parametrise ((check-test-name	'read-write))

  (check	;writing
      (with-blob (blob)
	(sqlite3-blob-write blob 0 '#ve(ascii "ciao") 0 4))
    => SQLITE_OK)

  (check	;reading and writing
      (with-blob (blob)
	(sqlite3-blob-write blob 0 '#ve(ascii "ciao") 0 4)
	(let ((buffer (make-bytevector 4)))
	  (sqlite3-blob-read blob 0 buffer 0 4)
	  (utf8->string buffer)))
    => "ciao")

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; eval: (put 'with-blob 'scheme-indent-function 1)
;; End:
