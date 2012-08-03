;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: basic tests to be included in the documentation
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
  (vicare syntactic-extensions)
  (prefix (vicare ffi) ffi.))

(set-port-buffer-mode! (current-output-port) (buffer-mode none))
(set-port-buffer-mode! (current-error-port)  (buffer-mode line))


;;;; simple code execution

(when #f
  (let ()
    (define database-pathname "sqlite.proof.db")
    (define connection
      (sqlite3-open database-pathname))

    (define sql-snippet
      "create table accounts
         (id       INTEGER PRIMARY KEY,
          nickname TEXT,
          password TEXT);
       insert into accounts (nickname, password)
         values ('ichigo', 'abcde');
       insert into accounts (nickname, password)
         values ('rukia', '12345');
       insert into accounts (nickname, password)
         values ('chad', 'fist');
       select * from accounts;")

    (define result-names #f)
    (define result-texts '())

    (define each-row-callback
      (make-sqlite3-exec-callback
       (lambda (number-of-rows texts names)
	 (unless result-names
	   (set! result-names
		 (map utf8->string (vector->list names))))
	 (set! result-texts
	       (cons (map utf8->string (vector->list texts))
		     result-texts))
	 #f)))

    (let-values (((rv error-message)
		  (sqlite3-exec connection sql-snippet
				each-row-callback)))
      (if (= SQLITE_OK rv)
	  (begin
	    (printf "names: ~s\n" result-names)
	    (printf "texts: ~s\n" (reverse result-texts)))
	(printf "error: ~a\n" error-message)))

    (ffi.free-c-callback each-row-callback)
    (sqlite3-close connection)
    (delete-file database-pathname)

    #f))


;;;; prepared statement execution

(when #t
  (let ()
    (define database-pathname ":memory:")

    (define sql-preparation
      "create table accounts
         (id       INTEGER PRIMARY KEY,
          nickname TEXT,
          password TEXT);
       insert into accounts (nickname, password)
         values ('ichigo', 'abcde');
       insert into accounts (nickname, password)
         values ('rukia', '12345');
       insert into accounts (nickname, password)
         values ('chad', 'fist');")

    (define sql-query
      "select * from accounts;")

    (let ((conn (sqlite3-open database-pathname)))
      (when (sqlite3?/open conn)
	(let-values (((rv error-message)
		      (sqlite3-exec conn sql-preparation)))
	  (when (= rv SQLITE_OK)
	    (let-values (((code stmt end-offset)
			  (sqlite3-prepare-v2 conn sql-query)))
	      (when (= code SQLITE_OK)
		(unwind-protect
		    (do ((rv (sqlite3-step stmt) (sqlite3-step stmt))
			 (row 0 (+ 1 row)))
			((or (= rv SQLITE_DONE)
			     (not (= rv SQLITE_ROW))))
		      (let ((colnum (sqlite3-data-count stmt)))
			(printf "row ~a: ~a, ~a\n" row
				(sqlite3-column-text/string stmt 1)
				(sqlite3-column-text/string stmt 2))))
		  (sqlite3-finalize stmt))))))
	(sqlite3-close conn)))

    #f))


;;;; done


;;; end of file
