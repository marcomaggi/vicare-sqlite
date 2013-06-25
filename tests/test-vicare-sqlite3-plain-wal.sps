;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: tests for SQLite WAL functions
;;;Date: Thu Sep 20, 2012
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
(check-display "*** testing Vicare SQLite bindings, WAL functions\n")

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


(parametrise ((check-test-name	'basic))

  (check	;just activate WAL mode and nothing else
      (with-connection (conn)
	(let ((sql "pragma journal_mode=WAL;"))
	  (sqlite3-exec* conn sql)))
    => SQLITE_OK)

  (check	;sqlite3-wal-hook
      (with-result
       (let ((wal-cb (make-sqlite3-wal-hook
		      (lambda (custom-data conn database-name number-of-pages)
			(add-result database-name)
			SQLITE_OK))))
	 (unwind-protect
	     (with-connection (conn)
	       (sqlite3-wal-hook conn wal-cb)
	       (let ((sql "pragma journal_mode=WAL; \
                    create table accounts \
                      (id       INTEGER PRIMARY KEY, \
                       nickname TEXT, \
                       password TEXT); \
                    insert into accounts (nickname, password) \
                      values ('ichigo', 'abcde'); \
                    insert into accounts (nickname, password) \
                      values ('rukia', '12345'); \
                    insert into accounts (nickname, password) \
                      values ('chad', 'fist'); \
                    pragma main.wal_checkpoint(FULL);
                    select * from accounts;"))
		 (sqlite3-exec* conn sql)))
	   (ffi.free-c-callback wal-cb))))
    => `(,SQLITE_OK ()))

  (check	;sqlite3-wal-autocheckpoint
      (with-connection (conn)
	(let ((sql "pragma journal_mode=WAL; \
                    create table accounts \
                      (id       INTEGER PRIMARY KEY, \
                       nickname TEXT, \
                       password TEXT); \
                    insert into accounts (nickname, password) \
                      values ('ichigo', 'abcde'); \
                    insert into accounts (nickname, password) \
                      values ('rukia', '12345'); \
                    insert into accounts (nickname, password) \
                      values ('chad', 'fist');"))
	  (assert (= SQLITE_OK (sqlite3-exec* conn sql))))
	(sqlite3-wal-autocheckpoint conn 1))
    => SQLITE_OK)

  (check	;sqlite3-wal-checkpoint-v2
      (with-connection (conn)
	(let ((sql "pragma journal_mode=WAL; \
                    create table accounts \
                      (id       INTEGER PRIMARY KEY, \
                       nickname TEXT, \
                       password TEXT); \
                    insert into accounts (nickname, password) \
                      values ('ichigo', 'abcde'); \
                    insert into accounts (nickname, password) \
                      values ('rukia', '12345'); \
                    insert into accounts (nickname, password) \
                      values ('chad', 'fist');"))
	  (assert (= SQLITE_OK (sqlite3-exec* conn sql))))
	(let-values (((code size-of-wal-log number-of-checkpointed-frames)
		      (sqlite3-wal-checkpoint-v2 conn "main" SQLITE_CHECKPOINT_FULL)))
;;;	  (check-pretty-print (list code size-of-wal-log number-of-checkpointed-frames))
	  code))
    => SQLITE_OK)

  #t)


(parametrise ((check-test-name	'callback))

  (check	;make-sqlite3-wal-hook
      (with-result
       (let ((co (ffi.make-c-callout-maker 'signed-int
					   '(pointer pointer pointer signed-int)))
	     (cb (make-sqlite3-wal-hook
		  (lambda (custom-data conn database-name number-of-pages)
		    (add-result (list custom-data (sqlite3? conn)
				      database-name number-of-pages))
		    SQLITE_OK))))
	 (unwind-protect
	     ((co cb) (null-pointer) (null-pointer) (string->cstring "spiffy") 123)
	   (ffi.free-c-callback cb))))
    => `(,SQLITE_OK ((#f #t "spiffy" 123))))

  #t)


;;;; done

(collect)
(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; End:
