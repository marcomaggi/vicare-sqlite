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
(check-display "*** testing Vicare SQLite bindings, core functions\n")

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

  (check
      (integer? (sqlite3-release-memory 100))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (sqlite3-soft-heap-limit #e1e5)
    => (void))

  (check
      (unwind-protect
	  (begin
	    (sqlite3-soft-heap-limit64 #e1e10)
	    (sqlite3-soft-heap-limit64 0))
	(sqlite3-soft-heap-limit64 0))
    => #e1e10)

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


(parametrise ((check-test-name	'misc))

  (check
      (sqlite3-sleep 1)
    => 1)

  (check
      (with-result
       (let ((cb (make-sqlite3-log-callback
		  (lambda (error-code message)
		    (add-result (vector error-code (cstring->string message)))))))
	 (sqlite3-shutdown)
	 (unwind-protect
	     (let ((rv (sqlite3-config SQLITE_CONFIG_LOG cb)))
	       (sqlite3-initialize)
	       (if (= SQLITE_OK rv)
		   (unwind-protect
		       (sqlite3-log SQLITE_ERROR "the error")
		     (begin
		       (sqlite3-shutdown)
		       (sqlite3-config SQLITE_CONFIG_LOG (null-pointer))))
		 rv))
	   (ffi.free-c-callback cb))))
    => `(,(void) (#(,SQLITE_ERROR "the error"))))

;;; --------------------------------------------------------------------

  (check
      (let ((rv (sqlite3-randomness 10)))
;;;(check-pretty-print rv)
	(bytevector-length rv))
    => 10)

  (check
      (let* ((bv (make-bytevector 10))
	     (rv (sqlite3-randomness! bv)))
;;;(check-pretty-print rv)
	(eq? bv rv))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; End:
