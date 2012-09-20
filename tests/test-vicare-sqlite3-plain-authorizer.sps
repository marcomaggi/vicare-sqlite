;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: tests for data access authorizer
;;;Date: Fri Aug 24, 2012
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (vicare databases sqlite3)
  (vicare databases sqlite3 constants)
  (vicare databases sqlite3 features)
  (vicare syntactic-extensions)
  (prefix (vicare ffi) ffi.)
  (prefix (vicare words) words.)
  (vicare platform-constants)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare SQLite bindings, data access authorizer\n")

#;(set-port-buffer-mode! (current-error-port) (buffer-mode line))
#;(set-port-buffer-mode! (current-output-port) (buffer-mode none))
#;(struct-guardian-logger #t)


;;;; helpers

(define-syntax with-connection
  (syntax-rules ()
    ((_ (?connect-var) . ?body)
     (let ((?connect-var (sqlite3-open ":memory:")))
       (unwind-protect
	   (let () . ?body)
	 (when (sqlite3? ?connect-var)
	   (sqlite3-close ?connect-var)))))))

(define-syntax with-statement
  (syntax-rules ()
    ((_ (?statement-var ?conn) . ?body)
     (begin
       (let ((snippet "insert into accounts (nickname, password) \
                         values (?1, ?2);"))
	 (let-values (((code ?statement-var end-offset)
		       (sqlite3-prepare-v2 ?conn snippet)))
	   (unwind-protect
	       (let ((rv (let () . ?body)))
;;;		 (check-pretty-print (sqlite3-errmsg conn))
		 rv)
	     (when (sqlite3-stmt?/valid ?statement-var)
	       (sqlite3-finalize ?statement-var)))))))))


(parametrise ((check-test-name	'base))

  (check
      (with-result
       (let ((auth-cb (make-sqlite3-authorizer-callback
		       (lambda (action a b c d)
			 (define-inline (cstr->str s)
			   (and (not (pointer-null? s))
				(cstring->string s)))
			 (let ((data (list (sqlite3-authorizer-action-code->symbol action)
					   (cstr->str a) (cstr->str b)
					   (cstr->str c) (cstr->str d))))
;;;(check-pretty-print data)
			   (add-result data))
			 SQLITE_OK))))
	 (unwind-protect
	     (with-connection (conn)
	       (sqlite3-exec conn
			     "create table Stuff \
                               (A TEXT, B TEXT, C TEXT);
                             insert into Stuff
                               (A, B, C) values ('one', 'two', 'three');
                             insert into Stuff
                               (A, B, C) values ('four', 'five', 'six');")
	       (let ((rv (sqlite3-set-authorizer conn auth-cb)))
		 (if (= rv SQLITE_OK)
		     (let-values (((code stmt end-offset)
				   (sqlite3-prepare-v2 conn "select * from Stuff;")))
		       code)
		   rv)))
	   (ffi.free-c-callback auth-cb))))
    => `(,SQLITE_OK
	 ((SQLITE_SELECT #f #f #f #f)
	  (SQLITE_READ "Stuff" "A" "main" #f)
	  (SQLITE_READ "Stuff" "B" "main" #f)
	  (SQLITE_READ "Stuff" "C" "main" #f))))

  #t)


;;;; done

(collect)
(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; End:
