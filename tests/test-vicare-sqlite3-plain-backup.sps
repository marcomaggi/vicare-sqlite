;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: tests for database backup
;;;Date: Tue Aug 28, 2012
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare SQLite bindings, backup functions\n")

#;(set-port-buffer-mode! (current-error-port) (buffer-mode line))
#;(set-port-buffer-mode! (current-output-port) (buffer-mode none))
#;(struct-guardian-logger #t)


;;;; helpers

(define-syntax with-connection
  (syntax-rules ()
    ((_ (?connect-var ?filename) . ?body)
     (let ((pathname ?filename))
       (unwind-protect
	   (let ((?connect-var (sqlite3-open pathname)))
	     (unwind-protect
		 (begin . ?body)
	       (when (sqlite3? ?connect-var)
		 (sqlite3-close ?connect-var))))
	 (when (file-exists? pathname)
	   (delete-file pathname)))))))

(define-syntax with-backup
  (syntax-rules ()
    ((_ (?bck ?dst-conn ?src-conn) . ?body)
     (let ((?bck (sqlite3-backup-init ?dst-conn "main" ?src-conn "main")))
       (unwind-protect
	   (let () . ?body)
	 (when (sqlite3-backup?/running ?bck)
	   (sqlite3-backup-finish ?bck)))))))


(parametrise ((check-test-name	'base))

  (check	;sqlite3-backup-step
      (with-connection (conn1 "sqlite.test1.db")
	(with-connection (conn2 "sqlite.test2.db")
	  (sqlite3-exec conn2
			"create table Stuff (alpha TEXT, beta TEXT);
                         insert into Stuff (alpha, beta) values ('A', 'B');
                         insert into Stuff (alpha, beta) values ('C', 'D');
                         insert into Stuff (alpha, beta) values ('E', 'F');
                         insert into Stuff (alpha, beta) values ('G', 'H');")
	  (with-backup (bck conn1 conn2)
;;;	    (check-pretty-print bck)
	    (sqlite3-backup-step bck -1))))
    => SQLITE_DONE)

;;; --------------------------------------------------------------------

  (check	;sqlite3-backup-remaining
      (with-connection (conn1 "sqlite.test1.db")
	(with-connection (conn2 "sqlite.test2.db")
	  (sqlite3-exec conn2
			"create table Stuff (alpha TEXT, beta TEXT);
                         insert into Stuff (alpha, beta) values ('A', 'B');
                         insert into Stuff (alpha, beta) values ('C', 'D');
                         insert into Stuff (alpha, beta) values ('E', 'F');
                         insert into Stuff (alpha, beta) values ('G', 'H');")
	  (with-backup (bck conn1 conn2)
	    (sqlite3-backup-remaining bck))))
    => 0)

  (check	;sqlite3-backup-pagecount
      (with-connection (conn1 "sqlite.test1.db")
	(with-connection (conn2 "sqlite.test2.db")
	  (sqlite3-exec conn2 "create table Stuff (alpha TEXT, beta TEXT);")
	  (do ((i 0 (+ 1 i)))
	      ((= i 100))
	    (sqlite3-exec conn2
			  (format "insert into Stuff (alpha, beta) values ('~a', '~a');"
			    i (* 100 i))))
	  (with-backup (bck conn1 conn2)
	    (sqlite3-backup-pagecount bck))))
    => 0)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
       (with-connection (conn1 "sqlite.test1.db")
	(with-connection (conn2 "sqlite.test2.db")
	  (with-backup (bck conn1 conn2)
	    (set-sqlite3-backup-destructor! bck
					    (lambda (bck)
					      (add-result 123)))
	    #t))))
    => '(#t (123)))

  (collect))


;;;; done

(collect)
(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-backup 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; End:
