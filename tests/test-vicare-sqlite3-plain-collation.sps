;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: tests for collation functions
;;;Date: Wed Aug 29, 2012
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
  (vicare language-extensions syntaxes)
  (prefix (vicare ffi) ffi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare SQLite bindings, collation functions\n")

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


(parametrise ((check-test-name	'collate))

  (check	;sqlite3-create-collation
      (with-result
       (let ()
	 (define sql-snippet
	   "create table Stuff (alpha TEXT);
            insert into Stuff (alpha) values ('hello');
            insert into Stuff (alpha) values ('salut');
            insert into Stuff (alpha) values ('ciao');
            select * from Stuff order by alpha collate ThisWay;")

	 (define (collation.comparison S1 S2)
	   (cond ((string<? S1 S2)	-1)
		 ((string=? S1 S2)	0)
		 (else			+1)))

	 (define (collation.callback custom-data len1 ptr1 len2 ptr2)
	   (let ((S1 (cstring->string ptr1 len1))
		 (S2 (cstring->string ptr2 len2)))
	     (collation.comparison S1 S2)))

	 (define (exec-callback nrows texts names)
	   (add-result (utf8->string (vector-ref texts 0)))
	   #f)

	 (let ((collate-cb (make-sqlite3-collation-callback collation.callback))
	       (exec-cb    (make-sqlite3-exec-callback      exec-callback)))
	   (unwind-protect
	       (with-connection (conn)
		 (sqlite3-create-collation conn "ThisWay" SQLITE_UTF8
					   #f collate-cb)
		 (sqlite3-exec* conn sql-snippet exec-cb))
	     (ffi.free-c-callback collate-cb)
	     (ffi.free-c-callback exec-cb)))))
    => `(,SQLITE_OK ("ciao" "hello" "salut")))

;;; --------------------------------------------------------------------

  (check	;sqlite3-create-collation16
      (with-result
       (let ()
	 (define sql-snippet
	   "create table Stuff (alpha TEXT);
            insert into Stuff (alpha) values ('hello');
            insert into Stuff (alpha) values ('salut');
            insert into Stuff (alpha) values ('ciao');
            select * from Stuff order by alpha collate ThisWay;")

	 (define (collation.comparison S1 S2)
	   (cond ((string<? S1 S2)	-1)
		 ((string=? S1 S2)	0)
		 (else			+1)))

	 (define (collation.callback custom-data len1 ptr1 len2 ptr2)
	   (let ((S1 (cstring->string ptr1 len1))
		 (S2 (cstring->string ptr2 len2)))
	     (collation.comparison S1 S2)))

	 (define (exec-callback nrows texts names)
	   (add-result (utf8->string (vector-ref texts 0)))
	   #f)

	 (let ((collate-cb (make-sqlite3-collation-callback collation.callback))
	       (exec-cb    (make-sqlite3-exec-callback      exec-callback)))
	   (unwind-protect
	       (with-connection (conn)
		 (sqlite3-create-collation16 conn "ThisWay" SQLITE_UTF8
					     #f collate-cb)
		 (sqlite3-exec* conn sql-snippet exec-cb))
	     (ffi.free-c-callback collate-cb)
	     (ffi.free-c-callback exec-cb)))))
    => `(,SQLITE_OK ("ciao" "hello" "salut")))

;;; --------------------------------------------------------------------

  (check	;sqlite3-create-collation-v2, no data
      (with-result
       (let ()
	 (define sql-snippet
	   "create table Stuff (alpha TEXT);
            insert into Stuff (alpha) values ('hello');
            insert into Stuff (alpha) values ('salut');
            insert into Stuff (alpha) values ('ciao');
            select * from Stuff order by alpha collate ThisWay;")

	 (define (collation.comparison S1 S2)
	   (cond ((string<? S1 S2)	-1)
		 ((string=? S1 S2)	0)
		 (else			+1)))

	 (define (collation.callback custom-data len1 ptr1 len2 ptr2)
	   (let ((S1 (cstring->string ptr1 len1))
		 (S2 (cstring->string ptr2 len2)))
	     (collation.comparison S1 S2)))

	 (define (exec-callback nrows texts names)
	   (add-result (utf8->string (vector-ref texts 0)))
	   #f)

	 (let ((collate-cb (make-sqlite3-collation-callback collation.callback))
	       (exec-cb    (make-sqlite3-exec-callback      exec-callback)))
	   (unwind-protect
	       (with-connection (conn)
		 (sqlite3-create-collation-v2 conn "ThisWay" SQLITE_UTF8
					      #f collate-cb #f)
		 (sqlite3-exec* conn sql-snippet exec-cb))
	     (ffi.free-c-callback collate-cb)
	     (ffi.free-c-callback exec-cb)))))
    => `(,SQLITE_OK ("ciao" "hello" "salut")))

  (check	;sqlite3-create-collation-v2, with data
      (with-result
       (let ()
	 (define sql-snippet
	   "create table Stuff (alpha TEXT);
            insert into Stuff (alpha) values ('hello');
            insert into Stuff (alpha) values ('salut');
            insert into Stuff (alpha) values ('ciao');
            select * from Stuff order by alpha collate ThisWay;")

	 (define (collation.comparison S1 S2)
	   (cond ((string<? S1 S2)	-1)
		 ((string=? S1 S2)	0)
		 (else			+1)))

	 (define (collation.callback custom-data len1 ptr1 len2 ptr2)
	   (let ((CC (retrieve-to-avoid-collecting custom-data))
		 (S1 (cstring->string ptr1 len1))
		 (S2 (cstring->string ptr2 len2)))
	     (CC S1 S2)))

	 (define (exec-callback nrows texts names)
	   (add-result (utf8->string (vector-ref texts 0)))
	   #f)

	 (let ((collate-cb (make-sqlite3-collation-callback collation.callback))
	       (data       (register-to-avoid-collecting collation.comparison))
	       (destroy    (make-sqlite3-collation-destructor
			    forget-to-avoid-collecting))
	       (exec-cb    (make-sqlite3-exec-callback exec-callback)))
	   (unwind-protect
	       (with-connection (conn)
		 (sqlite3-create-collation-v2 conn "ThisWay" SQLITE_UTF8
					      data collate-cb destroy)
		 (sqlite3-exec* conn sql-snippet exec-cb))
	     (ffi.free-c-callback collate-cb)
	     (ffi.free-c-callback destroy)
	     (ffi.free-c-callback exec-cb)))))
    => `(,SQLITE_OK ("ciao" "hello" "salut")))

  (collect))


(parametrise ((check-test-name	'needed))

  (check	;sqlite3-collation-needed
      (with-result
       (let ()
	 (define sql-snippet
	   "create table Stuff (alpha TEXT);
            insert into Stuff (alpha) values ('hello');
            insert into Stuff (alpha) values ('salut');
            insert into Stuff (alpha) values ('ciao');
            select * from Stuff order by alpha collate ThisWay;")

	 (define (collation.comparison S1 S2)
	   (cond ((string<? S1 S2)	-1)
		 ((string=? S1 S2)	0)
		 (else			+1)))

	 (define (collation.callback custom-data len1 ptr1 len2 ptr2)
	   (let ((S1 (cstring->string ptr1 len1))
		 (S2 (cstring->string ptr2 len2)))
	     (collation.comparison S1 S2)))

	 (define collation-cb
	   (make-sqlite3-collation-callback collation.callback))

	 (define (needed-callback custom-data conn encoding name)
	   (cond ((string=? name "ThisWay")
		  (sqlite3-create-collation conn "ThisWay" encoding
					    #f collation-cb))))

	 (define (exec-callback nrows texts names)
	   (add-result (utf8->string (vector-ref texts 0)))
	   #f)

	 (let ((needed-cb (make-sqlite3-collation-needed-callback needed-callback))
	       (exec-cb   (make-sqlite3-exec-callback             exec-callback)))
	   (unwind-protect
	       (with-connection (conn)
		 (sqlite3-collation-needed conn #f needed-cb)
		 (sqlite3-exec* conn sql-snippet exec-cb))
	     (ffi.free-c-callback needed-cb)
	     (ffi.free-c-callback exec-cb)
	     (ffi.free-c-callback collation-cb)))))
    => `(,SQLITE_OK ("ciao" "hello" "salut")))

;;; --------------------------------------------------------------------

  (check	;sqlite3-collation-needed16
      (with-result
       (let ()
	 (define sql-snippet
	   "create table Stuff (alpha TEXT);
            insert into Stuff (alpha) values ('hello');
            insert into Stuff (alpha) values ('salut');
            insert into Stuff (alpha) values ('ciao');
            select * from Stuff order by alpha collate ThisWay;")

	 (define (collation.comparison S1 S2)
	   (cond ((string<? S1 S2)	-1)
		 ((string=? S1 S2)	0)
		 (else			+1)))

	 (define collation.callback
	   (make-sqlite3-collation-callback
	    (lambda (custom-data len1 ptr1 len2 ptr2)
	      (let ((S1 (cstring->string ptr1 len1))
		    (S2 (cstring->string ptr2 len2)))
		(collation.comparison S1 S2)))))

	 (define (needed-callback custom-data conn encoding name)
	   (cond ((string=? name "ThisWay")
		  (sqlite3-create-collation conn "ThisWay" encoding
					    #f collation.callback))))

	 (define (exec-callback nrows texts names)
	   (add-result (utf8->string (vector-ref texts 0)))
	   #f)

	 (let ((needed-cb (make-sqlite3-collation-needed16-callback needed-callback))
	       (exec-cb   (make-sqlite3-exec-callback               exec-callback)))
	   (unwind-protect
	       (with-connection (conn)
		 (sqlite3-collation-needed16 conn #f needed-cb)
		 (sqlite3-exec* conn sql-snippet exec-cb))
	     (ffi.free-c-callback needed-cb)
	     (ffi.free-c-callback exec-cb)))))
    => `(,SQLITE_OK ("ciao" "hello" "salut")))

  (collect))


;;;; done

(collect)
(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; End:
