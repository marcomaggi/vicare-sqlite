;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: tests for SQLite custom SQL functions
;;;Date: Wed Aug 15, 2012
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
(check-display "*** testing Vicare SQLite bindings, custom SQL functions\n")

#;(set-port-buffer-mode! (current-error-port) (buffer-mode line))
#;(set-port-buffer-mode! (current-output-port) (buffer-mode none))


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
	       (let ((rv (let () . ?body)))
;;;		 (check-pretty-print (sqlite3-errmsg conn))
		 rv)
	     (when (sqlite3-stmt?/valid ?statement-var)
	       (sqlite3-finalize ?statement-var)))))))))


(parametrise ((check-test-name	'function-creation))

  (check ;sqlite3-create-function, sqlite3-value-double, sqlite3-result-double
      (with-result
       (with-connection (conn)
	 (define func-cb
	   (make-sqlite3-function
	    (lambda (context args)
	      (guard (E (else
			 (check-pretty-print E)
			 (void)))
		(let* ((x1 (vector-ref args 0))
		       (x2 (sqlite3-value-double x1))
		       (y  (sin x2)))
;;;		  (check-pretty-print x1)
;;;		  (check-pretty-print x2)
;;;		  (check-pretty-print y1)
		  (sqlite3-result-double context y))))))

	 (define exec-cb
	   (make-sqlite3-exec-callback
	    (lambda (number-of-cols texts names)
	      (add-result
	       (vector number-of-cols
		       (car (map utf8->string (vector->list names)))
		       (real? (car (map string->number
				     (map utf8->string (vector->list texts)))))))
	      #f)))

	 (define sql-snippet
	   "create table stuff (x TEXT);
            insert into stuff (x) values ('1.0');
            insert into stuff (x) values ('1.1');
            insert into stuff (x) values ('1.2');
            select mysine(x) from stuff;")

	 (unwind-protect
	     (let ((rv (sqlite3-create-function conn "mysine" 1 SQLITE_ANY #f func-cb #f #f)))
	       (if (= rv SQLITE_OK)
		   (let-values
		       (((rv errmsg)
			 (sqlite3-exec conn sql-snippet exec-cb)))
		     (list rv errmsg))
		 (list rv (sqlite3-errmsg conn))))
	   (ffi.free-c-callback func-cb)
	   (ffi.free-c-callback exec-cb))))
    => `((,SQLITE_OK #f)
	 (#(1 "mysine(x)" #t)
	  #(1 "mysine(x)" #t)
	  #(1 "mysine(x)" #t))))

;;; --------------------------------------------------------------------

  (check ;sqlite3-create-function16, sqlite3-value-double, sqlite3-result-double
      (with-result
       (with-connection (conn)
	 (define func-cb
	   (make-sqlite3-function
	    (lambda (context args)
	      (guard (E (else
			 (check-pretty-print E)
			 (void)))
		(let* ((x (sqlite3-value-double (vector-ref args 0)))
		       (y (sin x)))
		  (sqlite3-result-double context y))))))

	 (define exec-cb
	   (make-sqlite3-exec-callback
	    (lambda (number-of-cols texts names)
	      (add-result
	       (vector number-of-cols
		       (car (map utf8->string (vector->list names)))
		       (real? (car (map string->number
				     (map utf8->string (vector->list texts)))))))
	      #f)))

	 (define sql-snippet
	   "create table stuff (x TEXT);
            insert into stuff (x) values ('1.0');
            insert into stuff (x) values ('1.1');
            insert into stuff (x) values ('1.2');
            select mysine(x) from stuff;")

	 (unwind-protect
	     (let ((rv (sqlite3-create-function16 conn "mysine" 1 SQLITE_ANY #f func-cb #f #f)))
	       (if (= rv SQLITE_OK)
		   (let-values
		       (((rv errmsg)
			 (sqlite3-exec conn sql-snippet exec-cb)))
		     (list rv errmsg))
		 (list rv (sqlite3-errmsg conn))))
	   (ffi.free-c-callback func-cb)
	   (ffi.free-c-callback exec-cb))))
    => `((,SQLITE_OK #f)
	 (#(1 "mysine(x)" #t)
	  #(1 "mysine(x)" #t)
	  #(1 "mysine(x)" #t))))

;;; --------------------------------------------------------------------

  (check ;sqlite3-create-function-v2, sqlite3-value-double, sqlite3-result-double
      (with-result
       (with-connection (conn)
	 (define func-cb
	   (make-sqlite3-function
	    (lambda (context args)
	      (guard (E (else
			 (check-pretty-print E)
			 (void)))
		(let* ((x (sqlite3-value-double (vector-ref args 0)))
		       (y (sin x)))
		  (sqlite3-result-double context y))))))

	 (define exec-cb
	   (make-sqlite3-exec-callback
	    (lambda (number-of-cols texts names)
	      (add-result
	       (vector number-of-cols
		       (car (map utf8->string (vector->list names)))
		       (real? (car (map string->number
				     (map utf8->string (vector->list texts)))))))
	      #f)))

	 (define sql-snippet
	   "create table stuff (x TEXT);
            insert into stuff (x) values ('1.0');
            insert into stuff (x) values ('1.1');
            insert into stuff (x) values ('1.2');
            select mysine(x) from stuff;")

	 (unwind-protect
	     (let ((rv (sqlite3-create-function-v2 conn "mysine" 1 SQLITE_ANY
						   #f func-cb #f #f #f)))
	       (if (= rv SQLITE_OK)
		   (let-values
		       (((rv errmsg)
			 (sqlite3-exec conn sql-snippet exec-cb)))
		     (list rv errmsg))
		 (list rv (sqlite3-errmsg conn))))
	   (ffi.free-c-callback func-cb)
	   (ffi.free-c-callback exec-cb))))
    => `((,SQLITE_OK #f)
	 (#(1 "mysine(x)" #t)
	  #(1 "mysine(x)" #t)
	  #(1 "mysine(x)" #t))))

;;; --------------------------------------------------------------------

  (check	;sqlite3-delete-function
      (with-connection (conn)
	(define func-cb
	  (make-sqlite3-function
	   (lambda (context args)
	     (guard (E (else
			(check-pretty-print E)
			(void)))
	       (let* ((x (sqlite3-value-double (vector-ref args 0)))
		      (y (sin x)))
		 (sqlite3-result-double context y))))))

	(define exec-cb
	  (make-sqlite3-exec-callback
	   (lambda (number-of-cols texts names)
	     (add-result
	      (vector number-of-cols
		      (car (map utf8->string (vector->list names)))
		      (real? (car (map string->number
				    (map utf8->string (vector->list texts)))))))
	     #f)))

	(define sql-snippet
	  "create table stuff (x TEXT);
            insert into stuff (x) values ('1.0');
            insert into stuff (x) values ('1.1');
            insert into stuff (x) values ('1.2');
            select mysine(x) from stuff;")

	(unwind-protect
	    (let ((rv (sqlite3-create-function conn "mysine" 1 SQLITE_ANY #f func-cb #f #f)))
	      (if (= rv SQLITE_OK)
		  (let-values
		      (((rv errmsg)
			(sqlite3-exec conn sql-snippet exec-cb)))
		    (list rv (sqlite3-delete-function conn "mysine")))
		(list rv (sqlite3-errmsg conn))))
	  (ffi.free-c-callback func-cb)
	  (ffi.free-c-callback exec-cb)))
    => `(,SQLITE_OK ,SQLITE_OK))

;;; --------------------------------------------------------------------

  (check	;sqlite3-delete-function16
      (with-connection (conn)
	(define func-cb
	  (make-sqlite3-function
	   (lambda (context args)
	     (guard (E (else
			(check-pretty-print E)
			(void)))
	       (let* ((x (sqlite3-value-double (vector-ref args 0)))
		      (y (sin x)))
		 (sqlite3-result-double context y))))))

	(define exec-cb
	  (make-sqlite3-exec-callback
	   (lambda (number-of-cols texts names)
	     (add-result
	      (vector number-of-cols
		      (car (map utf8->string (vector->list names)))
		      (real? (car (map string->number
				    (map utf8->string (vector->list texts)))))))
	     #f)))

	(define sql-snippet
	  "create table stuff (x TEXT);
            insert into stuff (x) values ('1.0');
            insert into stuff (x) values ('1.1');
            insert into stuff (x) values ('1.2');
            select mysine(x) from stuff;")

	(unwind-protect
	    (let ((rv (sqlite3-create-function16 conn "mysine" 1 SQLITE_ANY #f func-cb #f #f)))
	      (if (= rv SQLITE_OK)
		  (let-values
		      (((rv errmsg)
			(sqlite3-exec conn sql-snippet exec-cb)))
		    (list rv (sqlite3-delete-function16 conn "mysine")))
		(list rv (sqlite3-errmsg conn))))
	  (ffi.free-c-callback func-cb)
	  (ffi.free-c-callback exec-cb)))
    => `(,SQLITE_OK ,SQLITE_OK))

  #t)


(parametrise ((check-test-name	'aggregate-creation))

  (check ;sqlite3-create-function, sqlite3-value-double, sqlite3-result-double
      (with-result
       (with-connection (conn)
	 (define state
	   (make-eqv-hashtable))

	 (define step-cb
	   (make-sqlite3-aggregate-step
	    (lambda (context args)
	      (guard (E (else
			 (check-pretty-print E)
			 (void)))
		(let* ((data.len 8)
		       (data.ptr (sqlite3-aggregate-context context data.len))
		       (S        (pointer-ref-c-double data.ptr 0))
		       (x1       (vector-ref args 0))
		       (x2        (sqlite3-value-double x1)))
		  (pointer-set-c-double! data.ptr 0 (if S (+ x2 S) x2))
;;;		  (check-pretty-print (list 'step x S (pointer-ref-c-double data.ptr 0)))
		  )))))

	 (define final-cb
	   (make-sqlite3-aggregate-final
	    (lambda (context)
	      (guard (E (else
			 (check-pretty-print E)
			 (void)))
		(let* ((data.len 8)
		       (data.ptr (sqlite3-aggregate-context context data.len))
		       (S        (pointer-ref-c-double data.ptr 0)))
		  (if (rational? S)
		      (sqlite3-result-double context S)
		    (begin
		      (sqlite3-result-error-code context SQLITE_ERROR)
		      (sqlite3-result-error      context
						 "mysum: unable to produce result"))))))))

	 (define exec-cb
	   (make-sqlite3-exec-callback
	    (lambda (number-of-cols texts names)
	      (add-result
	       (vector number-of-cols
		       (car (map utf8->string (vector->list names)))
		       (car (map string->number
			      (map utf8->string (vector->list texts))))
		       #;(real? (car (map string->number
				     (map utf8->string (vector->list texts)))))
		       ))
	      #f)))

	 (define sql-snippet
	   "create table stuff (x TEXT);
            insert into stuff (x) values ('1.0');
            insert into stuff (x) values ('1.1');
            insert into stuff (x) values ('1.2');
            select mysum(x) as 'Sum' from stuff;")

	 (unwind-protect
	     (let ((rv (sqlite3-create-function conn "mysum" 1 SQLITE_ANY #f
						#f step-cb final-cb)))
	       (if (= rv SQLITE_OK)
		   (let-values
		       (((rv errmsg)
			 (sqlite3-exec conn sql-snippet exec-cb)))
		     (list rv errmsg))
		 (list rv (sqlite3-errmsg conn))))
	   (ffi.free-c-callback step-cb)
	   (ffi.free-c-callback final-cb)
	   (ffi.free-c-callback exec-cb))))
    => `((,SQLITE_OK #f) (#(1 "Sum" 3.3))))

  #f)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; End:
