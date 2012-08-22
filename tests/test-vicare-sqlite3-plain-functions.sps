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
  (vicare syntactic-extensions)
  (prefix (vicare ffi) ffi.)
  (prefix (vicare words) words.)
  (prefix (vicare glibc) glibc.)
  (vicare platform-constants)
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

  ;;Aggregate function "mysum" using aggregate context to store state.
  ;;
  ;;	sqlite3-create-function		make-sqlite3-aggregate-step
  ;;	make-sqlite3-aggregate-final	sqlite3-aggregate-context
  ;;
  (check
      (with-result
       (let ()
	 (define (mysum.data context)
	   (sqlite3-aggregate-context context words.SIZEOF_DOUBLE))

	 (define (mysum.accumulated-sum-ref data.ptr)
	   (pointer-ref-c-double data.ptr 0))

	 (define (mysum.accumulated-sum-set! data.ptr sum)
	   (pointer-set-c-double! data.ptr 0 sum))

	 (define (mysum.step context args)
	   (guard (E (else
		      (check-pretty-print E)
		      (void)))
	     (let* ((P (mysum.data context))
		    (S (mysum.accumulated-sum-ref P))
		    (A (vector-ref args 0))
		    (X (sqlite3-value-double A))
		    (S (+ X S)))
	       (mysum.accumulated-sum-set! P S))))

	 (define (mysum.final context)
	   (guard (E (else
		      (check-pretty-print E)
		      (void)))
	     (let* ((P (mysum.data context))
		    (S (mysum.accumulated-sum-ref P)))
	       (sqlite3-result-double context S))))

	 (define (exec-cb number-of-cols texts names)
	   (guard (E (else
		      (check-pretty-print E)
		      (void)))
	     (add-result (vector number-of-cols
				 (car (map utf8->string (vector->list names)))
				 (car (map string->number
					(map utf8->string (vector->list texts))))))
	     #f))

	 (define sql-snippet
	   "create table stuff (x TEXT);
            insert into stuff (x) values ('1.0');
            insert into stuff (x) values ('1.1');
            insert into stuff (x) values ('1.2');
            select mysum(x) as 'Sum' from stuff;")

	 (let ((mysum.step  (make-sqlite3-aggregate-step  mysum.step))
	       (mysum.final (make-sqlite3-aggregate-final mysum.final))
	       (exec-cb     (make-sqlite3-exec-callback   exec-cb)))
	   (unwind-protect
	       (with-connection (conn)
		 (let ((rv (sqlite3-create-function conn "mysum" 1 SQLITE_ANY #f
						    #f mysum.step mysum.final)))
		   (if (= rv SQLITE_OK)
		       (let-values
			   (((rv errmsg)
			     (sqlite3-exec conn sql-snippet exec-cb)))
			 (list rv errmsg))
		     (list rv (sqlite3-errmsg conn)))))
	     (ffi.free-c-callback mysum.step)
	     (ffi.free-c-callback mysum.final)
	     (ffi.free-c-callback exec-cb)))
	 ))
    => `((,SQLITE_OK #f) (#(1 "Sum" 3.3))))

;;; --------------------------------------------------------------------

  ;;Aggregate function "mymax".
  ;;
  ;;	sqlite3-create-function		make-sqlite3-aggregate-step
  ;;	make-sqlite3-aggregate-final	sqlite3-aggregate-context
  ;;
  (check
      (with-result
       (let ()
	 (define mymax.context-size
	   (+ words.SIZEOF_LONG words.SIZEOF_DOUBLE))

	 (define (mymax.context-pointer context)
	   (sqlite3-aggregate-context context mymax.context-size))

	 (define-inline (mymax.initialised! pointer)
	   (pointer-set-c-unsigned-long! pointer 0 1))

	 (define-inline (mymax.initialised? pointer)
	   (not (zero? (pointer-ref-c-unsigned-long pointer 0))))

	 (define-inline (mymax.max-set! pointer flonum)
	   (pointer-set-c-double! pointer words.SIZEOF_POINTER flonum))

	 (define-inline (mymax.max-ref pointer)
	   (pointer-ref-c-double  pointer words.SIZEOF_POINTER))

	 (define (mymax.step context args)
	   (guard (E (else
		      (check-pretty-print E)
		      (void)))
	     (let* ((P (mymax.context-pointer context))
		    (A (vector-ref args 0))
		    (X (sqlite3-value-double A)))
	       (if (mymax.initialised? P)
		   (let ((M (mymax.max-ref P)))
		     (mymax.max-set! P (max X M)))
		 (begin
		   (mymax.initialised! P)
		   (mymax.max-set! P X))))))

	 (define (mymax.final context)
	   (guard (E (else
		      (check-pretty-print E)
		      (void)))
	     (sqlite3-result-double context
				    (let ((P (mymax.context-pointer context)))
				      (if (mymax.initialised? P)
					  (mymax.max-ref P)
					-inf.0)))))

	 (define (exec-cb number-of-cols texts names)
	   (add-result
	    (vector number-of-cols
		    (car (map utf8->string (vector->list names)))
		    (car (map (lambda (T)
				(let ((S (utf8->string T)))
				  (cond ((string->number S)
					 => (lambda (x) x))
					((string=? S "+Inf")
					 +inf.0)
					((string=? S "-Inf")
					 -inf.0)
					(else +nan.0))))
			   (vector->list texts)))))
	   #f)

	 ;; (define sql-snippet
	 ;;   "create table stuff (x TEXT);
         ;;    select mymax(x) as 'Max' from stuff;")
	 (define sql-snippet
	   "create table stuff (x TEXT);
            insert into stuff (x) values ('1.0');
            insert into stuff (x) values ('3.0');
            insert into stuff (x) values ('2.0');
            select mymax(x) as 'Max' from stuff;")

	 (let ((mymax.step  (make-sqlite3-aggregate-step  mymax.step))
	       (mymax.final (make-sqlite3-aggregate-final mymax.final))
	       (exec-cb     (make-sqlite3-exec-callback   exec-cb)))
	   (unwind-protect
	       (with-connection (conn)
		 (let ((rv (sqlite3-create-function conn "mymax" 1 SQLITE_ANY #f
						    #f mymax.step mymax.final)))
		   (if (= rv SQLITE_OK)
		       (let-values
			   (((rv errmsg)
			     (sqlite3-exec conn sql-snippet exec-cb)))
			 (list rv errmsg))
		     (list rv (sqlite3-errmsg conn)))))
	     (ffi.free-c-callback mymax.step)
	     (ffi.free-c-callback mymax.final)
	     (ffi.free-c-callback exec-cb)))))
    => `((,SQLITE_OK #f) (#(1 "Max" 3.0))))

  #f)


(parametrise ((check-test-name	'values))

  (define-syntax bindings	(syntax-rules ()))
  (define-syntax results	(syntax-rules ()))

  (define-syntax with-statement
    (syntax-rules (bindings results)
      ((_ (?connection-var ?statement-var1 ?statement-var2)
	  (bindings . ?bind-body)
	  (results  . ?result-body))
       (begin
	 (sqlite3-exec ?connection-var "create table stuff (thing TEXT);")
	 (let*-values (((snippet) "insert into stuff (thing) values (?1);")
		       ((code ?statement-var1 end-offset)
			(sqlite3-prepare-v2 ?connection-var snippet)))
	   (if (= code SQLITE_OK)
	       (unwind-protect
		   (begin
		     (begin . ?bind-body)
		     (unless (= SQLITE_DONE (sqlite3-step ?statement-var1))
		       (error 'sqlite3-step (sqlite3-errmsg ?connection-var))))
		 (when (sqlite3-stmt?/valid ?statement-var1)
		   (sqlite3-finalize ?statement-var1)))
	     (error 'sqlite3-prepare-v2 (sqlite3-errmsg ?connection-var))))
	 (let*-values (((snippet) "select myfunc(thing) as 'Thing' from stuff;")
		       ((code ?statement-var2 end-offset)
			(sqlite3-prepare-v2 ?connection-var snippet)))
	   (if (= code SQLITE_OK)
	       (unwind-protect
		   (let () . ?result-body)
		 (when (sqlite3-stmt?/valid ?statement-var2)
		   (sqlite3-finalize ?statement-var2)))
	     (error 'sqlite3-prepare-v2 (sqlite3-errmsg ?connection-var))))
	 ))))

;;; --------------------------------------------------------------------
;;; double arguments and return value

  (check
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-double A)))
	      (sqlite3-result-double context X))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-double stmt-1 1 1.2))
		  (results
		   (let ((rv1	(sqlite3-step stmt-2))
			 (res	(sqlite3-column-double stmt-2 0)))
		     (let ((rv2	(sqlite3-step stmt-2)))
		       (list rv1 res rv2))))))
	    (ffi.free-c-callback func-cb))))
    => `(,SQLITE_ROW 1.2 ,SQLITE_DONE))

;;; --------------------------------------------------------------------
;;; int arguments and return value

  (check
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-int A)))
	      (sqlite3-result-int context X))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-int stmt-1 1 123))
		  (results
		   (let ((rv1	(sqlite3-step stmt-2))
			 (res	(sqlite3-column-int stmt-2 0)))
		     (let ((rv2	(sqlite3-step stmt-2)))
		       (list rv1 res rv2))))))
	    (ffi.free-c-callback func-cb))))
    => `(,SQLITE_ROW 123 ,SQLITE_DONE))

;;; --------------------------------------------------------------------
;;; int64 arguments and return value

  (check
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-int64 A)))
	      (sqlite3-result-int64 context X))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-int64 stmt-1 1 123))
		  (results
		   (let ((rv1	(sqlite3-step stmt-2))
			 (res	(sqlite3-column-int64 stmt-2 0)))
		     (let ((rv2	(sqlite3-step stmt-2)))
		       (list rv1 res rv2))))))
	    (ffi.free-c-callback func-cb))))
    => `(,SQLITE_ROW 123 ,SQLITE_DONE))

;;; --------------------------------------------------------------------
;;; int argument, NULL return value

  (check
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-int A)))
	      (sqlite3-result-null context))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-int stmt-1 1 123))
		  (results
		   (let ((rv1	(sqlite3-step stmt-2))
			 (res	(sqlite3-column-type stmt-2 0)))
		     (let ((rv2	(sqlite3-step stmt-2)))
		       (list rv1 res rv2))))))
	    (ffi.free-c-callback func-cb))))
    => `(,SQLITE_ROW ,SQLITE_NULL ,SQLITE_DONE))

;;; --------------------------------------------------------------------
;;; text arguments and return value

  (check
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-text/string A)))
	      (sqlite3-result-text context X 0 (string-length X) SQLITE_TRANSIENT))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-text stmt-1 1 "ciao" 0 4 SQLITE_TRANSIENT))
		  (results
		   (let ((rv1	(sqlite3-step stmt-2))
			 (res	(sqlite3-column-text/string stmt-2 0)))
		     (let ((rv2	(sqlite3-step stmt-2)))
		       (list rv1 res rv2))))))
	    (ffi.free-c-callback func-cb))))
    => `(,SQLITE_ROW "ciao" ,SQLITE_DONE))

;;; --------------------------------------------------------------------
;;; text16 arguments and return value

  (check
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-text16/string A))
		   (Y (string->utf16n X)))
	      (sqlite3-result-text16 context Y 0 (bytevector-length Y) SQLITE_TRANSIENT))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-text16 stmt-1 1 "ciao" 0 8 SQLITE_TRANSIENT))
		  (results
		   (let ((rv1	(sqlite3-step stmt-2))
			 (res	(sqlite3-column-text16/string stmt-2 0)))
		     (let ((rv2	(sqlite3-step stmt-2)))
		       (list rv1 res rv2))))))
	    (ffi.free-c-callback func-cb))))
    => `(,SQLITE_ROW "ciao" ,SQLITE_DONE))

;;; --------------------------------------------------------------------
;;; text16le arguments and return value

  (check
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-text16le/string A))
		   (Y (string->utf16le X)))
	      (sqlite3-result-text16le context Y 0 (bytevector-length Y) SQLITE_TRANSIENT))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-text16 stmt-1 1 "ciao" 0 8 SQLITE_TRANSIENT))
		  (results
		   (let ((rv1	(sqlite3-step stmt-2))
			 (res	(sqlite3-column-text16/string stmt-2 0)))
		     (let ((rv2	(sqlite3-step stmt-2)))
		       (list rv1 res rv2))))))
	    (ffi.free-c-callback func-cb))))
    => `(,SQLITE_ROW "ciao" ,SQLITE_DONE))

;;; --------------------------------------------------------------------
;;; text16be arguments and return value

  (check
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-text16be/string A))
		   (Y (string->utf16be X)))
	      (sqlite3-result-text16be context Y 0 (bytevector-length Y) SQLITE_TRANSIENT))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-text16 stmt-1 1 "ciao" 0 8 SQLITE_TRANSIENT))
		  (results
		   (let ((rv1	(sqlite3-step stmt-2))
			 (res	(sqlite3-column-text16/string stmt-2 0)))
		     (let ((rv2	(sqlite3-step stmt-2)))
		       (list rv1 res rv2))))))
	    (ffi.free-c-callback func-cb))))
    => `(,SQLITE_ROW "ciao" ,SQLITE_DONE))

;;; --------------------------------------------------------------------
;;; BLOB arguments and return value

  (check	;full bytevector
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-blob A)))
	      (sqlite3-result-blob context X 0 (bytevector-length X) SQLITE_TRANSIENT))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-blob stmt-1 1 '#vu8(1 2 3 4) 0 4 SQLITE_TRANSIENT))
		  (results
		   (let ((rv1	(sqlite3-step stmt-2))
			 (res	(sqlite3-column-blob stmt-2 0)))
		     (let ((rv2	(sqlite3-step stmt-2)))
		       (list rv1 res rv2))))))
	    (ffi.free-c-callback func-cb))))
    => `(,SQLITE_ROW #vu8(1 2 3 4) ,SQLITE_DONE))

  (check	;sub-bytevector
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-blob A)))
	      (sqlite3-result-blob context X 1
				   (- (bytevector-length X) 2)
				   SQLITE_TRANSIENT))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-blob stmt-1 1 '#vu8(1 2 3 4) 0 4 SQLITE_TRANSIENT))
		  (results
		   (let ((rv1	(sqlite3-step stmt-2))
			 (res	(sqlite3-column-blob stmt-2 0)))
		     (let ((rv2	(sqlite3-step stmt-2)))
		       (list rv1 res rv2))))))
	    (ffi.free-c-callback func-cb))))
    => `(,SQLITE_ROW #vu8(2 3) ,SQLITE_DONE))

;;; --------------------------------------------------------------------
;;; int argument, zero-BLOB return value

  (check
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-int A)))
	      (sqlite3-result-zeroblob context 4))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-int stmt-1 1 123))
		  (results
		   (let ((rv1	(sqlite3-step stmt-2))
			 (res	(sqlite3-column-blob stmt-2 0)))
		     (let ((rv2	(sqlite3-step stmt-2)))
		       (list rv1 res rv2))))))
	    (ffi.free-c-callback func-cb))))
    => `(,SQLITE_ROW #vu8(0 0 0 0) ,SQLITE_DONE))

;;; --------------------------------------------------------------------
;;; error too big

  (check
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-int A)))
	      (sqlite3-result-error-toobig context))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-int stmt-1 1 123))
		  (results
		   (sqlite3-step stmt-2))))
	    (ffi.free-c-callback func-cb))))
    => SQLITE_TOOBIG)

;;; --------------------------------------------------------------------
;;; error no memory

  (check
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-int A)))
	      (sqlite3-result-error-nomem context))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-int stmt-1 1 123))
		  (results
		   (sqlite3-step stmt-2))))
	    (ffi.free-c-callback func-cb))))
    => SQLITE_NOMEM)

;;; --------------------------------------------------------------------
;;; custom error

  (check	;sqlite3-result-error
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-int A)))
	      (sqlite3-result-error context "this is my message"))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-int stmt-1 1 123))
		  (results
		   (let ((rv (sqlite3-step stmt-2)))
		     (list rv (sqlite3-errmsg conn))))))
	    (ffi.free-c-callback func-cb))))
    => `(,SQLITE_ERROR "this is my message"))

  (check	;sqlite3-result-error16
      (with-connection (conn)
	(define (func context args)
	  (guard (E (else (check-pretty-print E) (void)))
	    (let* ((A (vector-ref args 0))
		   (X (sqlite3-value-int A)))
	      (sqlite3-result-error16 context "this is my message"))))
	(let ((func-cb (make-sqlite3-function func)))
	  (unwind-protect
	      (begin
		(unless
		    (= SQLITE_OK
		       (sqlite3-create-function conn "myfunc" 1 SQLITE_ANY #f func-cb #f #f))
		  (error 'sqlite3-create-function (sqlite3-errmsg conn)))
		(with-statement (conn stmt-1 stmt-2)
		  (bindings
		   (sqlite3-bind-int stmt-1 1 123))
		  (results
		   (let ((rv (sqlite3-step stmt-2)))
		     (list rv (sqlite3-errmsg conn))))))
	    (ffi.free-c-callback func-cb))))
    => `(,SQLITE_ERROR "this is my message"))

  #t)


(parametrise ((check-test-name	'custom-data))

  (check	;no data destructor
      (with-result
       (let ()
	 (define (mydata context args)
	   (guard (E (else
		      (check-pretty-print E)
		      (void)))
	     (let* ((D (sqlite3-user-data context))
		    (X (pointer-ref-c-signed-int D 0)))
	       (sqlite3-result-int context X))))

	 (define (exec-cb number-of-cols texts names)
	   (guard (E (else
		      (check-pretty-print E)
		      (void)))
	     (add-result (vector number-of-cols
				 (car (map utf8->string (vector->list names)))
				 (car (map utf8->string (vector->list texts))))))
	   #f)

	 (define sql-snippet
	   "create table stuff (x TEXT);
            insert into stuff (x) values ('123');
            select mydata(x) as 'Data' from stuff;")

	 (let ((mydata	(make-sqlite3-function      mydata))
	       (exec-cb	(make-sqlite3-exec-callback exec-cb)))
	   (unwind-protect
	       (with-connection (conn)
		 (let* ((D  (malloc words.SIZEOF_INT))
			(rv (sqlite3-create-function conn "mydata" 1 SQLITE_ANY D
						     mydata #f #f)))
		   (if (= rv SQLITE_OK)
		       (begin
			 (pointer-set-c-signed-int! D 0 123)
			 (let-values
			     (((rv errmsg)
			       (sqlite3-exec conn sql-snippet exec-cb)))
			   (list rv errmsg)))
		     (list rv (sqlite3-errmsg conn)))))
	     (ffi.free-c-callback mydata)
	     (ffi.free-c-callback exec-cb)))
	 ))
    => `((,SQLITE_OK #f) (#(1 "Data" "123"))))

;;; --------------------------------------------------------------------

  (check	;with data destructor
      (with-result
       (let ()
	 (define (mydata context args)
	   (guard (E (else
		      (check-pretty-print E)
		      (void)))
	     (let* ((D (sqlite3-user-data context))
		    (X (pointer-ref-c-signed-int D 0)))
	       (sqlite3-result-int context X))))

	 (define (mydata.destructor data)
	   (and (pointer? data)
		(not (pointer-null? data))
		(free data)))

	 (define (exec-cb number-of-cols texts names)
	   (guard (E (else
		      (check-pretty-print E)
		      (void)))
	     (add-result (vector number-of-cols
				 (car (map utf8->string (vector->list names)))
				 (car (map utf8->string (vector->list texts))))))
	   #f)

	 (define sql-snippet
	   "create table stuff (x TEXT);
            insert into stuff (x) values ('123');
            select mydata(x) as 'Data' from stuff;")

	 (let ((mydata	(make-sqlite3-function mydata))
	       (destroy	(make-sqlite3-function-destructor mydata.destructor))
	       (exec-cb	(make-sqlite3-exec-callback exec-cb)))
	   (unwind-protect
	       (with-connection (conn)
		 (let* ((D  (malloc words.SIZEOF_INT))
			(rv (sqlite3-create-function-v2 conn "mydata" 1 SQLITE_ANY D
							mydata #f #f destroy)))
		   (if (= rv SQLITE_OK)
		       (begin
			 (pointer-set-c-signed-int! D 0 123)
			 (let-values
			     (((rv errmsg)
			       (sqlite3-exec conn sql-snippet exec-cb)))
			   (list rv errmsg)))
		     (list rv (sqlite3-errmsg conn)))))
	     (begin
	       (ffi.free-c-callback mydata)
	       (ffi.free-c-callback destroy)
	       (ffi.free-c-callback exec-cb))))
	 ))
    => `((,SQLITE_OK #f) (#(1 "Data" "123"))))

;;;(check-pretty-print "run garbage collector")
  (collect)
  #t)


(parametrise ((check-test-name	'aux-data))

  ;;In this example aux data usage  works because the regexp is known by
  ;;SQLite at SQL compilation time.
  (check
      (with-result
       (let ()
	 (define (matching context args)
	   (define (%compile-rex context rex)
	     (let ((cre (glibc.regcomp rex REG_EXTENDED)))
	       (sqlite3-set-auxdata context 0 cre
				    (make-sqlite3-auxdata-destructor %rex-destructor))
	       cre))
	   (define (%rex-destructor rex)
	     (glibc.regfree rex))
	   (guard (E (else
		      (check-pretty-print E)
		      (void)))
	     ;; (check-pretty-print (list (sqlite3-get-auxdata context 0)
	     ;; 			       (sqlite3-value-text/string (vector-ref args 0))))
	     (let* ((rex (sqlite3-value-text/string (vector-ref args 0)))
		    (str (sqlite3-value-text/string (vector-ref args 1)))
		    (cre (or (sqlite3-get-auxdata context 0)
			     (%compile-rex context rex))))
	       (sqlite3-result-int context (if (glibc.regexec cre str 0)
					       1
					     0)))))

	 (define (exec-cb number-of-cols texts names)
	   (guard (E (else
		      (check-pretty-print E)
		      (void)))
	     (add-result (vector number-of-cols
				 (utf8->string (vector-ref names 0))
				 (utf8->string (vector-ref texts 0))))
	     #f))

	 (define sql-snippet
	   "create table Strings (x TEXT);
            insert into Strings (x) values ('ciao');
            insert into Strings (x) values ('123');
            insert into Strings (x) values ('hello');
            insert into Strings (x) values ('456');
            select x as 'Match' from Strings where matching('[a-z]+', x) = 1;")

	 (let ((matching (make-sqlite3-function      matching))
	       (exec-cb  (make-sqlite3-exec-callback exec-cb)))
	   (unwind-protect
	       (with-connection (conn)
		 (let ((rv (sqlite3-create-function conn "matching" 2 SQLITE_ANY #f
						    matching #f #f)))
		   (if (= rv SQLITE_OK)
		       (let-values
			   (((rv errmsg)
			     (sqlite3-exec conn sql-snippet exec-cb)))
			 (list rv errmsg))
		     (list rv (sqlite3-errmsg conn)))))
	     (ffi.free-c-callback matching)
	     (ffi.free-c-callback exec-cb)))
	 ))
    => `((,SQLITE_OK #f)
	 (#(1 "Match" "ciao")
	  #(1 "Match" "hello"))))

;;; --------------------------------------------------------------------

  ;;In this example  aux data usage does NOT work  because the regexp is
  ;;the result of a query.
  (check
      (with-result
       (let ()
	 (define (matching context args)
	   (define (%compile-rex context rex)
	     (let ((cre (glibc.regcomp rex REG_EXTENDED)))
	       (sqlite3-set-auxdata context 0 cre
				    (make-sqlite3-auxdata-destructor %rex-destructor))
	       cre))
	   (define (%rex-destructor rex)
	     (glibc.regfree rex))
	   (guard (E (else
		      (check-pretty-print E)
		      (void)))
	     ;; (check-pretty-print (list (sqlite3-get-auxdata context 0)
	     ;; 			  (sqlite3-value-text/string (vector-ref args 0))))
	     (let* ((rex (sqlite3-value-text/string (vector-ref args 0)))
		    (str (sqlite3-value-text/string (vector-ref args 1)))
		    (cre (or (sqlite3-get-auxdata context 0)
			     (%compile-rex context rex))))
	       (sqlite3-result-int context (if (glibc.regexec cre str 0)
					       1
					     0)))))

	 (define (exec-cb number-of-cols texts names)
	   (guard (E (else
		      (check-pretty-print E)
		      (void)))
	     (add-result (vector number-of-cols
				 (utf8->string (vector-ref names 0))
				 (utf8->string (vector-ref texts 0))))
	     #f))

	 (define sql-snippet
	   "create table Rexes (x TEXT);
            insert into Rexes (x) values ('[a-z]+');
            insert into Rexes (x) values ('[0-9]+');
            create table Strings (y TEXT);
            insert into Strings (y) values ('ciao');
            insert into Strings (y) values ('123');
            insert into Strings (y) values ('!!!');
            insert into Strings (y) values ('hello');
            insert into Strings (y) values (',,,');
            insert into Strings (y) values ('456');
            select y as 'Match' from Strings
              inner join Rexes on matching(Rexes.x, Strings.y) = 1;")

	 (let ((matching (make-sqlite3-function      matching))
	       (exec-cb  (make-sqlite3-exec-callback exec-cb)))
	   (unwind-protect
	       (with-connection (conn)
		 (let ((rv (sqlite3-create-function conn "matching" 2 SQLITE_ANY #f
						    matching #f #f)))
		   (if (= rv SQLITE_OK)
		       (let-values
			   (((rv errmsg)
			     (sqlite3-exec conn sql-snippet exec-cb)))
			 (list rv errmsg))
		     (list rv (sqlite3-errmsg conn)))))
	     (ffi.free-c-callback matching)
	     (ffi.free-c-callback exec-cb)))
	 ))
    => `((,SQLITE_OK #f)
	 (#(1 "Match" "ciao")
	  #(1 "Match" "123")
	  #(1 "Match" "hello")
	  #(1 "Match" "456"))))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; End:
