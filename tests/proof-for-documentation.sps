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
  (prefix (vicare ffi) ffi.)
  (prefix (vicare words) words.)
  (prefix (vicare glibc) glibc.)
  (vicare platform-constants))

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

(when #f
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


;;;; BLOB API for incremental input/output

#!vicare
(when #f
  (let ()

    (define-syntax with-connection
      (syntax-rules ()
	((_ (?connect-var) . ?body)
	 (let ((?connect-var (sqlite3-open ":memory:")))
	   (unwind-protect
	       (let () . ?body)
	     (when (sqlite3? ?connect-var)
	       (sqlite3-close ?connect-var)))))))

    (with-connection (conn)
      (sqlite3-exec conn "create table stuff \
         (id INTEGER PRIMARY KEY, data TEXT);")
      (let-values (((code stmt end-offset1)
		    (sqlite3-prepare-v2 conn
					"insert into stuff (id, data) \
                                           values (?1, ?2);")))
	(unwind-protect
	    (begin
	      (sqlite3-bind-int64    stmt 1 1)
	      (sqlite3-bind-zeroblob stmt 2 4)
	      (sqlite3-step stmt))
	  (when (sqlite3-stmt?/valid stmt)
	    (sqlite3-finalize stmt))))
      (let-values (((rv blob)
		    (sqlite3-blob-open conn "main" "stuff" "data" 1 #t)))
	(unwind-protect
	    (begin
	      (sqlite3-blob-write blob 0 '#ve(ascii "ciao") 0 4)
	      (let ((buffer (make-bytevector 4)))
		(sqlite3-blob-read blob 0 buffer 0 4)
		(printf "~a\n" (utf8->string buffer))))
	  (sqlite3-blob-close blob))))
    ))


;;;; custom SQL functions: implementation of the SIN function

(when #f
  (let ()
    (define-syntax with-connection
      (syntax-rules ()
	((_ (?connect-var) . ?body)
	 (let ((?connect-var (sqlite3-open ":memory:")))
	   (unwind-protect
	       (let () . ?body)
	     (when (sqlite3? ?connect-var)
	       (sqlite3-close ?connect-var)))))))

    (define (mysine-cb context args)
      (define (%error message)
	(sqlite3-result-error-code context SQLITE_ERROR)
	(sqlite3-result-error context message))
      (guard (E (else
		 (%error (if (message-condition? E)
			     (condition-message E)
			   "internal error while executing mysine function"))))
	(let* ((x (vector-ref args 0))
	       (T (sqlite3-value-numeric-type x)))
	  (if (or (= T SQLITE_INTEGER)
		  (= T SQLITE_FLOAT))
	      (let* ((x (sqlite3-value-double x))
		     (y (sin x)))
		(sqlite3-result-double context y))
	    (%error "expected numeric argument for mysine function")))))

    (define (exec-cb number-of-cols texts names)
      (let ((names (map utf8->string (vector->list names)))
	    (texts (map string->number
		     (map utf8->string
		       (vector->list texts)))))
	(printf "~a: ~a\n" (car names) (car texts))
	#f))

    (define sql-snippet-1
      "create table stuff (x TEXT);
       insert into stuff (x) values ('1.0');
       insert into stuff (x) values ('1.1');
       insert into stuff (x) values ('1.2');
       select mysine(x) as 'Sine' from stuff;")

    (define sql-snippet-2
      "create table stuff2 (x TEXT);
       insert into stuff2 (x) values ('ciao');
       select mysine(x) as 'Sine' from stuff2;")

    (define (exec-snippet conn cb snippet)
      (let-values
	  (((rv errmsg)
	    (sqlite3-exec conn snippet cb)))
	(unless (= rv SQLITE_OK)
	  (printf "~a: ~a\n"
		  (sqlite3-error-code->symbol rv)
		  errmsg))))

    (let ((mysine-cb (make-sqlite3-function      mysine-cb))
	  (exec-cb   (make-sqlite3-exec-callback exec-cb)))
      (unwind-protect
	  (with-connection (conn)
	    (sqlite3-create-function conn "mysine" 1 SQLITE_ANY #f
				     mysine-cb #f #f)
	    (exec-snippet conn exec-cb sql-snippet-1)
	    (exec-snippet conn exec-cb sql-snippet-2))
	(ffi.free-c-callback mysine-cb)
	(ffi.free-c-callback exec-cb)))

    #f))


;;;; custom SQL functions: implementation of the SUM function

(when #f
  (let ()
    (define-syntax with-connection
      (syntax-rules ()
	((_ (?connect-var) . ?body)
	 (let ((?connect-var (sqlite3-open ":memory:")))
	   (unwind-protect
	       (let () . ?body)
	     (when (sqlite3? ?connect-var)
	       (sqlite3-close ?connect-var)))))))

    (define (mysum.data context)
      (sqlite3-aggregate-context context words.SIZEOF_DOUBLE))

    (define (mysum.accumulated-sum-ref data.ptr)
      (pointer-ref-c-double data.ptr 0))

    (define (mysum.accumulated-sum-set! data.ptr sum)
      (pointer-set-c-double! data.ptr 0 sum))

    (define (mysum.step context args)
      (define (%error message)
	(sqlite3-result-error-code context SQLITE_ERROR)
	(sqlite3-result-error context message))
      (guard (E (else
		 (%error (if (message-condition? E)
			     (condition-message E)
			   "internal error while executing \
                            mysum function"))))
	(let* ((P (mysum.data context))
	       (x (vector-ref args 0))
	       (T (sqlite3-value-numeric-type x)))
	  (if (or (= T SQLITE_INTEGER)
		  (= T SQLITE_FLOAT))
	      (let* ((S (mysum.accumulated-sum-ref P))
		     (A (vector-ref args 0))
		     (X (sqlite3-value-double A))
		     (S (+ X S)))
		(mysum.accumulated-sum-set! P S))
	    (mysum.accumulated-sum-set! P +nan.0)))))

    (define (mysum.final context)
      (define (%error message)
	(sqlite3-result-error-code context SQLITE_ERROR)
	(sqlite3-result-error context message))
      (guard (E (else
		 (%error (if (message-condition? E)
			     (condition-message E)
			   "internal error while executing \
                            mysum function"))))
	(let* ((P (mysum.data context))
	       (S (mysum.accumulated-sum-ref P)))
	  (if (nan? S)
	      (%error "expected numeric argument for \
                       mysum function")
	    (sqlite3-result-double context S)))))

    (define (exec-cb number-of-cols texts names)
      (printf "~a: ~a\n"
	      (utf8->string (vector-ref names 0))
	      (string->number (utf8->string (vector-ref texts 0))))
      #f)

    (define (exec-snippet conn cb snippet)
      (let-values
	  (((rv errmsg)
	    (sqlite3-exec conn snippet cb)))
	(unless (= rv SQLITE_OK)
	  (printf "~a: ~a\n"
		  (sqlite3-error-code->symbol rv)
		  errmsg))))

    (define sql-snippet-1
      "create table stuff (x TEXT);
       insert into stuff (x) values ('1.0');
       insert into stuff (x) values ('1.1');
       insert into stuff (x) values ('1.2');
       select mysum(x) as 'Sum' from stuff;")

    (define sql-snippet-2
      "create table stuff2 (x TEXT);
       insert into stuff2 (x) values ('ciao');
       select mysum(x) as 'Sum' from stuff2;")

    (let ((mysum.step  (make-sqlite3-aggregate-step  mysum.step))
	  (mysum.final (make-sqlite3-aggregate-final mysum.final))
	  (exec-cb     (make-sqlite3-exec-callback   exec-cb)))
      (unwind-protect
	  (with-connection (conn)
	    (sqlite3-create-function conn "mysum" 1 SQLITE_ANY #f
				     #f mysum.step mysum.final)
	    (exec-snippet conn exec-cb sql-snippet-1)
	    (exec-snippet conn exec-cb sql-snippet-2))
	(ffi.free-c-callback mysum.step)
	(ffi.free-c-callback mysum.final)
	(ffi.free-c-callback exec-cb)))
    #f))


;;;; custom SQL functions: implementation of the MAX function

(when #f
  (let ()
    (define-syntax with-connection
      (syntax-rules ()
	((_ (?connect-var) . ?body)
	 (let ((?connect-var (sqlite3-open ":memory:")))
	   (unwind-protect
	       (let () . ?body)
	     (when (sqlite3? ?connect-var)
	       (sqlite3-close ?connect-var)))))))

    (define mymax.context-size
      (+ words.SIZEOF_LONG words.SIZEOF_DOUBLE))

    (define (mymax.context-pointer context)
      (sqlite3-aggregate-context context mymax.context-size))

    (define-inline (mymax.initialised! pointer)
      (pointer-set-c-unsigned-long! pointer 0 1))

    (define-inline (mymax.initialised? pointer)
      (not (zero? (pointer-ref-c-unsigned-long pointer 0))))

    (define-inline (mymax.max-set! pointer flonum)
      (pointer-set-c-double! pointer words.SIZEOF_LONG flonum))

    (define-inline (mymax.max-ref pointer)
      (pointer-ref-c-double  pointer words.SIZEOF_LONG))

    (define (mymax.step context args)
      (define (%error message)
	(sqlite3-result-error-code context SQLITE_ERROR)
	(sqlite3-result-error context message))
      (guard (E (else
		 (%error (if (message-condition? E)
			     (condition-message E)
			   "internal error while executing \
                            mysum function"))))
	(let* ((P (mymax.context-pointer context))
	       (A (vector-ref args 0))
	       (T (sqlite3-value-numeric-type A)))
	  (if (or (= T SQLITE_INTEGER)
		  (= T SQLITE_FLOAT))
	      (let ((X (sqlite3-value-double A))
		    (M (mymax.max-ref P)))
		(if (mymax.initialised? P)
		    (unless (nan? M)
		      (mymax.max-set! P (max X M)))
		  (begin
		    (mymax.initialised! P)
		    (mymax.max-set! P X))))
	    (begin
	      (unless (mymax.initialised? P)
		(mymax.initialised! P))
	      (mymax.max-set! P +nan.0))))))

    (define (mymax.final context)
      (define (%error message)
	(sqlite3-result-error-code context SQLITE_ERROR)
	(sqlite3-result-error context message))
      (guard (E (else
		 (%error (if (message-condition? E)
			     (condition-message E)
			   "internal error while executing \
                            mysum function"))))
	(let ((P (mymax.context-pointer context)))
	  (if (mymax.initialised? P)
	      (let ((M (mymax.max-ref P)))
		(if (nan? M)
		    (%error "expected numeric argument for \
                             mymax function")
		  (sqlite3-result-double context M)))
	    -inf.0))))

    (define (exec-cb number-of-cols texts names)
      (printf "~a: ~a\n"
	      (utf8->string (vector-ref names 0))
	      (let ((S (utf8->string (vector-ref texts 0))))
		(cond ((string->number S)
		       => (lambda (x) x))
		      ((string=? S "+Inf")
		       +inf.0)
		      ((string=? S "-Inf")
		       -inf.0)
		      (else +nan.0))))
      #f)

    (define (exec-snippet conn cb snippet)
      (let-values
	  (((rv errmsg)
	    (sqlite3-exec conn snippet cb)))
	(unless (= rv SQLITE_OK)
	  (printf "~a: ~a\n"
		  (sqlite3-error-code->symbol rv)
		  errmsg))))

    (define sql-snippet-1
      "create table stuff (x TEXT);
       insert into stuff (x) values ('1.0');
       insert into stuff (x) values ('3.0');
       insert into stuff (x) values ('2.0');
       select mymax(x) as 'Max' from stuff;")

    (define sql-snippet-2
      "create table stuff2 (x TEXT);
       insert into stuff2 (x) values ('ciao');
       select mymax(x) as 'Max' from stuff2;")

    (let ((mymax.step  (make-sqlite3-aggregate-step  mymax.step))
	  (mymax.final (make-sqlite3-aggregate-final mymax.final))
	  (exec-cb     (make-sqlite3-exec-callback   exec-cb)))
      (unwind-protect
	  (with-connection (conn)
	    (sqlite3-create-function conn "mymax" 1 SQLITE_ANY #f
				     #f mymax.step mymax.final)
	    (exec-snippet conn exec-cb sql-snippet-1)
	    (exec-snippet conn exec-cb sql-snippet-2))
	(ffi.free-c-callback mymax.step)
	(ffi.free-c-callback mymax.final)
	(ffi.free-c-callback exec-cb)))
    #f))


;;;; custom SQL functions: matching regular expressions

;;In this  example aux data usage  works because the regexp  is known by
;;SQLite at SQL compilation time.
(when #t
  (let ()
    (define-syntax with-connection
      (syntax-rules ()
	((_ (?connect-var) . ?body)
	 (let ((?connect-var (sqlite3-open ":memory:")))
	   (unwind-protect
	       (let () . ?body)
	     (when (sqlite3? ?connect-var)
	       (sqlite3-close ?connect-var)))))))

    (define (regexp context args)
      (define (%compile-rex context rex)
	(let ((cre (glibc.regcomp rex REG_EXTENDED)))
	  (sqlite3-set-auxdata context 0 cre
			       (make-sqlite3-auxdata-destructor %rex-destructor))
	  cre))
      (define (%rex-destructor rex)
	(glibc.regfree rex))
      (let* ((rex (sqlite3-value-text/string (vector-ref args 0)))
	     (str (sqlite3-value-text/string (vector-ref args 1)))
	     (cre (or (sqlite3-get-auxdata context 0)
		      (%compile-rex context rex))))
	(sqlite3-result-int context
			    (if (glibc.regexec cre str 0)
				1
			      0))))

    (define (exec-cb number-of-cols texts names)
      (printf "~a: ~a\n"
	      (utf8->string (vector-ref names 0))
	      (utf8->string (vector-ref texts 0)))
      #f)

    (define sql-snippet
      "create table Strings (x TEXT);
       insert into Strings (x) values ('ciao');
       insert into Strings (x) values ('123');
       insert into Strings (x) values ('hello');
       insert into Strings (x) values ('456');
       select x as 'Match' from Strings
         where regexp('[a-z]+', x) = 1;")

    (let ((regexp  (make-sqlite3-function      regexp))
	  (exec-cb (make-sqlite3-exec-callback exec-cb)))
      (unwind-protect
	  (with-connection (conn)
	    (sqlite3-create-function conn "regexp" 2 SQLITE_ANY #f
				     regexp #f #f)
	    (let-values
		(((rv errmsg)
		  (sqlite3-exec conn sql-snippet exec-cb)))
	      rv))
	(ffi.free-c-callback regexp)
	(ffi.free-c-callback exec-cb)))
    ))

;;; --------------------------------------------------------------------

;;In this example aux data usage does NOT work because the regexp is the
;;result of a query.
(when #t
  (let ()
    (define-syntax with-connection
      (syntax-rules ()
	((_ (?connect-var) . ?body)
	 (let ((?connect-var (sqlite3-open ":memory:")))
	   (unwind-protect
	       (let () . ?body)
	     (when (sqlite3? ?connect-var)
	       (sqlite3-close ?connect-var)))))))

    (define (regexp context args)
      (define (%compile-rex context rex)
	(let ((cre (glibc.regcomp rex REG_EXTENDED)))
	  (sqlite3-set-auxdata context 0 cre
			       (make-sqlite3-auxdata-destructor %rex-destructor))
	  cre))
      (define (%rex-destructor rex)
	(glibc.regfree rex))
      (let* ((rex (sqlite3-value-text/string (vector-ref args 0)))
	     (str (sqlite3-value-text/string (vector-ref args 1)))
	     (cre (or (sqlite3-get-auxdata context 0)
		      (%compile-rex context rex))))
	(sqlite3-result-int context
			    (if (glibc.regexec cre str 0)
				1
			      0))))

    (define (exec-cb number-of-cols texts names)
      (printf "~a: ~a\n"
	      (utf8->string (vector-ref names 0))
	      (utf8->string (vector-ref texts 0)))
      #f)

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
         inner join Rexes on regexp(Rexes.x, Strings.y) = 1;")

    (let ((regexp  (make-sqlite3-function      regexp))
	  (exec-cb (make-sqlite3-exec-callback exec-cb)))
      (unwind-protect
	  (with-connection (conn)
	    (sqlite3-create-function conn "regexp" 2 SQLITE_ANY #f
				     regexp #f #f)
	    (let-values
		(((rv errmsg)
		  (sqlite3-exec conn sql-snippet exec-cb)))
	      rv))
	(ffi.free-c-callback regexp)
	(ffi.free-c-callback exec-cb)))
    ))


;;;; done


;;; end of file
;; Local Variables:
;; eval: (put 'with-connection 'scheme-indent-function 1)
;; eval: (put 'with-statement 'scheme-indent-function 1)
;; eval: (put 'with-blob 'scheme-indent-function 1)
;; End:
