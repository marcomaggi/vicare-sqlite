;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: SQLite binding backend
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


#!vicare
#!(load-shared-library "vicare-sqlite3")
(library (vicare sqlite3)
  (export

    ;; version functions
    sqlite3_libversion

    )
  (import (vicare)
    (vicare sqlite3 constants)
    (vicare syntactic-extensions)
    (prefix (vicare ffi) ffi.)
    #;(prefix (vicare words) words.))


;;;; arguments validation

(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))

(define-argument-validation (pointer who obj)
  (ffi.pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

(define-argument-validation (callback who obj)
  (ffi.pointer? obj)
  (assertion-violation who "expected callback as argument" obj))

(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (false/bytevector who obj)
  (or (not obj) (bytevector? obj))
  (assertion-violation who "expected false or bytevector as argument" obj))

(define-argument-validation (pointer/bytevector who obj)
  (or (ffi.pointer? obj) (bytevector? obj))
  (assertion-violation who "expected pointer or bytevector as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (database who obj)
  (ffi.pointer? obj)
  (assertion-violation who "expected pointer to Sqlite database as argument" obj))


;;;; data structures

(define %sqlite-guardian
  (make-guardian))

(define (%sqlite-guardian-destructor)
  (do ((P (%sqlite-guardian) (%sqlite-guardian)))
      ((not P))
    #f
    #;(foreign-call "ik_sqlite_parser_free" P)))

;;; --------------------------------------------------------------------

;; (define-struct XML_ParsingStatus
;;   (parsing final-buffer?))

;; (define (%struct-XML_ParsingStatus-printer S port sub-printer)
;;   (define-inline (%display thing)
;;     (display thing port))
;;   (%display "#[sqlite:XML_ParsingStatus")
;;   (%display " parsing=")	(%display (let ((status (XML_ParsingStatus-parsing S)))
;; 					    (cond ((= status XML_INITIALIZED)
;; 						   "XML_INITIALIZED")
;; 						  ((= status XML_PARSING)
;; 						   "XML_PARSING")
;; 						  ((= status XML_FINISHED)
;; 						   "XML_FINISHED")
;; 						  ((= status XML_SUSPENDED)
;; 						   "XML_SUSPENDED")
;; 						  (else status))))
;;   (%display " final-buffer?=")	(%display (XML_ParsingStatus-final-buffer? S))
;;   (%display "]"))

;; (define-inline (XML_Content.children pointer index)
;;   (foreign-call "ik_sqlite_xml_content_children_ref" pointer index))


;;;; version functions

(define-inline (capi.sqlite3_libversion)
  (foreign-call "ik_sqlite3_libversion"))

;;; --------------------------------------------------------------------

(define (sqlite3_libversion)
  (latin1->string (capi.sqlite3_libversion)))


;;;; done

#;(set-rtd-printer! (type-descriptor XML_Content)       %struct-XML_Content-printer)

(post-gc-hooks (cons* %sqlite-guardian-destructor
		      (post-gc-hooks)))

)

;;; end of file
