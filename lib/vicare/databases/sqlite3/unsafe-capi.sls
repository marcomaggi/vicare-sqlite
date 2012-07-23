;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: unsafe interface to the C language API
;;;Date: Mon Jul 23, 2012
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
(library (vicare databases sqlite3 unsafe-capi)
  (export

    ;; version functions
    vicare-sqlite3-version-string
    vicare-sqlite3-version-interface-current
    vicare-sqlite3-version-interface-revision
    vicare-sqlite3-version-interface-age
    sqlite3-libversion
    sqlite3-sourceid


    )
  (import (vicare))


;;;; helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))


;;;; version functions

(define-inline (vicare-sqlite3-version-string)
  (foreign-call "vicare_sqlite3_version_string"))

(define-inline (vicare-sqlite3-version-interface-current)
  (foreign-call "vicare_sqlite3_version_interface_current"))

(define-inline (vicare-sqlite3-version-interface-revision)
  (foreign-call "vicare_sqlite3_version_interface_revision"))

(define-inline (vicare-sqlite3-version-interface-age)
  (foreign-call "vicare_sqlite3_version_interface_age"))

;;; --------------------------------------------------------------------

(define-inline (sqlite3-libversion)
  (foreign-call "ik_sqlite3_libversion"))

(define-inline (sqlite3-sourceid)
  (foreign-call "ik_sqlite3_sourceid"))


;;;; done

)

;;; end of file
