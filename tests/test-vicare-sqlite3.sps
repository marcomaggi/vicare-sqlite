;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/SQLite
;;;Contents: tests for Expat bindings
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare SQLite bindings\n")


;;;; helpers




(parametrise ((check-test-name	'version))

  (when #t
    (check-pretty-print
     `((Vicare/SQLite.version		,(vicare-sqlite3-version-string))
       (Vicare/SQLite.current		,(vicare-sqlite3-version-interface-current))
       (Vicare/SQLite.revision		,(vicare-sqlite3-version-interface-revision))
       (Vicare/SQLite.age		,(vicare-sqlite3-version-interface-age))
       (SQLite.libversion		,(sqlite3-libversion))
       (SQLite.libversion-number	,(sqlite3-libversion-number))
       (SQLite.sourceid			,(sqlite3-sourceid)))))


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

  (when #t
    (let loop ((i  0)
	       (op (sqlite3-compileoption-get 0)))
      (and op
	   (begin
	     (check-pretty-print (list 'compileoption i op))
	     (let ((i (+ 1 i)))
	       (loop i (sqlite3-compileoption-get i)))))))


  #t)


;;;; done

(check-report)

;;; end of file
